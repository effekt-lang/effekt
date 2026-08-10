package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cpsds.*
import effekt.core.{Declaration, DeclarationContext, Id}
import effekt.util.UByte

import scala.collection.mutable

object TransformerCpsDs extends Transformer {

  val RUN_TOPLEVEL = js.Variable(JSName("RUN_TOPLEVEL"))
  val RESET = js.Variable(JSName("RESET"))
  val SHIFT = js.Variable(JSName("SHIFT"))
  val RESUME = js.Variable(JSName("RESUME"))

  // --- Context ---

  case class FunctionKind(isRecursive: Boolean, escapes: Boolean) {
    def isSecondClass: Boolean = !escapes
  }

  case class SecondClassDef(params: List[Id], isRecursive: Boolean)

  case class DispatchState(
    dispatch: Defunctionalization.ContinuationDispatch,
    continuation: JSName,
    arguments: Vector[JSName],
    entryLabel: JSName,
    applyLabel: JSName
  )

  case class TransformerContext(
    externs: Map[Id, cpsds.Extern.Def],
    kinds: Map[Id, FunctionKind],
    escaping: Set[Id],
    localVars: Set[Id],
    // Second-class defs currently in scope — maps id to param list and recursion info
    secondClass: Map[Id, SecondClassDef],
    // Whether we are inside the body (while loop) of a recursive second-class def
    insideBody: Set[Id],
    // Mutable second-class params currently in scope (from recursive second-class defs).
    // Any JS closure capturing these must snapshot them first to avoid capture-by-reference bugs.
    mutableParams: Set[Id],
    defunctionalization: Defunctionalization.Plan,
    stackSafety: StackSafety.Plan,
    dispatches: Map[Id, DispatchState],
    dispatchAliases: Map[Id, Defunctionalization.ContinuationDispatch],
    renamedCaptures: Map[Id, Id],
    applying: Set[Id],
    declarations: DeclarationContext,
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors

  /** JavaScript labels and the mutable parameters implementing them are local
   *  to one JavaScript function. They cannot be referenced from a nested
   *  function, even when they remain in lexical scope in the CPS tree.
   */
  def functionBodyContext(using ctx: TransformerContext): TransformerContext =
    ctx.copy(
      secondClass = Map.empty,
      insideBody = Set.empty,
      mutableParams = Set.empty,
      dispatches = Map.empty,
      dispatchAliases = Map.empty,
      applying = Set.empty
    )

  def computeKinds(m: cpsds.ModuleDecl): Map[Id, FunctionKind] = {
    val uses = m.uses.toMap
    val escape = cpsds.escapeAnalysis.escapes(m)
    uses.map { case (id, callees) =>
      id -> FunctionKind(
        isRecursive = callees.contains(id),
        escapes = escape.contains(id)
      )
    }
  }

  def kindOf(id: Id)(using ctx: TransformerContext): FunctionKind =
    ctx.kinds.getOrElse(id, FunctionKind(isRecursive = false, escapes = true))

  /** Reference the runtime value of an identifier. Continuation cases and
   *  nested JavaScript functions sometimes bind a stable snapshot under a
   *  fresh name; binding occurrences deliberately continue to use the
   *  original identifier.
   */
  def valueRef(id: Id)(using ctx: TransformerContext): js.Expr =
    nameRef(ctx.renamedCaptures.getOrElse(id, id))

  // --- Backup mutable params for closures ---

  /**
   * Backup mutable second-class params that are free in the given body,
   * returning the backup statements and the renaming to use while emitting
   * the body.
   *
   * This prevents capture-by-reference bugs: JS `let` variables inside a
   * `while` loop are captured by reference, so closures defined inside the
   * loop body would see the mutated value rather than the value at definition time.
   * Keeping the CPSDS body unchanged also preserves the identity of analyzed
   * call sites.
   */
  def backupMutableParams(body: cpsds.Stmt, boundParams: Set[Id] = Set.empty)(using ctx: TransformerContext): (List[js.Stmt], Map[Id, Id]) = {
    val freeInBody = body.free -- boundParams
    val captured = freeInBody.intersect(ctx.mutableParams)

    if captured.nonEmpty then
      val backups = captured.toList.sortBy(_.id).map { p =>
        val tmp = Id(s"backup_${p}")
        (p, tmp)
      }
      val backupStmts = backups.map { case (p, tmp) =>
        js.Const(nameDef(tmp), valueRef(p))
      }
      (backupStmts, backups.toMap)
    else
      (Nil, Map.empty)
  }

  // --- Entry points ---

  def compile(input: cpsds.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): js.Module = {
    resetNames()
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil,
      js.Return(Call(RUN_TOPLEVEL, nameRef(mainSymbol))))))
    given DeclarationContext = new DeclarationContext(coreModule.declarations, coreModule.externs)
    toJS(input, exports)
  }

  def compileLSP(input: cpsds.ModuleDecl, coreModule: core.ModuleDecl)(using C: Context): List[js.Stmt] =
    ???

  def toJS(module: cpsds.ModuleDecl, exports: List[js.Export])(using D: DeclarationContext, C: Context): js.Module =
    module match {
      case cpsds.ModuleDecl(includes, declarations, externs, definitions, _) =>
        val kinds = computeKinds(module)
        val targetFlows = definitions.map(GuardedEquality.targets).toVector
        val defunctionalization = Defunctionalization.analyze(
          module,
          id => kinds.get(id).exists(_.isRecursive),
          id => kinds.get(id).exists(_.isSecondClass),
          targetFlows)
        val stackSafety = StackSafety.analyze(
          module,
          id => kinds.get(id).exists(_.isRecursive),
          id => kinds.get(id).exists(_.isSecondClass),
          defunctionalization,
          targetFlows)
        given ctx: TransformerContext = TransformerContext(
          externs.collect { case d: cpsds.Extern.Def => (d.id, d) }.toMap,
          kinds,
          cpsds.escapeAnalysis.escapes(module),
          Set.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          defunctionalization,
          stackSafety,
          Map.empty,
          Map.empty,
          Map.empty,
          Set.empty,
          D, C)

        val name = JSName(jsModuleName("main"))
        val jsExterns = module.externs.filterNot(canInline).map(toJS)
        val jsDecls = module.declarations.flatMap(toJSDecl)
        val stmts = module.definitions.map(toJSToplevel)

        js.Module(name, Nil, exports, jsDecls ++ jsExterns ++ stmts)
    }

  // --- Toplevel ---

  def toJSToplevel(d: cpsds.ToplevelDefinition)(using ctx: TransformerContext): js.Stmt = d match {
    case cpsds.ToplevelDefinition.Def(id, params, body) =>
      val kind = kindOf(id)
      js.Function(nameDef(id), params.map(nameDef),
        secondClassDef(id, params, body, None, kind.isRecursive).stmts)

    case cpsds.ToplevelDefinition.Val(id, ks, k, binding) =>
      js.Const(nameDef(id), Call(RUN_TOPLEVEL, js.Lambda(List(nameDef(ks), nameDef(k)), toJS(binding).stmts)))
  }

  // --- Externs ---

  def toJS(e: cpsds.Extern)(using C: TransformerContext): js.Stmt = e match {
    case cpsds.Extern.Def(id, params, true, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          val ks = freshName("ks_")
          val k = freshName("k_")
          js.Function(nameDef(id), params.map(nameDef) ++ List(ks, k),
            List(js.Return(js.Call(toJSTemplate(contents), List(js.Variable(ks), js.Variable(k))))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), params.map(nameDef), List(js.Return($effekt.call("unreachable"))))
      }

    case cpsds.Extern.Def(id, params, false, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          js.Function(nameDef(id), params.map(nameDef), List(js.Return(toJSTemplate(contents))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), params.map(nameDef), List(js.Return($effekt.call("unreachable"))))
      }

    case cpsds.Extern.Include(_, contents) =>
      js.RawStmt(contents)
  }

  def toJSTemplate(t: Template[cpsds.Expr])(using TransformerContext): js.Expr =
    js.RawExpr(t.strings, t.args.map(toJS))

  def canInline(extern: cpsds.Extern): Boolean = extern match {
    case cpsds.Extern.Def(_, _, false, ExternBody.StringExternBody(_, _)) => true
    case _ => false
  }

  def inlineExtern(id: Id, args: List[cpsds.Expr])(using T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cpsds.Extern.Def(_, params, false, ExternBody.StringExternBody(_, Template(strings, templateArgs)))) =>
        val subst = params.zip(args).toMap
        val resolvedArgs = templateArgs.map {
          case tArg @ Expr.Variable(id) => subst.get(id) match {
            case Some(replaced) => toJS(replaced)
            case None => toJS(tArg)
          }
          case other => toJS(other)
        }
        js.RawExpr(strings, resolvedArgs)
      case _ => js.Call(nameRef(id), args.map(toJS))
    }


  // --- Declarations ---

  def toJSDecl(d: core.Declaration): List[js.Stmt] = d match {
    case core.Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }
    case core.Interface(id, tparams, operations) =>
      Nil
  }

  // --- Expressions ---

  def toJS(e: cpsds.Expr)(using ctx: TransformerContext): js.Expr = e match {
    case Expr.Variable(id) => valueRef(id)
    case Expr.Literal((), core.Type.TUnit) => $effekt.field("unit")
    case Expr.Literal(s: String, core.Type.TString) => JsString(escape(s))
    case Expr.Literal(b: Byte, core.Type.TByte) => js.RawExpr(UByte.unsafeFromByte(b).toHexString)
    case Expr.Literal(value, _) => js.RawExpr(value.toString)
    case Expr.Make(data, tag, vargs) => js.New(nameRef(tag), vargs.map(toJS))
    case Expr.Abort => js.Undefined
    case Expr.Return => js.Undefined
    case Expr.Toplevel => js.Undefined
  }

  // --- Statements ---

  def toJS(s: cpsds.Stmt)(using ctx: TransformerContext): Binding[List[js.Stmt]] = s match {

    // --- Def ---
    case cpsds.Stmt.Def(id, params, body, rest) =>
      ctx.defunctionalization.caseOf(id) match {
        case Some(continuationCase) =>
          Binding { k =>
            val properties = (`tag` -> js.RawExpr(continuationCase.tag.toString)) +:
              continuationCase.captures.map { capture =>
                memberNameRef(capture) ->
                  valueRef(capture)
              }
            js.Const(nameDef(id), js.Object(properties.toList)) :: toJS(rest).run(k)
          }

        case None =>
          val kind = kindOf(id)
          if kind.isSecondClass then
            secondClassDef(id, params, body, Some(rest), kind.isRecursive)
          else
            firstClassDef(id, params, body, rest, kind.isRecursive)
      }

    // --- New ---
    case cpsds.Stmt.New(id, interface, operations, rest) =>
      Binding { k =>
        val ops = operations.map { op =>
          val (backups, renamings) = backupMutableParams(op.body, op.params.toSet)
          val bodyCtx = functionBodyContext.copy(
            renamedCaptures = ctx.renamedCaptures ++ renamings)
          val body = toJS(op.body)(using bodyCtx).stmts
          (backups, nameDef(op.name) -> js.Lambda(op.params.map(nameDef), body))
        }
        val allBackups = ops.flatMap(_._1)
        val jsObj = js.Object(ops.map(_._2))
        allBackups ++ List(js.Const(nameDef(id), jsObj)) ++ toJS(rest).run(k)
      }

    // --- Let ---
    case cpsds.Stmt.Let(id, binding, rest) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJS(rest).run(k)
      }

    case app @ cpsds.Stmt.App(id, args, direct) =>
      ctx.dispatchAliases.get(id).orElse(ctx.defunctionalization.dispatchForCallee(id)) match {
        case Some(dispatch) => dispatchCall(id, args, dispatch)
        case None => ctx.secondClass.get(id) match {
        case Some(sci) =>
          // Second-class call: assign args to params, then jump.
          // Need temporaries for params that appear free in later arguments
          // to avoid overwriting values before they're read.
          val stmts = mutable.ListBuffer.empty[js.Stmt]

          val freeInArgs = args.flatMap(_.free).toSet
          val paramSet = sci.params.toSet
          val overlapping = freeInArgs.intersect(paramSet)

          val tmpMap = overlapping.map { param =>
            val tmp = Id(s"tmp_${param}")
            stmts.append(js.Const(nameDef(tmp), valueRef(param)))
            param -> tmp
          }.toMap

          val subst = substitutions.Substitution(
            tmpMap.map { case (p, t) => p -> Expr.Variable(t) }
          )

          sci.params.zip(args).foreach { case (param, arg) =>
            val jsArg = toJS(substitutions.substitute(arg)(using subst))
            stmts.append(js.Assign(nameRef(param), jsArg))
          }

          val jump = if sci.isRecursive && ctx.insideBody.contains(id) then
            js.Continue(Some(nameDef(id)))
          else
            js.Break(Some(nameDef(id)))
          stmts.append(jump)

          pure(stmts.toList)

        // In the App case for first-class calls:
        case None =>
          val call = js.Call(valueRef(id), args.map(toJS))
          ctx.stackSafety.transferOf(app) match {
            case StackSafety.Transfer.Direct => pure(js.Return(call) :: Nil)
            case StackSafety.Transfer.Jump | StackSafety.Transfer.Bounce =>
              pure(js.Return(js.Lambda(Nil, js.Return(call))) :: Nil)
          }
        }
      }

    // --- Invoke ---
    case invoke @ cpsds.Stmt.Invoke(id, method, args) =>
      val call = MethodCall(valueRef(id), memberNameRef(method), args.map(toJS): _*)
      ctx.stackSafety.transferOf(invoke) match {
        case StackSafety.Transfer.Direct => pure(js.Return(call) :: Nil)
        case StackSafety.Transfer.Jump | StackSafety.Transfer.Bounce =>
          pure(js.Return(js.Lambda(Nil, js.Return(call))) :: Nil)
      }

    // --- Run ---
    case cpsds.Stmt.Run(id, callee, args, Purity.Pure | Purity.Impure, rest) =>
      Binding { k =>
        js.Const(nameDef(id), inlineExtern(callee, args)) :: toJS(rest).run(k)
      }

    // Async: needs CPS — call with continuation
    case cpsds.Stmt.Run(id, callee, args, Purity.Async, rest) =>
      ???
    //      val ks = JSName("ks")
    //      val kParam = JSName("k")
    //      pure(js.Return(js.Call(nameRef(callee),
    //        args.map(toJS) ++ List(
    //          // TODO: where do ks and k come from in this context?
    //          // For now, pass a continuation that binds the result and continues
    //          js.Variable(ks),
    //          js.Lambda(List(nameDef(id)), toJS(rest).stmts)
    //        ))) :: Nil)

    // --- If ---
    case cpsds.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJS(thn).block, toJS(els).block) :: Nil)

    // --- Match ---
    case cpsds.Stmt.Match(sc, Nil, None) =>
      pure(js.Return($effekt.call("unreachable")) :: Nil)

    case cpsds.Stmt.Match(sc, List((tag, clause)), None) =>
      val scrutinee = toJS(sc)
      val (_, stmts) = toJSClause(scrutinee, tag, clause)
      stmts

    case cpsds.Stmt.Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)
      pure(js.Switch(js.Member(scrutinee, `tag`),
        clauses.map { case (tag, clause) =>
          val (e, binding) = toJSClause(scrutinee, tag, clause)
          val stmts = binding.stmts
          stmts.lastOption match {
            case Some(_: (js.Stmt.Return | js.Stmt.Break | js.Stmt.Continue)) => (e, stmts)
            case _ => (e, stmts :+ js.Break(None))
          }
        },
        default.map(s => toJS(s).stmts)) :: Nil)

    // --- Region ---
    case cpsds.Stmt.Region(id, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("newRegion"))) ::
          toJS(rest).run(k)
      }

    // --- Alloc ---
    case cpsds.Stmt.Alloc(id, init, region, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(valueRef(region), JSName("fresh"), toJS(init))) ::
          toJS(rest).run(k)
      }

    // --- Var ---
    case cpsds.Stmt.Var(id, init, ks, rest) if !ctx.escaping.contains(id) =>
      Binding { k =>
        js.Let(nameDef(id), toJS(init)) ::
          toJS(rest)(using ctx.copy(localVars = ctx.localVars + id)).run(k)
      }

    case cpsds.Stmt.Var(id, init, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("fresh"), toJS(init))) ::
          toJS(rest).run(k)
      }

    // --- Dealloc ---
    case cpsds.Stmt.Dealloc(ref, rest) =>
      toJS(rest)

    // --- Get ---
    case cpsds.Stmt.Get(ref, id, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Const(nameDef(id), valueRef(ref)) :: toJS(rest).run(k)
      }

    case cpsds.Stmt.Get(ref, id, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Member(valueRef(ref), JSName("value"))) ::
          toJS(rest).run(k)
      }

    // --- Put ---
    case cpsds.Stmt.Put(ref, value, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Assign(valueRef(ref), toJS(value)) :: toJS(rest).run(k)
      }

    case cpsds.Stmt.Put(ref, value, rest) =>
      Binding { k =>
        js.ExprStmt(js.MethodCall(valueRef(ref), JSName("set"), toJS(value))) ::
          toJS(rest).run(k)
      }

    // --- Reset ---
    case cpsds.Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(p, ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          Call(RESET, toJS(ks1), toJS(k1))) ::
          toJS(body).run(next)
      }

    // --- Shift ---
    case cpsds.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(resume, ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          Call(SHIFT, valueRef(prompt), toJS(ks1), toJS(k1))) ::
          toJS(body).run(next)
      }

    // --- Resume ---
    case cpsds.Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          Call(RESUME, valueRef(r), toJS(ks1), toJS(k1))) ::
          toJS(body).run(next)
      }

    // --- Hole ---
    case cpsds.Stmt.Hole(span) =>
      pure(js.Return($effekt.call("hole", JsString(span.range.from.format))) :: Nil)
  }

  // --- Defunctionalized continuations ---

  private def dispatchCall(
    callee: Id,
    args: List[cpsds.Expr],
    dispatch: Defunctionalization.ContinuationDispatch
  )(using ctx: TransformerContext): Binding[List[js.Stmt]] = {
    val state = ctx.dispatches.get(dispatch.entry)
      .orElse(ctx.applying.iterator.flatMap(ctx.dispatches.get)
        .find(_.dispatch.targets == dispatch.targets))
      .orElse(ctx.dispatches.values.find(_.dispatch.targets == dispatch.targets))
      .getOrElse(sys.error(s"Continuation dispatch for ${dispatch.entry} is not in scope"))

    // Evaluate everything before changing a register. Case bodies can refer to
    // the old continuation and argument registers through their local bindings.
    val nextContinuation = freshName("next_cont_")
    val nextArguments = args.map(_ => freshName("next_arg_")).toVector
    val evaluate =
      js.Const(nextContinuation, valueRef(callee)) +:
        args.zip(nextArguments).map { case (argument, temporary) =>
          js.Const(temporary, toJS(argument))
        }
    val assign =
      js.Assign(js.Variable(state.continuation), js.Variable(nextContinuation)) +:
        state.arguments.zip(nextArguments).map { case (register, temporary) =>
          js.Assign(js.Variable(register), js.Variable(temporary))
        }
    val jump =
      if ctx.applying.contains(state.dispatch.entry) then
        js.Continue(Some(state.applyLabel))
      else
        js.Break(Some(state.entryLabel))

    pure((evaluate ++ assign :+ jump).toList)
  }

  private def dispatchLoop(
    state: DispatchState,
    ctx: TransformerContext
  ): js.Stmt = {
    val scrutinee = js.Member(js.Variable(state.continuation), `tag`)
    val boundary = Option.when(state.dispatch.boundary) {
      val target = freshName("boundary_continuation_")
      js.RawExpr(Defunctionalization.BoundaryTag.toString) -> List(js.Block(None, List(
        js.Const(target,
          js.Member(js.Variable(state.continuation), memberNameRef(state.dispatch.callee))),
        js.Return(js.Lambda(Nil,
          js.Return(js.Call(js.Variable(target), state.arguments.map(js.Variable(_)).toList))))
      )))
    }
    val localCases = state.dispatch.cases.map { continuationCase =>
      val captureRenamings = continuationCase.captures
        .filterNot(ctx.secondClass.contains)
        .map { capture =>
          capture -> Id(s"frame_${capture.name}")
        }
      val parameters = continuationCase.params.zip(state.arguments).map {
        case (parameter, register) =>
          js.Const(nameDef(parameter), js.Variable(register))
      }
      val captures = captureRenamings.map { case (capture, local) =>
        js.Const(nameDef(local),
          js.Member(js.Variable(state.continuation), memberNameRef(capture)))
      }
      val aliases = captureRenamings.flatMap { case (capture, _) =>
        ctx.defunctionalization.dispatchForCallee(capture).map(capture -> _)
      }.toMap
      val caseCtx = ctx.copy(
        applying = ctx.applying + state.dispatch.entry,
        dispatchAliases = ctx.dispatchAliases ++ aliases,
        renamedCaptures = ctx.renamedCaptures ++ captureRenamings)
      val body = toJS(continuationCase.body)(using caseCtx).stmts
      js.RawExpr(continuationCase.tag.toString) ->
        List(js.Block(None, (parameters ++ captures).toList ++ body))
    }.toList
    val branches = boundary.toList ++ localCases

    js.While(
      Some(state.applyLabel),
      js.RawExpr("true"),
      List(js.Switch(scrutinee, branches,
        Some(List(js.Return($effekt.call("unreachable")))))))
  }

  private def dispatchState(dispatch: Defunctionalization.ContinuationDispatch): DispatchState =
    DispatchState(
      dispatch,
      freshName("cont_"),
      Vector.tabulate(dispatch.arity)(_ => freshName("value_")),
      freshName("apply_entry_"),
      freshName("apply_"))

  private def dispatchDeclarations(state: DispatchState): List[js.Stmt] =
    js.Let(state.continuation, js.Undefined) ::
      state.arguments.map(argument => js.Let(argument, js.Undefined)).toList

  /** Preserve an arbitrary continuation entering a first-class recursive
   *  function as the distinguished boundary frame. */
  private def boundaryFrame(state: DispatchState): List[js.Stmt] =
    if state.dispatch.boundary then
      val continuation = nameRef(state.dispatch.callee)
      List(js.Assign(continuation, js.IfExpr(
        js"typeof ${continuation} === \"function\"",
        js.Object(List(
          `tag` -> js.RawExpr(Defunctionalization.BoundaryTag.toString),
          memberNameRef(state.dispatch.callee) -> continuation)),
        continuation)))
    else Nil

  private def recursiveBody(
    state: Option[DispatchState],
    body: List[js.Stmt],
    ctx: TransformerContext
  ): List[js.Stmt] =
    state.fold(body) { dispatch =>
      List(
        js.Block(Some(dispatch.entryLabel), body),
        dispatchLoop(dispatch, ctx))
    }

  // --- First-class Def ---

  def firstClassDef(id: Id, params: List[Id], body: cpsds.Stmt, rest: cpsds.Stmt, isRecursive: Boolean)(using ctx: TransformerContext): Binding[List[js.Stmt]] =
    Binding { k =>
      val (backups, renamings) = backupMutableParams(body, params.toSet)

      val state = ctx.defunctionalization.dispatchFor(id).map(dispatchState)

      val functionCtx = functionBodyContext
      val recursiveCtx = if isRecursive then
        functionCtx.copy(
          secondClass = Map(id -> SecondClassDef(params, isRecursive = true)),
          insideBody = Set(id),
          mutableParams = params.toSet
        )
      else functionCtx
      val bodyCtx = state.fold(recursiveCtx) { dispatch =>
        recursiveCtx.copy(dispatches = Map(id -> dispatch))
      }.copy(renamedCaptures = recursiveCtx.renamedCaptures ++ renamings)

      val translatedBody = toJS(body)(using bodyCtx).stmts
      val bodyStmts = if isRecursive then
        state.toList.flatMap(dispatchDeclarations) ++
          state.toList.flatMap(boundaryFrame) ++
          List(js.While(Some(nameDef(id)), RawExpr("true"),
            recursiveBody(state, translatedBody, bodyCtx)))
      else translatedBody

      backups ++
        List(js.Function(nameDef(id), params.map(nameDef), bodyStmts)) ++
        toJS(rest).run(k)
    }

  // --- Second-class Def ---

  /**
   * Non-recursive:
   *   let params...;
   *   id: { [[rest]] }        // call: params = args; break id
   *   [[body]]
   *
   * Recursive:
   *   let params...;
   *   id: { [[rest]] }        // initial call: params = args; break id
   *   id: while (true) {      // recursive call: params = args; continue id
   *     [[body]]
   *   }
   *
   * If `rest` is defined, this is a local function definition. Otherwise it is a toplevel function and the params
   * do not need to be initialized.
   */
  def secondClassDef(id: Id, params: List[Id], body: cpsds.Stmt, rest: Option[cpsds.Stmt], isRecursive: Boolean)(using ctx: TransformerContext): Binding[List[js.Stmt]] = {
    val label = nameDef(id)
    val sci = SecondClassDef(params, isRecursive)

    // Register this def as second-class for nested code
    val ctxWithDef = ctx.copy(secondClass = ctx.secondClass + (id -> sci))

    // Translate rest: calls to id will become assignments + break
    val entryBlock = rest.map { r =>
      js.Block(Some(label), toJS(r)(using ctxWithDef).stmts)
    }

    val state = ctx.defunctionalization.dispatchFor(id).map(dispatchState)

    // Translate body: for recursive defs, calls to id will become assignments + continue.
    // Also track params as mutable so that closures inside the body will backup them.
    val recursiveCtx = if isRecursive then
      ctxWithDef.copy(
        insideBody = ctxWithDef.insideBody + id,
        mutableParams = ctxWithDef.mutableParams ++ params.toSet
      )
    else ctxWithDef
    val bodyCtx = state.fold(recursiveCtx) { dispatch =>
      recursiveCtx.copy(dispatches = recursiveCtx.dispatches + (id -> dispatch))
    }
    val bodyStmts = toJS(body)(using bodyCtx).stmts

    val paramDecls = if rest.isDefined then params.map(p => js.Let(nameDef(p), js.Undefined)) else Nil
    val dispatchDecls = state.toList.flatMap(dispatchDeclarations)
    val boundary = state.toList.flatMap(boundaryFrame)

    if isRecursive then
      // A local labeled entry assigns its parameters in `entryBlock`; only
      // then can an unknown incoming continuation be wrapped as a frame.
      pure(paramDecls ++ dispatchDecls ++ entryBlock ++ boundary ++ List(
        js.While(Some(label), RawExpr("true"), recursiveBody(state, bodyStmts, bodyCtx))
      ))
    else
      pure(paramDecls ++ dispatchDecls ++ entryBlock ++ bodyStmts)
  }

  // --- Pattern matching ---

  def toJSClause(scrutinee: js.Expr, variant: Id, clause: cpsds.Clause)(using C: TransformerContext): (js.Expr, Binding[List[js.Stmt]]) =
    clause match {
      case cpsds.Clause(params, body) =>
        val fields = C.declarations.getConstructor(variant).fields.map(_.id)
        val tag = js.RawExpr(C.declarations.getConstructorTag(variant).toString)

        val extractedFields = params.zip(fields).map { case (p, f) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tag, Binding { k => extractedFields ++ toJS(body).run(k) })
    }
}
