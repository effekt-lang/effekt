package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cps.*
import effekt.core.{Declaration, DeclarationContext, Id}
import effekt.util.UByte

import scala.collection.mutable

object TransformerCps extends Transformer {

  val RUN_TOPLEVEL = js.Variable(JSName("RUN_TOPLEVEL"))
  val RESET = js.Variable(JSName("RESET"))
  val SHIFT = js.Variable(JSName("SHIFT"))
  val RESUME = js.Variable(JSName("RESUME"))
  val BOUNDARY_CONTINUATION = JSName("__boundary")

  case class SecondClassDef(params: List[Id], isRecursive: Boolean)

  case class DispatchState(
    dispatch: Defunctionalization.ContinuationDispatch,
    continuation: JSName,
    arguments: Vector[JSName],
    entryLabel: JSName,
    applyLabel: JSName
  )

  case class TransformerContext(
    externs: Map[Id, cps.Extern.Def],
    kinds: Map[Id, DefinitionPlanning.Kind],
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
    callingConvention: CallingConvention.Plan,
    workers: Map[Id, JSName],
    directWorkers: Map[Id, JSName],
    segmentFlow: SegmentEntries.Plan,
    // Continuations obtained by leaving or entering a delimited continuation
    // segment. Applying one crosses a segment boundary and therefore bounces.
    segmentEntries: Set[Id],
    dispatches: Map[Id, DispatchState],
    dispatchAliases: Map[Id, Defunctionalization.ContinuationDispatch],
    renamedCaptures: Map[Id, Id],
    // Function values carried by these variables use the direct ABI.
    directParameters: Map[Id, Int],
    applying: Set[Id],
    directBody: Option[(Id, List[Id])],
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
      applying = Set.empty,
      directBody = None
    )

  def computePlan(m: cps.ModuleDecl): DefinitionPlanning.Plan =
    DefinitionPlanning.analyze(
      m,
      m.definitions.map(cps.GuardedEquality.targets).toVector)

  def kindOf(id: Id)(using ctx: TransformerContext): DefinitionPlanning.Kind =
    ctx.kinds.getOrElse(id,
      DefinitionPlanning.Kind(isRecursive = false, isFirstClass = true))

  /** Reference the runtime value of an identifier. Continuation cases and
   *  nested JavaScript functions sometimes bind a stable snapshot under a
   *  fresh name; binding occurrences deliberately continue to use the
   *  original identifier.
   */
  def valueRef(id: Id)(using ctx: TransformerContext): js.Expr =
    nameRef(ctx.renamedCaptures.getOrElse(id, id))

  /** A statically known direct call can bypass the stack-safe entry exposed
   *  as a function value and enter its worker immediately. */
  def directRef(id: Id)(using ctx: TransformerContext): js.Expr =
    ctx.workers.get(id).fold(valueRef(id))(js.Variable.apply)

  /** Entry whose JavaScript result is an ordinary value, never a trampoline
   *  thunk. This is deliberately separate from `directRef`, which is an
   *  immediate entry into the CPS calling convention. */
  def directResultRef(id: Id)(using ctx: TransformerContext): js.Expr =
    ctx.directWorkers.get(id).fold(valueRef(id))(js.Variable.apply)

  /** The public value-level entry resets the native stack before entering the
   *  worker. Its arguments are evaluated and captured exactly once. */
  private def safeEntry(id: Id, params: List[Id], worker: JSName): js.Stmt = {
    val call = js.Call(js.Variable(worker), params.map(nameRef))
    js.Function(
      nameDef(id),
      params.map(nameDef),
      List(js.Return(js.Lambda(Nil, call))))
  }

  /** Adapt a CPS function value to the value-returning ABI. */
  private def toDirectFunction(callee: js.Expr, arity: Int): js.Expr = {
    val arguments = List.fill(arity)(freshName("arg_"))
    val ks = freshName("ks_")
    val k = freshName("k_")
    val computation = js.Lambda(
      List(ks, k),
      js.Return(js.Call(
        callee,
        arguments.map(js.Variable.apply) ++ List(js.Variable(ks), js.Variable(k)))))
    js.Lambda(
      arguments,
      js.Return(js.Call(RUN_TOPLEVEL, List(computation))))
  }

  /** Adapt a value-returning function to the stack-safe CPS ABI. */
  private def toCpsFunction(callee: js.Expr, arity: Int): js.Expr = {
    val arguments = List.fill(arity)(freshName("arg_"))
    val ks = freshName("ks_")
    val k = freshName("k_")
    val result = freshName("result_")
    val suspended = js.Lambda(Nil, js.Block(None, List(
      js.Const(result, js.Call(callee, arguments.map(js.Variable.apply))),
      js.Return(js.Call(
        js.Variable(k),
        List(js.Variable(result), js.Variable(ks)))))))
    js.Lambda(arguments ++ List(ks, k), js.Return(suspended))
  }

  /** Coerce a function value to the direct ABI expected at a compositional
   *  call. Known direct definitions and direct parameters need no wrapper;
   *  an ordinary CPS value is run to completion locally. */
  private def toDirectFunctionValue(value: cps.Expr, arity: Int)(using ctx: TransformerContext): js.Expr = value match {
    case cps.Expr.Variable(id) if ctx.callingConvention.isDirect(id) =>
      val actual = ctx.callingConvention.original(id).params.size - 2
      require(actual == arity, s"Direct function $id has arity $actual, expected $arity")
      directResultRef(id)
    case cps.Expr.Variable(id) if ctx.directParameters.contains(id) =>
      val actual = ctx.directParameters(id)
      require(actual == arity, s"Direct parameter $id has arity $actual, expected $arity")
      valueRef(id)
    case _ =>
      toDirectFunction(toValueJS(value), arity)
  }

  /** CPS-facing entry for a value-returning worker. The suspension is the
   *  representation boundary: unknown callers remain stack safe, while every
   *  statically selected call bypasses this adapter. */
  private def directAdapter(id: Id, params: List[Id], worker: JSName)(using ctx: TransformerContext): js.Stmt = {
    val List(ks, k) = ctx.callingConvention.original(id).params.takeRight(2).map(nameDef): @unchecked
    val result = freshName("result_")
    val workerArguments = params.zipWithIndex.map { case (param, position) =>
      ctx.callingConvention.directParameterArity(id, position)
        .fold(nameRef(param))(toDirectFunction(nameRef(param), _))
    }
    val workerCall = js.Call(js.Variable(worker), workerArguments)
    val resume = js.Call(js.Variable(k), List(js.Variable(result), js.Variable(ks)))
    val suspended = js.Lambda(Nil, js.Block(None, List(
      js.Const(result, workerCall),
      js.Return(resume))))
    js.Function(
      nameDef(id),
      params.map(nameDef) ++ List(ks, k),
      List(js.Return(suspended)))
  }

  private def directImplementation(
    id: Id,
    params: List[Id],
    body: cps.Stmt,
    renamings: Map[Id, Id] = Map.empty
  )(using ctx: TransformerContext): List[js.Stmt] = {
    val directParams = params
    val parameterArities = directParams.zipWithIndex.flatMap { case (param, position) =>
      ctx.callingConvention.directParameterArity(id, position).map(param -> _)
    }.toMap
    val bodyCtx = functionBodyContext.copy(
      mutableParams = directParams.toSet,
      renamedCaptures = ctx.renamedCaptures ++ renamings,
      directParameters = ctx.directParameters ++ parameterArities,
      directBody = Some((id, directParams)))
    val translated = toJS(body)(using bodyCtx).stmts
    if ctx.callingConvention.isTailRecursive(id) then
      List(js.While(Some(nameDef(id)), js.RawExpr("true"), translated))
    else translated
  }

  private def directDefinitions(
    id: Id,
    params: List[Id],
    body: cps.Stmt,
    renamings: Map[Id, Id] = Map.empty
  )(using ctx: TransformerContext): List[js.Stmt] = {
    val worker = ctx.directWorkers(id)
    val implementation = directImplementation(id, params, body, renamings)
    val definition = js.Function(worker, params.map(nameDef), implementation)
    if ctx.callingConvention.needsCpsEntry(id) then
      List(definition, directAdapter(id, params, worker))
    else List(definition)
  }

  /**
   * Backup mutable second-class params that are free in the given body,
   * returning the backup statements and the renaming to use while emitting
   * the body.
   *
   * This prevents capture-by-reference bugs: JS `let` variables inside a
   * `while` loop are captured by reference, so closures defined inside the
   * loop body would see the mutated value rather than the value at definition time.
   * Keeping the CPS body unchanged also preserves the identity of analyzed
   * call sites.
   */
  def backupMutableParams(body: cps.Stmt, boundParams: Set[Id] = Set.empty)(using ctx: TransformerContext): (List[js.Stmt], Map[Id, Id]) = {
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

  def compile(input: cps.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): js.Module = {
    resetNames()
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil,
      js.Return(js.Call(RUN_TOPLEVEL, nameRef(mainSymbol))))))
    given DeclarationContext = new DeclarationContext(coreModule.declarations, coreModule.externs)
    toJS(input, exports)
  }

  def compileLSP(input: cps.ModuleDecl, coreModule: core.ModuleDecl)(using C: Context): List[js.Stmt] =
    ???

  def toJS(module: cps.ModuleDecl, exports: List[js.Export])(using D: DeclarationContext, C: Context): js.Module = {
    val conventionFlows = module.definitions.map(GuardedEquality.targets).toVector
    val callingConvention = CallingConvention.analyze(module, conventionFlows)
    val lowered = cps.Inliner.transformDirectCalls(
      CallingConvention.lower(module, callingConvention),
      callingConvention.directDefinitions.filter(callingConvention.isFirstOrder))

    lowered match {
      case cps.ModuleDecl(includes, declarations, externs, definitions, _) =>
        val targetFlows = definitions.map(GuardedEquality.targets).toVector
        val liveDefinitions = lowered.uses.toMap.keySet
        val liveDirect = callingConvention.directDefinitions.intersect(liveDefinitions)

        // A uniquely used non-recursive direct definition has already
        // commuted into its call site above. Remaining direct definitions
        // need a JavaScript function only when they have multiple entries or
        // recursion; ordinary value escape is discovered by DefinitionPlanning
        // itself. This keeps calling convention and representation separate.
        val directFunctionRequirements = liveDirect.filter { id =>
          lowered.refs.getOrElse(id, 0) != 1
        }
        val representations = DefinitionPlanning.analyze(
          lowered,
          targetFlows,
          directFunctionRequirements,
          liveDirect)
        val kinds = representations.kinds
        val defunctionalization = representations.defunctionalization
        val stackSafety = StackSafety.analyze(
          lowered,
          id => kinds.get(id).exists(_.isRecursive),
          id => kinds.get(id).exists(_.isSecondClass),
          defunctionalization,
          targetFlows)
        val segmentFlow = SegmentEntries.analyze(lowered, targetFlows)
        val workers = (stackSafety.safeEntries.definitions -- liveDirect).toVector
          .sortBy(id => (id.name.name, id.id))
          .map(id => id -> freshName("worker_"))
          .toMap
        val directWorkers = liveDirect.toVector
          .sortBy(id => (id.name.name, id.id))
          .map(id => id -> freshName("direct_"))
          .toMap
        given ctx: TransformerContext = TransformerContext(
          externs.collect { case d: cps.Extern.Def => (d.id, d) }.toMap,
          kinds,
          lowered.escapes,
          Set.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          defunctionalization,
          stackSafety,
          callingConvention,
          workers,
          directWorkers,
          segmentFlow,
          segmentFlow.entries,
          Map.empty,
          Map.empty,
          Map.empty,
          Map.empty,
          Set.empty,
          None,
          D, C)

        val name = JSName(jsModuleName("main"))
        val jsExterns = externs.filterNot(canInline).map(toJS)
        val jsDecls = declarations.flatMap(toJSDecl)
        val stmts = definitions.flatMap(toJSToplevel)

        js.Module(name, Nil, exports, jsDecls ++ jsExterns ++ stmts)
    }
  }

  def toJSToplevel(d: cps.ToplevelDefinition)(using ctx: TransformerContext): List[js.Stmt] = d match {
    case cps.ToplevelDefinition.Def(id, params, body)
        if ctx.callingConvention.isDirect(id) =>
      // Reserve the original binders before emitting worker and adapter names.
      nameDef(id)
      params.foreach(nameDef)
      directDefinitions(id, params, body)

    case cps.ToplevelDefinition.Def(id, params, body) =>
      val kind = kindOf(id)
      // Reserve binder names before translating the body, as in the original
      // constructor expression. This keeps generated names independent of
      // evaluation order inside the lowering implementation.
      val functionName = nameDef(id)
      val parameterNames = params.map(nameDef)
      val implementation = secondClassDef(id, params, body, None, kind.isRecursive).stmts
      ctx.workers.get(id) match {
        case Some(worker) => List(
          js.Function(worker, parameterNames, implementation),
          safeEntry(id, params, worker))
        case None => List(js.Function(functionName, parameterNames, implementation))
      }

    case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
      List(js.Const(nameDef(id), js.Call(RUN_TOPLEVEL, js.Lambda(List(nameDef(ks), nameDef(k)), toJS(binding).stmts))))
  }

  def toJS(e: cps.Extern)(using C: TransformerContext): js.Stmt = e match {
    case cps.Extern.Def(id, params, true, body) =>
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

    case cps.Extern.Def(id, params, false, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          js.Function(nameDef(id), params.map(nameDef), List(js.Return(toJSTemplate(contents))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), params.map(nameDef), List(js.Return($effekt.call("unreachable"))))
      }

    case cps.Extern.Include(_, contents) =>
      js.RawStmt(contents)
  }

  def toJSTemplate(t: Template[cps.Expr])(using TransformerContext): js.Expr =
    js.RawExpr(t.strings, t.args.map(toJS))

  def canInline(extern: cps.Extern): Boolean = extern match {
    case cps.Extern.Def(_, _, false, ExternBody.StringExternBody(_, _)) => true
    case _ => false
  }

  def inlineExtern(id: Id, args: List[cps.Expr])(using T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cps.Extern.Def(_, params, false, ExternBody.StringExternBody(_, Template(strings, templateArgs)))) =>
        val subst = params.zip(args).toMap
        val resolvedArgs = templateArgs.map {
          case tArg @ Expr.Variable(id) => subst.get(id) match {
            case Some(replaced) => toValueJS(replaced)
            case None => toJS(tArg)
          }
          case other => toJS(other)
        }
        js.RawExpr(strings, resolvedArgs)
      case _ => js.Call(nameRef(id), args.map(toValueJS))
    }


  def toJSDecl(d: core.Declaration): List[js.Stmt] = d match {
    case core.Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }
    case core.Interface(id, tparams, operations) =>
      Nil
  }

  def toJS(e: cps.Expr)(using ctx: TransformerContext): js.Expr = e match {
    case Expr.Variable(id) => valueRef(id)
    case Expr.Literal((), core.Type.TUnit) => $effekt.field("unit")
    case Expr.Literal(s: String, core.Type.TString) => JsString(escape(s))
    case Expr.Literal(b: Byte, core.Type.TByte) => js.RawExpr(UByte.unsafeFromByte(b).toHexString)
    case Expr.Literal(value, _) => js.RawExpr(value.toString)
    case Expr.Make(data, tag, vargs) => js.New(nameRef(tag), vargs.map(toValueJS))
    case Expr.Abort => js.Undefined
    case Expr.Toplevel => js.Undefined
  }

  /** A segment entry can be called directly only at a site that emits the
   *  segment-boundary bounce. When it flows into an ordinary value position,
   *  expose the usual stack-safe function convention with a fixed arity.
   *
   *  Continuations always receive a result and a meta-continuation. Functions
   *  whose parameters were dropped simply ignore the extra arguments.
   */
  def toValueJS(e: cps.Expr)(using ctx: TransformerContext): js.Expr = e match {
    case Expr.Variable(id) if ctx.segmentEntries.contains(id) =>
      val value = freshName("value_")
      val ks = freshName("ks_")
      val call = js.Call(valueRef(id), List(js.Variable(value), js.Variable(ks)))
      js.Lambda(List(value, ks), js.Lambda(Nil, call))
    case Expr.Variable(id) if ctx.directParameters.contains(id) =>
      toCpsFunction(valueRef(id), ctx.directParameters(id))
    case Expr.Variable(id) if ctx.callingConvention.isDirect(id) =>
      require(ctx.callingConvention.needsCpsEntry(id),
        s"Direct function $id is used as a CPS value without an entry")
      valueRef(id)
    case other => toJS(other)
  }

  /** A closed internal call preserves the provenance of a raw segment entry
   *  in its corresponding formal parameter. Open calls receive its ordinary
   *  stack-safe value representation. */
  private def toArgumentJS(call: cps.Stmt.App, argument: cps.Expr)(using ctx: TransformerContext): js.Expr =
    if ctx.segmentFlow.preserves(call) then toJS(argument) else toValueJS(argument)

  def toJS(s: cps.Stmt)(using ctx: TransformerContext): Binding[List[js.Stmt]] = s match {

    case cps.Stmt.Def(id, params, body, rest)
        if ctx.callingConvention.isDirect(id) =>
      Binding { k =>
        val (backups, renamings) = backupMutableParams(body, params.toSet)
        backups ++ directDefinitions(id, params, body, renamings) ++ toJS(rest).run(k)
      }

    case cps.Stmt.Def(id, params, body, rest) =>
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

    case cps.Stmt.New(id, interface, operations, rest) =>
      Binding { k =>
        val ops = operations.map { op =>
          val (backups, renamings) = backupMutableParams(op.body, op.params.toSet)
          val bodyCtx = functionBodyContext.copy(
            renamedCaptures = ctx.renamedCaptures ++ renamings)
          val body = toJS(op.body)(using bodyCtx).stmts
          if ctx.stackSafety.safeEntries.needsAdapter(op) then {
            val worker = freshName("operation_worker_")
            val declaration = js.Function(worker, op.params.map(nameDef), body)
            val call = js.Call(js.Variable(worker), op.params.map(nameRef))
            val entry = js.Lambda(op.params.map(nameDef), js.Lambda(Nil, call))
            (backups, List(declaration), nameDef(op.name) -> entry)
          } else
            (backups, Nil, nameDef(op.name) -> js.Lambda(op.params.map(nameDef), body))
        }
        val allBackups = ops.flatMap(_._1)
        val workers = ops.flatMap(_._2)
        val jsObj = js.Object(ops.map(_._3))
        allBackups ++ workers ++ List(js.Const(nameDef(id), jsObj)) ++ toJS(rest).run(k)
      }

    case cps.Stmt.Let(id, Expr.Variable(source), rest) if ctx.segmentEntries.contains(source) =>
      Binding { k =>
        js.Const(nameDef(id), valueRef(source)) ::
          toJS(rest)(using ctx.copy(segmentEntries = ctx.segmentEntries + id)).run(k)
      }

    case cps.Stmt.Let(id, binding, rest) =>
      Binding { k =>
        js.Const(nameDef(id), toValueJS(binding)) :: toJS(rest).run(k)
      }

    case call @ cps.Stmt.Call(result, id, args, _, rest)
        if ctx.callingConvention.isDirect(call) =>
      val directPositions = ctx.callingConvention.directArguments(call)
      val arguments = args.zipWithIndex.map { case (argument, index) =>
        directPositions.get(index)
          .fold(toValueJS(argument))(toDirectFunctionValue(argument, _))
      }

      ctx.directBody match {
        case Some((owner, params))
            if owner == id && ctx.callingConvention.isTailSelf(call) =>
          // Evaluate the parallel substitution before mutating any parameter.
          val temporaries = arguments.map(_ => freshName("next_arg_"))
          pure(
            arguments.zip(temporaries).map { case (argument, temporary) =>
              js.Const(temporary, argument)
            } ++
            params.zip(temporaries).map { case (param, temporary) =>
              js.Assign(nameRef(param), js.Variable(temporary))
            } :+ js.Continue(Some(nameDef(owner))))

        case _ =>
          Binding { k =>
            val callee = toDirectFunctionValue(cps.Expr.Variable(id), args.size)
            js.Const(nameDef(result), js.Call(callee, arguments)) ::
              toJS(rest).run(k)
          }
      }

    // A candidate rejected by the convention analysis becomes ordinary CPS.
    // The explicit remainder is reified exactly once, here at the boundary.
    case cps.Stmt.Call(result, id, args, ks, rest) =>
      Binding { k =>
        val returnedKs = Id("ks")
        val (backups, renamings) = backupMutableParams(rest, Set(result, returnedKs))
        val bodyCtx = functionBodyContext.copy(
          renamedCaptures = ctx.renamedCaptures ++ renamings)
        val continuationBody = toJS(rest)(using bodyCtx).stmts
        val continuation = js.Lambda(
          List(nameDef(result), nameDef(returnedKs)),
          js.Block(None, continuationBody))
        val loweredArgs =
          args.map(toValueJS) ++ List(toValueJS(ks), continuation)
        backups :+ js.Return(js.Call(valueRef(id), loweredArgs))
      }

    case app @ cps.Stmt.App(id, args) if ctx.segmentEntries.contains(id) =>
      val call = js.Call(valueRef(id), args.map(toValueJS))
      pure(js.Return(js.Lambda(Nil, js.Return(call))) :: Nil)

    case app @ cps.Stmt.App(id, args) =>
      ctx.dispatchAliases.get(id).orElse(ctx.defunctionalization.dispatchForCallee(id)) match {
        case Some(dispatch) => dispatchCall(app, args, dispatch)
        case None => ctx.secondClass.get(id) match {
        case Some(sci) =>
          // Second-class call: assign args to params, then jump.
          // Need temporaries for params that appear free in later arguments
          // to avoid overwriting values before they're read.
          val stmts = mutable.ListBuffer.empty[js.Stmt]

          // A jump assigns all arguments to the loop parameters
          // simultaneously. Work only on the support of that substitution:
          // an identity component p := p neither writes p nor needs a backup.
          val updates = sci.params.zip(args).filterNot {
            // A captured variable in a defunctionalized case can retain its
            // CPS id while being represented by a freshly bound frame field.
            // Compare the emitted source register, not just the CPS ids.
            case (param, Expr.Variable(argument)) =>
              ctx.renamedCaptures.getOrElse(argument, argument) == param
            case _ => false
          }
          val written = updates.map(_._1).toSet
          val freeInArgs = updates.flatMap(_._2.free).toSet
          val overlapping = freeInArgs.intersect(written)

          val tmpMap = overlapping.map { param =>
            val tmp = Id(s"tmp_${param}")
            stmts.append(js.Const(nameDef(tmp), valueRef(param)))
            param -> tmp
          }.toMap

          val subst = substitutions.Substitution(
            tmpMap.map { case (p, t) => p -> Expr.Variable(t) }
          )

          val temporaryEntries = tmpMap.collect {
            case (source, temporary) if ctx.segmentEntries.contains(source) => temporary
          }
          val argumentCtx = ctx.copy(segmentEntries = ctx.segmentEntries ++ temporaryEntries)

          updates.foreach { case (param, arg) =>
            val substituted = substitutions.substitute(arg)(using subst)
            val jsArg = toArgumentJS(app, substituted)(using argumentCtx)
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
          val callee = ctx.stackSafety.transferOf(app) match {
            case StackSafety.Transfer.Direct => directRef(id)
            case StackSafety.Transfer.Jump | StackSafety.Transfer.Safe => valueRef(id)
          }
          pure(js.Return(js.Call(callee, args.map(toArgumentJS(app, _)))) :: Nil)
        }
      }

    case invoke @ cps.Stmt.Invoke(id, method, args) =>
      val call = MethodCall(valueRef(id), memberNameRef(method), args.map(toValueJS): _*)
      pure(js.Return(call) :: Nil)

    case cps.Stmt.Return(value) =>
      val result = toValueJS(value)
      if ctx.directBody.nonEmpty then
        pure(js.Return(result) :: Nil)
      else
        pure(js.Return(js.Object(List(JSName("result") -> result))) :: Nil)

    case cps.Stmt.Run(id, callee, args, Purity.Pure | Purity.Impure, rest) =>
      Binding { k =>
        js.Const(nameDef(id), inlineExtern(callee, args)) :: toJS(rest).run(k)
      }

    // Async: needs CPS — call with continuation
    case cps.Stmt.Run(id, callee, args, Purity.Async, rest) =>
      ???

    case cps.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJS(thn).block, toJS(els).block) :: Nil)

    case cps.Stmt.Match(sc, Nil, None) =>
      pure(js.Return($effekt.call("unreachable")) :: Nil)

    case cps.Stmt.Match(sc, List((tag, clause)), None) =>
      val scrutinee = toJS(sc)
      val (_, stmts) = toJSClause(scrutinee, tag, clause)
      stmts

    case cps.Stmt.Match(sc, clauses, default) =>
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

    case cps.Stmt.Region(id, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("newRegion"))) ::
          toJS(rest).run(k)
      }

    case cps.Stmt.Alloc(id, init, region, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(valueRef(region), JSName("fresh"), toValueJS(init))) ::
          toJS(rest).run(k)
      }

    case cps.Stmt.Var(id, init, ks, rest) if !ctx.escaping.contains(id) =>
      Binding { k =>
        js.Let(nameDef(id), toValueJS(init)) ::
          toJS(rest)(using ctx.copy(localVars = ctx.localVars + id)).run(k)
      }

    case cps.Stmt.Var(id, init, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("fresh"), toValueJS(init))) ::
          toJS(rest).run(k)
      }

    case cps.Stmt.Dealloc(ref, rest) =>
      toJS(rest)

    case cps.Stmt.Get(ref, id, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Const(nameDef(id), valueRef(ref)) :: toJS(rest).run(k)
      }

    case cps.Stmt.Get(ref, id, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Member(valueRef(ref), JSName("value"))) ::
          toJS(rest).run(k)
      }

    case cps.Stmt.Put(ref, value, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Assign(valueRef(ref), toValueJS(value)) :: toJS(rest).run(k)
      }

    case cps.Stmt.Put(ref, value, rest) =>
      Binding { k =>
        js.ExprStmt(js.MethodCall(valueRef(ref), JSName("set"), toValueJS(value))) ::
          toJS(rest).run(k)
      }

    case cps.Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(p, ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          js.Call(RESET, toJS(ks1), toJS(k1))) ::
          toJS(body).run(next)
      }

    case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(resume, ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          js.Call(SHIFT, valueRef(prompt), toJS(ks1), toJS(k1))) ::
          toJS(body)(using ctx.copy(segmentEntries = ctx.segmentEntries + k)).run(next)
      }

    case cps.Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Binding { next =>
        js.Const(
          js.Pattern.Array(List(ks, k).map(id => js.Pattern.Variable(nameDef(id)))),
          js.Call(RESUME, valueRef(r), toJS(ks1), toJS(k1))) ::
          toJS(body)(using ctx.copy(segmentEntries = ctx.segmentEntries + k)).run(next)
      }

    case cps.Stmt.Hole(span) =>
      pure(js.Return($effekt.call("hole", JsString(span.range.from.format))) :: Nil)
  }

  private def dispatchCall(
    call: cps.Stmt.App,
    args: List[cps.Expr],
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
      js.Const(nextContinuation, valueRef(call.id)) +:
        args.zip(nextArguments).map { case (argument, temporary) =>
          js.Const(temporary, toArgumentJS(call, argument))
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
          js.Member(js.Variable(state.continuation), BOUNDARY_CONTINUATION)),
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
        js"""typeof ${continuation} === "function"""",
        js.Object(List(
          `tag` -> js.RawExpr(Defunctionalization.BoundaryTag.toString),
          BOUNDARY_CONTINUATION -> continuation)),
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

  def firstClassDef(id: Id, params: List[Id], body: cps.Stmt, rest: cps.Stmt, isRecursive: Boolean)(using ctx: TransformerContext): Binding[List[js.Stmt]] =
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

      val definitions = ctx.workers.get(id) match {
        case Some(worker) => List(
          js.Function(worker, params.map(nameDef), bodyStmts),
          safeEntry(id, params, worker))
        case None => List(js.Function(nameDef(id), params.map(nameDef), bodyStmts))
      }

      backups ++ definitions ++ toJS(rest).run(k)
    }

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
  def secondClassDef(id: Id, params: List[Id], body: cps.Stmt, rest: Option[cps.Stmt], isRecursive: Boolean)(using ctx: TransformerContext): Binding[List[js.Stmt]] = {
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

  def toJSClause(scrutinee: js.Expr, variant: Id, clause: cps.Clause)(using C: TransformerContext): (js.Expr, Binding[List[js.Stmt]]) =
    clause match {
      case cps.Clause(params, body) =>
        val fields = C.declarations.getConstructor(variant).fields.map(_.id)
        val tag = js.RawExpr(C.declarations.getConstructorTag(variant).toString)

        val extractedFields = params.zip(fields).map { case (p, f) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tag, Binding { k => extractedFields ++ toJS(body).run(k) })
    }
}
