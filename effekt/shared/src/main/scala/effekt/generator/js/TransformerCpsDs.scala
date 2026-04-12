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
    declarations: DeclarationContext,
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors

  def computeKinds(m: cpsds.ModuleDecl): Map[Id, FunctionKind] = {
    val analysis = cpsds.UsageAnalysis(m)
    val escape = cpsds.EscapeAnalysis(m)
    val kinds = mutable.Map.empty[Id, FunctionKind]
    analysis.functions.foreach { case (id, info) =>
      kinds(id) = FunctionKind(
        isRecursive = info.isRecursive,
        escapes = escape.contains(id)
      )
    }
    kinds.toMap
  }

  def kindOf(id: Id)(using ctx: TransformerContext): FunctionKind =
    ctx.kinds.getOrElse(id, FunctionKind(isRecursive = false, escapes = true))

  // --- Backup mutable params for closures ---

  /**
   * Backup mutable second-class params that are free in the given body,
   * returning (backup statements, substituted body).
   *
   * This prevents capture-by-reference bugs: JS `let` variables inside a
   * `while` loop are captured by reference, so closures defined inside the
   * loop body would see the mutated value rather than the value at definition time.
   */
  def backupMutableParams(body: cpsds.Stmt, boundParams: Set[Id] = Set.empty)(using ctx: TransformerContext): (List[js.Stmt], cpsds.Stmt) = {
    val freeInBody = body.free -- boundParams
    val captured = freeInBody.intersect(ctx.mutableParams)

    if captured.nonEmpty then
      val backups = captured.toList.map { p =>
        val tmp = Id(s"backup_${p}")
        (p, tmp)
      }
      val backupStmts = backups.map { case (p, tmp) =>
        js.Const(nameDef(tmp), nameRef(p))
      }
      val subst = substitutions.Substitution(
        backups.map { case (p, tmp) => p -> Expr.Variable(tmp) }.toMap
      )
      (backupStmts, substitutions.substitute(body)(using subst))
    else
      (Nil, body)
  }

  // --- Entry points ---

  def compile(input: cpsds.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): js.Module = {
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil,
      js.Return(Call(RUN_TOPLEVEL, nameRef(mainSymbol))))))
    given DeclarationContext = new DeclarationContext(coreModule.declarations, coreModule.externs)
    toJS(input, exports)
  }

  def compileLSP(input: cpsds.ModuleDecl, coreModule: core.ModuleDecl)(using C: Context): List[js.Stmt] =
    ???

  def toJS(module: cpsds.ModuleDecl, exports: List[js.Export])(using D: DeclarationContext, C: Context): js.Module =
    module match {
      case cpsds.ModuleDecl(path, includes, declarations, externs, definitions, _) =>
        given ctx: TransformerContext = TransformerContext(
          externs.collect { case d: cpsds.Extern.Def => (d.id, d) }.toMap,
          computeKinds(module),
          cpsds.EscapeAnalysis(module),
          Set.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          D, C)

        val name = JSName(jsModuleName(module.path))
        val jsExterns = module.externs.filterNot(canInline).map(toJS)
        val jsDecls = module.declarations.flatMap(toJSDecl)
        val stmts = module.definitions.map(toJSToplevel)

        js.Module(name, Nil, exports, jsDecls ++ jsExterns ++ stmts)
    }

  // --- Toplevel ---

  def toJSToplevel(d: cpsds.ToplevelDefinition)(using ctx: TransformerContext): js.Stmt = d match {
    case cpsds.ToplevelDefinition.Def(id, params, body) =>
      js.Function(nameDef(id), params.map(nameDef), toJS(body).stmts)

    case cpsds.ToplevelDefinition.Val(id, ks, k, binding) =>
      js.Const(nameDef(id), Call(RUN_TOPLEVEL, js.Lambda(List(nameDef(ks), nameDef(k)), toJS(binding).stmts)))

    case cpsds.ToplevelDefinition.Let(id, binding) =>
      js.Const(nameDef(id), toJS(binding))
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
    case Expr.Variable(id) => nameRef(id)
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
      val kind = kindOf(id)
      if kind.isSecondClass then
        secondClassDef(id, params, body, rest, kind.isRecursive)
      else
        firstClassDef(id, params, body, rest)

    // --- New ---
    case cpsds.Stmt.New(id, interface, operations, rest) =>
      Binding { k =>
        val ops = operations.map { op =>
          val (backups, substBody) = backupMutableParams(op.body, op.params.toSet)
          (backups, nameDef(op.name) -> js.Lambda(op.params.map(nameDef), toJS(substBody).stmts))
        }
        val allBackups = ops.flatMap(_._1)
        val jsObj = js.Object(ops.map(_._2))
        allBackups ++ List(js.Const(nameDef(id), jsObj)) ++ toJS(rest).run(k)
      }

    // --- Val ---
    case cpsds.Stmt.Val(id, binding, rest) =>
      Binding { k =>
        js.Let(nameDef(id), js.Undefined) ::
          toJS(binding).stmts ++
          toJS(rest).run(k)
      }

    // --- Let ---
    case cpsds.Stmt.Let(id, binding, rest) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJS(rest).run(k)
      }

    case cpsds.Stmt.App(id, args) =>
      ctx.secondClass.get(id) match {
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
            stmts.append(js.Const(nameDef(tmp), nameRef(param)))
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
          pure(js.Return(js.Lambda(Nil, js.Return(js.Call(nameRef(id), args.map(toJS))))) :: Nil)
      }

    // --- Invoke ---
    case cpsds.Stmt.Invoke(id, method, args) =>
      pure(js.Return(MethodCall(nameRef(id), memberNameRef(method), args.map(toJS): _*)) :: Nil)

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
        js.Const(nameDef(id), js.MethodCall(nameRef(region), JSName("fresh"), toJS(init))) ::
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
    case cpsds.Stmt.Get(ref, id, rest) if !ctx.escaping.contains(ref) =>
      Binding { k =>
        js.Const(nameDef(id), nameRef(ref)) :: toJS(rest).run(k)
      }

    case cpsds.Stmt.Get(ref, id, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Member(nameRef(ref), JSName("value"))) ::
          toJS(rest).run(k)
      }

    // --- Put ---
    case cpsds.Stmt.Put(ref, value, rest) if !ctx.escaping.contains(ref) =>
      Binding { k =>
        js.Assign(nameRef(ref), toJS(value)) :: toJS(rest).run(k)
      }

    case cpsds.Stmt.Put(ref, value, rest) =>
      Binding { k =>
        js.ExprStmt(js.MethodCall(nameRef(ref), JSName("set"), toJS(value))) ::
          toJS(rest).run(k)
      }

    // --- Reset ---
    case cpsds.Stmt.Reset(p, ks, k, body, ks1, k1) =>
      val (backups, substBody) = backupMutableParams(body, Set(p, ks, k))
      pure(backups ++ List(js.Return(Call(RESET,
        js.Lambda(List(nameDef(p), nameDef(ks), nameDef(k)), toJS(substBody).stmts),
        toJS(ks1), toJS(k1)))))

    // --- Shift ---
    case cpsds.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val (backups, substBody) = backupMutableParams(body, Set(resume, ks, k))
      pure(backups ++ List(js.Return(Call(SHIFT, nameRef(prompt),
        js.Lambda(List(nameDef(resume), nameDef(ks), nameDef(k)), toJS(substBody).stmts),
        toJS(ks1), toJS(k1)))))

    // --- Resume ---
    case cpsds.Stmt.Resume(r, ks, k, body, ks1, k1) =>
      val (backups, substBody) = backupMutableParams(body, Set(ks, k))
      pure(backups ++ List(js.Return(Call(RESUME, nameRef(r),
        js.Lambda(List(nameDef(ks), nameDef(k)), toJS(substBody).stmts),
        toJS(ks1), toJS(k1)))))

    // --- Hole ---
    case cpsds.Stmt.Hole(span) =>
      pure(js.Return($effekt.call("hole", JsString(span.range.from.format))) :: Nil)
  }

  // --- First-class Def ---

  def firstClassDef(id: Id, params: List[Id], body: cpsds.Stmt, rest: cpsds.Stmt)(using ctx: TransformerContext): Binding[List[js.Stmt]] =
    Binding { k =>
      val (backups, substBody) = backupMutableParams(body, params.toSet)
      backups ++
        List(js.Function(nameDef(id), params.map(nameDef), toJS(substBody).stmts)) ++
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
   */
  def secondClassDef(id: Id, params: List[Id], body: cpsds.Stmt, rest: cpsds.Stmt, isRecursive: Boolean)(using ctx: TransformerContext): Binding[List[js.Stmt]] = {
    val label = nameDef(id)
    val sci = SecondClassDef(params, isRecursive)

    // Register this def as second-class for nested code
    val ctxWithDef = ctx.copy(secondClass = ctx.secondClass + (id -> sci))

    // Translate rest: calls to id will become assignments + break
    val restStmts = toJS(rest)(using ctxWithDef).stmts
    val entryBlock = js.Block(Some(label), restStmts)

    // Translate body: for recursive defs, calls to id will become assignments + continue.
    // Also track params as mutable so that closures inside the body will backup them.
    val bodyCtx = if isRecursive then
      ctxWithDef.copy(
        insideBody = ctxWithDef.insideBody + id,
        mutableParams = ctxWithDef.mutableParams ++ params.toSet
      )
    else ctxWithDef
    val bodyStmts = toJS(body)(using bodyCtx).stmts

    val paramDecls = params.map(p => js.Let(nameDef(p), js.Undefined))

    if isRecursive then
      pure(paramDecls ++ List(
        entryBlock,
        js.While(Some(label), RawExpr("true"), bodyStmts)
      ))
    else
      pure(paramDecls ++ List(entryBlock) ++ bodyStmts)
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
