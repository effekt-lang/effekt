package effekt
package generator
package js

import effekt.context.Context
import effekt.cps.*
import effekt.core.{ DeclarationContext, Id }
import effekt.util.UByte

import scala.collection.mutable

/**
 * Translates the CPS IR to JavaScript.
 *
 * Definitions that do not escape (see [[cps.escapeAnalysis]]) are translated to labeled blocks:
 *
 *   let params...;
 *   id: { [[rest]] }        // call: params = args; break id
 *   [[body]]
 *
 * If such a definition is recursive, its body is wrapped in a loop:
 *
 *   let params...;
 *   id: { [[rest]] }        // initial call: params = args; break id
 *   id: while (true) {      // recursive call: params = args; continue id
 *     [[body]]
 *   }
 *
 * All other definitions are translated to JavaScript functions. Self-recursive calls
 * within the same JavaScript function are also translated to loops.
 *
 * Stack safety: calls ([[ReturnPoint.Tail]], [[ReturnPoint.Bind]]) are direct JavaScript calls,
 * while transfers to a function ([[ReturnPoint.Jump]], e.g. returning to a continuation)
 * return a thunk to the trampoline.
 */
object TransformerCps extends Transformer {

  // Defined in effekt_runtime.js
  // ---------------------------
  val RUN_TOPLEVEL = js.Variable(JSName("RUN_TOPLEVEL"))
  val TOPLEVEL_KS = js.Variable(JSName("TOPLEVEL_KS"))
  val RESET = js.Variable(JSName("RESET"))
  val SHIFT = js.Variable(JSName("SHIFT"))
  val RESUME = js.Variable(JSName("RESUME"))
  val VAR = js.Variable(JSName("VAR"))
  val REGION = js.Variable(JSName("REGION"))

  /**
   * A definition that is translated to a label (or the loop of a function).
   * Jumping to it assigns the arguments to [[params]] and then executes [[jump]].
   */
  class Label(val params: List[Id], val jump: js.Stmt) {
    var used: Boolean = false
  }

  case class TransformerContext(
    // definitions of externs (used to inline them)
    externs: Map[Id, cps.Extern.Def],
    // all escaping variables of the module
    escaping: Set[Id],
    // labels in scope of the current JavaScript function
    labels: Map[Id, Label],
    // parameters in scope of the current JavaScript function that are reassigned when jumping to a label.
    // Any JS closure capturing these must back them up first to avoid capture-by-reference bugs.
    mutableParams: Set[Id],
    // mutable variables that do not escape and are thus represented as native JS variables
    localVars: Set[Id],
    // the original declaration context (used to compile pattern matching)
    declarations: DeclarationContext,
    // the arities of resets in this module (used to generate their return continuations)
    resets: mutable.Set[Int],
    // the usual compiler context
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors

  /**
   * JavaScript labels and the mutable parameters implementing them are local
   * to one JavaScript function. They cannot be referenced from a nested
   * function, even when they remain in lexical scope in the CPS tree.
   */
  def functionBodyContext(using ctx: TransformerContext): TransformerContext =
    ctx.copy(labels = Map.empty, mutableParams = Set.empty)

  def isEscaping(id: Id)(using ctx: TransformerContext): Boolean = ctx.escaping.contains(id)

  // --- Entry points ---

  /**
   * Entrypoint used by the compiler to compile whole programs
   */
  def compile(input: cps.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    resetNames()
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil,
      js.Return(Some(js.Call(RUN_TOPLEVEL, nameRef(mainSymbol)))))))
    given DeclarationContext = new DeclarationContext(coreModule.declarations, coreModule.externs)
    toJS(input, exports)

  def compileLSP(input: cps.ModuleDecl, coreModule: core.ModuleDecl)(using C: Context): List[js.Stmt] =
    given ctx: TransformerContext = initialContext(input)(using new DeclarationContext(coreModule.declarations, coreModule.externs), C)
    val stmts = input.definitions.map(toJS)
    returns ++ stmts

  def toJS(module: cps.ModuleDecl, exports: List[js.Export])(using D: DeclarationContext, C: Context): js.Module = {
    given ctx: TransformerContext = initialContext(module)

    val name = JSName(jsModuleName(module.path))
    val jsExterns = module.externs.filterNot(canInline).map(toJS)
    val jsDecls = module.declarations.flatMap(toJS)
    val stmts = module.definitions.map(toJS)

    js.Module(name, Nil, exports, jsDecls ++ jsExterns ++ returns ++ stmts)
  }

  private def initialContext(module: cps.ModuleDecl)(using D: DeclarationContext, C: Context): TransformerContext =
    TransformerContext(
      module.externs.collect { case d: cps.Extern.Def => (d.id, d) }.toMap,
      module.escapes,
      Map.empty,
      Set.empty,
      Set.empty,
      D, mutable.Set.empty, C)

  // --- Toplevel ---

  def toJS(d: cps.ToplevelDefinition)(using ctx: TransformerContext): js.Stmt = d match {
    case cps.ToplevelDefinition.Def(id, params, body) =>
      js.Function(nameDef(id), params.map(nameDef), functionBody(id, params, body))

    case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
      js.Const(nameDef(id), js.Call(RUN_TOPLEVEL, js.Lambda(List(nameDef(ks), nameDef(k)), toJS(binding)(using functionBodyContext).stmts)))
  }

  // --- Externs ---

  def toJS(e: cps.Extern)(using C: TransformerContext): js.Stmt = e match {
    case cps.Extern.Def(id, params, true, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          val ks = freshName("ks_")
          val k = freshName("k_")
          js.Function(nameDef(id), params.map(nameDef) ++ List(ks, k),
            List(js.Return(Some(js.Call(toJS(contents), List(js.Variable(ks), js.Variable(k)))))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), params.map(nameDef), List(js.Return(Some($effekt.call("unreachable")))))
      }

    case cps.Extern.Def(id, params, false, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          js.Function(nameDef(id), params.map(nameDef), List(js.Return(Some(toJS(contents)))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), params.map(nameDef), List(js.Return(Some($effekt.call("unreachable")))))
      }

    case cps.Extern.Include(_, contents) =>
      js.RawStmt(contents)
  }

  def toJS(t: Template[cps.Expr])(using TransformerContext): js.Expr =
    js.RawExpr(t.strings, t.args.map(toJS))

  def canInline(extern: cps.Extern): Boolean = extern match {
    case cps.Extern.Def(_, _, false, ExternBody.StringExternBody(_, _)) => true
    case _ => false
  }

  def inlineExtern(id: Id, args: List[cps.Expr])(using T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cps.Extern.Def(_, params, false, ExternBody.StringExternBody(_, Template(strings, templateArgs)))) =>
        val subst = (params zip args).toMap
        js.RawExpr(strings, templateArgs.map { arg => toJS(substitutions.substitute(arg, subst)) })
      case _ => js.Call(nameRef(id), args.map(toJS))
    }

  // --- Declarations ---

  def toJS(d: core.Declaration): List[js.Stmt] = d match {
    case core.Data(did, tparams, ctors) =>
      ctors.zipWithIndex.flatMap { case (ctor, index) =>
        val cls = generateConstructor(ctor, index)
        if ctor.fields.isEmpty then List(cls, js.Const(instanceName(ctor.id), js.New(nameRef(ctor.id), Nil)))
        else List(cls)
      }

    // interfaces are structurally typed at the moment, no need to generate anything.
    case core.Interface(id, tparams, operations) =>
      Nil
  }

  // --- Expressions ---

  def toJS(e: cps.Expr)(using ctx: TransformerContext): js.Expr = e match {
    case Expr.Variable(id) => nameRef(id)
    case Expr.Literal((), core.Type.TUnit) => $effekt.field("unit")
    case Expr.Literal(s: String, core.Type.TString) => JsString(escape(s))
    case Expr.Literal(b: Byte, core.Type.TByte) => js.RawExpr(UByte.unsafeFromByte(b).toHexString)
    case Expr.Literal(value, _) => js.RawExpr(value.toString)
    case Expr.Make(data, tag, Nil) => js.Variable(instanceName(tag))
    case Expr.Make(data, tag, vargs) => js.New(nameRef(tag), vargs.map(toJS))
    case Expr.Abort => js.Undefined
    case Expr.Toplevel => TOPLEVEL_KS
  }

  def toJS(callee: cps.Callee)(using ctx: TransformerContext): js.Expr = callee match {
    case Callee.Function(id) => nameRef(id)
    case Callee.Method(receiver, method) => js.Member(nameRef(receiver), memberNameRef(method))
  }

  // JavaScript returns no value for zero results, a scalar for one result, and an array for several results.
  private def pack(values: List[js.Expr]): Option[js.Expr] = values match {
    case Nil => None
    case value :: Nil => Some(value)
    case values => Some(js.ArrayLiteral(values))
  }

  private def bind(results: List[Id], value: js.Expr): js.Stmt = results match {
    case Nil => js.ExprStmt(value)
    case result :: Nil => js.Const(nameDef(result), value)
    case results => js.Const(js.Pattern.Array(results.map(r => js.Pattern.Variable(nameDef(r)))), value)
  }

  // --- Statements ---

  def toJS(s: cps.Stmt)(using ctx: TransformerContext): Binding[List[js.Stmt]] = s match {

    // --- Def ---
    case cps.Stmt.Def(id, params, body, rest) if isEscaping(id) =>
      Binding { k =>
        val (backups, substBody) = backupMutableParams(body, params.toSet)
        backups ++
          List(js.Function(nameDef(id), params.map(nameDef), functionBody(id, params, substBody))) ++
          toJS(rest).run(k)
      }

    case cps.Stmt.Def(id, params, body, rest) =>
      secondClassDef(id, params, body, rest)

    // --- New ---
    case cps.Stmt.New(id, interface, operations, rest) =>
      Binding { k =>
        val ops = operations.map { op =>
          val (backups, lambda) = closure(op.params, op.body)
          (backups, nameDef(op.name) -> lambda)
        }
        ops.flatMap(_._1) ++ List(js.Const(nameDef(id), js.Object(ops.map(_._2)))) ++ toJS(rest).run(k)
      }

    // --- Let ---
    case cps.Stmt.Let(id, binding, rest) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJS(rest).run(k)
      }

    // --- Calls ---
    case call @ cps.Stmt.Call(Callee.Function(id), args, returnsTo) if ctx.labels.contains(id) =>
      pure(jumpTo(ctx.labels(id), call))

    case cps.Stmt.Call(callee, args, ReturnPoint.Tail(ks, k)) =>
      pure(js.Return(Some(js.Call(toJS(callee), (args ++ List(ks, k)).map(toJS)))) :: Nil)

    case cps.Stmt.Call(callee, args, ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
      val (backups, continuation) = closure(results :+ returnedKs, rest)
      pure(backups :+ js.Return(Some(js.Call(toJS(callee), (args :+ ks).map(toJS) :+ continuation))))

    case cps.Stmt.Call(callee, args, ReturnPoint.Jump) =>
      pure(js.Return(Some(js.Lambda(Nil, js.Call(toJS(callee), args.map(toJS))))) :: Nil)

    case cps.Stmt.Call(callee, args, ReturnPoint.Direct(results, rest)) =>
      Binding { k =>
        bind(results, js.Call(toJS(callee), args.map(toJS))) :: toJS(rest).run(k)
      }

    case cps.Stmt.Return(values) =>
      pure(js.Return(pack(values.map(toJS))) :: Nil)

    // --- Run ---
    case cps.Stmt.Run(id, callee, args, _, rest) =>
      Binding { k =>
        js.Const(nameDef(id), inlineExtern(callee, args)) :: toJS(rest).run(k)
      }

    // --- If ---
    case cps.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJS(thn).block, toJS(els).block) :: Nil)

    // --- Match ---
    case cps.Stmt.Match(sc, Nil, None) =>
      pure(js.Return(Some($effekt.call("unreachable"))) :: Nil)

    case cps.Stmt.Match(sc, List((tag, clause)), None) =>
      val scrutinee = toJS(sc)
      val (_, stmts) = toJS(scrutinee, tag, clause)
      stmts

    case cps.Stmt.Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)
      pure(js.Switch(js.Member(scrutinee, `tag`),
        clauses.map { case (tag, clause) =>
          val (e, binding) = toJS(scrutinee, tag, clause)
          val stmts = binding.stmts
          stmts.lastOption match {
            case Some(_: (js.Stmt.Return | js.Stmt.Break | js.Stmt.Continue)) => (e, stmts)
            case _ => (e, stmts :+ js.Break(None))
          }
        },
        default.map(s => toJS(s).stmts)) :: Nil)

    // --- Region ---
    // const r = REGION(ks); rest
    case cps.Stmt.Region(id, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Call(REGION, List(toJS(ks)))) :: toJS(rest).run(k)
      }

    // const x = r.fresh(init); rest
    case cps.Stmt.Alloc(id, init, region, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(nameRef(region), JSName("fresh"), toJS(init))) :: toJS(rest).run(k)
      }

    // let x = init; rest
    case cps.Stmt.Var(id, init, ks, rest) if !isEscaping(id) =>
      Binding { k =>
        js.Let(nameDef(id), toJS(init)) :: toJS(rest)(using ctx.copy(localVars = ctx.localVars + id)).run(k)
      }

    // const x = VAR(init, ks); rest
    case cps.Stmt.Var(id, init, ks, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Call(VAR, List(toJS(init), toJS(ks)))) :: toJS(rest).run(k)
      }

    case cps.Stmt.Dealloc(ref, rest) =>
      toJS(rest)

    // const id = x; rest
    case cps.Stmt.Get(ref, id, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Const(nameDef(id), nameRef(ref)) :: toJS(rest).run(k)
      }

    // const id = ref.value; rest
    case cps.Stmt.Get(ref, id, rest) =>
      Binding { k =>
        js.Const(nameDef(id), js.Member(nameRef(ref), JSName("value"))) :: toJS(rest).run(k)
      }

    // x = value; rest
    case cps.Stmt.Put(ref, value, rest) if ctx.localVars.contains(ref) =>
      Binding { k =>
        js.Assign(nameRef(ref), toJS(value)) :: toJS(rest).run(k)
      }

    // ref.set(value); rest
    case cps.Stmt.Put(ref, value, rest) =>
      Binding { k =>
        js.ExprStmt(js.MethodCall(nameRef(ref), JSName("set"), toJS(value))) :: toJS(rest).run(k)
      }

    // --- Control ---
    case cps.Stmt.Reset(p, ks, k, body, arity, ks1, k1) =>
      val (backups, prog) = closure(List(p, ks, k), body)
      pure(backups :+ js.Return(Some(js.Call(RESET, List(prog, returnFrom(arity), toJS(ks1), toJS(k1))))))

    case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val (backups, handler) = closure(List(resume, ks, k), body)
      pure(backups :+ js.Return(Some(js.Call(SHIFT, List(nameRef(prompt), handler, toJS(ks1), toJS(k1))))))

    case cps.Stmt.Resume(r, ks, k, body, ks1, k1) =>
      val (backups, resumed) = closure(List(ks, k), body)
      pure(backups :+ js.Return(Some(js.Call(RESUME, List(nameRef(r), resumed, toJS(ks1), toJS(k1))))))

    case cps.Stmt.Hole(span) =>
      pure(js.Return(Some($effekt.call("hole", JsString(span.range.from.format)))) :: Nil)
  }

  // Returns the answer of a reset to the stack below
  def returnFrom(arity: Int)(using C: TransformerContext): js.Expr =
    C.resets += arity
    js.Variable(JSName(s"RETURN_$arity"))

  // A reset in tail position of another reset's body returns to a frame whose stack is again
  // RETURN_n. We pop all such frames at once, instead of recursing through them (which can
  // overflow the JS stack for deep handler recursion):
  //
  //   function RETURN_n(x0, ..., xn, ks) {
  //     let rest = ks.rest
  //     while (rest.stack === RETURN_n) rest = rest.rest
  //     return rest.stack(x0, ..., xn, rest)
  //   }
  def returns(using C: TransformerContext): List[js.Stmt] =
    C.resets.toList.sorted.map { arity =>
      val name = s"RETURN_$arity"
      val values = List.tabulate(arity) { i => s"x$i" }
      js.RawStmt(
        s"""function ${name}(${(values :+ "ks").mkString(", ")}) {
           |  let rest = ks.rest
           |  while (rest.stack === ${name}) rest = rest.rest
           |  return rest.stack(${(values :+ "rest").mkString(", ")})
           |}""".stripMargin)
    }

  // --- Functions and labels ---

  /**
   * A JavaScript function with the given parameters and body, backing up captured mutable parameters.
   */
  def closure(params: List[Id], body: cps.Stmt)(using ctx: TransformerContext): (List[js.Stmt], js.Expr) =
    val (backups, substBody) = backupMutableParams(body, params.toSet)
    (backups, js.Lambda(params.map(nameDef), toJS(substBody)(using functionBodyContext).stmts))

  /**
   * The body of the JavaScript function [[id]]. Self-recursive calls become a loop.
   */
  def functionBody(id: Id, params: List[Id], body: cps.Stmt)(using ctx: TransformerContext): List[js.Stmt] =
    val label = Label(params, js.Continue(Some(nameDef(id))))
    val isRecursive = body.free.contains(id)
    val bodyCtx = functionBodyContext.copy(
      labels = Map(id -> label),
      mutableParams = if isRecursive then params.toSet else Set.empty)
    val stmts = toJS(body)(using bodyCtx).stmts
    if label.used then List(js.While(Some(nameDef(id)), RawExpr("true"), stmts)) else stmts

  def secondClassDef(id: Id, params: List[Id], body: cps.Stmt, rest: cps.Stmt)(using ctx: TransformerContext): Binding[List[js.Stmt]] = {
    val name = nameDef(id)
    val isRecursive = body.free.contains(id)

    val restStmts = toJS(rest)(using ctx.copy(labels = ctx.labels + (id -> Label(params, js.Break(Some(name)))))).stmts

    val bodyStmts =
      if isRecursive then
        val loop = Label(params, js.Continue(Some(name)))
        val bodyCtx = ctx.copy(labels = ctx.labels + (id -> loop), mutableParams = ctx.mutableParams ++ params)
        List(js.While(Some(name), RawExpr("true"), toJS(body)(using bodyCtx).stmts))
      else
        toJS(body).stmts

    pure(params.map(p => js.Let(nameDef(p), js.Undefined)) ++ (js.Block(Some(name), restStmts) :: bodyStmts))
  }

  /**
   * Assigns the arguments of the call to the parameters of the label and jumps to it.
   *
   * Parameters that are referred to in the arguments are saved in temporaries first,
   * to avoid overwriting values before they are read.
   */
  def jumpTo(label: Label, call: cps.Stmt.Call)(using ctx: TransformerContext): List[js.Stmt] = {
    label.used = true
    val stmts = mutable.ListBuffer.empty[js.Stmt]

    val freeInArgs = call.args.flatMap(_.free).toSet ++ (call.returnsTo match {
      case ReturnPoint.Tail(ks, k) => ks.free ++ k.free
      case ReturnPoint.Bind(results, returnedKs, ks, rest) => ks.free ++ (rest.free -- results - returnedKs)
      case ReturnPoint.Jump => Set.empty
      case ReturnPoint.Direct(_, _) => sys.error("Cannot return to the call site of a jump to a label")
    })

    // parameters that are passed unchanged do not need to be reassigned
    val reassigned = (label.params zip call.knownArguments).collect {
      case (param, arg) if arg != Expr.Variable(param) => param
    }.toSet ++ label.params.drop(call.knownArguments.size)

    val temporaries = freeInArgs.intersect(reassigned).toList.map { param =>
      val tmp = Id(s"tmp_${param}")
      stmts.append(js.Const(nameDef(tmp), nameRef(param)))
      param -> Expr.Variable(tmp)
    }.toMap

    val args = call.knownArguments.map { arg => toJS(substitutions.substitute(arg, temporaries)) }

    val continuation = call.returnsTo match {
      case ReturnPoint.Bind(results, returnedKs, _, rest) =>
        val (backups, lambda) = closure(results :+ returnedKs, substitutions.substitute(rest, temporaries -- results - returnedKs))
        stmts.appendAll(backups)
        List(lambda)
      case _ => Nil
    }

    (label.params zip (args ++ continuation)).foreach {
      case (param, arg) if reassigned.contains(param) => stmts.append(js.Assign(nameRef(param), arg))
      case _ => ()
    }

    stmts.append(label.jump)
    stmts.toList
  }

  /**
   * Backup mutable params that are free in the given body,
   * returning (backup statements, substituted body).
   *
   * This prevents capture-by-reference bugs: parameters reassigned before jumping
   * to a label are captured by reference, so closures defined in the body would see
   * the mutated value rather than the value at definition time.
   */
  def backupMutableParams(body: cps.Stmt, boundParams: Set[Id])(using ctx: TransformerContext): (List[js.Stmt], cps.Stmt) = {
    val captured = (body.free -- boundParams).intersect(ctx.mutableParams)
    if captured.isEmpty then (Nil, body) else {
      val backups = captured.toList.map { p => p -> Id(s"tmp_${p}") }
      val stmts = backups.map { case (p, tmp) => js.Const(nameDef(tmp), nameRef(p)) }
      val subst = backups.map { case (p, tmp) => p -> Expr.Variable(tmp) }.toMap
      (stmts, substitutions.substitute(body, subst))
    }
  }

  // --- Pattern matching ---

  def toJS(scrutinee: js.Expr, variant: Id, clause: cps.Clause)(using C: TransformerContext): (js.Expr, Binding[List[js.Stmt]]) =
    clause match {
      case cps.Clause(params, body) =>
        val fields = C.declarations.getConstructor(variant).fields.map(_.id)
        val tag = js.RawExpr(C.declarations.getConstructorTag(variant).toString)

        val extractedFields = (params zip fields).collect { case (p, f) if body.free.contains(p) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tag, Binding { k => extractedFields ++ toJS(body).run(k) })
    }
}
