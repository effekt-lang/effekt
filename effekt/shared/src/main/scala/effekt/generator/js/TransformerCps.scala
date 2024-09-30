package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cps.*
import effekt.core.{ DeclarationContext, Id }


// TODO
// 2. get rid of separate compilation for JS
object TransformerCps extends Transformer {

  val RUN   = Variable(JSName("RUN"))
  val RESET = Variable(JSName("RESET"))
  val SHIFT = Variable(JSName("SHIFT"))
  val THUNK = Variable(JSName("THUNK"))


  def thunked(stmt: js.Stmt): js.Stmt = js.Return(js.Lambda(Nil, stmt))
  def thunked(expr: js.Expr): js.Expr = js.Lambda(Nil, expr)

  case class TransformerContext(
    requiresThunk: Boolean,
    bindings: Map[Id, js.Expr],
    externs: Map[Id, cps.Extern.Def])

  def requiringThunk[T](prog: TransformerContext ?=> T)(using C: TransformerContext): T =
    prog(using C.copy(requiresThunk = true))

  def noThunking[T](prog: TransformerContext ?=> T)(using C: TransformerContext): T =
    prog(using C.copy(requiresThunk = false))

  def maybeThunking(stmt: js.Stmt)(using T: TransformerContext): js.Stmt =
    if T.requiresThunk then thunked(stmt) else stmt

  def maybeThunking(expr: js.Expr)(using T: TransformerContext): js.Expr =
    if T.requiresThunk then thunked(expr) else expr

  def run(body: js.Expr): js.Stmt =
    js.Return(Call(RUN, body))

  def lookup(id: Id)(using C: TransformerContext): js.Expr = C.bindings.getOrElse(id, nameRef(id))

  def bindingAll[R](bs: List[(Id, js.Expr)])(body: TransformerContext ?=> R)(using C: TransformerContext): R =
    body(using C.copy(bindings = C.bindings ++ bs))


  /**
   * Entrypoint used by the compiler to compile whole programs
   */
  def compile(input: CoreTransformed, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil, run(nameRef(mainSymbol)))))

    val moduleDecl = input.core

    val cpsTransformed = cps.Transformer.transform(moduleDecl)

    // println(util.PrettyPrinter.format(cpsTransformed).layout)

    given DeclarationContext = new DeclarationContext(moduleDecl.declarations, moduleDecl.externs)

    toJS(cpsTransformed, Nil, exports)

  def toJS(module: cps.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module =
    module match {
      case cps.ModuleDecl(path, includes, declarations, externs, definitions, _) =>
        given TransformerContext(false, Map.empty, externs.collect { case d: Extern.Def => (d.id, d) }.toMap)

        val name    = JSName(jsModuleName(module.path))
        val jsExterns = module.externs.filterNot(canInline).map(toJS)
        val jsDecls   = module.declarations.flatMap(toJS)
        val stmts   = module.definitions.map(toJS)
        val state   = generateStateAccessors
        js.Module(name, imports, exports, state ++ jsDecls ++ jsExterns ++ stmts)
    }

  def toJS(d: cps.ToplevelDefinition)(using DeclarationContext, Context, TransformerContext): js.Stmt = d match {
    case cps.ToplevelDefinition.Def(id, block) => js.Const(nameDef(id), requiringThunk { toJS(block) })
    case cps.ToplevelDefinition.Val(id, binding) => ???
    case cps.ToplevelDefinition.Let(id, binding) => js.Const(nameDef(id), toJS(binding))
  }

  def toJSParam(id: Id): JSName = nameDef(id)

  def toJS(e: cps.Extern)(using DeclarationContext, Context, TransformerContext): js.Stmt = e match {
    case cps.Extern.Def(id, vps, bps, capt, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          js.Function(nameDef(id), (vps ++ bps) map toJSParam, List(js.Return(toJS(contents))))
        case u: ExternBody.Unsupported =>
          u.report
          js.Function(nameDef(id), (vps ++ bps) map toJSParam, List(js.Return(monadic.Run(monadic.Builtin("hole")))))
      }

    case cps.Extern.Include(ff, contents) =>
      js.RawStmt(contents)
  }

  def toJS(t: Template[Pure])(using DeclarationContext, Context, TransformerContext): js.Expr =
    js.RawExpr(t.strings, t.args.map(toJS))

  def toJS(d: core.Declaration)(using Context): List[js.Stmt] = d match {
    case core.Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }

    // interfaces are structurally typed at the moment, no need to generate anything.
    case core.Interface(id, tparams, operations) =>
      Nil
  }

  def toJS(b: cps.Block)(using DeclarationContext, Context, TransformerContext): js.Expr = b match {
    case cps.BlockVar(v) => nameRef(v)
    case cps.BlockLit(vps, bps, ks, k, body) =>
      js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJSStmt(body))
    case cps.Unbox(e)     => toJS(e)
    case cps.New(handler) => toJS(handler)
  }

  def toJS(handler: cps.Implementation)(using DeclarationContext, Context, TransformerContext): js.Expr = handler match {
    case cps.Implementation(interface, operations) =>
      js.Object(operations.map {
        case cps.Operation(id, vps, bps, ks, k, body) =>
          nameDef(id) -> js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJSStmt(body))
      })
  }

  def toJS(ks: cps.MetaCont): js.Expr = nameRef(ks.id)

  def toJS(k: cps.Cont)(using DeclarationContext, Context, TransformerContext): js.Expr = k match {
    case Cont.ContVar(id) => nameRef(id)
    case Cont.ContLam(result, ks, body) => js.Lambda(List(nameDef(result), nameDef(ks)), toJSStmt(body))
  }

  def toJS(e: cps.Expr)(using D: DeclarationContext, C: Context, T: TransformerContext): js.Expr = e match {
    case cps.Pure.ValueVar(id) => lookup(id)
    case cps.Pure.Literal(()) => $effekt.field("unit")
    case cps.Pure.Literal(s: String) => JsString(escape(s))
    case literal: cps.Pure.Literal => js.RawExpr(literal.value.toString)
    case DirectApp(id, vargs, Nil) => inlineExtern(id, vargs)
    case DirectApp(id, vargs, bargs) => js.Call(nameRef(id), vargs.map(toJS) ++ bargs.map(toJS))
    case cps.Pure.PureApp(id, vargs) => inlineExtern(id, vargs)
    case cps.Pure.Make(data, tag, vargs) => js.New(nameRef(tag), vargs map toJS)
    case cps.Pure.Select(target, field) => js.Member(toJS(target), memberNameRef(field))
    case cps.Pure.Box(b) => toJS(b)
  }

  // in JS statement position (e.g. function body)
  def toJSStmt(s: cps.Stmt)(using D: DeclarationContext, C: Context, T: TransformerContext): Binding[js.Stmt] = s match {
    case cps.Stmt.Scope(defs, body) =>
      Binding { k =>
        defs.map(toJS) ++ toJSStmt(body).run(k)
      }

    case cps.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJSStmt(thn).block, toJSStmt(els).block))

    case cps.Stmt.LetExpr(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJSStmt(body).run(k)
      }

    case cps.Stmt.LetCont(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: requiringThunk { toJSStmt(body) }.run(k)
      }

    case cps.Stmt.Match(sc, Nil, None) =>
      pure(js.Return($effekt.call("emptyMatch")))

    case cps.Stmt.Match(sc, List((tag, clause)), None) =>
      val scrutinee = toJS(sc)
      val (_, stmts) = toJS(scrutinee, tag, clause)
      pure(js.MaybeBlock(stmts))

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case cps.Stmt.Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)

      pure(js.Switch(js.Member(scrutinee, `tag`),
        clauses.map { case (tag, clause) => toJS(scrutinee, tag, clause) },
        default.map { s => toJSStmt(s).stmts }))

    case other => toJSExpr(other).map { expr => js.Return(expr) }
  }

  def toJS(scrutinee: js.Expr, tag: Id, clause: cps.Clause)(using D: DeclarationContext, C: Context, T: TransformerContext): (js.Expr, List[js.Stmt]) =
    clause match {
      case cps.Clause(vparams, body) =>
        val fields = D.getConstructor(tag).fields.map(_.id)

        // TODO compute free variables to only bind used fields
        //val freeVars = core.Variables.free(body).toSet.map(_.id)
        def isUsed(x: Id) = true /// freeVars contains x

        val extractedFields = (vparams zip fields).collect { case (p, f) if isUsed(p) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tagFor(tag), extractedFields ++ toJSStmt(body).stmts)
    }

  def toJS(d: cps.Def)(using D: DeclarationContext, C: Context, T: TransformerContext): js.Stmt = d match {
    case cps.Def(id, block) => js.Const(nameDef(id), requiringThunk { toJS(block) })
  }

  def toJSExpr(s: cps.Stmt)(using D: DeclarationContext, C: Context, T: TransformerContext): Binding[js.Expr] = s match {
    case cps.Stmt.Jump(k, arg, ks) =>
      pure(maybeThunking { js.Call(nameRef(k), toJS(arg), toJS(ks)) })

    case cps.Stmt.Scope(defs, body) =>
      Binding { k =>
        defs.map(toJS) ++ toJSExpr(body).run(k)
      }
    case cps.Stmt.App(callee, vargs, bargs, ks, k) =>
      pure(maybeThunking(js.Call(toJS(callee), vargs.map(toJS) ++ bargs.map(toJS) ++ List(toJS(ks),
        noThunking { toJS(k) }))))
    case cps.Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      val args = vargs.map(toJS) ++ bargs.map(toJS) ++ List(toJS(ks), toJS(k))
      pure(MethodCall(toJS(callee), memberNameRef(method), args:_*))
    case cps.Stmt.If(cond, thn, els) => pure(js.IfExpr(toJS(cond), toJSStmt(thn).toExpr, toJSStmt(els).toExpr))
    case cps.Stmt.LetExpr(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJSExpr(body).run(k)
      }
    case cps.Stmt.LetCont(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJSExpr(body).run(k)
      }
    case cps.Stmt.Reset(prog, ks, k) => pure(Call(RESET, toJS(prog), toJS(ks), toJS(k)))
    case cps.Stmt.Shift(prompt, body, ks, k) => pure(Call(SHIFT, nameRef(prompt), noThunking { toJS(body) }, toJS(ks), toJS(k)))
    case cps.Stmt.Hole() => pure($effekt.call("hole"))

    // TODO this might be horrible since we create a thunk
    case cps.Stmt.Match(sc, Nil, None) => pure($effekt.call("emptyMatch"))
    case cps.Stmt.Match(sc, clauses, default) => pure(js.Call(js.Lambda(Nil, toJSStmt(s))))
  }

  def inlineExtern(id: Id, args: List[cps.Pure])(using C: Context, D: DeclarationContext, T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cps.Extern.Def(id, params, Nil, annotatedCapture,
        ExternBody.StringExternBody(featureFlag, Template(strings, templateArgs)))) if !annotatedCapture.contains(symbols.builtins.AsyncCapability.capture) =>
          bindingAll(params.zip(args.map(toJS))) {
            js.RawExpr(strings, templateArgs.map(toJS))
          }
      case _ => js.Call(nameRef(id), args.map(toJS))
    }

  def canInline(extern: cps.Extern): Boolean = extern match {
    case cps.Extern.Def(id, vparams, Nil, annotatedCapture, ExternBody.StringExternBody(featureFlag, Template(strings, templateArgs))) =>
      !annotatedCapture.contains(symbols.builtins.AsyncCapability.capture)
    case _ => false
  }
}
