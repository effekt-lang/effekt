package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cps.*
import effekt.core.{ DeclarationContext, Id }

object TransformerCps extends Transformer {

  // Defined in effekt_runtime.js
  // ---------------------------
  val RUN_TOPLEVEL = Variable(JSName("RUN_TOPLEVEL"))
  val RESET = Variable(JSName("RESET"))
  val SHIFT = Variable(JSName("SHIFT"))
  val THUNK = Variable(JSName("THUNK"))
  val DEALLOC = Variable(JSName("DEALLOC"))
  val TRAMPOLINE = Variable(JSName("TRAMPOLINE"))

  case class TransformerContext(
    requiresThunk: Boolean,
    bindings: Map[Id, js.Expr],
    externs: Map[Id, cps.Extern.Def],
    declarations: DeclarationContext, // to be refactored
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors

  def lookup(id: Id)(using C: TransformerContext): js.Expr = C.bindings.getOrElse(id, nameRef(id))

  def bindingAll[R](bs: List[(Id, js.Expr)])(body: TransformerContext ?=> R)(using C: TransformerContext): R =
    body(using C.copy(bindings = C.bindings ++ bs))

  /**
   * Entrypoint used by the compiler to compile whole programs
   */
  def compile(input: cps.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil,
      js.Return(Call(RUN_TOPLEVEL, nameRef(mainSymbol))))))

    given DeclarationContext = new DeclarationContext(coreModule.declarations, coreModule.externs)
    toJS(input, exports)

  def toJS(module: cps.ModuleDecl, exports: List[js.Export])(using D: DeclarationContext, C: Context): js.Module =
    module match {
      case cps.ModuleDecl(path, includes, declarations, externs, definitions, _) =>
        given TransformerContext(
          false,
          Map.empty,
          externs.collect { case d: Extern.Def => (d.id, d) }.toMap,
          D, C)

        val name      = JSName(jsModuleName(module.path))
        val jsExterns = module.externs.filterNot(canInline).map(toJS)
        val jsDecls   = module.declarations.flatMap(toJS)
        val stmts     = module.definitions.map(toJS)

        val state = js.Const(
          nameDef(symbols.builtins.globalRegion),
          js.Variable(JSName("global"))
        ) :: Nil

        js.Module(name, Nil, exports, jsDecls ++ jsExterns ++ state ++ stmts)
    }

  def compileLSP(input: cps.ModuleDecl, coreModule: core.ModuleDecl)(using C: Context): List[js.Stmt] =
    val D = new DeclarationContext(coreModule.declarations, coreModule.externs)
    given TransformerContext(
          false,
          Map.empty,
          input.externs.collect { case d: Extern.Def => (d.id, d) }.toMap,
          D, C)

    input.definitions.map(toJS)


  def toJS(d: cps.ToplevelDefinition)(using TransformerContext): js.Stmt = d match {
    case cps.ToplevelDefinition.Def(id, block) =>
      js.Const(nameDef(id), requiringThunk { toJS(block) })
    case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
      js.Const(nameDef(id), Call(RUN_TOPLEVEL, js.Lambda(List(nameDef(ks), nameDef(k)), toJS(binding).stmts)))
    case cps.ToplevelDefinition.Let(id, binding) =>
      js.Const(nameDef(id), toJS(binding))
  }

  def toJSParam(id: Id): JSName = nameDef(id)

  def toJS(e: cps.Extern)(using C: TransformerContext): js.Stmt = e match {
    case cps.Extern.Def(id, vps, bps, true, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          val ks = freshName("ks_")
          val k = freshName("k_")
          js.Function(nameDef(id), (vps ++ bps).map(toJSParam) ++ List(ks, k),
            List(js.Return(js.Call(toJS(contents), List(js.Variable(ks), js.Variable(k))))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), (vps ++ bps) map toJSParam, List(js.Return($effekt.call("hole"))))
      }


    case cps.Extern.Def(id, vps, bps, false, body) =>
      body match {
        case ExternBody.StringExternBody(_, contents) =>
          js.Function(nameDef(id), (vps ++ bps) map toJSParam, List(js.Return(toJS(contents))))
        case ExternBody.Unsupported(err) =>
          C.errors.report(err)
          js.Function(nameDef(id), (vps ++ bps) map toJSParam, List(js.Return($effekt.call("hole"))))
      }

    case cps.Extern.Include(ff, contents) =>
      js.RawStmt(contents)
  }

  def toJS(t: Template[Pure])(using TransformerContext): js.Expr =
    js.RawExpr(t.strings, t.args.map(toJS))

  def toJS(d: core.Declaration): List[js.Stmt] = d match {
    case core.Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }

    // interfaces are structurally typed at the moment, no need to generate anything.
    case core.Interface(id, tparams, operations) =>
      Nil
  }

  def toJS(b: cps.Block)(using TransformerContext): js.Expr = b match {
    case cps.BlockVar(v)  => nameRef(v)
    case cps.Unbox(e)     => toJS(e)
    case cps.New(handler) => toJS(handler)

    case cps.BlockLit(vps, bps, ks, k, body) =>
      js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJS(body).stmts)
  }

  def toJS(handler: cps.Implementation)(using TransformerContext): js.Expr = handler match {
    case cps.Implementation(interface, operations) =>
      js.Object(operations.map {
        case cps.Operation(id, vps, bps, ks, k, body) =>
          nameDef(id) -> js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJS(body).stmts)
      })
  }

  def toJS(ks: cps.MetaCont): js.Expr = nameRef(ks.id)

  def toJS(k: cps.Cont)(using T: TransformerContext): js.Expr = k match {
    case Cont.ContVar(id) =>
      nameRef(id)
    case Cont.ContLam(result, ks, body) =>
      js.Lambda(List(nameDef(result), nameDef(ks)), toJS(body).stmts)
  }

  def toJS(e: cps.Expr)(using D: TransformerContext): js.Expr = e match {
    case Pure.ValueVar(id)           => lookup(id)
    case Pure.Literal(())            => $effekt.field("unit")
    case Pure.Literal(s: String)     => JsString(escape(s))
    case literal: Pure.Literal       => js.RawExpr(literal.value.toString)
    case DirectApp(id, vargs, Nil)   => inlineExtern(id, vargs)
    case DirectApp(id, vargs, bargs) => js.Call(nameRef(id), vargs.map(toJS) ++ bargs.map(toJS))
    case Pure.PureApp(id, vargs)     => inlineExtern(id, vargs)
    case Pure.Make(data, tag, vargs) => js.New(nameRef(tag), vargs map toJS)
    case Pure.Select(target, field)   => js.Member(toJS(target), memberNameRef(field))
    case Pure.Box(b)                 => toJS(b)
  }

  def toJS(s: cps.Stmt)(using D: TransformerContext): Binding[js.Stmt] = s match {
    case cps.Stmt.Scope(defs, body) =>
      Binding { k =>
        defs.map(toJS) ++ toJS(body).run(k)
      }

    case cps.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJS(thn).block, toJS(els).block))

    case cps.Stmt.LetExpr(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJS(body).run(k)
      }

    case cps.Stmt.LetCont(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: requiringThunk { toJS(body) }.run(k)
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
        default.map { s => toJS(s).stmts }))

    case cps.Stmt.Jump(k, arg, ks) =>
      pure(js.Return(maybeThunking(js.Call(nameRef(k), toJS(arg), toJS(ks)))))

    case cps.Stmt.App(callee, vargs, bargs, ks, k) =>
      pure(js.Return(maybeThunking(js.Call(toJS(callee), vargs.map(toJS) ++ bargs.map(toJS) ++ List(toJS(ks),
        requiringThunk { toJS(k) })))))

    case cps.Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      val args = vargs.map(toJS) ++ bargs.map(toJS) ++ List(toJS(ks), toJS(k))
      pure(js.Return(MethodCall(toJS(callee), memberNameRef(method), args:_*)))

    // const r = ks.arena.newRegion(); body
    case cps.Stmt.Region(id, ks, body) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("newRegion"))) ::
          toJS(body).run(k)
      }

    // const x = r.alloc(init); body
    case cps.Stmt.Alloc(id, init, region, body) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(nameRef(region), JSName("fresh"), toJS(init))) ::
          toJS(body).run(k)
      }

    // const x = ks.arena.fresh(1); body
    case cps.Stmt.Var(id, init, ks, body) =>
      Binding { k =>
        js.Const(nameDef(id), js.MethodCall(js.Member(toJS(ks), JSName("arena")), JSName("fresh"), toJS(init))) ::
          toJS(body).run(k)
      }

    // DEALLOC(ref); body
    case cps.Stmt.Dealloc(ref, body) =>
      Binding { k =>
        js.ExprStmt(js.Call(DEALLOC, nameRef(ref))) ::
          toJS(body).run(k)
      }

    // const id = ref.value; body
    case cps.Stmt.Get(ref, id, body) =>
      Binding { k =>
        js.Const(nameDef(id), js.Member(nameRef(ref), JSName("value"))) ::
          toJS(body).run(k)
      }

    // ref.value = _value; body
    case cps.Stmt.Put(ref, value, body) => Binding { k =>
      js.Assign(js.Member(nameRef(ref), JSName("value")), toJS(value)) ::
        toJS(body).run(k)
    }

    case cps.Stmt.Reset(BlockLit(vparams, List(prompt), ks3, k3, body), ks2, k2) =>
      Binding { k =>
        js.Const(js.Pattern.Array(List(js.Pattern.Variable(nameDef(prompt)), js.Pattern.Variable(nameDef(ks3)), js.Pattern.Variable(nameDef(k3)))),
          Call(RESET, toJS(ks2), toJS(k2))) ::
          toJS(body).run(k)
      }
    case cps.Stmt.Reset(body, ks, k) => ???

    case cps.Stmt.Shift(prompt, BlockLit(vparams, List(resume), ks3, k3, body), ks2, k2) =>
      Binding { k =>
        js.Const(js.Pattern.Array(List(js.Pattern.Variable(nameDef(resume)), js.Pattern.Variable(nameDef(ks3)), js.Pattern.Variable(nameDef(k3)))),
          Call(SHIFT, nameRef(prompt), toJS(ks2), toJS(k2))) ::
          toJS(body).run(k)
      }
    case cps.Stmt.Shift(prompt, body, ks, k) => ???

    case cps.Stmt.Hole() =>
      pure(js.Return($effekt.call("hole")))
  }

  def toJS(scrutinee: js.Expr, variant: Id, clause: cps.Clause)(using C: TransformerContext): (js.Expr, List[js.Stmt]) =
    clause match {
      case cps.Clause(vparams, body) =>
        val fields = C.declarations.getConstructor(variant).fields.map(_.id)
        val tag = js.RawExpr(C.declarations.getConstructorTag(variant).toString)

        val freeVars = cps.Variables.free(body)
        def isUsed(x: Id) = freeVars contains x

        val extractedFields = (vparams zip fields).collect { case (p, f) if isUsed(p) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tag, extractedFields ++ toJS(body).stmts)
    }

  def toJS(d: cps.Def)(using T: TransformerContext): js.Stmt = d match {
    case cps.Def(id, block) => js.Const(nameDef(id), requiringThunk { toJS(block) })
  }


  // Inlining Externs
  // ----------------

  def inlineExtern(id: Id, args: List[cps.Pure])(using T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cps.Extern.Def(id, params, Nil, async,
        ExternBody.StringExternBody(featureFlag, Template(strings, templateArgs)))) if !async =>
          bindingAll(params.zip(args.map(toJS))) {
            js.RawExpr(strings, templateArgs.map(toJS))
          }
      case _ => js.Call(nameRef(id), args.map(toJS))
    }

  def canInline(extern: cps.Extern): Boolean = extern match {
    case cps.Extern.Def(_, _, Nil, async, ExternBody.StringExternBody(_, Template(_, _))) => !async
    case _ => false
  }


  // Thunking
  // --------

  def thunked(stmt: js.Stmt): js.Stmt = js.Return(js.Lambda(Nil, stmt))
  def thunked(expr: js.Expr): js.Expr = js.Lambda(Nil, expr)

  def requiringThunk[T](prog: TransformerContext ?=> T)(using C: TransformerContext): T =
    prog(using C.copy(requiresThunk = true))

  def noThunking[T](prog: TransformerContext ?=> T)(using C: TransformerContext): T =
    prog(using C.copy(requiresThunk = false))

  def maybeThunking(stmt: js.Stmt)(using T: TransformerContext): js.Stmt =
    if T.requiresThunk then thunked(stmt) else stmt

  def maybeThunking(expr: js.Expr)(using T: TransformerContext): js.Expr =
    if T.requiresThunk then thunked(expr) else expr
}
