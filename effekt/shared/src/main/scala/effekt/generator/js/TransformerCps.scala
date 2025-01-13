package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cps.*
import effekt.core.{ DeclarationContext, Id }

import scala.collection.mutable

object TransformerCps extends Transformer {

  // Defined in effekt_runtime.js
  // ---------------------------
  val RUN_TOPLEVEL = Variable(JSName("RUN_TOPLEVEL"))
  val RESET = Variable(JSName("RESET"))
  val SHIFT = Variable(JSName("SHIFT"))
  val RESUME = Variable(JSName("RESUME"))
  val THUNK = Variable(JSName("THUNK"))
  val DEALLOC = Variable(JSName("DEALLOC"))
  val TRAMPOLINE = Variable(JSName("TRAMPOLINE"))

  class Used(var used: Boolean)
  case class DefInfo(id: Id, vparams: List[Id], bparams: List[Id], ks: Id, k: Id, used: Used)

  object DefInfo {
    def unapply(b: cps.Block)(using C: TransformerContext): Option[(Id, List[Id], List[Id], Id, Id, Used)] = b match {
      case cps.Block.BlockVar(id) => C.definitions.get(id) match {
        case Some(DefInfo(id, vparams, bparams, ks, k, used)) => Some((id, vparams, bparams, ks, k, used))
        case None => None
      }
      case _ => None
    }
  }

  case class TransformerContext(
    requiresThunk: Boolean,
    bindings: Map[Id, js.Expr],
    // definitions of externs (used to inline them)
    externs: Map[Id, cps.Extern.Def],
    // currently, lexically enclosing functions and their parameters (used to determine whether a call is recursive, to rewrite into a loop)
    definitions: Map[Id, DefInfo],
    // the original declaration context (used to compile pattern matching)
    declarations: DeclarationContext,
    // the usual compiler context
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors

  def lookup(id: Id)(using C: TransformerContext): js.Expr = C.bindings.getOrElse(id, nameRef(id))

  def enterDefinition(id: Id, used: Used, block: cps.Block)(using C: TransformerContext): TransformerContext = block match {
    case cps.BlockLit(vparams, bparams, ks, k, body) =>
      C.copy(definitions = Map(id -> DefInfo(id, vparams, bparams, ks, k, used)))
    case _ => C
  }

  def clearDefinitions(using C: TransformerContext): TransformerContext = C.copy(definitions = Map.empty)

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
          Map.empty,
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
          Map.empty,
          D, C)

    input.definitions.map(toJS)


  def toJS(d: cps.ToplevelDefinition)(using TransformerContext): js.Stmt = d match {
    case cps.ToplevelDefinition.Def(id, block) =>
      js.Const(nameDef(id), requiringThunk { toJS(id, block) })
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

  def toJS(id: Id, b: cps.Block)(using TransformerContext): js.Expr = b match {
    case cps.Block.BlockLit(vparams, bparams, ks, k, body) =>
      val used = new Used(false)

      val translatedBody = toJS(body)(using enterDefinition(id, used, b)).stmts

      if used.used then
        js.Lambda(vparams.map(nameDef) ++ bparams.map(nameDef) ++ List(nameDef(ks), nameDef(k)),
          List(js.While(RawExpr("true"), translatedBody, Some(uniqueName(id)))))
      else
        js.Lambda(vparams.map(nameDef) ++ bparams.map(nameDef) ++ List(nameDef(ks), nameDef(k)),
          translatedBody)

    case other => toJS(other)(using clearDefinitions)
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
          nameDef(id) -> js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJS(body)(using clearDefinitions).stmts)
      })
  }

  def toJS(ks: cps.MetaCont): js.Expr = nameRef(ks.id)

  def toJS(k: cps.Cont)(using T: TransformerContext): js.Expr = k match {
    case Cont.ContVar(id) =>
      nameRef(id)
    case Cont.ContLam(result, ks, body) =>
      js.Lambda(List(nameDef(result), nameDef(ks)), toJS(body)(using clearDefinitions).stmts)
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
    case Pure.Box(b)                 => toJS(b)
  }

  def toJS(s: cps.Stmt)(using D: TransformerContext): Binding[js.Stmt] = s match {

    case cps.Stmt.LetDef(id, block, body) =>
      Binding { k =>
        js.Const(nameDef(id), requiringThunk { toJS(id, block) }) :: toJS(body).run(k)
      }

    case cps.Stmt.If(cond, thn, els) =>
      pure(js.If(toJS(cond), toJS(thn).block, toJS(els).block))

    case cps.Stmt.LetExpr(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: toJS(body).run(k)
      }

    case cps.Stmt.LetCont(id, binding, body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)) :: requiringThunk { toJS(body)(using clearDefinitions) }.run(k)
      }

    case cps.Stmt.Match(sc, Nil, None) =>
      pure(js.Return($effekt.call("emptyMatch")))

    case cps.Stmt.Match(sc, List((tag, clause)), None) =>
      val scrutinee = toJS(sc)
      val (_, stmts) = toJS(scrutinee, tag, clause)
      stmts

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case cps.Stmt.Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)

      pure(js.Switch(js.Member(scrutinee, `tag`),
        clauses.map { case (tag, clause) =>
          val (e, binding) = toJS(scrutinee, tag, clause);
          (e, binding.stmts)
        },
        default.map { s => toJS(s).stmts }))

    case cps.Stmt.Jump(k, arg, ks) =>
      pure(js.Return(maybeThunking(js.Call(nameRef(k), toJS(arg), toJS(ks)))))

    case cps.Stmt.App(callee @ DefInfo(id, vparams, bparams, ks1, k1, used), vargs, bargs, MetaCont(ks), Cont.ContVar(k)) if ks1 == ks && k1 == k =>
      Binding { k2 =>
        val stmts = mutable.ListBuffer.empty[js.Stmt]
        stmts.append(js.RawStmt("/* prepare tail call */"))

        used.used = true

        // const x3 = [[ arg ]]; ...
        val vtmps = (vparams zip vargs).map { (id, arg) =>
          val tmp = Id(id)
          stmts.append(js.Const(nameDef(tmp), toJS(arg)))
          tmp
        }
        val btmps = (bparams zip bargs).map { (id, arg) =>
          val tmp = Id(id)
          stmts.append(js.Const(nameDef(tmp), toJS(arg)))
          tmp
        }

        // x = x3;
        (vparams zip vtmps).foreach {
          (param, tmp) => stmts.append(js.Assign(nameRef(param), nameRef(tmp)))
        }
        (bparams zip btmps).foreach {
          (param, tmp) => stmts.append(js.Assign(nameRef(param), nameRef(tmp)))
        }

        // continue f;
        val jump = js.Continue(Some(uniqueName(id)));

        stmts.appendAll(k2(jump))
        stmts.toList
      }

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

    case cps.Stmt.Reset(prog, ks, k) =>
      pure(js.Return(Call(RESET, toJS(prog)(using clearDefinitions), toJS(ks), toJS(k))))

    case cps.Stmt.Shift(prompt, body, ks, k) =>
      pure(js.Return(Call(SHIFT, nameRef(prompt), noThunking { toJS(body)(using clearDefinitions) }, toJS(ks), toJS(k))))

    case cps.Stmt.Resume(r, b, ks2, k2) =>
      pure(js.Return(js.Call(RESUME, nameRef(r), toJS(b)(using clearDefinitions), toJS(ks2), toJS(k2))))

    case cps.Stmt.Hole() =>
      pure(js.Return($effekt.call("hole")))
  }

  def toJS(scrutinee: js.Expr, variant: Id, clause: cps.Clause)(using C: TransformerContext): (js.Expr, Binding[js.Stmt]) =
    clause match {
      case cps.Clause(vparams, body) =>
        val fields = C.declarations.getConstructor(variant).fields.map(_.id)
        val tag = js.RawExpr(C.declarations.getConstructorTag(variant).toString)

        val freeVars = cps.Variables.free(body)
        def isUsed(x: Id) = freeVars contains x

        val extractedFields = (vparams zip fields).collect { case (p, f) if isUsed(p) =>
          js.Const(nameDef(p), js.Member(scrutinee, memberNameRef(f)))
        }

        (tag, Binding { k => extractedFields ++ toJS(body).run(k) })
    }

  def toJS(d: cps.Def)(using T: TransformerContext): js.Stmt = d match {
    case cps.Def(id, block) =>
      js.Const(nameDef(id), requiringThunk { toJS(id, block) })
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
