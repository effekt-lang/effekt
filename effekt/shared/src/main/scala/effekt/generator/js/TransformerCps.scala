package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.cps.*
import effekt.core.{ DeclarationContext, Id }
import effekt.cps.Variables.{ all, free }
import effekt.cps.substitutions.Substitution
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

  class RecursiveUsage(var jumped: Boolean)
  case class RecursiveDefInfo(id: Id, label: Id, vparams: List[Id], bparams: List[Id], ks: Id, k: Id, used: RecursiveUsage)
  case class ContinuationInfo(k: Id, param: Id, ks: Id)

  case class TransformerContext(
    requiresThunk: Boolean,
    // definitions of externs (used to inline them)
    externs: Map[Id, cps.Extern.Def],
    // the innermost (in direct style) enclosing functions (used to rewrite a definition to a loop)
    recursive: Option[RecursiveDefInfo],
    // the direct-style continuation, if available (used in case cps.Stmt.LetCont)
    directStyle: Option[ContinuationInfo],
    // the current direct-style metacontinuation
    metacont: Option[Id],
    // the original declaration context (used to compile pattern matching)
    declarations: DeclarationContext,
    // the usual compiler context
    errors: Context
  )
  implicit def autoContext(using C: TransformerContext): Context = C.errors


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
          externs.collect { case d: Extern.Def => (d.id, d) }.toMap,
          None,
          None,
          None,
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
          input.externs.collect { case d: Extern.Def => (d.id, d) }.toMap,
          None,
          None,
          None,
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
      val used = new RecursiveUsage(false)
      val label = Id(id)

      val translatedBody = toJS(body)(using recursive(id, label, used, b)).stmts

      if used.jumped then
        js.Lambda(vparams.map(nameDef) ++ bparams.map(nameDef) ++ List(nameDef(ks), nameDef(k)),
          List(js.While(RawExpr("true"), translatedBody, Some(uniqueName(label)))))
      else
        js.Lambda(vparams.map(nameDef) ++ bparams.map(nameDef) ++ List(nameDef(ks), nameDef(k)),
          translatedBody)

    case other => toJS(other)
  }

  def toJS(b: cps.Block)(using TransformerContext): js.Expr = b match {
    case cps.BlockVar(v)  => nameRef(v)
    case cps.Unbox(e)     => toJS(e)
    case cps.New(handler) => toJS(handler)

    case cps.BlockLit(vps, bps, ks, k, body) =>
      js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJS(body).stmts)
  }

  def argumentToJS(b: cps.Block)(using TransformerContext): js.Expr = b match {
    case cps.BlockLit(vps, bps, ks, k, body) => toJS(b)(using nonrecursive(ks))
    case other => toJS(b)
  }

  def toJS(handler: cps.Implementation)(using TransformerContext): js.Expr = handler match {
    case cps.Implementation(interface, operations) =>
      js.Object(operations.map {
        case cps.Operation(id, vps, bps, ks, k, body) =>
          nameDef(id) -> js.Lambda(vps.map(nameDef) ++ bps.map(nameDef) ++ List(nameDef(ks), nameDef(k)), toJS(body)(using nonrecursive(ks)).stmts)
      })
  }

  def toJS(ks: cps.MetaCont)(using T: TransformerContext): js.Expr = nameRef(ks.id)

  def toJS(k: cps.Cont)(using T: TransformerContext): js.Expr = k match {
    case Cont.ContVar(id) =>
      nameRef(id)
    case Cont.ContLam(result, ks, body) =>
      js.Lambda(List(nameDef(result), nameDef(ks)), toJS(body)(using nonrecursive(ks)).stmts)
    case Cont.Abort => js.Undefined
  }

  def toJS(e: cps.Expr)(using D: TransformerContext): js.Expr = e match {
    case Pure.ValueVar(id)           => nameRef(id)
    case Pure.Literal(())            => $effekt.field("unit")
    case Pure.Literal(s: String)     => JsString(escape(s))
    case literal: Pure.Literal       => js.RawExpr(literal.value.toString)
    case DirectApp(id, vargs, Nil)   => inlineExtern(id, vargs)
    case DirectApp(id, vargs, bargs) => js.Call(nameRef(id), vargs.map(toJS) ++ bargs.map(argumentToJS))
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

    // [[ let k(x, ks) = ...; if (...) jump k(42, ks2) else jump k(10, ks3) ]] =
    //    let x; if (...) { x = 42; ks = ks2 } else { x = 10; ks = ks3 } ...
    case cps.Stmt.LetCont(id, Cont.ContLam(param, ks, body), body2) if canBeDirect(id, body2) =>
      Binding { k =>
        js.Let(nameDef(param), js.Undefined) ::
          toJS(body2)(using markDirectStyle(id, param, ks)).stmts ++
          toJS(maintainDirectStyle(ks, body)).run(k)
      }

    case cps.Stmt.LetCont(id, binding @ Cont.ContLam(result2, ks2, body2), body) =>
      Binding { k =>
        js.Const(nameDef(id), toJS(binding)(using nonrecursive(ks2))) :: requiringThunk { toJS(body) }.run(k)
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

          val stmts = binding.stmts

          stmts.last match {
            case terminator : (js.Stmt.Return | js.Stmt.Break | js.Stmt.Continue) => (e, stmts)
            case other => (e, stmts :+ js.Break())
          }
        },
        default.map { s => toJS(s).stmts }))

    case cps.Stmt.Jump(k, arg, ks) if D.directStyle.exists(c => c.k == k) => D.directStyle match {
      case Some(ContinuationInfo(k2, param2, ks2)) => pure(js.Assign(nameRef(param2), toJS(arg)))
      case None => sys error "Should not happen"
    }

    case cps.Stmt.Jump(k, arg, ks) =>
      pure(js.Return(maybeThunking(js.Call(nameRef(k), toJS(arg), toJS(ks)))))


    case cps.Stmt.App(Recursive(id, label, vparams, bparams, ks1, k1, used), vargs, bargs, MetaCont(ks), k) =>
      Binding { k2 =>
        val stmts = mutable.ListBuffer.empty[js.Stmt]
        stmts.append(js.RawStmt("/* prepare call */"))

        used.jumped = true

        // We need to create temporaries for all free variables that appear in arguments
        val freeInArgs = (vargs.flatMap(Variables.free) ++ bargs.flatMap(Variables.free)).toSet
        // Only compute free vars of continuation if it's not a ContVar
        val (isTailCall, freeInK) = k match {
          case Cont.ContVar(kid) => (kid == k1 && ks == ks1, Set.empty[Id])
          case _ => (false, Variables.free(k))
        }
        val allFreeVars = freeInArgs ++ freeInK
        val needsKsTmp = allFreeVars.contains(ks1)
        val overlapping = allFreeVars.intersect((vparams ++ bparams).toSet)

        // Create temporaries for parameters that are used in the arguments or continuation
        val paramTmps = overlapping.map { param =>
          val tmp = Id(s"tmp_${param}")
          stmts.append(js.Const(nameDef(tmp), nameRef(param)))
          param -> tmp
        }.toMap

        val tmp_ks = if (needsKsTmp) {
          val tmp = Id("tmp_ks")
          stmts.append(js.Const(nameDef(tmp), nameRef(ks1)))
          Some(tmp)
        } else None

        // For non-tail calls, we need a continuation temporary if it's not a simple variable rename
        val tmp_k = if (!isTailCall) k match {
          case Cont.ContVar(kid) if kid != k1 =>
            // simple continuation variable, no need for temp binding
            None
          case _ =>
            val tmp = Id("tmp_k")
            stmts.append(js.Const(nameDef(tmp), nameRef(k1)))
            Some(tmp)
        } else {
          // For tail calls, only create temporary if k1 appears in arguments
          if (freeInArgs.contains(k1)) {
            val tmp = Id("tmp_k")
            stmts.append(js.Const(nameDef(tmp), nameRef(k1)))
            Some(tmp)
          } else None
        }

        // Prepare the substitution
        val subst = Substitution(
          values = paramTmps.map { case (p, t) => p -> Pure.ValueVar(t) },
          blocks = Map.empty,
          conts = tmp_k.map(t => k1 -> Cont.ContVar(t)).toMap,
          metaconts = tmp_ks.map(t => ks1 -> MetaCont(t)).toMap
        )

        // Update the continuation if this is not a tail call
        if (!isTailCall) k match {
          case Cont.ContVar(kid) if kid != k1 =>
            // simple variable rename
            stmts.append(js.Assign(nameRef(k1), nameRef(kid)))
          case _ =>
            stmts.append(js.Assign(nameRef(k1), toJS(substitutions.substitute(k)(using subst))))
        }

        // Assign the substituted arguments
        (vparams zip vargs).foreach { (param, arg) =>
          stmts.append(js.Assign(nameRef(param), toJS(substitutions.substitute(arg)(using subst))))
        }
        (bparams zip bargs).foreach { (param, arg) =>
          stmts.append(js.Assign(nameRef(param), argumentToJS(substitutions.substitute(arg)(using subst))))
        }

        // Restore metacont if needed
        if (needsKsTmp) stmts.append(js.Assign(nameRef(ks1), nameRef(tmp_ks.get)))

        val jump = js.Continue(Some(uniqueName(label)))
        stmts.appendAll(k2(jump))
        stmts.toList
      }

    case cps.Stmt.App(callee, vargs, bargs, ks, k) =>
      pure(js.Return(js.Call(toJS(callee), vargs.map(toJS) ++ bargs.map(argumentToJS) ++ List(toJS(ks),
        requiringThunk { toJS(k) }))))

    case cps.Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      val args = vargs.map(toJS) ++ bargs.map(argumentToJS) ++ List(toJS(ks), toJS(k))
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
      pure(js.Return(Call(RESET, requiringThunk { toJS(prog)(using nonrecursive(prog)) }, toJS(ks), toJS(k))))

    case cps.Stmt.Shift(prompt, body, ks, k) =>
      pure(js.Return(Call(SHIFT, nameRef(prompt), requiringThunk { toJS(body)(using nonrecursive(body)) }, toJS(ks), toJS(k))))

    case cps.Stmt.Resume(r, b, ks2, k2) =>
      pure(js.Return(js.Call(RESUME, nameRef(r), toJS(b)(using nonrecursive(b)), toJS(ks2), requiringThunk { toJS(k2) })))

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

  private def inlineExtern(id: Id, args: List[cps.Pure])(using T: TransformerContext): js.Expr =
    T.externs.get(id) match {
      case Some(cps.Extern.Def(id, params, Nil, async,
        ExternBody.StringExternBody(featureFlag, Template(strings, templateArgs)))) if !async =>
          val subst = substitutions.Substitution(
            values = (params zip args).toMap,
            blocks = Map.empty,
            conts = Map.empty,
            metaconts = Map.empty
          )

          // Apply substitution to template arguments
          val substitutedArgs = templateArgs.map { arg =>
            toJS(substitutions.substitute(arg)(using subst))
          }

          js.RawExpr(strings, substitutedArgs)
      case _ => js.Call(nameRef(id), args.map(toJS))
    }

  private def canInline(extern: cps.Extern): Boolean = extern match {
    case cps.Extern.Def(_, _, Nil, async, ExternBody.StringExternBody(_, Template(_, _))) => !async
    case _ => false
  }

  // Helpers for Direct-Style Transformation
  // ---------------------------------------

  /**
   * Marks continuation `id` to be optimized to direct assignments to `param` instead of return statements.
   * This is only valid in the same metacontinuation scope `ks`.
   */
  private def markDirectStyle(id: Id, param: Id, ks: Id)(using C: TransformerContext): TransformerContext =
    C.copy(directStyle = Some(ContinuationInfo(id, param, ks)))

  private def recursive(id: Id, label: Id, used: RecursiveUsage, block: cps.Block)(using C: TransformerContext): TransformerContext = block match {
    case cps.BlockLit(vparams, bparams, ks, k, body) =>
      C.copy(recursive = Some(RecursiveDefInfo(id, label, vparams, bparams, ks, k, used)), directStyle = None, metacont = Some(ks))
    case _ => C
  }

  private def nonrecursive(ks: Id)(using C: TransformerContext): TransformerContext =
    C.copy(recursive = None, directStyle = None, metacont = Some(ks))

  private def nonrecursive(block: cps.BlockLit)(using C: TransformerContext): TransformerContext = nonrecursive(block.ks)

  /**
   * Ensures let-bound continuations can stay in direct style by aligning metacont scopes.
   * This is used when the let-bound body jumps to an outer continuation.
   *   ks |  let k1 x1 ks1 = { let k2 x2 ks2 = jump k v ks2 }; ...  = jump k v ks
   */
  private def maintainDirectStyle(ks: Id, body: Stmt)(using C: TransformerContext): Stmt = {
    val outer = C.metacont.getOrElse { sys error "Metacontinuation missing..." }
    substitutions.substitute(body)(using Substitution(metaconts = Map(ks -> MetaCont(outer))))
  }

  private object Recursive {
    def unapply(b: cps.Block)(using C: TransformerContext): Option[(Id, Id, List[Id], List[Id], Id, Id, RecursiveUsage)] = b match {
      case cps.Block.BlockVar(id) => C.recursive.collect {
        case RecursiveDefInfo(id2, label, vparams, bparams, ks, k, used) if id == id2 => (id, label, vparams, bparams, ks, k, used)
      }
      case _ => None
    }
  }

  private def canBeDirect(k: Id, stmt: Stmt)(using T: TransformerContext): Boolean =
    def notIn(term: Stmt | Block | Expr | (Id, Clause) | Cont) =
      val freeVars = term match {
        case s: Stmt => free(s)
        case b: Block => free(b)
        case p: Expr => free(p)
        case (id, Clause(_, body)) => free(body)
        case c: Cont => free(c)
      }
      !freeVars.contains(k)
    stmt match {
      case Stmt.Jump(k2, arg, ks2) if k2 == k => notIn(arg) && T.metacont.contains(ks2.id)
      case Stmt.Jump(k2, arg, ks2) => notIn(arg)
      // TODO this could be a tailcall!
      case Stmt.App(callee, vargs, bargs, ks, k) => notIn(stmt)
      case Stmt.Invoke(callee, method, vargs, bargs, ks, k2) => notIn(stmt)
      case Stmt.If(cond, thn, els) => canBeDirect(k, thn) && canBeDirect(k, els)
      case Stmt.Match(scrutinee, clauses, default) => clauses.forall {
        case (id, Clause(vparams, body)) => canBeDirect(k, body)
      } && default.forall(body => canBeDirect(k, body))
      case Stmt.LetDef(id, binding, body) => notIn(binding) && canBeDirect(k, body)
      case Stmt.LetExpr(id, binding, body) => notIn(binding) && canBeDirect(k, body)
      case Stmt.LetCont(id, Cont.ContLam(result, ks2, body), body2) =>
        def willBeDirectItself = canBeDirect(id, body2) && canBeDirect(k, maintainDirectStyle(ks2, body))
        def notFreeinContinuation = notIn(body) && canBeDirect(k, body2)
        willBeDirectItself || notFreeinContinuation
      case Stmt.Region(id, ks, body) => notIn(body)
      case Stmt.Alloc(id, init, region, body) => notIn(init) && canBeDirect(k, body)
      case Stmt.Var(id, init, ks2, body) => notIn(init) && canBeDirect(k, body)
      case Stmt.Dealloc(ref, body) => canBeDirect(k, body)
      case Stmt.Get(ref, id, body) => canBeDirect(k, body)
      case Stmt.Put(ref, value, body) => notIn(value) && canBeDirect(k, body)
      case Stmt.Reset(prog, ks, k) => notIn(stmt)
      case Stmt.Shift(prompt, body, ks, k) => notIn(stmt)
      case Stmt.Resume(resumption, body, ks, k) => notIn(stmt)
      case Stmt.Hole() => true
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
