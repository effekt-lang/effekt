package effekt
package core
package optimizer

import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR }

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Removes "cuts", that is it performs a step of computation if enough information
 * is available.
 *
 *    def foo(n: Int) = return n + 1
 *
 *    foo(42)
 *
 * becomes
 *
 *    def foo(n: Int) = return n + 1
 *    return 42 + 1
 *
 * removing the overhead of the function call. Under the following conditions,
 * cuts are _not_ removed:
 *
 * - the definition is recursive
 * - inlining would exceed the maxInlineSize
 *
 * If the function is called _exactly once_, it is inlined regardless of the maxInlineSize.
 */
object Normalizer { normal =>

  def assertNormal(t: Tree)(using E: ErrorReporter): Unit = Tree.visit(t) {
    // The only allowed forms are the following.
    // In the future, Stmt.Shift should also be performed statically.
    case Stmt.Val(_, _, binding: (Stmt.Reset | Stmt.Var | Stmt.App | Stmt.Invoke | Stmt.Region | Stmt.Shift | Stmt.Resume), body) =>
      assertNormal(binding); assertNormal(body)
    /*
    val x = if (...) { return 1 } else { return 2 }; s
    is always normalized to
    def joinpoint(x: Int) = s
    if (...) { joinpoint(1) } else { joinpoint(2) }
     */
    case t @ Stmt.Val(_, _, binding, body) =>
      E.warning(s"Not allowed as binding of Val: ${util.show(t)}")
    case t @ Stmt.App(b: BlockLit, targs, vargs, bargs) =>
      E.warning(s"Unreduced beta-redex: ${util.show(t)}")
    case t @ Stmt.Invoke(b: New, method, tpe, targs, vargs, bargs) =>
      E.warning(s"Unreduced beta-redex: ${util.show(t)}")
    case t @ Stmt.If(cond: Literal, thn, els) =>
      E.warning(s"Unreduced if: ${util.show(t)}")
    case t @ Stmt.Match(sc: Make, clauses, default) =>
      E.warning(s"Unreduced match: ${util.show(t)}")
  }


  case class Context(
    blocks: Map[Id, Block],
    exprs: Map[Id, Expr],
    decls: DeclarationContext,     // for field selection
    usage: mutable.Map[Id, Usage], // mutable in order to add new information after renaming
    maxInlineSize: Int,            // to control inlining and avoid code bloat
  ) {
    def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
    def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
  }

  private def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  private def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  private def isRecursive(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Recursive
      // We assume it is recursive, if (for some reason) we do not have information;
      // since reducing might diverge, otherwise.
      //
      // This is, however, a strange case since this means we call a function we deemed unreachable.
      // It _can_ happen, for instance, by updating the usage (subtracting) and not deadcode eliminating.
      // This is the case for examples/pos/bidirectional/scheduler.effekt
      case None => true // sys error s"No info for ${id}"
    }

  private def isOnce(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Once
      case None => false
    }

  private def isUnused(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id).forall { u => u == Usage.Never }

  def normalize(entrypoints: Set[Id], m: ModuleDecl, maxInlineSize: Int): ModuleDecl = {
    // usage information is used to detect recursive functions (and not inline them)
    val usage = Reachable(entrypoints, m)

    val defs = m.definitions.collect {
      case Toplevel.Def(id, block) => id -> block
    }.toMap
    val context = Context(defs, Map.empty, DeclarationContext(m.declarations, m.externs), mutable.Map.from(usage), maxInlineSize)

    val (normalizedDefs, _) = normalizeToplevel(m.definitions)(using context)

    m.copy(definitions = normalizedDefs)
  }

  private def normalizeToplevel(definitions: List[Toplevel])(using ctx: Context): (List[Toplevel], Context) =
    var contextSoFar = ctx
    val defs = definitions.map {
      case Toplevel.Def(id, block) =>
        val normalized = normalize(block)(using contextSoFar)
        contextSoFar = contextSoFar.bind(id, normalized)
        Toplevel.Def(id, normalized)

      case Toplevel.Val(id, tpe, binding) =>
        // TODO commute (similar to normalizeVal)
        // val foo = { val bar = ...; ... }   =   val bar = ...; val foo = ...;
        val normalized = normalize(binding)(using contextSoFar)
        normalized match {
          case Stmt.Return(expr) =>
            contextSoFar = contextSoFar.bind(id, expr)
          case normalized => ()
        }
        Toplevel.Val(id, tpe, normalized)
    }
    (defs, contextSoFar)

  private enum NormalizedBlock {
    case Known(b: BlockLit | New | Unbox, boundBy: Option[BlockVar])
    case Unknown(b: BlockVar)

    def dealiased: Block = this match {
      case NormalizedBlock.Known(b, boundBy) => b
      case NormalizedBlock.Unknown(b) => b
    }
    def shared: Block = this match {
      case NormalizedBlock.Known(b, boundBy) => boundBy.getOrElse(b)
      case NormalizedBlock.Unknown(b) => b
    }
  }

  /**
   * This is a bit tricky: depending on the call-site of `active`
   * we either want to find a redex (BlockLit | New), maximally dealias (in def bindings),
   * discover the outmost Unbox (when boxing again), or preserve some sharing otherwise.
   *
   * A good testcase to look at for this is:
   *   examples/pos/capture/regions.effekt
   */
  @tailrec
  private def active[R](b: Block)(using C: Context): NormalizedBlock =
    normalize(b) match {
      case b: Block.BlockLit   => NormalizedBlock.Known(b, None)
      case b @ Block.New(impl) => NormalizedBlock.Known(b, None)

      case x @ Block.BlockVar(id, annotatedTpe, annotatedCapt) => blockFor(id) match {
        case Some(b: (BlockLit | New | Unbox)) => NormalizedBlock.Known(b, Some(x))
        case _ => NormalizedBlock.Unknown(x)
      }
      case Block.Unbox(expr) => active(expr) match {
        case Expr.Box(b, annotatedCapture) => active(b)
        case other => NormalizedBlock.Known(Block.Unbox(expr), None)
      }
    }

  // TODO for `New` we should track how often each operation is used, not the object itself
  //   to decide inlining.
  private def shouldInline(b: BlockLit, boundBy: Option[BlockVar], blockArgs: List[Block])(using C: Context): Boolean = boundBy match {
    case Some(id) if isRecursive(id.id) => false
    case Some(id) => isOnce(id.id) || b.body.size <= C.maxInlineSize
    case _ => blockArgs.exists { b => b.isInstanceOf[BlockLit] } // higher-order function with known arg
  }

  private def active(e: Expr)(using Context): Expr =
    normalize(e) match {
      case x @ Expr.ValueVar(id, annotatedType) => exprFor(id) match {
        case Some(p: Expr.Make)    => p
        case Some(p: Expr.Literal) => p
        case Some(p: Expr.Box)     => p
        // We only inline non side-effecting expressions
        case Some(other) if other.capt.isEmpty  => other
        case _ => x // stuck
      }
      case other => other // stuck
    }

  def normalize(s: Stmt)(using C: Context): Stmt = s match {

    // see #798 for context (led to stack overflow)
    case Stmt.Def(id, block, body) if isUnused(id) =>
      normalize(body)

    case Stmt.Def(id, block, body) =>
      val normalized = active(block).dealiased
      Stmt.Def(id, normalized, normalize(body)(using C.bind(id, normalized)))

    case Stmt.Let(id, tpe, expr, body) =>
      active(expr) match {
        // [[ val x = ABORT; body ]] = ABORT
        case abort if abort.tpe == Type.TBottom =>
          Stmt.Let(id, tpe, abort, Return(ValueVar(id, tpe)))

        case normalized =>
          Stmt.Let(id, tpe, normalized, normalize(body)(using C.bind(id, normalized)))
      }

    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      Stmt.ImpureApp(id, callee, targs, vargs.map(normalize), bargs.map(normalize), normalize(body))

    // Redexes
    // -------
    case Stmt.App(b, targs, vargs, bargs) =>
      active(b) match {
        case NormalizedBlock.Known(b: BlockLit, boundBy) if shouldInline(b, boundBy, bargs) =>
          val blockUsage = boundBy.flatMap { bv => C.usage.get(bv.id) }.getOrElse(Usage.Once)
          if (blockUsage == Usage.Many) {
            // This is a conservative approximation:
            // Since the block is used more than once, we will use the free variables multiple times
            // after inlining.
            Variables.free(b).toSet.foreach { v =>
              C.usage.put(v.id, C.usage.getOrElse(v.id, Usage.Never) * Usage.Many)
            }
          }
          reduce(b, targs, vargs.map(normalize), bargs.map(normalize))
        case normalized =>
           Stmt.App(normalized.shared, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      active(b) match {
        case n @ NormalizedBlock.Known(Block.New(impl), boundBy) =>
          selectOperation(impl, method) match {
            case b: BlockLit if shouldInline(b, boundBy, bargs) => reduce(b, targs, vargs.map(normalize), bargs.map(normalize))
            case _ => Stmt.Invoke(n.shared, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
          }

        case normalized =>
          Stmt.Invoke(normalized.shared, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Match(scrutinee, clauses, default) if scrutinee.tpe == Type.TBottom =>
      Stmt.Return(normalize(scrutinee))

    case Stmt.Match(scrutinee, clauses, default) => active(scrutinee) match {
      case Expr.Make(data, tag, targs, vargs) if clauses.exists { case (id, _) => id == tag } =>
        val clause: BlockLit = clauses.collectFirst { case (id, cl) if id == tag => cl }.get
        normalize(reduce(clause, targs, vargs.map(normalize), Nil))
      case Expr.Make(data, tag, targs, vargs) if default.isDefined =>
        normalize(default.get)
      case _ =>
        val normalized = normalize(scrutinee)
        Stmt.Match(normalized, clauses.map { case (id, value) => id -> normalize(value) }, default.map(normalize))
    }

    // [[ if (true) stmt1 else stmt2 ]] = [[ stmt1 ]]
    case Stmt.If(cond, thn, els) => active(cond) match {
      case Expr.Literal(true, annotatedType) => normalize(thn)
      case Expr.Literal(false, annotatedType) => normalize(els)
      case _ => If(normalize(cond), normalize(thn), normalize(els))
    }

    case Stmt.Val(id, tpe, binding, body) =>

      def joinpoint(id: Id, tpe: ValueType, body: Stmt)(f: BlockVar => Context ?=> Stmt)(using C: Context): Stmt = body match {
        // do not eta-expand variables
        case Stmt.App(k: BlockVar, Nil, ValueVar(x, tpe) :: Nil, Nil) if x == id || tpe == Type.TUnit => f(k)
        case _ =>
          val k = Id("k")
          C.usage.put(k, Usage.Many)
          val kDef = Block.BlockLit(Nil, Nil, ValueParam(id, tpe) :: Nil, Nil, body)
          Stmt.Def(k, kDef, f(Block.BlockVar(k, kDef.tpe, kDef.capt))(using C.bind(k, kDef)))
      }

      def normalizeVal(id: Id, tpe: ValueType, binding: Stmt, body: Stmt)(using C: Context): Stmt = normalize(binding) match {

        // [[ val x = ABORT; body ]] = ABORT
        case abort if abort.tpe == Type.TBottom  =>
          abort

        // [[ val x: A = shift(p) { {k: A => R} => body2 }; body: B ]] = shift(p) { {k: >>>B<<< => R} => body2 }
        case abort @ Stmt.Shift(p, BlockLit(tparams, cparams, vparams,
          BlockParam(k, BlockType.Interface(Type.ResumeSymbol, List(tpeA, answer)), captures) :: Nil, body2))
              if !Variables.free(body2).containsBlock(k) =>
            val tpeB = body.tpe
            Stmt.Shift(p,
              BlockLit(tparams, cparams, vparams, BlockParam(k, BlockType.Interface(Type.ResumeSymbol, List(tpeB, answer)), captures) :: Nil,
                normalize(body2)))

        case Stmt.Match(sc, List((id2, BlockLit(tparams2, cparams2, vparams2, bparams2, body2))), None) =>
          Stmt.Match(sc, List((id2, BlockLit(tparams2, cparams2, vparams2, bparams2,
            normalizeVal(id, tpe, body2, body)))), None)

        // Introduce joinpoints that are potentially later inlined or garbage collected
        // [[ val x = if (cond) { thn } else { els }; body ]] =
        //   def k(x) = [[ body ]]
        //   if (cond) { [[ val x1 = thn; k(x1) ]] } else { [[ val x2 = els; k(x2) ]] }
        case Stmt.If(cond, thn, els) =>
          joinpoint(id, tpe, normalize(body)) { k =>
            val x1 = Id(id.name)
            val x2 = Id(id.name)
            Stmt.If(cond,
              normalizeVal(x1, tpe, thn, Stmt.App(k, Nil, List(ValueVar(x1, tpe)), Nil)),
              normalizeVal(x2, tpe, els, Stmt.App(k, Nil, List(ValueVar(x2, tpe)), Nil)))
          }

        case Stmt.Match(sc, clauses, default) =>
          joinpoint(id, tpe, normalize(body)) { k =>
            Stmt.Match(sc, clauses.map {
              case (tag, BlockLit(tparams, cparams, vparams, bparams, body)) =>
                val x = Id(id.name)
                (tag, BlockLit(tparams, cparams, vparams, bparams,
                  normalizeVal(x, tpe, body, Stmt.App(k, Nil, List(ValueVar(x, tpe)), Nil))))
            }, default.map { stmt =>
              val x = Id(id.name)
              normalizeVal(x, tpe, stmt, Stmt.App(k, Nil, List(ValueVar(x, tpe)), Nil))
            })
          }

        // [[ val x = return e; s ]] = let x = [[ e ]]; [[ s ]]
        case Stmt.Return(expr2) =>
          Stmt.Let(id, tpe, expr2, normalize(body)(using C.bind(id, expr2)))

        // Commute val and bindings
        // [[ val x = { def f = ...; STMT }; STMT ]] = def f = ...; val x = STMT; STMT
        case Stmt.Def(id2, block2, body2) =>
          Stmt.Def(id2, block2, normalizeVal(id, tpe, body2, body))

        // Commute val and bindings
        // [[ val x = { let y = ...; STMT }; STMT ]] = let y = ...; val x = STMT; STMT
        case Stmt.Let(id2, tpe2, binding2, body2) =>
          Stmt.Let(id2, tpe2, binding2, normalizeVal(id, tpe, body2, body))

        case Stmt.ImpureApp(id2, callee2, targs2, vargs2, bargs2, body2) =>
          Stmt.ImpureApp(id2, callee2, targs2, vargs2, bargs2, normalizeVal(id, tpe, body2, body))

        // Flatten vals. This should be non-leaking since we use garbage free refcounting.
        // [[ val x = { val y = stmt1; stmt2 }; stmt3 ]] = [[ val y = stmt1; val x = stmt2; stmt3 ]]
        case Stmt.Val(id2, tpe2, binding2, body2) =>
          normalizeVal(id2, tpe2, binding2, normalizeVal(id, tpe, body2, body))

        // [[ val x = { var y in r = e; stmt2 }; stmt1 ]] = var y in r = e; [[ val x = stmt2; stmt1 ]]
        case Stmt.Alloc(id2, init2, region2, body2) =>
          Stmt.Alloc(id2, init2, region2, normalizeVal(id, tpe, body2, body))

        // [[ val x = { let x = !ref; stmt2 }; stmt1 ]] = let x = !ref; [[ val x = stmt2; stmt1 ]]
        case Stmt.Get(id2, tpe2, ref2, capt2, body2) =>
          Stmt.Get(id2, tpe2, ref2, capt2, normalizeVal(id, tpe, body2, body))

        // [[ val x = { ref := e; stmt2 }; stmt1 ]] = ref := e; [[ val x = stmt2; stmt1 ]]
        case Stmt.Put(ref2, capt2, value2, body2) =>
          Stmt.Put(ref2, capt2, value2, normalizeVal(id, tpe, body2, body))

        case other => normalize(body) match {
          // [[ val x = stmt; return x ]]   =   [[ stmt ]]
          case Stmt.Return(x: ValueVar) if x.id == id => other
          // [[ val x: Unit = stmt; return () ]]   =   [[ stmt ]]
          case Stmt.Return(x) if x.tpe == Type.TUnit && other.tpe == Type.TUnit => other
          // [[ val x = stmt; body ]]   =   val x = [[ stmt ]]; [[ body ]]
          case normalizedBody => Stmt.Val(id, tpe, other, normalizedBody)
        }
      }
      normalizeVal(id, tpe, binding, body)


    // "Congruences"
    // -------------

    case Stmt.Reset(body) => Stmt.Reset(normalize(body))
    case Stmt.Shift(prompt, body) => Shift(prompt, normalize(body))
    case Stmt.Return(expr) => Return(normalize(expr))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, normalize(init), region, normalize(body))
    case Stmt.Resume(k, body) => Resume(k, normalize(body))
    case Stmt.Region(body) => Region(normalize(body))
    case Stmt.Var(ref, init, capture, body) => Stmt.Var(ref, normalize(init), capture, normalize(body))
    case Stmt.Get(id, tpe, ref, capt, body) => Stmt.Get(id, tpe, ref, capt, normalize(body))
    case Stmt.Put(ref, capt, value, body) => Stmt.Put(ref, capt, normalize(value), normalize(body))
    case Stmt.Hole(span) => s
  }
  def normalize(b: BlockLit)(using Context): BlockLit =
    b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams, normalize(body))
    }

  def normalize(b: Block)(using Context): Block = b match {
    case b @ Block.BlockVar(id, _, _) => b
    case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) => normalize(b)

    // [[ unbox (box b) ]] = [[ b ]]
    case Block.Unbox(expr) => normalize(expr) match {
      case Expr.Box(b, _) => b
      case p => Block.Unbox(p)
    }
    case Block.New(impl) => New(normalize(impl))
  }

  def normalize(s: Implementation)(using Context): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = normalize(op.body))
      })
    }

  def normalize(p: Expr)(using ctx: Context): Expr = p match {
    // [[ box (unbox e) ]] = [[ e ]]
    case Expr.Box(b, annotatedCapture) => active(b) match {
      case NormalizedBlock.Known(Unbox(p), boundBy) => p
      case _ => normalize(b) match {
        case Block.Unbox(expr) => expr
        case b => Expr.Box(b, annotatedCapture)
      }
    }

    // congruences
    case Expr.PureApp(f, targs, vargs) => Expr.PureApp(f, targs, vargs.map(normalize))
    case Expr.Make(data, tag, targs, vargs) => Expr.Make(data, tag, targs, vargs.map(normalize))
    case Expr.ValueVar(id, annotatedType) => p
    case Expr.Literal(value, annotatedType) => p
  }

  // Helpers for beta-reduction
  // --------------------------

  private def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Expr], bargs: List[Block])(using C: Context): Stmt = {
    // To update usage information
    val usage = C.usage
    def copyUsage(from: Id, to: Id) = usage.get(from) match {
      case Some(info) => usage.update(to, info)
      case None => ()
    }

    // Only bind if not already a variable!!!
    var ids: Set[Id] = Set.empty
    var bindings: List[Binding] = Nil
    var bvars: List[Block.BlockVar] = Nil

    // (1) first bind
    (b.bparams zip bargs) foreach {
      case (bparam, x: Block.BlockVar) =>
        // Update usage: u1 + (u2 - 1)
        usage.update(x.id, usage.getOrElse(bparam.id, Usage.Never) + usage.getOrElse(x.id, Usage.Never).decrement)
        bvars = bvars :+ x
      // introduce a binding
      case (bparam, block) =>
        val id = symbols.TmpBlock("blockBinding")
        bindings = bindings :+ Binding.Def(id, block)
        bvars = bvars :+ Block.BlockVar(id, block.tpe, block.capt)
        copyUsage(bparam.id, id)
        ids += id
    }

    val (renamedLit: BlockLit, renamedIds) = Renamer.rename(b)

    renamedIds.foreach(copyUsage)


    // (2) substitute
    val body = substitutions.substitute(renamedLit, targs, vargs, bvars)

    normalize(Binding(bindings, body))
  }

  private def selectOperation(impl: Implementation, method: Id): Block.BlockLit =
    impl.operations.collectFirst {
      case Operation(name, tps, cps, vps, bps, body) if name == method => BlockLit(tps, cps, vps, bps, body): Block.BlockLit
    }.getOrElse { INTERNAL_ERROR("Should not happen") }
}
