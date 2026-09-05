package effekt
package core
package optimizer

import effekt.util.messages.INTERNAL_ERROR
import effekt.util.debug

import scala.annotation.{ tailrec, targetName }
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

  case class Context(
    blocks: Map[Id, Block],
    exprs: Map[Id, Expr],
    decls: DeclarationContext,     // for field selection
    usage: mutable.Map[Id, Usage], // mutable in order to add new information after renaming
    policy: InliningPolicy,        // whether to inline a call (see [[InliningPolicy]])
    facts: Map[Expr, Expr],        // maps a pure expression to something simpler it is known to equal
    prompts: Int,                  // how many enclosing `Reset`s we are inside (see [[Default.usedOnce]])
  ) {
    def enterPrompt: Context = copy(prompts = prompts + 1)

    // knowing `x = e`, we also know `e = x`, which is what lets us share `e`
    def bind(id: Id, expr: Expr): Context =
      val known = if shareable(expr)(using this) then facts + (expr -> ValueVar(id, expr.tpe)) else facts
      copy(exprs = exprs + (id -> expr), facts = known)

    def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))

    /** Records that [[expr]] equals the simpler [[value]] for the subtree we normalize next. */
    def knowing(expr: Expr, value: Expr): Context = expr match {
      // variables belong into the environment, which `active` already consults
      case ValueVar(id, _) => bind(id, value)
      case _ if shareable(expr)(using this) => copy(facts = facts + (expr -> value))
      case _ => this
    }
  }

  /**
   * Do the clauses name every constructor of the scrutinee's type?
   * If so, the default is unreachable!
   *
   * Beware: Deadcode might have dropped constructors that are never built.
   */
  private def covers(tpe: ValueType, clauses: List[(Id, BlockLit)])(using C: Context): Boolean = tpe match {
    case ValueType.Data(name, _) => C.decls.findData(name).exists { data =>
      clauses.length >= data.constructors.length && {
        val tags = clauses.iterator.map(_._1).toSet
        data.constructors.forall { c => tags.contains(c.id) }
      }
    }
    case _ => false
  }

  /** Within a branch, we know the value of the condition. */
  private def assuming(cond: Expr, value: Boolean)(using C: Context): Context =
    C.knowing(cond, Expr.Literal(value, Type.TBoolean))

  /** Within the clause for [[tag]], we know that [[scrutinee]] is a value of the data type with that tag. */
  private def selecting(scrutinee: Expr, tag: Id, clause: BlockLit)(using C: Context): Context = scrutinee.tpe match {
    case data: ValueType.Data => C.knowing(scrutinee, destructured(data, tag, clause))
    case _ => C
  }

  /** Creates the [[Expr.Make]] that represents the value of [[scrutinee]] in the context of [[clause]]. */
  private def destructured(data: ValueType.Data, tag: Id, clause: BlockLit): Expr.Make =
    Expr.Make(data, tag,
      clause.tparams.map(ValueType.Var.apply),
      clause.vparams.map { p => ValueVar(p.id, p.tpe) })

  /** Replaces an expression by something simpler that is known to be equal to it. */
  private def available(expr: Expr)(using ctx: Context): Expr =
    if shareable(expr) then ctx.facts.getOrElse(expr, expr) else expr

  /** Is it worth remembering that a variable holds pure expression [[expr]]? */
  private def shareable(expr: Expr)(using C: Context): Boolean = expr match {
    case _: Expr.PureApp => transparent(expr.tpe)
    case _: Expr.Make => true
    case _ => false
  }

  /** Are two equal values of this type interchangeable or could someone observe their identity? */
  private def transparent(tpe: ValueType)(using C: Context): Boolean = tpe match {
    case ValueType.Data(name, targs) => !C.decls.externDatas.contains(name) && targs.forall(transparent)
    case ValueType.Var(_) => false
    case ValueType.Boxed(_, _) => false
  }

  private def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  private def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  private[optimizer] def isRecursive(id: Id)(using ctx: Context): Boolean =
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

  private[optimizer] def isOnce(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Once
      case None => false
    }

  private def isUnused(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id).forall { u => u == Usage.Never }

  def normalize(entrypoints: Set[Id], m: ModuleDecl, policy: InliningPolicy): ModuleDecl = {
    // usage information is used to detect recursive functions (and not inline them)
    val usage = Reachable(entrypoints, m)

    val defs = m.definitions.collect {
      case Toplevel.Def(id, block) => id -> block
    }.toMap
    val context = Context(defs, Map.empty, DeclarationContext(m.declarations, m.externs), mutable.Map.from(usage), policy, Map.empty, 0)

    val (normalizedDefs, _) = normalizeToplevel(m.definitions)(using context)
    m.copy(definitions = normalizedDefs)
  }

  def normalizeToplevel(definitions: List[Toplevel])(using ctx: Context): (List[Toplevel], Context) =
    var contextSoFar = ctx
    val defs = definitions.map {
      case Toplevel.Def(id, block) =>
        val normalized = normalize(block)(using contextSoFar)
        contextSoFar = contextSoFar.bind(id, normalized)
        Toplevel.Def(id, normalized)

      case Toplevel.Val(id, binding) =>
        // TODO commute (similar to normalizeVal)
        // val foo = { val bar = ...; ... }   =   val bar = ...; val foo = ...;
        val normalized = normalize(binding)(using contextSoFar)
        normalized match {
          case Stmt.Return(expr) =>
            contextSoFar = contextSoFar.bind(id, expr)
          case normalized => ()
        }
        Toplevel.Val(id, normalized)
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
  private def shouldInline(b: BlockLit, boundBy: Option[BlockVar], valueArgs: List[Expr], blockArgs: List[Block])(using C: Context): Boolean =
    C.policy(CallSite(b, boundBy, valueArgs, blockArgs))

  private[optimizer] def active(e: Expr)(using Context): Expr =
    normalize(e) match {
      case x @ Expr.ValueVar(id, annotatedType) => exprFor(id) match {
        case Some(other) => other
        case None => x // stuck
      }
      case other => other // stuck
    }

  /**
   * [[ let x = e; body ]] = let x = y; [[ body ]]   if we already know `y = e`
   *
   * Shared with `val x = return e`. The lookup cannot live in `normalize(e: Expr)` alone, since
   * `active` dealiases and would resolve `y` back to `e` again.
   */
  private def normalizeLet(id: Id, expr: Expr, body: Stmt)(using C: Context): Stmt =
    val bound = available(expr)
    Stmt.Let(id, bound, normalize(body)(using C.bind(id, bound)))

  def normalize(s: Stmt)(using C: Context): Stmt = preserveTypes(s) {

    // see #798 for context (led to stack overflow)
    case Stmt.Def(id, block, body) if isUnused(id) =>
      normalize(body)

    case Stmt.Def(id, block, body) =>
      val normalized = active(block).dealiased
      Stmt.Def(id, normalized, normalize(body)(using C.bind(id, normalized)))

    case Stmt.Let(id, expr, body) =>
      active(expr) match {
        // [[ val x = ABORT; body ]] = ABORT
        //        case abort if abort.tpe == Type.TBottom =>
        //          Stmt.Let(id, abort, Return(ValueVar(id, tpe)))

        case normalized => normalizeLet(id, normalized, body)
      }

    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      Stmt.ImpureApp(id, callee, targs, vargs.map(normalize), bargs.map(normalize), normalize(body))

    // Redexes
    // -------
    case Stmt.App(b, targs, vargs, bargs) =>
      active(b) match {
        case NormalizedBlock.Known(b: BlockLit, boundBy) if shouldInline(b, boundBy, vargs, bargs) =>
          val blockUsage = boundBy.flatMap { bv => C.usage.get(bv.id) }.getOrElse(Usage.Once)
          if (blockUsage == Usage.Many) {
            // This is a conservative approximation:
            // Since the block is used more than once, we will use the free variables multiple times
            // after inlining.
            b.free.freeIds.foreach { v =>
              C.usage.put(v, C.usage.getOrElse(v, Usage.Never) * Usage.Many)
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
            case b: BlockLit if shouldInline(b, boundBy, vargs, bargs) => reduce(b, targs, vargs.map(normalize), bargs.map(normalize))
            case _ => Stmt.Invoke(n.shared, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
          }

        case normalized =>
          Stmt.Invoke(normalized.shared, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Match(scrutinee, tpe, clauses, default) => active(scrutinee) match {
      case Expr.Make(data, tag, targs, vargs) if clauses.exists { case (id, _) => id == tag } =>
        val clause: BlockLit = clauses.collectFirst { case (id, cl) if id == tag => cl }.get
        val result = reduce(clause, targs, vargs.map(normalize), Nil)
        util.assert(Type.equals(result.tpe, tpe))
        normalize(result)
      case Expr.Make(data, tag, targs, vargs) if default.isDefined =>
        normalize(default.get)
      case _ =>
        val normalized = normalize(scrutinee)
        Stmt.Match(normalized, tpe, clauses.map { case (tag, clause) =>
          tag -> normalize(clause)(using selecting(normalized, tag, clause))
        }, default.filter(_ => !covers(normalized.tpe, clauses)).map(normalize))
    }

    // [[ if (true) stmt1 else stmt2 ]] = [[ stmt1 ]]
    case Stmt.If(cond, thn, els) => active(cond) match {
      case Expr.Literal(true, annotatedType) => normalize(thn)
      case Expr.Literal(false, annotatedType) => normalize(els)
      case _ =>
        util.assert(Type.equals(thn.tpe, els.tpe), s"Then and else branch have different types: ${util.show(thn.tpe)} != ${util.show(els.tpe)}\n\n${util.show(thn)}\n\n${util.show(els)}\n\n${util.show(s)}")
        val condition = normalize(cond)
        If(condition,
          normalize(thn)(using assuming(condition, true)),
          normalize(els)(using assuming(condition, false)))
    }

    case Stmt.Val(id, binding, body) =>

      def joinpoint(id: Id, tpe: ValueType, body: Stmt)(f: BlockVar => Context ?=> Stmt)(using C: Context): Stmt = body match {
        // do not eta-expand variables
        case Stmt.App(k: BlockVar, Nil, ValueVar(x, tpe) :: Nil, Nil) if x == id || tpe == Type.TUnit => f(k)
        case _ =>
          val k = Id("k")
          C.usage.put(k, Usage.Many)
          val kDef = Block.BlockLit(Nil, Nil, ValueParam(id, tpe) :: Nil, Nil, body)
          Stmt.Def(k, kDef, f(Block.BlockVar(k, kDef.tpe, kDef.capt))(using C.bind(k, kDef)))
      }

      def normalizeVal(id: Id, binding: Stmt, body: Stmt)(using C: Context): Stmt = normalize(binding) match {

        // [[ val x: A = shift(p) { {k: A => R} => body2 }; body: B ]] = shift(p) { {k: >>>B<<< => R} => body2 }
        case abort @ Stmt.Shift(p, BlockParam(k, BlockType.Interface(Type.ResumeSymbol, List(tpeA, answer)), captures), body2)
              if !body2.free.freeIds.contains(k) =>
            val tpeB = body.tpe
            Stmt.Shift(p, BlockParam(k, BlockType.Interface(Type.ResumeSymbol, List(tpeB, answer)), captures),
                normalize(body2))

        // [[ val x: A = sc match [A] { case ... => body2: A }; body: B ]] == sc match [B] { case ... => [[ val x: A = body2; body: B ]] }
        case Stmt.Match(sc, tpe, List((id2, clause @ BlockLit(tparams2, cparams2, vparams2, bparams2, body2))), None) =>
          val res = normalizeVal(id, body2, body)(using selecting(sc, id2, clause))
          Stmt.Match(sc, res.tpe, List((id2, BlockLit(tparams2, cparams2, vparams2, bparams2, res))), None)

        // Introduce joinpoints that are potentially later inlined or garbage collected
        // [[ val x = if (cond) { thn } else { els }; body ]] =
        //   def k(x) = [[ body ]]
        //   if (cond) { [[ val x1 = thn; k(x1) ]] } else { [[ val x2 = els; k(x2) ]] }
        case Stmt.If(cond, thn, els) =>
          val tpe = thn.tpe
          util.assert(Type.equals(thn.tpe, els.tpe))
          joinpoint(id, tpe, normalize(body)) { k => (C: Context) ?=>
            val x1 = Id(id.name)
            val x2 = Id(id.name)
            Stmt.If(cond,
              normalizeVal(x1, thn, Stmt.App(k, Nil, List(ValueVar(x1, tpe)), Nil))(using assuming(cond, true)),
              normalizeVal(x2, els, Stmt.App(k, Nil, List(ValueVar(x2, tpe)), Nil))(using assuming(cond, false)))
          }

        // avoid dead joinpoints on coercions
        case Stmt.Match(sc, tpe, Nil, None) =>
          Stmt.Match(sc, body.tpe, Nil, None)

        case Stmt.Match(sc, tpe, clauses, default) =>
          val res = normalize(body)
          // [[ val id: A = sc match[A] { ... }; body : B ]] =
          //   def k(id: A): B  = [[ body ]]
          //   sc match [B] { ... k() ... }
          joinpoint(id, tpe, res) { k => (C: Context) ?=>
            // since we commuted Val and Match, we need to change the type of the match!
            Stmt.Match(sc, res.tpe, clauses.map {
              case (tag, clause @ BlockLit(tparams, cparams, vparams, bparams, body)) =>
                val x = Id(id.name)
                val res = normalizeVal(x, body, Stmt.App(k, Nil, List(ValueVar(x, tpe)), Nil))(using selecting(sc, tag, clause))
                (tag, BlockLit(tparams, cparams, vparams, bparams, res))
            }, default.map { stmt =>
              val x = Id(id.name)
              normalizeVal(x, stmt, Stmt.App(k, Nil, List(ValueVar(x, tpe)), Nil))
            })
          }

        // [[ val x = return e; s ]] = let x = [[ e ]]; [[ s ]]
        case Stmt.Return(expr2) => normalizeLet(id, expr2, body)

        // Commute val and bindings
        // [[ val x = { def f = ...; STMT }; STMT ]] = def f = ...; val x = STMT; STMT
        case Stmt.Def(id2, block2, body2) =>
          Stmt.Def(id2, block2, normalizeVal(id, body2, body))

        // Commute val and bindings
        // [[ val x = { let y = ...; STMT }; STMT ]] = let y = ...; val x = STMT; STMT
        case Stmt.Let(id2, binding2, body2) =>
          Stmt.Let(id2, binding2, normalizeVal(id, body2, body))

        case Stmt.ImpureApp(id2, callee2, targs2, vargs2, bargs2, body2) =>
          Stmt.ImpureApp(id2, callee2, targs2, vargs2, bargs2, normalizeVal(id, body2, body))

        // Flatten vals. This should be non-leaking since we use garbage free refcounting.
        // [[ val x = { val y = stmt1; stmt2 }; stmt3 ]] = [[ val y = stmt1; val x = stmt2; stmt3 ]]
        case Stmt.Val(id2, binding2, body2) =>
          normalizeVal(id2, binding2, normalizeVal(id, body2, body))

        // [[ val x = { var y in r = e; stmt2 }; stmt1 ]] = var y in r = e; [[ val x = stmt2; stmt1 ]]
        case Stmt.Alloc(id2, init2, region2, body2) =>
          Stmt.Alloc(id2, init2, region2, normalizeVal(id, body2, body))

        // [[ val x = { let x = !ref; stmt2 }; stmt1 ]] = let x = !ref; [[ val x = stmt2; stmt1 ]]
        case Stmt.Get(id2, tpe2, ref2, capt2, body2) =>
          Stmt.Get(id2, tpe2, ref2, capt2, normalizeVal(id, body2, body))

        // [[ val x = { ref := e; stmt2 }; stmt1 ]] = ref := e; [[ val x = stmt2; stmt1 ]]
        case Stmt.Put(ref2, capt2, value2, body2) =>
          Stmt.Put(ref2, capt2, value2, normalizeVal(id, body2, body))

        case other => normalize(body) match {
          // [[ val x = stmt; return x ]]   =   [[ stmt ]]
          case Stmt.Return(x: ValueVar) if x.id == id => other
          // [[ val x: Unit = stmt; return () ]]   =   [[ stmt ]]
          case Stmt.Return(x) if x.tpe == Type.TUnit && other.tpe == Type.TUnit => other
          // [[ val x = stmt; body ]]   =   val x = [[ stmt ]]; [[ body ]]
          case normalizedBody => Stmt.Val(id, other, normalizedBody)
        }
      }
      normalizeVal(id, binding, body)


    // "Congruences"
    // -------------

    case Stmt.Reset(body) => Stmt.Reset(normalize(body)(using C.enterPrompt))
    case Stmt.Shift(prompt, k, body) => Shift(prompt, k, normalize(body))
    case Stmt.Return(expr) => Return(normalize(expr))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, normalize(init), region, normalize(body))
    case Stmt.Resume(k, body) => Resume(k, normalize(body))
    case Stmt.Region(body) => Region(normalize(body))
    case Stmt.Var(ref, init, capture, body) => Stmt.Var(ref, normalize(init), capture, normalize(body))
    case Stmt.Get(id, tpe, ref, capt, body) => Stmt.Get(id, tpe, ref, capt, normalize(body))
    case Stmt.Put(ref, capt, value, body) => Stmt.Put(ref, capt, normalize(value), normalize(body))
    case Stmt.Hole(tpe, span) => s
  }
  def normalize(b: BlockLit)(using Context): BlockLit =
    b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams, normalize(body))
    }

  def normalize(b: Block)(using Context): Block = preserveTypes(b) {
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

  def normalize(p: Expr)(using ctx: Context): Expr = preserveTypes(p) {
    // [[ box (unbox e) ]] = [[ e ]]
    case Expr.Box(b, annotatedCapture) => active(b) match {
      case NormalizedBlock.Known(Unbox(p), boundBy) => p
      case _ => normalize(b) match {
        case Block.Unbox(expr) => expr
        case b => Expr.Box(b, annotatedCapture)
      }
    }

    // congruences
    // [[ let x = f(y); f(y) ]] = let x = f(y); x
    case Expr.PureApp(f, targs, vargs) => available(Expr.PureApp(f, targs, vargs.map(normalize)))
    case Expr.Make(data, tag, targs, vargs) => available(Expr.Make(data, tag, targs, vargs.map(normalize)))
    // [[ x ]] = y   if `x` was bound to `y`
    // Sound because an alias is only ever bound to something already in scope where the alias is.
    case Expr.ValueVar(id, annotatedType) => ctx.exprs.get(id) match {
      case Some(v: Expr.ValueVar) => normalize(v)
      case _ => p
    }
    case Expr.Literal(value, annotatedType) => p
  }

  // Helpers for beta-reduction
  // --------------------------

  private def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Expr], bargs: List[Block])(using C: Context): Stmt = {
    // To update usage information
    val usage = C.usage
    def copyUsage(to: Id, from: Id) = usage.get(from) match {
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
        copyUsage(id, bparam.id)
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

  @targetName("preserveTypesStmt")
  inline def preserveTypes(before: Stmt)(inline f: Stmt => Stmt): Stmt = debug {
    val after = f(before)
    val tpeBefore = before.typing.tpe
    val tpeAfter = after.typing.tpe
    util.assert(Type.equals(tpeBefore, tpeAfter), s"Normalization doesn't preserve types.\nBefore: ${tpeBefore}\nAfter:  ${tpeAfter}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
    after
  } { f(before) }

  @targetName("preserveTypesExpr")
  inline def preserveTypes(before: Expr)(inline f: Expr => Expr): Expr = debug {
    val after = f(before)
    val tpeBefore = before.typing.tpe
    val tpeAfter = after.typing.tpe
    util.assert(Type.equals(tpeBefore, tpeAfter), s"Normalization doesn't preserve types.\nBefore: ${tpeBefore}\nAfter:  ${tpeAfter}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
    after
  } { f(before) }

  @targetName("preserveTypesBlock")
  inline def preserveTypes(before: Block)(inline f: Block => Block): Block = debug {
    val after = f(before)
    val tpeBefore = before.typing.tpe
    val tpeAfter = after.typing.tpe
    util.assert(Type.equals(tpeBefore, tpeAfter), s"Normalization doesn't preserve types.\nBefore: ${tpeBefore}\nAfter:  ${tpeAfter}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
    after
  } { f(before) }
}
