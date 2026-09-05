package effekt
package core
package optimizer

object RemoveTailResumptions {

  def apply(m: ModuleDecl): ModuleDecl = removal.rewrite(m)

  object removal extends Tree.Rewrite {
    override def rewrite(stmt: Stmt): Stmt = stmt match {
      case Stmt.Shift(prompt, BlockParam(k, Type.TResume(from, to), capt), body) if tailResumptive(k, body) =>
        removeTailResumption(k, from, body)

      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt), body)) =>
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt),
          removeTailAborts(prompt.id, rewrite(body))))

      case other => super.rewrite(other)
    }
  }

  /**
   * Tail positions of a statement, storing only its rewrite.
   *
   * @param transparent blocks whose tail calls are tail positions too
   */
  case class TailPositions(rewrite: (Stmt => Stmt) => Stmt, skipped: Free, transparent: Set[Id] = Set.empty) {
    def forall(p: Stmt => Boolean): Boolean = {
      var holds = true
      rewrite { child => holds &&= p(child); child }
      holds
    }
  }

  /**
   * A statement stands in tail position when the enclosing computation's answer is its answer.
   *
   * @param crossesVar whether a `Var` preserves tail position.
   *   It does preserve it for a handler that never resumes, but does **not** for one that observes it.
   *   (in other words: crossing a [[Var]] is not always semantics-preserving)
   */
  def tailPositions(stmt: Stmt, crossesVar: Stmt.Var => Boolean): Option[TailPositions] = stmt match {
    // a block that is only ever tail-called adds no frame ~> its own tail positions are tail positions here (and so is every call to it)
    case Stmt.Def(id, BlockLit(tps, cps, vps, bps, inner), body)
      if tailCalledOnly(id, body, crossesVar) && tailCalledOnly(id, inner, crossesVar) =>
        Some(TailPositions(f => Stmt.Def(id, BlockLit(tps, cps, vps, bps, f(inner)), f(body)), Free.empty, Set(id)))

    case Stmt.Val(id, binding, body) =>
      Some(TailPositions(f => Stmt.Val(id, binding, f(body)), binding.free))
    case Stmt.Let(id, binding, body) =>
      Some(TailPositions(f => Stmt.Let(id, binding, f(body)), binding.free))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      val arguments = (vargs.map(_.free) ++ bargs.map(_.free)).foldLeft(callee.free)(_ ++ _)
      Some(TailPositions(f => Stmt.ImpureApp(id, callee, targs, vargs, bargs, f(body)), arguments))
    case Stmt.Def(id, block, body) =>
      Some(TailPositions(f => Stmt.Def(id, block, f(body)), block.free))
    case Stmt.Alloc(id, init, region, body) =>
      Some(TailPositions(f => Stmt.Alloc(id, init, region, f(body)), init.free))
    case Stmt.Get(id, tpe, ref, capt, body) =>
      Some(TailPositions(f => Stmt.Get(id, tpe, ref, capt, f(body)), Free.empty))
    case Stmt.Put(ref, capt, value, body) =>
      Some(TailPositions(f => Stmt.Put(ref, capt, value, f(body)), value.free))
    case v @ Stmt.Var(ref, init, capture, body) if crossesVar(v) =>
      Some(TailPositions(f => Stmt.Var(ref, init, capture, f(body)), init.free))

    case Stmt.If(cond, thn, els) =>
      Some(TailPositions(f => Stmt.If(cond, f(thn), f(els)), cond.free))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      def rewrite(f: Stmt => Stmt): Stmt =
        Stmt.Match(scrutinee, tpe, clauses.map {
          case (tag, BlockLit(tps, cps, vps, bps, body)) => tag -> BlockLit(tps, cps, vps, bps, f(body))
        }, default.map(f))
      Some(TailPositions(rewrite, scrutinee.free))

    case _ => None
  }


  /**
   * Replaces an abort on [[prompt]] by what it aborts with, iff it is in tail position.
   *
   *   [[ reset { (p) => ... shift(p) { {k} => b } ... }  ]] = reset { (p) => ... b ... }
   *
   * Note: `k` must not occur in `b`, since its binder goes away, same for `p`
   */
  def removeTailAborts(prompt: Id, stmt: Stmt): Stmt = stmt match {
    case Stmt.Shift(Block.BlockVar(p, _, _), k, body)
      if p == prompt && !Stmt.demandsResumption(k, body) && !body.free.contains(prompt) => body

    case other => tailPositions(other, crossesVar = _ => true) match {
      case Some(positions) => positions.rewrite(removeTailAborts(prompt, _))
      case None => other
    }
  }

  /** Whether every use of [[id]] in [[stmt]] is a call to it in tail position. */
  def tailCalledOnly(id: Id, stmt: Stmt, crossesVar: Stmt.Var => Boolean = _ => true): Boolean =
    tailPositions(stmt, crossesVar) match {
      case Some(positions) =>
        !positions.skipped.contains(id) && positions.forall(tailCalledOnly(id, _, crossesVar))

      case None => stmt match {
        // the only permitted occurrence: a tail call, whose arguments must not mention it again
        case Stmt.App(Block.BlockVar(callee, _, _), _, vargs, bargs) if callee == id =>
          !vargs.exists(_.free.contains(id)) && !bargs.exists(_.free.contains(id))
        // anywhere else it must not occur
        case other => !other.free.contains(id)
      }
    }

  /** Whether a resumption of [[k]] may cross this variable's scope. */
  private def unobserved(k: Id, v: Stmt.Var): Boolean =
    object query extends Tree.Query[Unit, Boolean] {
      def empty = false
      def combine = _ || _
      override def stmt(using Unit) = {
        case Stmt.Resume(k2, body) if k2.id == k => body.typing.capt.contains(v.capture)
      }
    }
    !query.query(v.body)(using ())


  /**
   * Whether every path through [[stmt]] ends by resuming [[k]].
   *
   * @param transparent blocks known to resume in tail position, so a call to one is a tail position
   */
  def tailResumptive(k: Id, stmt: Stmt, transparent: Set[Id] = Set.empty): Boolean =
    tailPositions(stmt, crossesVar = unobserved(k, _)) match {
      case Some(positions) =>
        !positions.skipped.contains(k) &&
          positions.forall(tailResumptive(k, _, transparent ++ positions.transparent))

      // each leaf has to account for every occurrence of `k` in its subterms
      // (the cases above handle this with `skipped`)
      case None => stmt match {
        case Stmt.Resume(k2, body) => k2.id == k && !body.free.contains(k)
        // an abort never returns, so it resumes vacuously; **unless** it resumes `k`!
        case Stmt.Shift(_, _, body) => stmt.tpe == Type.TBottom && !body.free.contains(k)
        case _: Stmt.Hole => true
        case Stmt.App(Block.BlockVar(callee, _, _), _, _, _) => transparent.contains(callee)
        // the answer is produced here, or a frame stands in the way
        case other => false
      }
    }

  /**
   * Replaces the tail resumptions of [[k]] by what they resume with.
   *
   * Must agree with [[tailResumptive]].
   */
  def removeTailResumption(k: Id, tpe: ValueType, stmt: Stmt, transparent: Set[Id] = Set.empty): Stmt =
    tailPositions(stmt, crossesVar = unobserved(k, _)) match {
      case Some(positions) =>
        val blocks = transparent ++ positions.transparent
        retypeAnswer(positions.rewrite(removeTailResumption(k, tpe, _, blocks)), tpe)

      case None => stmt match {
        case Stmt.Resume(k2, body) if k2.id == k => body
        // a tail call to such a block is now a tail position
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, tpe), capt), targs, vargs, bargs) // note that we must fix the type here
        case other => retypeAnswer(other, tpe)
      }
    }

  private def retypeAnswer(stmt: Stmt, tpe: ValueType): Stmt = stmt match {
    case Stmt.Match(scrutinee, _, clauses, default) => Stmt.Match(scrutinee, tpe, clauses, default)
    case Stmt.Hole(_, span) => Stmt.Hole(tpe, span)
    case other => other
  }
}
