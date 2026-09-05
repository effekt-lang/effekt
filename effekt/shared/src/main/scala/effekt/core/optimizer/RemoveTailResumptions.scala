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

  /** Tail positions of a statement, storing only its rewrite */
  case class TailPositions(rewrite: (Stmt => Stmt) => Stmt, skipped: Free) {
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
   *   It does preserve it for a rewrite that never resumes, but does **not** for one that may resume *twice*!
   *   (in other words: crossing a [[Var]] is not always semantics-preserving)
   */
  def tailPositions(stmt: Stmt, crossesVar: Boolean, crossesTailCalls: Boolean = false): Option[TailPositions] = stmt match {
    // a block that is only ever tail-called adds no frame ~> its own tail positions are tail positions here
    case Stmt.Def(id, BlockLit(tps, cps, vps, bps, inner), body)
      if crossesTailCalls && tailCalledOnly(id, body, crossesVar) && tailCalledOnly(id, inner, crossesVar) =>
        Some(TailPositions(f => Stmt.Def(id, BlockLit(tps, cps, vps, bps, f(inner)), f(body)), Free.empty))

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
    case Stmt.Var(ref, init, capture, body) if crossesVar =>
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

    case other => tailPositions(other, crossesVar = true, crossesTailCalls = true) match {
      case Some(positions) => positions.rewrite(removeTailAborts(prompt, _))
      case None => other
    }
  }

  /** Whether every use of [[id]] in [[stmt]] is a call to it in tail position. */
  def tailCalledOnly(id: Id, stmt: Stmt, crossesVar: Boolean = true): Boolean =
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

  /** Whether every path through [[stmt]] ends by resuming [[k]]. */
  def tailResumptive(k: Id, stmt: Stmt): Boolean =
    tailPositions(stmt, crossesVar = false) match {
      case Some(positions) =>
        !positions.skipped.contains(k) && positions.forall(tailResumptive(k, _))

      case None => stmt match {
        case Stmt.Resume(k2, body) => k2.id == k // what if k is free in body?
        case _: Stmt.Shift => stmt.tpe == Type.TBottom
        case _: Stmt.Hole => true
        // the answer is produced here, or a frame stands in the way
        case other => false
      }
    }

  /** 
   * Replaces the tail resumptions of [[k]] by what they resume with.
   *
   * Must agree with [[tailResumptive]].
   */
  def removeTailResumption(k: Id, tpe: ValueType, stmt: Stmt): Stmt =
    tailPositions(stmt, crossesVar = false) match {
      case Some(positions) =>
        retypeAnswer(positions.rewrite(removeTailResumption(k, tpe, _)), tpe)

      case None => stmt match {
        case Stmt.Resume(k2, body) if k2.id == k => body
        case other => retypeAnswer(other, tpe)
      }
    }

  private def retypeAnswer(stmt: Stmt, tpe: ValueType): Stmt = stmt match {
    case Stmt.Match(scrutinee, _, clauses, default) => Stmt.Match(scrutinee, tpe, clauses, default)
    case Stmt.Hole(_, span) => Stmt.Hole(tpe, span)
    case other => other
  }
}
