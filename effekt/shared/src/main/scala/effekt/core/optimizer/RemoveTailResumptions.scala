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
   * Replaces an abort on [[prompt]] by what it aborts with, iff it is in tail position.
   *
   *   [[ reset { (p) => ... shift(p) { {k} => b } ... }  ]] = reset { (p) => ... b ... }
   *
   * Note: `k` must not occur in `b`, since its binder goes away, same for `p`
   */
  def removeTailAborts(prompt: Id, stmt: Stmt): Stmt = stmt match {
    case Stmt.Shift(Block.BlockVar(p, _, _), k, body)
      if p == prompt && !Stmt.demandsResumption(k, body) && !body.free.contains(prompt) => body

    // a binder leaves the prompt as the next frame
    case Stmt.Val(id, binding, body) => Stmt.Val(id, binding, removeTailAborts(prompt, body))
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, removeTailAborts(prompt, body))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      Stmt.ImpureApp(id, callee, targs, vargs, bargs, removeTailAborts(prompt, body))
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, removeTailAborts(prompt, body))
    case Stmt.Get(id, tpe, ref, capt, body) => Stmt.Get(id, tpe, ref, capt, removeTailAborts(prompt, body))
    case Stmt.Put(ref, capt, value, body) => Stmt.Put(ref, capt, value, removeTailAborts(prompt, body))
    case Stmt.Var(ref, init, capture, body) => Stmt.Var(ref, init, capture, removeTailAborts(prompt, body))

    // a block that is only ever tail-called adds no frame ~> its own tail positions are tail positions here
    case Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, inner), body) if tailCalledOnly(id, body) && tailCalledOnly(id, inner) =>
      Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, removeTailAborts(prompt, inner)), removeTailAborts(prompt, body))
    case Stmt.Def(id, block, body) => Stmt.Def(id, block, removeTailAborts(prompt, body))

    // every branch ends where the whole statement does
    case Stmt.If(cond, thn, els) =>
      Stmt.If(cond, removeTailAborts(prompt, thn), removeTailAborts(prompt, els))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      Stmt.Match(scrutinee, tpe, clauses.map {
        case (tag, BlockLit(tparams, cparams, vparams, bparams, body)) =>
          tag -> BlockLit(tparams, cparams, vparams, bparams, removeTailAborts(prompt, body))
      }, default.map { stmt => removeTailAborts(prompt, stmt) })

    // anything else either returns, or puts a frame between the abort and the prompt
    case other => other
  }

  /** Whether every use of [[id]] in [[stmt]] is a call to it in tail position. */
  def tailCalledOnly(id: Id, stmt: Stmt): Boolean =
    def freeInStmt(stmt: Stmt): Boolean = stmt.free.contains(id)
    def freeInExpr(expr: Expr): Boolean = expr.free.contains(id)
    def freeInBlock(block: Block): Boolean = block.free.contains(id)

    stmt match {
      // a tail call, whose arguments must not mention it again
      case Stmt.App(Block.BlockVar(callee, _, _), _, vargs, bargs) if callee == id =>
        !vargs.exists(freeInExpr) && !bargs.exists(freeInBlock)

      // matching [[removeTailAborts]]
      case Stmt.Val(_, binding, body) => !freeInStmt(binding) && tailCalledOnly(id, body)
      case Stmt.Let(_, binding, body) => !freeInExpr(binding) && tailCalledOnly(id, body)
      case Stmt.ImpureApp(_, callee, _, vargs, bargs, body) =>
        !freeInBlock(callee) && !vargs.exists(freeInExpr) && !bargs.exists(freeInBlock) &&
          tailCalledOnly(id, body)
      case Stmt.Def(_, block, body) => !freeInBlock(block) && tailCalledOnly(id, body)
      case Stmt.Alloc(_, init, _, body) => !freeInExpr(init) && tailCalledOnly(id, body)
      case Stmt.Get(_, _, _, _, body) => tailCalledOnly(id, body)
      case Stmt.Put(_, _, value, body) => !freeInExpr(value) && tailCalledOnly(id, body)
      case Stmt.Var(_, init, _, body) => !freeInExpr(init) && tailCalledOnly(id, body)

      case Stmt.If(cond, thn, els) =>
        !freeInExpr(cond) && tailCalledOnly(id, thn) && tailCalledOnly(id, els)
      case Stmt.Match(scrutinee, _, clauses, default) =>
        !freeInExpr(scrutinee) && clauses.forall {
          case (_, BlockLit(_, _, _, _, body)) => tailCalledOnly(id, body)
        } && default.forall { stmt => tailCalledOnly(id, stmt) }

      // anywhere else it simply may not occur
      case other => !freeInStmt(other)
    }

  // A simple syntactic check whether this stmt is tailresumptive in k
  def tailResumptive(k: Id, stmt: Stmt): Boolean =
    def freeInStmt(stmt: Stmt): Boolean = stmt.free.contains(k)
    def freeInExpr(expr: Expr): Boolean = expr.free.contains(k)
    def freeInBlock(block: Block): Boolean = block.free.contains(k)

    stmt match {
      case Stmt.Def(id, block, body) => !freeInBlock(block) && tailResumptive(k, body)
      case Stmt.Let(id, binding, body) => !freeInExpr(binding) && tailResumptive(k, body)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => tailResumptive(k, body) && !freeInBlock(callee) && !vargs.exists(freeInExpr) && !bargs.exists(freeInBlock)
      case Stmt.Return(expr) => false
      case Stmt.Val(id, binding, body) => tailResumptive(k, body) && !freeInStmt(binding)
      case Stmt.App(callee, targs, vargs, bargs) => false
      case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => false
      case Stmt.If(cond, thn, els) => !freeInExpr(cond) && tailResumptive(k, thn) && tailResumptive(k, els)
      // Interestingly, we introduce a join point making this more difficult to implement properly
      case Stmt.Match(scrutinee, tpe, clauses, default) => !freeInExpr(scrutinee) && clauses.forall {
        case (_, BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body)
      } && default.forall { stmt => tailResumptive(k, stmt) }
      case Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, body)) => false
      case Stmt.Alloc(id, init, region, body) => tailResumptive(k, body) && !freeInExpr(init)
      // Conceptually, a mutable variable definition can be seen as a handler for get and put operations.
      // Treating this as tail-resumptive leads to a failure of semantics preservation.
      // See https://github.com/effekt-lang/effekt/issues/1153 for an example.
      case Stmt.Var(ref, init, capture, body) => false
      case Stmt.Get(ref, annotatedCapt, tpe, id, body) => tailResumptive(k, body)
      case Stmt.Put(ref, annotatedCapt, value, body) => tailResumptive(k, body) && !freeInExpr(value)
      case Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, body)) => false
      case Stmt.Shift(prompt, k, body) => stmt.tpe == Type.TBottom
      case Stmt.Resume(k2, body) => k2.id == k // what if k is free in body?
      case Stmt.Hole(tpe, span) => true
    }

  def removeTailResumption(k: Id, tpe: ValueType, stmt: Stmt): Stmt = stmt match {
    case Stmt.Def(id, block, body) => Stmt.Def(id, block, removeTailResumption(k, tpe, body))
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, removeTailResumption(k, tpe, body))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Stmt.ImpureApp(id, callee, targs, vargs, bargs, removeTailResumption(k, tpe, body))
    case Stmt.Val(id, binding, body) => Stmt.Val(id, binding, removeTailResumption(k, tpe, body))
    case Stmt.If(cond, thn, els) => Stmt.If(cond, removeTailResumption(k, tpe, thn), removeTailResumption(k, tpe, els))
    case Stmt.Match(scrutinee, _, clauses, default) => Stmt.Match(scrutinee, tpe, clauses.map {
      case (tag, block) => tag -> removeTailResumption(k, tpe, block)
    }, default.map(removeTailResumption(k, tpe, _)))
    case Stmt.Region(body : BlockLit) =>
      Stmt.Region(removeTailResumption(k, tpe, body))
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, removeTailResumption(k, tpe, body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, init, capture, removeTailResumption(k, tpe, body))
    case Stmt.Reset(body) => stmt
    case Stmt.Resume(k2, body) if k2.id == k => body

    case Stmt.Resume(k, body) => stmt
    case Stmt.Shift(prompt, k, body) => stmt
    case Stmt.Hole(_, span) => Stmt.Hole(tpe, span)
    case Stmt.Return(expr) => stmt
    case Stmt.App(callee, targs, vargs, bargs) => stmt
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => stmt
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => Stmt.Get(id, annotatedTpe, ref, annotatedCapt, removeTailResumption(k, tpe, body))
    case Stmt.Put(ref, annotatedCapt, value, body) => Stmt.Put(ref, annotatedCapt, value, removeTailResumption(k, tpe, body))
  }

  def removeTailResumption(k: Id, tpe: ValueType, block: BlockLit): BlockLit = block match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, removeTailResumption(k, tpe, body))
  }
}
