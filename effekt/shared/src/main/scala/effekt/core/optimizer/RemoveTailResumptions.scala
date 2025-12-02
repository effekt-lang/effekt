package effekt
package core
package optimizer

object RemoveTailResumptions {

  def apply(m: ModuleDecl): ModuleDecl = removal.rewrite(m)

  object removal extends Tree.Rewrite {
    override def stmt: PartialFunction[Stmt, Stmt] = {
      case Stmt.Shift(prompt, BlockLit(tparams, cparams, vparams, List(BlockParam(k, Type.TResume(from, to), capt)), body)) if tailResumptive(k, body) =>
        removeTailResumption(k, from, body)
      case Stmt.Shift(prompt, body) => Shift(prompt, rewrite(body))
    }
  }

  // A simple syntactic check whether this stmt is tailresumptive in k
  def tailResumptive(k: Id, stmt: Stmt): Boolean =
    def freeInStmt(stmt: Stmt): Boolean = Variables.free(stmt).containsBlock(k)
    def freeInExpr(expr: Expr): Boolean = Variables.free(expr).containsBlock(k)
    def freeInBlock(block: Block): Boolean = Variables.free(block).containsBlock(k)

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
      case Stmt.Shift(prompt, body) => stmt.tpe == Type.TBottom
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
    case Stmt.Shift(prompt, body) => stmt
    case Stmt.Hole(tpe, span) => stmt
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
