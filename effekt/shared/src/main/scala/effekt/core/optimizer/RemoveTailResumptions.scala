package effekt
package core
package optimizer

object RemoveTailResumptions {

  def apply(m: ModuleDecl): ModuleDecl = removal.rewrite(m)

  object removal extends Tree.Rewrite {
    override def stmt: PartialFunction[Stmt, Stmt] = {
      case Stmt.Shift(prompt, BlockLit(tparams, cparams, vparams, List(k), body)) if tailResumptive(k.id, body) =>
        removeTailResumption(k.id, body)
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
      case Stmt.Let(id, tpe, binding, body) => !freeInExpr(binding) && tailResumptive(k, body)
      case Stmt.Return(expr) => false
      case Stmt.Val(id, annotatedTpe, binding, body) => tailResumptive(k, body) && !freeInStmt(binding)
      case Stmt.App(callee, targs, vargs, bargs) => false
      case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => false
      case Stmt.If(cond, thn, els) => !freeInExpr(cond) && tailResumptive(k, thn) && tailResumptive(k, els)
      // Interestingly, we introduce a join point making this more difficult to implement properly
      case Stmt.Match(scrutinee, clauses, default) => !freeInExpr(scrutinee) && clauses.forall {
        case (_, BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body)
      } && default.forall { stmt => tailResumptive(k, stmt) }
      case Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body)
      case Stmt.Alloc(id, init, region, body) => tailResumptive(k, body) && !freeInExpr(init)
      case Stmt.Var(id, init, capture, body) => tailResumptive(k, body) && !freeInExpr(init)
      case Stmt.Get(id, annotatedCapt, annotatedTpe) => false
      case Stmt.Put(id, annotatedCapt, value) => false
      case Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body) // is this correct?
      case Stmt.Shift(prompt, body) => stmt.tpe == Type.TBottom
      case Stmt.Resume(k2, body) => k2.id == k // what if k is free in body?
      case Stmt.Hole() => true
    }

  def removeTailResumption(k: Id, stmt: Stmt): Stmt = stmt match {
    case Stmt.Def(id, block, body) => Stmt.Def(id, block, removeTailResumption(k, body))
    case Stmt.Let(id, tpe, binding, body) => Stmt.Let(id, tpe, binding, removeTailResumption(k, body))
    case Stmt.Val(id, annotatedTpe, binding, body) => Stmt.Val(id, annotatedTpe, binding, removeTailResumption(k, body))
    case Stmt.If(cond, thn, els) => Stmt.If(cond, removeTailResumption(k, thn), removeTailResumption(k, els))
    case Stmt.Match(scrutinee, clauses, default) => Stmt.Match(scrutinee, clauses.map {
      case (tag, block) => tag -> removeTailResumption(k, block)
    }, default.map(removeTailResumption(k, _)))
    case Stmt.Region(body : BlockLit) =>
      Stmt.Region(removeTailResumption(k, body))
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, removeTailResumption(k, body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, init, capture, removeTailResumption(k, body))
    case Stmt.Reset(body) => Stmt.Reset(removeTailResumption(k, body))
    case Stmt.Resume(k2, body) if k2.id == k => body

    case Stmt.Resume(k, body) => stmt
    case Stmt.Shift(prompt, body) => stmt
    case Stmt.Hole() => stmt
    case Stmt.Return(expr) => stmt
    case Stmt.App(callee, targs, vargs, bargs) => stmt
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => stmt
    case Stmt.Get(id, annotatedCapt, annotatedTpe) => stmt
    case Stmt.Put(id, annotatedCapt, value) => stmt
  }

  def removeTailResumption(k: Id, block: BlockLit): BlockLit = block match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, removeTailResumption(k, body))
  }
}
