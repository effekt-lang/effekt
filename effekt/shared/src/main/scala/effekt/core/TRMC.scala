package effekt
package core

object TRMC {

  def run(core: ModuleDecl): ModuleDecl = core match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions, exports)
  }

  def transform(stmt: Stmt): Stmt = stmt match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => ???
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => ???
    case Stmt.Val(id, binding, body) => ???
    case Stmt.App(callee, targs, vargs, bargs) => ???
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => ???
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => ???
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???
    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, k, body) => ???
    case Stmt.Resume(k, body) => ???
    case Stmt.Hole(annotatedTpe, span) => ???
  }
  def transform(expr: Expr): Expr = expr
  def transform(impl: Implementation): Implementation = impl

}
