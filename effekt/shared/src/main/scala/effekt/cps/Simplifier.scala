package effekt
package cps

// For now only
// - drops deallocations.
// - does some eta-reduction on function definitions
object Simplifier {

  def rewrite(s: Stmt): Stmt = s match {

    case Stmt.Def(id, params, body, rest) =>
      rewrite(body) match {
        // eta-reduction
        case Stmt.App(id2, args) if args == params.map(Expr.Variable(_)) =>
          Stmt.Let(id, Expr.Variable(id2), rewrite(rest))
        case newBody =>
          Stmt.Def(id, params, newBody, rewrite(rest))
      }

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(rewrite), rewrite(rest))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding), rewrite(rest))

    case Stmt.Call(id, returnedKs, callee, args, ks, rest) =>
      Stmt.Call(id, returnedKs, callee, args.map(rewrite), rewrite(ks), rewrite(rest))

    case Stmt.App(id, args) =>
      Stmt.App(id, args.map(rewrite))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite))

    case Stmt.Return(values) =>
      Stmt.Return(values.map(rewrite))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(rewrite), purity, rewrite(rest))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond), rewrite(thn), rewrite(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(rewrite(scrutinee),
        clauses.map { case (id, cl) => (id, rewrite(cl)) },
        default.map(rewrite))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, rewrite(ks), rewrite(rest))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, rewrite(init), region, rewrite(rest))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, rewrite(init), rewrite(ks), rewrite(rest))

    case Stmt.Dealloc(_, rest) =>
      rewrite(rest)

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, rewrite(rest))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, rewrite(value), rewrite(rest))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(r, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case h: Stmt.Hole => h
  }

  def rewrite(e: Expr): Expr = e match {
    case Expr.Variable(_) => e
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite))
    case Expr.Abort => e
    case Expr.Toplevel => e
  }

  def rewrite(op: Operation): Operation =
    Operation(op.name, op.params, rewrite(op.body))

  def rewrite(cl: Clause): Clause =
    Clause(cl.params, rewrite(cl.body))

  def rewrite(d: ToplevelDefinition): ToplevelDefinition = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      ToplevelDefinition.Def(id, params, rewrite(body))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      ToplevelDefinition.Val(id, ks, k, rewrite(binding))
  }

  // --- Entry point ---

  def transform(m: ModuleDecl): ModuleDecl =
    m.copy(definitions = m.definitions.map(rewrite))
}
