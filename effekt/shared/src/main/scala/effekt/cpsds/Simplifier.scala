package effekt
package cpsds

import core.Id

import scala.::

// For now only
// - drops dealloc on non-escaping refs.
// - does some eta-reduction on function definitions
object Simplifier {

  def rewrite(s: Stmt, escaping: Set[Id]): Stmt = s match {

    case Stmt.Def(id, params, body, rest) =>
      rewrite(body, escaping) match {
        // eta-reduction
        case Stmt.App(id2, args, _) if args == params.map(Expr.Variable(_)) =>
          Stmt.Let(id, Expr.Variable(id2), rewrite(rest, escaping))
        case newBody =>
          Stmt.Def(id, params, newBody, rewrite(rest, escaping))
      }

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(op => rewrite(op, escaping)), rewrite(rest, escaping))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding, escaping), rewrite(rest, escaping))

    // TODO this is only useful if the continuation k is non-trivial
    //    case Stmt.App(id, args, true) if escaping.contains(id) =>
    //      val tmp = Id("tmp")
    //      val (init, List(ks, Expr.Variable(k))) = args.splitAt(args.size - 2) : @unchecked
    //      Stmt.Run(tmp, id, init, Purity.Impure,
    //        Stmt.App(k, List(Expr.Variable(tmp), ks), false))

    case Stmt.App(id, args, direct) =>
      Stmt.App(id, args.map(e => rewrite(e, escaping)), direct)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(e => rewrite(e, escaping)))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(e => rewrite(e, escaping)), purity, rewrite(rest, escaping))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond, escaping), rewrite(thn, escaping), rewrite(els, escaping))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(rewrite(scrutinee, escaping),
        clauses.map { case (id, cl) => (id, rewrite(cl, escaping)) },
        default.map(rewrite(_, escaping)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, rewrite(ks, escaping), rewrite(rest, escaping))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, rewrite(init, escaping), region, rewrite(rest, escaping))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, rewrite(init, escaping), rewrite(ks, escaping), rewrite(rest, escaping))

    case Stmt.Dealloc(ref, rest) if !escaping.contains(ref) =>
      rewrite(rest, escaping)

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, rewrite(rest, escaping))

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, rewrite(rest, escaping))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, rewrite(value, escaping), rewrite(rest, escaping))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, rewrite(body, escaping), rewrite(ks1, escaping), rewrite(k1, escaping))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, rewrite(body, escaping), rewrite(ks1, escaping), rewrite(k1, escaping))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(r, ks, k, rewrite(body, escaping), rewrite(ks1, escaping), rewrite(k1, escaping))

    case h: Stmt.Hole => h
  }

  def rewrite(e: Expr, escaping: Set[Id]): Expr = e match {
    case Expr.Variable(_) => e
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite(_, escaping)))
    case Expr.Abort => e
    case Expr.Return => e
    case Expr.Toplevel => e
  }

  def rewrite(op: Operation, escaping: Set[Id]): Operation =
    Operation(op.name, op.params, rewrite(op.body, escaping))

  def rewrite(cl: Clause, escaping: Set[Id]): Clause =
    Clause(cl.params, rewrite(cl.body, escaping))

  def rewrite(d: ToplevelDefinition, escaping: Set[Id]): ToplevelDefinition = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      ToplevelDefinition.Def(id, params, rewrite(body, escaping))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      ToplevelDefinition.Val(id, ks, k, rewrite(binding, escaping))

    case ToplevelDefinition.Let(id, binding) =>
      ToplevelDefinition.Let(id, rewrite(binding, escaping))
  }

  // --- Entry point ---

  def transform(m: ModuleDecl): ModuleDecl = {
    val escaping = EscapeAnalysis(m)
    m.copy(definitions = m.definitions.map(d => rewrite(d, escaping)))
  }
}
