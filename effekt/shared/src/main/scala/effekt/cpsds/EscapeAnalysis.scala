package effekt
package cpsds

import core.Id
import scala.collection.mutable

private class EscapeAnalysis(
  val escaping: mutable.Set[Id] = mutable.Set.empty
) {

  private def isEscaping(id: Id): Boolean = escaping.contains(id)

  private def escapes(id: Id): Unit = escaping.add(id)

  private def escapes(ids: Set[Id]): Unit = escaping.addAll(ids)

  private def escapes(e: Expr): Unit = e match {
    case Expr.Variable(id) => escapes(id)
    case Expr.Make(_, _, vargs) => vargs.foreach(escapes)
    case _ => ()
  }

  // --- Free variables ---


  // --- Escape analysis ---

  def analyze(s: Stmt): Unit = s match {
    case Stmt.Def(id, params, body, rest) =>
      analyze(body)
      analyze(rest)
      if (isEscaping(id)) {
        escapes(body.free -- params -- Set(id))
      }

    case Stmt.New(id, _, operations, rest) =>
      operations.foreach { op => analyze(op.body) }
      analyze(rest)

    case Stmt.Val(id, binding, rest) =>
      analyze(binding)
      analyze(rest)

    case Stmt.Let(id, binding, rest) =>
      escapes(binding)
      analyze(rest)

    // callee does NOT escape
    case Stmt.App(id, args) =>
      args.foreach(escapes)

    case Stmt.Invoke(id, _, args) =>
      args.foreach(escapes)

    // This is the essence of async computation: we need to reify the continuation
    case Stmt.Run(id, callee, args, Purity.Async, rest) =>
      args.foreach(escapes)
      escapes(rest.free)
      analyze(rest)

    case Stmt.Run(id, callee, args, purity, rest) =>
      args.foreach(escapes)
      analyze(rest)

    case Stmt.If(cond, thn, els) =>
      escapes(cond)
      analyze(thn)
      analyze(els)

    case Stmt.Match(scrutinee, clauses, default) =>
      escapes(scrutinee)
      clauses.foreach { case (_, cl) => analyze(cl.body) }
      default.foreach(analyze)

    case Stmt.Region(id, ks, rest) =>
      analyze(rest)

    case Stmt.Alloc(id, init, region, rest) =>
      escapes(init)
      analyze(rest)

    case Stmt.Var(id, init, ks, rest) =>
      escapes(init)
      analyze(rest)

    case Stmt.Dealloc(ref, rest) =>
      analyze(rest)

    case Stmt.Get(ref, id, rest) =>
      analyze(rest)

    case Stmt.Put(ref, value, rest) =>
      escapes(value)
      analyze(rest)

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      analyze(body)
      escapes(body.free)
      escapes(k1)
      escapes(ks1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      escapes(k1)
      escapes(ks1)
      escapes(body.free)
      analyze(body)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      escapes(k1)
      escapes(ks1)
      escapes(body.free)
      analyze(body)

    case Stmt.Hole(_) => ()
  }

  // --- Entry points ---
  def process(d: ToplevelDefinition): Unit = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      escapes(id)
      analyze(body)

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      analyze(binding)

    case ToplevelDefinition.Let(id, binding) =>
      escapes(binding)
  }

  def process(m: ModuleDecl): Unit = {
    m.definitions.foreach(process)
  }
}

object EscapeAnalysis {
  def apply(m: ModuleDecl): Set[Id] = {
    val analysis = new EscapeAnalysis()
    analysis.process(m)
    analysis.escaping.toSet
  }
}
