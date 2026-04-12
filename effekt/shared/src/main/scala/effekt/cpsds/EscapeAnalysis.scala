package effekt
package cpsds

import core.Id
import scala.collection.mutable

class EscapeAnalysis(
  // maps each function to its free variables
  val freeVars: mutable.Map[Id, Set[Id]] = mutable.Map.empty,
  val escaping: mutable.Set[Id] = mutable.Set.empty
) {

  def isEscaping(id: Id): Boolean = escaping.contains(id)

  def isSecondClass(id: Id): Boolean = !isEscaping(id)

  private def escapes(id: Id): Unit = {
    if escaping.add(id) then
      freeVars.get(id).foreach { fvs =>
        fvs.foreach(escapes)
      }
  }

  private def escapes(e: Expr): Unit = e match {
    case Expr.Variable(id) => escapes(id)
    case Expr.Make(_, _, vargs) => vargs.foreach(escapes)
    case _ => ()
  }

  // --- Free variables ---

  def free(e: Expr): Set[Id] = e match {
    case Expr.Variable(id) => Set(id)
    case Expr.Literal(_, _) => Set.empty
    case Expr.Make(_, _, vargs) => vargs.flatMap(free).toSet
    case Expr.Abort => Set.empty
    case Expr.Return => Set.empty
    case Expr.Toplevel => Set.empty
  }

  def free(s: Stmt): Set[Id] = s match {
    case Stmt.Def(id, params, body, rest) =>
      val bodyFree = free(body) -- params.toSet - id
      freeVars(id) = bodyFree
      bodyFree ++ (free(rest) - id)

    case Stmt.New(id, _, operations, rest) =>
      val opsFree = operations.flatMap { op =>
        free(op.body) -- op.params.toSet
      }.toSet
      freeVars(id) = opsFree
      opsFree ++ (free(rest) - id)

    case Stmt.Val(id, binding, rest) =>
      free(binding) ++ (free(rest) - id)

    case Stmt.Let(id, binding, rest) =>
      free(binding) ++ (free(rest) - id)

    case Stmt.App(id, args) =>
      Set(id) ++ args.flatMap(free)

    case Stmt.Invoke(id, _, args) =>
      Set(id) ++ args.flatMap(free)

    case Stmt.Run(id, callee, args, _, rest) =>
      Set(callee) ++ args.flatMap(free) ++ (free(rest) - id)

    case Stmt.If(cond, thn, els) =>
      free(cond) ++ free(thn) ++ free(els)

    case Stmt.Match(scrutinee, clauses, default) =>
      free(scrutinee) ++
        clauses.flatMap { case (_, cl) => free(cl.body) -- cl.params.toSet } ++
        default.map(free).getOrElse(Set.empty)

    case Stmt.Region(id, ks, rest) =>
      free(ks) ++ (free(rest) - id)

    case Stmt.Alloc(id, init, region, rest) =>
      free(init) + region ++ (free(rest) - id)

    case Stmt.Var(id, init, ks, rest) =>
      free(init) ++ free(ks) ++ (free(rest) - id)

    case Stmt.Dealloc(ref, rest) =>
      Set(ref) ++ free(rest)

    case Stmt.Get(ref, id, rest) =>
      Set(ref) ++ (free(rest) - id)

    case Stmt.Put(ref, value, rest) =>
      Set(ref) ++ free(value) ++ free(rest)

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      (free(body) - p - ks - k) ++ free(ks1) ++ free(k1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Set(prompt) ++ (free(body) - resume - ks - k) ++ free(ks1) ++ free(k1)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Set(r) ++ (free(body) - ks - k) ++ free(ks1) ++ free(k1)

    case Stmt.Hole(_) => Set.empty
  }


  // --- Escape analysis ---

  def analyze(s: Stmt): Unit = s match {
    case Stmt.Def(id, params, body, rest) =>
      analyze(body)
      analyze(rest)

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
      free(rest).foreach(escapes)
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
      escapes(k1)
      escapes(ks1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      escapes(k1)
      escapes(ks1)
      analyze(body)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      escapes(k1)
      escapes(ks1)
      analyze(body)

    case Stmt.Hole(_) => ()
  }

  // --- Entry points ---

  def process(d: ToplevelDefinition): Unit = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      val bodyFree = free(body) -- params.toSet - id
      freeVars(id) = bodyFree
      analyze(body)

    case ToplevelDefinition.Val(id, binding) =>
      free(binding)
      analyze(binding)

    case ToplevelDefinition.Let(id, binding) =>
      escapes(binding)
  }

  def process(m: ModuleDecl): Unit = {
    m.definitions.foreach(process)
    m.exports.foreach(escapes)
  }
}

object EscapeAnalysis {
  def apply(m: ModuleDecl): EscapeAnalysis = {
    val analysis = new EscapeAnalysis()
    analysis.process(m)
    analysis
  }
}
