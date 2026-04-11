package effekt
package cpsds

import core.Id
import scala.collection.mutable

class EscapeAnalysis(
  val freeVars: mutable.Map[Id, Set[Id]] = mutable.Map.empty,
  val escaping: mutable.Set[Id] = mutable.Set.empty
) {

  def escapes(id: Id): Boolean = escaping.contains(id)

  def isSecondClass(id: Id): Boolean = !escapes(id)

  private def markEscaping(id: Id): Unit = {
    if escaping.add(id) then
      freeVars.get(id).foreach { fvs =>
        fvs.foreach(markEscaping)
      }
  }

  private def markExprEscaping(e: Expr): Unit = e match {
    case Expr.Variable(id) => markEscaping(id)
    case Expr.Make(_, _, vargs) => vargs.foreach(markExprEscaping)
    case _ => ()
  }

  // --- Free variables ---

  def freeOfExpr(e: Expr): Set[Id] = e match {
    case Expr.Variable(id) => Set(id)
    case Expr.Literal(_, _) => Set.empty
    case Expr.Make(_, _, vargs) => vargs.flatMap(freeOfExpr).toSet
    case Expr.Abort => Set.empty
    case Expr.Return => Set.empty
    case Expr.Toplevel => Set.empty
  }

  def freeOfStmt(s: Stmt): Set[Id] = s match {
    case Stmt.Def(id, params, body, rest) =>
      val bodyFree = freeOfStmt(body) -- params.toSet - id
      freeVars(id) = bodyFree
      bodyFree ++ (freeOfStmt(rest) - id)

    case Stmt.New(id, _, operations, rest) =>
      val opsFree = operations.flatMap { op =>
        freeOfStmt(op.body) -- op.params.toSet
      }.toSet
      freeVars(id) = opsFree
      opsFree ++ (freeOfStmt(rest) - id)

    case Stmt.Val(id, binding, rest) =>
      freeOfStmt(binding) ++ (freeOfStmt(rest) - id)

    case Stmt.Let(id, binding, rest) =>
      freeOfExpr(binding) ++ (freeOfStmt(rest) - id)

    case Stmt.App(id, args) =>
      Set(id) ++ args.flatMap(freeOfExpr)

    case Stmt.Invoke(id, _, args) =>
      Set(id) ++ args.flatMap(freeOfExpr)

    case Stmt.Run(id, callee, args, _, rest) =>
      Set(callee) ++ args.flatMap(freeOfExpr) ++ (freeOfStmt(rest) - id)

    case Stmt.If(cond, thn, els) =>
      freeOfExpr(cond) ++ freeOfStmt(thn) ++ freeOfStmt(els)

    case Stmt.Match(scrutinee, clauses, default) =>
      freeOfExpr(scrutinee) ++
        clauses.flatMap { case (_, cl) => freeOfStmt(cl.body) -- cl.params.toSet } ++
        default.map(freeOfStmt).getOrElse(Set.empty)

    case Stmt.Region(id, ks, rest) =>
      freeOfExpr(ks) ++ (freeOfStmt(rest) - id)

    case Stmt.Alloc(id, init, region, rest) =>
      freeOfExpr(init) + region ++ (freeOfStmt(rest) - id)

    case Stmt.Var(id, init, ks, rest) =>
      freeOfExpr(init) ++ freeOfExpr(ks) ++ (freeOfStmt(rest) - id)

    case Stmt.Dealloc(ref, rest) =>
      Set(ref) ++ freeOfStmt(rest)

    case Stmt.Get(ref, id, rest) =>
      Set(ref) ++ (freeOfStmt(rest) - id)

    case Stmt.Put(ref, value, rest) =>
      Set(ref) ++ freeOfExpr(value) ++ freeOfStmt(rest)

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      (freeOfStmt(body) - p - ks - k) ++ freeOfExpr(ks1) ++ freeOfExpr(k1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Set(prompt) ++ (freeOfStmt(body) - resume - ks - k) ++ freeOfExpr(ks1) ++ freeOfExpr(k1)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Set(r) ++ (freeOfStmt(body) - ks - k) ++ freeOfExpr(ks1) ++ freeOfExpr(k1)

    case Stmt.Hole(_) => Set.empty
  }

  // --- Escape analysis ---

  def analyzeStmt(s: Stmt): Unit = s match {
    case Stmt.Def(id, params, body, rest) =>
      analyzeStmt(body)
      analyzeStmt(rest)

    case Stmt.New(id, _, operations, rest) =>
      operations.foreach { op => analyzeStmt(op.body) }
      analyzeStmt(rest)

    case Stmt.Val(id, binding, rest) =>
      analyzeStmt(binding)
      analyzeStmt(rest)

    case Stmt.Let(id, binding, rest) =>
      markExprEscaping(binding)
      analyzeStmt(rest)

    case Stmt.App(id, args) =>
      // callee does NOT escape
      args.foreach(markExprEscaping)

    case Stmt.Invoke(id, _, args) =>
      // callee does NOT escape
      args.foreach(markExprEscaping)

    case Stmt.Run(id, callee, args, _, rest) =>
      // callee does NOT escape
      args.foreach(markExprEscaping)
      analyzeStmt(rest)

    case Stmt.If(cond, thn, els) =>
      markExprEscaping(cond)
      analyzeStmt(thn)
      analyzeStmt(els)

    case Stmt.Match(scrutinee, clauses, default) =>
      markExprEscaping(scrutinee)
      clauses.foreach { case (_, cl) => analyzeStmt(cl.body) }
      default.foreach(analyzeStmt)

    case Stmt.Region(id, ks, rest) =>
      // ks does NOT escape
      analyzeStmt(rest)

    case Stmt.Alloc(id, init, region, rest) =>
      markExprEscaping(init)
      // region does NOT escape
      analyzeStmt(rest)

    case Stmt.Var(id, init, ks, rest) =>
      markExprEscaping(init)
      // ks does NOT escape
      analyzeStmt(rest)

    case Stmt.Dealloc(ref, rest) =>
      // ref does NOT escape
      analyzeStmt(rest)

    case Stmt.Get(ref, id, rest) =>
      // ref does NOT escape
      analyzeStmt(rest)

    case Stmt.Put(ref, value, rest) =>
      // ref does NOT escape
      markExprEscaping(value)
      analyzeStmt(rest)

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      // ks1, k1 do NOT escape
      analyzeStmt(body)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      // ks1, k1 do NOT escape
      // resume escapes (captured continuation, used as first-class value)
      markEscaping(resume)
      analyzeStmt(body)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      // ks1, k1 do NOT escape
      analyzeStmt(body)

    case Stmt.Hole(_) => ()
  }

  // --- Entry points ---

  def process(d: ToplevelDefinition): Unit = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      val bodyFree = freeOfStmt(body) -- params.toSet - id
      freeVars(id) = bodyFree
      analyzeStmt(body)

    case ToplevelDefinition.Val(id, binding) =>
      freeOfStmt(binding)
      analyzeStmt(binding)

    case ToplevelDefinition.Let(id, binding) =>
      markExprEscaping(binding)
  }

  def process(m: ModuleDecl): Unit = {
    m.definitions.foreach(process)
    m.exports.foreach(markEscaping)
  }
}

object EscapeAnalysis {
  def apply(m: ModuleDecl): EscapeAnalysis = {
    val analysis = new EscapeAnalysis()
    analysis.process(m)
    analysis
  }
}