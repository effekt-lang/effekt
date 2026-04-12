package effekt
package cpsds
import core.Id
import cpsds.substitutions.{ Substitution, substitute }

object Inliner {

  def shouldInline(id: Id, analysis: UsageAnalysis): Boolean =
    (analysis.functions.get(id), analysis.usage.get(id)) match {
      case (Some(info), Some(usage)) =>
        !info.isRecursive && usage.isUsedOnce && info.externalCalls.size == 1
      case _ => false
    }

  def isUnused(id: Id, analysis: UsageAnalysis): Boolean =
    analysis.usage.get(id).forall(_.isUnused)

  def reduce(id: Id, args: List[Expr], analysis: UsageAnalysis): Stmt = {
    val info = analysis.functions(id)
    val (bindings, subst) = bindArgs(info.params, args)
    val body = substitute(info.body)(using Substitution(subst))
    bindings.foldRight(body) { case ((id, expr), rest) => Stmt.Let(id, expr, rest) }
  }

  private def bindArgs(params: List[Id], args: List[Expr]): (List[(Id, Expr)], Map[Id, Expr]) = {
    val bindings = List.newBuilder[(Id, Expr)]
    val subst = Map.newBuilder[Id, Expr]
    params.zip(args).foreach { case (param, arg) =>
      arg match {
        case v: Expr.Variable =>
          subst += (param -> v)
        case other =>
          val fresh = Id(param)
          bindings += (fresh -> other)
          subst += (param -> Expr.Variable(fresh))
      }
    }
    (bindings.result(), subst.result())
  }

  // --- Rewrite ---

  def rewrite(s: Stmt, analysis: UsageAnalysis): Stmt = s match {

    case Stmt.Def(id, params, body, rest) if shouldInline(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.Def(id, params, body, rest) if isUnused(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.Def(id, params, body, rest) =>
      Stmt.Def(id, params, rewrite(body, analysis), rewrite(rest, analysis))

    case Stmt.New(id, interface, operations, rest) if isUnused(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(op => rewrite(op, analysis)), rewrite(rest, analysis))

    case Stmt.Let(id, binding, rest) if isUnused(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding, analysis), rewrite(rest, analysis))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(e => rewrite(e, analysis)), purity, rewrite(rest, analysis))

    case Stmt.App(id, args) if shouldInline(id, analysis) =>
      rewrite(reduce(id, args.map(e => rewrite(e, analysis)), analysis), analysis)

    case Stmt.App(id, args) =>
      Stmt.App(id, args.map(e => rewrite(e, analysis)))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(e => rewrite(e, analysis)))

    case Stmt.Val(id, binding, rest) =>
      Stmt.Val(id, rewrite(binding, analysis), rewrite(rest, analysis))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond, analysis), rewrite(thn, analysis), rewrite(els, analysis))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(rewrite(scrutinee, analysis),
        clauses.map { case (id, cl) => (id, rewrite(cl, analysis)) },
        default.map(rewrite(_, analysis)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, rewrite(ks, analysis), rewrite(rest, analysis))

    case Stmt.Alloc(id, init, region, rest) if isUnused(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, rewrite(init, analysis), region, rewrite(rest, analysis))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, rewrite(init, analysis), rewrite(ks, analysis), rewrite(rest, analysis))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, rewrite(rest, analysis))

    case Stmt.Get(ref, id, rest) if isUnused(id, analysis) =>
      rewrite(rest, analysis)

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, rewrite(rest, analysis))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, rewrite(value, analysis), rewrite(rest, analysis))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, rewrite(body, analysis), rewrite(ks1, analysis), rewrite(k1, analysis))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, rewrite(body, analysis), rewrite(ks1, analysis), rewrite(k1, analysis))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(r, ks, k, rewrite(body, analysis), rewrite(ks1, analysis), rewrite(k1, analysis))

    case h: Stmt.Hole => h
  }

  def rewrite(e: Expr, analysis: UsageAnalysis): Expr = e match {
    case Expr.Variable(_) => e
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite(_, analysis)))
    case Expr.Abort => e
    case Expr.Return => e
    case Expr.Toplevel => e
  }

  def rewrite(op: Operation, analysis: UsageAnalysis): Operation =
    Operation(op.name, op.params, rewrite(op.body, analysis))

  def rewrite(cl: Clause, analysis: UsageAnalysis): Clause =
    Clause(cl.params, rewrite(cl.body, analysis))

  // --- Toplevel ---

  def rewrite(d: ToplevelDefinition, analysis: UsageAnalysis): Option[ToplevelDefinition] = d match {
    case ToplevelDefinition.Def(id, params, body) if shouldInline(id, analysis) =>
      None

    case ToplevelDefinition.Def(id, params, body) if isUnused(id, analysis) =>
      None

    case ToplevelDefinition.Def(id, params, body) =>
      Some(ToplevelDefinition.Def(id, params, rewrite(body, analysis)))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      Some(ToplevelDefinition.Val(id, ks, k, rewrite(binding, analysis)))

    case ToplevelDefinition.Let(id, binding) if isUnused(id, analysis) =>
      None

    case ToplevelDefinition.Let(id, binding) =>
      Some(ToplevelDefinition.Let(id, rewrite(binding, analysis)))
  }

  // --- Entry point ---

  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl = {
    val analysis = UsageAnalysis(m)
    // Mark the entrypoint and all exports as used so they aren't dropped
    analysis.usage.getOrElseUpdate(entrypoint, UsageInfo()).references += 1
    m.exports.foreach { id =>
      analysis.usage.getOrElseUpdate(id, UsageInfo()).references += 1
    }
    m.copy(definitions = m.definitions.flatMap(d => rewrite(d, analysis)))
  }
}
