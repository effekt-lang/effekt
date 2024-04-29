package effekt
package core

object ReportReachable extends Phase[CoreTransformed, CoreTransformed] {
  import effekt.context.Context

  override val phaseName: String = "report-reachable"

  override def run(input: PhaseResult.CoreTransformed)(using C: Context): Option[PhaseResult.CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) =>
      val rewrite = new Tree.Rewrite {
        override def stmt: PartialFunction[Stmt, Stmt] = {
          case ReportIfReachable(errs, body) =>
            errs.foreach(Context.report)
            body
        }
      }
      Some(CoreTransformed(source, tree, mod, rewrite.rewrite(core)))
  }
}
