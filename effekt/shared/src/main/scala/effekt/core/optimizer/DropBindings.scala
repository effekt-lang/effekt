package effekt
package core
package optimizer

import context.Context

/**
 * This phase drops (inlines) unique value bindings.
 *
 *   let x = 42
 *   let y = x + 1
 *   let z = y * 2
 *   z
 *
 * -->
 *
 *   (42 + 1) * 2
 *
 * This improves the performance for JS on some benchmarks (mostly match_options, sum_range, and parsing_dollars).
 */
object DropBindings extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "drop-bindings"

  def run(input: CoreTransformed)(using C: Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val main = C.ensureMainExists(mod)
        Some(CoreTransformed(source, tree, mod, apply(Set(main), core)))
    }

  def apply(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl =
    dropping.rewrite(m)(using DropContext(Reachable(entrypoints, m), Map.empty))

  private case class DropContext(
    usage: Map[Id, Usage],
    definitions: Map[Id, Expr]
  ) {
    def updated(id: Id, p: Expr): DropContext = this.copy(definitions = definitions.updated(id, p))
  }

  private def hasDefinition(id: Id)(using C: DropContext) = C.definitions.isDefinedAt(id)
  private def definitionOf(id: Id)(using C: DropContext): Expr = C.definitions(id)
  private def usedOnce(id: Id)(using C: DropContext) = C.usage.get(id).contains(Usage.Once)
  private def currentContext(using C: DropContext): C.type = C

  private object dropping extends Tree.RewriteWithContext[DropContext] {

    override def rewrite(expr: Expr)(using DropContext): Expr = expr match {
      case Expr.ValueVar(id, tpe) if usedOnce(id) && hasDefinition(id) => definitionOf(id)
      case other => super.rewrite(other)
    }

    override def rewrite(stmt: Stmt)(using C: DropContext): Stmt = stmt match {
      case Stmt.Let(id, p: Expr, body) if usedOnce(id) =>
        val transformed = rewrite(p)
        rewrite(body)(using C.updated(id, transformed))
      case other => super.rewrite(other)
    }
  }
}
