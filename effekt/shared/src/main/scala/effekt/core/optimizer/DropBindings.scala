package effekt
package core
package optimizer

import optimizer. { Normalizer => normal }

import context.Context

/**
 * This phase drops (inlines) unique value bindings.
 *
 * This improves the performance for JS on the benchmarks: ...
 */
object DropBindings extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "drop-bindings"

  def run(input: CoreTransformed)(using C: Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val main = C.checkMain(mod)
        Some(CoreTransformed(source, tree, mod, apply(Set(main), core)))
    }

  def apply(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl =
    dropping.rewrite(m)(using DropContext(Reachable(entrypoints, m), Map.empty))

  private case class DropContext(
    usage: Map[Id, Usage],
    definitions: Map[Id, Pure]
  ) {
    def updated(id: Id, p: Pure): DropContext = this.copy(definitions = definitions.updated(id, p))
  }

  private def hasDefinition(id: Id)(using C: DropContext) = C.definitions.isDefinedAt(id)
  private def definitionOf(id: Id)(using C: DropContext): Pure = C.definitions(id)
  private def usedOnce(id: Id)(using C: DropContext) = C.usage.get(id).contains(Usage.Once)
  private def currentContext(using C: DropContext): C.type = C

  private object dropping extends Tree.RewriteWithContext[DropContext] {

    override def pure(using DropContext) = {
      case Pure.ValueVar(id, tpe) if usedOnce(id) && hasDefinition(id) => definitionOf(id)
    }

    override def stmt(using DropContext) = {
      case Stmt.Scope(defs, body) =>
        var contextSoFar = currentContext
        val ds = defs.flatMap {
          case Definition.Let(id, tpe, p: Pure) if usedOnce(id) =>
            val transformed = rewrite(p)(using contextSoFar)
            contextSoFar = contextSoFar.updated(id, transformed)
            None
          case d => Some(rewrite(d)(using contextSoFar))
        }
        normal.Scope(ds, rewrite(body)(using contextSoFar))
    }
  }
}
