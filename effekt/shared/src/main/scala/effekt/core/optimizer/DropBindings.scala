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
 *
 * We make sure to drop a binding only if its use is at most as often as the binding itself.
 * Consider the program below where every iteration of a loop recomputes the value otherwise:
 *
 *   let n = size(arr)
 *   def go(i) = if (i < n) { ...; go(i + 1) } else { ... }
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
    dropping.rewrite(m)(using DropContext(Reachable(entrypoints, m), usedInCalledBlock.query(m)(using Set.empty), Map.empty))

  private case class DropContext(
    usage: Map[Id, Usage],
    usedInCalledBlock: Set[Id],
    definitions: Map[Id, Expr]
  ) {
    def updated(id: Id, p: Expr): DropContext = this.copy(definitions = definitions.updated(id, p))
  }

  private def hasDefinition(id: Id)(using C: DropContext) = C.definitions.isDefinedAt(id)
  private def definitionOf(id: Id)(using C: DropContext): Expr = C.definitions(id)
  private def usedOnce(id: Id)(using C: DropContext) = C.usage.get(id).contains(Usage.Once)
  private def droppable(id: Id)(using C: DropContext) = usedOnce(id) && !C.usedInCalledBlock.contains(id)
  private def currentContext(using C: DropContext): C.type = C

  private object dropping extends Tree.RewriteWithContext[DropContext] {

    override def rewrite(expr: Expr)(using DropContext): Expr = expr match {
      case Expr.ValueVar(id, tpe) if usedOnce(id) && hasDefinition(id) => definitionOf(id)
      case other => super.rewrite(other)
    }

    override def rewrite(stmt: Stmt)(using C: DropContext): Stmt = stmt match {
      case Stmt.Let(id, p: Expr, body) if droppable(id) =>
        val transformed = rewrite(p)
        rewrite(body)(using C.updated(id, transformed))
      case other => super.rewrite(other)
    }
  }

  /** The variables used inside a called block nested in their scope, the context itself are the let-bound variables of the current block. */
  private object usedInCalledBlock extends Tree.Query[Set[Id], Set[Id]] {
    def empty = Set.empty
    def combine = _ ++ _

    override def pure(using lets: Set[Id]) = {
      case Expr.ValueVar(id, _) if !lets.contains(id) => Set(id)
    }
    override def stmt(using lets: Set[Id]) = {
      case Stmt.Let(id, binding, body) => query(binding) ++ query(body)(using lets + id)
      // run in place
      case Stmt.Reset(b) => query(b.body)
      case Stmt.Region(b) => query(b.body)
      case Stmt.App(b: BlockLit, targs, vargs, bargs) => query(b.body) ++ all(vargs, query) ++ all(bargs, query)
    }
    override def block(using Set[Id]) = {
      case b: BlockLit => query(b.body)(using Set.empty)
    }
    override def operation(using Set[Id]) = {
      case o: Operation => query(o.body)(using Set.empty)
    }
    override def clause(using Set[Id]) = {
      case (_, b) => query(b.body)
    }
  }
}
