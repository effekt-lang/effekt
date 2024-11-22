package effekt
package core

class Deadcode(reachable: Map[Id, Usage]) extends core.Tree.Rewrite {

  override def stmt = {
    // Remove local unused definitions
    case Scope(defs, stmt) =>
      normal.scope(defs.collect {
        case d: Definition.Def if reachable.isDefinedAt(d.id) => rewrite(d)
        // we only keep non-pure OR reachable let bindings
        case d: Definition.Let if d.capt.nonEmpty || reachable.isDefinedAt(d.id) => rewrite(d)
      }, rewrite(stmt))
  }

  override def rewrite(m: ModuleDecl): ModuleDecl = m.copy(
    // Remove top-level unused definitions
    definitions = m.definitions.collect { case d if reachable.isDefinedAt(d.id) => rewrite(d) },
    externs = m.externs.collect {
      case e: Extern.Def if reachable.isDefinedAt(e.id) => e
      case e: Extern.Include => e
    }
  )
}

object Deadcode {
  def remove(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl =
    val reachable = Reachable(entrypoints, m.definitions.map(d => d.id -> d).toMap)
    Deadcode(reachable).rewrite(m)
  def remove(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    remove(Set(entrypoint), m)

  def remove(stmt: Stmt): Stmt =
    val reachable = Reachable(stmt)
    Deadcode(reachable).rewrite(stmt)
}

