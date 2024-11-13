package effekt
package core

import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.core.normal.*

class Deadcode(entrypoints: Set[Id], definitions: Map[Id, Definition]) extends core.Tree.Rewrite {

  val reachable = Reachable(entrypoints, definitions)

  override def stmt = {
    // Remove local unused definitions
    case Scope(defs, stmt) =>
      scope(defs.collect {
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
    Deadcode(entrypoints, m.definitions.map(d => d.id -> d).toMap).rewrite(m)
  def remove(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    remove(Set(entrypoint), m)
}

