package effekt
package core
package optimizer

class Deadcode(reachable: Map[Id, Usage]) extends core.Tree.Rewrite {

  override def stmt = {
    // Remove local unused definitions
    case Scope(defs, stmt) =>
      MaybeScope(defs.collect {
        case d: Definition.Def if reachable.isDefinedAt(d.id) => rewrite(d)
        // we only keep non-pure OR reachable let bindings
        case d: Definition.Let if d.capt.nonEmpty || reachable.isDefinedAt(d.id) => rewrite(d)
      }, rewrite(stmt))

    case Reset(body) =>
      rewrite(body) match {
        case BlockLit(tparams, cparams, vparams, List(prompt), body) if !reachable.isDefinedAt(prompt.id) => body
        case b => Stmt.Reset(b)
      }
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
    val reachable = Reachable(entrypoints, m)
    Deadcode(reachable).rewrite(m)

  def remove(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    remove(Set(entrypoint), m)
}

