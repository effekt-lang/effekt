package effekt
package core
package optimizer

class Deadcode(reachable: Map[Id, Usage]) extends core.Tree.Rewrite {

  private def used(id: Id): Boolean = reachable.get(id).exists(u => u != Usage.Never)

  private def unused(id: Id): Boolean = !used(id)

  override def stmt = {
    // Remove local unused definitions
    case Stmt.Def(id, block, body) if unused(id) => rewrite(body)
    case Stmt.Let(id, tpe, binding, body) if binding.capt.isEmpty && unused(id) => rewrite(body)

    case Stmt.Reset(body) =>
      rewrite(body) match {
        case BlockLit(tparams, cparams, vparams, List(prompt), body) if unused(prompt.id) => body
        case b => Stmt.Reset(b)
      }

    case Stmt.Match(sc, clauses, default) =>
      Stmt.Match(rewrite(sc), clauses.collect {
        case (id, clause) if used(id) => (id, rewrite(clause))
      }, default.map(rewrite))
  }

  override def implementation = {
    case Implementation(interface, operations) =>
      Implementation(rewrite(interface), operations.collect {
        case op if used(op.name) => rewrite(op)
      })
  }

  override def rewrite(m: ModuleDecl): ModuleDecl = m match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes,
        // drop unused constructors and operations
        declarations.map(rewrite),
        // drop unreachable externs
        m.externs.collect {
          case e: Extern.Def if used(e.id) => e
          case e: Extern.Include => e
        },
        // drop unreachable definitions
        definitions.collect { case d if used(d.id) => rewrite(d) },
        exports)
  }

  def rewrite(d: Declaration): Declaration = d match {
    case Declaration.Data(id, tparams, constructors) =>
      Declaration.Data(id, tparams, constructors.collect {
        case c if used(c.id) => c
      })
    case Declaration.Interface(id, tparams, properties) =>
      Declaration.Interface(id, tparams, properties.collect {
        case p if used(p.id) => p
      })
  }
}

object Deadcode {
  def remove(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl =
    val reachable = Reachable(entrypoints, m)
    Deadcode(reachable).rewrite(m)

  def remove(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    remove(Set(entrypoint), m)
}

