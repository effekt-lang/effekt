package effekt
package core
package optimizer

import util.Trampoline

class Deadcode(reachable: Map[Id, Usage]) extends core.Tree.RewriteTrampolined {

  private def used(id: Id): Boolean = reachable.get(id).exists(u => u != Usage.Never)

  private def unused(id: Id): Boolean = !used(id)

  override def rewrite(stmt: Stmt): Trampoline[Stmt] = stmt match {
    // Remove local unused definitions
    case Stmt.Def(id, block, body) if unused(id) => rewrite(body)
    case Stmt.Let(id, binding, body) if binding.capt.isEmpty && unused(id) => rewrite(body)

    case Stmt.Reset(body) =>
      rewrite(body).map {
        case BlockLit(tparams, cparams, vparams, List(prompt), body) if unused(prompt.id) => body
        case b => Stmt.Reset(b)
      }

    case Stmt.Match(sc, tpe, clauses, default) => for {
      sc2      <- rewrite(sc)
      clauses2 <- all(clauses.filter { case (id, clause) => used(id) }, rewrite)
      default2 <- opt(default, rewrite)
    } yield Match(sc2, tpe, clauses2, default2)

    case other => super.rewrite(other)
  }

  override def rewrite(impl: Implementation): Trampoline[Implementation] = impl match {
    case Implementation(interface, operations) => for {
      interface2  <- Trampoline.done(rewrite(interface))
      operations2 <- all(operations.filter(op => used(op.name)), rewrite)
    } yield Implementation(interface2, operations2)
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

  override def rewrite(d: Declaration): Declaration = d match {
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

