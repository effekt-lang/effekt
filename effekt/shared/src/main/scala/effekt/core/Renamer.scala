package effekt.core

import effekt.{ core, symbols }
import effekt.context.Context

/**
 * Freshens bound names in a given term
 *
 * @param names used to look up a reference by name to resolve to the same symbols.
 *              This is only used by tests to deterministically rename terms and check for
 *              alpha-equivalence.
 * @param prefix if the prefix is empty, the original name will be used
 *
 * @param C the context is used to copy annotations from old symbols to fresh symbols
 */
class Renamer(names: Names = Names(Map.empty), prefix: String = "") extends core.Tree.Rewrite {

  // TODO PROBLEM: if the input is not Barendregt, then the output will also not be...
  //   withBindings currently is invoked multiple times for the same id...

  // maps symbols bound in the current scope to their renamed variants.
  private var bound: Map[Id, Id] = Map.empty

  private var suffix: Int = 0

  def freshIdFor(id: Id): Id =
    suffix = suffix + 1
    val uniqueName = if prefix.isEmpty then id.name.name + suffix.toString else prefix + suffix.toString
    names.idFor(uniqueName)

  def withBindings[R](ids: List[Id])(f: => R): R =
    val before = bound
    try {
      ids.foreach { id =>
        bound.getOrElse(id, {
          bound = bound.updated(id, freshIdFor(id))
        })
      }
      f
    } finally { bound = before }

  def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)

  // free variables are left untouched
  override def id: PartialFunction[core.Id, core.Id] = {
    case id => bound.getOrElse(id, id)
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {

    // TODO here the scoping is not correct
    case core.Scope(definitions, rest) => withBindings(definitions.map {
      case core.Definition.Def(id, _) => id
      case core.Definition.Let(id, _) => id
    }) {
      core.Scope(definitions map rewrite, rewrite(rest))
    }
    case core.Val(id, binding, body) => withBinding(id) {
      import effekt.core
      core.Val(rewrite(id), rewrite(binding), rewrite(body))
    }
    case core.Alloc(id, init, reg, body) => withBinding(id) {
      core.Alloc(rewrite(id), rewrite(init), rewrite(reg), rewrite(body))
    }
    case core.Var(id, init, capt, body) => withBinding(id) {
      core.Var(rewrite(id), rewrite(init), rewrite(capt), rewrite(body))
    }
  }

  override def block: PartialFunction[Block, Block] = {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
        Block.BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite,
          rewrite(body))
      }
  }

  def apply(m: core.ModuleDecl): core.ModuleDecl =
    suffix = 0
    m match {
      case core.ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
        core.ModuleDecl(path, imports, declarations, externs, definitions map rewrite, exports)
    }
}

object Renamer {
  def rename(b: Block): Block = Renamer().rewrite(b)
}
