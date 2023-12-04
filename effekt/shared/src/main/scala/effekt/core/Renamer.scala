package effekt.core

import effekt.{ core, symbols }
import effekt.context.Context

/**
 * Freshens bound names in a given term
 *
 * @param names used to look up a reference by name to resolve to the same symbols.
 *              This is only used by tests to deterministically rename terms and check for
 *              alpha-equivalence.
 * @param prefix if the prefix is empty, the original name will be used as a prefix
 *
 * @param C the context is used to copy annotations from old symbols to fresh symbols
 */
class Renamer(names: Names = Names(Map.empty), prefix: String = "") extends core.Tree.Rewrite {

  // maps symbols bound in the current scope to their renamed variants.
  private var bound: List[Map[Id, Id]] = List.empty

  private var suffix: Int = 0

  def freshIdFor(id: Id): Id =
    suffix = suffix + 1
    val uniqueName = if prefix.isEmpty then id.name.name + suffix.toString else prefix + suffix.toString
    names.idFor(uniqueName)

  def withBindings[R](ids: List[Id])(f: => R): R =
    val before = bound
    val env = ids.map{ x => x -> freshIdFor(x) }.toMap
    try {
      bound = env :: bound
      f
    } finally { bound = before }

  /** Alias for withBindings(List(id)){...} */
  def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)
  /** Skip the innermost environment frame temporarily */
  def unbind[R](f: => R): R = {
    val before = bound
    try {
      bound = bound.tail
      f
    } finally { bound = before }
  }

  // free variables are left untouched
  override def id: PartialFunction[core.Id, core.Id] = {
    case id => bound.collectFirst{
      case bnds if bnds.contains(id) => bnds(id)
    }.getOrElse(id)
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {
    case core.Scope(definitions, body) =>

      def go(rest: List[Definition], defs: List[Definition]): core.Scope = rest match {
        case (d : core.Definition.Def) :: rest =>
          // can be recursive
          withBinding(d.id) { go(rest, defs :+ rewrite(d)) }
        case core.Definition.Let(id, binding) :: rest =>
          withBinding(id) { go(rest, defs :+ core.Definition.Let(rewrite(id), unbind{ rewrite(binding) } )) }
        case Nil => core.Scope(defs, rewrite(body))
      }

      go(definitions, Nil)

    case core.Val(id, binding, body) => withBinding(id) {
      core.Val(rewrite(id), unbind { rewrite(binding) }, rewrite(body))
    }
    case core.Alloc(id, init, reg, body) => withBinding(id) {
      core.Alloc(rewrite(id), unbind { rewrite(init) } , unbind { rewrite(reg) }, rewrite(body))
    }
    case core.Var(id, init, capt, body) => withBinding(id) {
      core.Var(rewrite(id), unbind { rewrite(init) }, unbind { rewrite(capt) }, rewrite(body))
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
