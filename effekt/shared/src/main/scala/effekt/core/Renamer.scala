package effekt.core

import scala.collection.mutable

import effekt.{ core, symbols }

/**
 * Freshens bound names in a given Core term.
 * Do not use for tests! See [[effekt.core.TestRenamer]].
 *
 * @param names used to look up a reference by name to resolve to the same symbols.
 *              This is only used by tests to deterministically rename terms and check for
 *              alpha-equivalence.
 * @param prefix if the prefix is empty, the original name will be used as a prefix
 *
 * @param C the context is used to copy annotations from old symbols to fresh symbols
 */
class Renamer(names: Names = Names(Map.empty), prefix: String = "") extends core.Tree.Rewrite {

  // Local renamings: map of bound symbols to their renamed variants in a given scope.
  private var scope: Map[Id, Id] = Map.empty

  // All renamings: map of bound symbols to their renamed variants, globally!
  val renamed: mutable.HashMap[Id, Id] = mutable.HashMap.empty

  def freshIdFor(id: Id): Id =
    if prefix.isEmpty then Id(id) else Id(id.name.rename { _current => prefix })

  def withBindings[R](ids: List[Id])(f: => R): R =
    val scopeBefore = scope
    try {
      ids.foreach { x =>
        val fresh = freshIdFor(x)
        scope = scope + (x -> fresh)
        renamed.put(x, fresh)
      }

      f
    } finally { scope = scopeBefore }

  /** Alias for withBindings(List(id)){...} */
  def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)

  // free variables are left untouched
  override def id: PartialFunction[core.Id, core.Id] = {
    id => scope.getOrElse(id, id)
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {
    case core.Def(id, block, body) =>
      // can be recursive
      withBinding(id) { core.Def(rewrite(id), rewrite(block), rewrite(body)) }

    case core.Let(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Let(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.LetDirectApp(id, callee, targs, vargs, bargs, body) =>
      val resolvedCallee = rewrite(callee)
      val resolvedTargs = targs map rewrite
      val resolvedVargs = vargs map rewrite
      val resolvedBargs = bargs map rewrite
      withBinding(id) { core.LetDirectApp(rewrite(id), resolvedCallee, resolvedTargs, resolvedVargs, resolvedBargs, rewrite(body)) }

    case core.Val(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Val(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.Alloc(id, init, reg, body) =>
      val resolvedInit = rewrite(init)
      val resolvedReg = rewrite(reg)
      withBinding(id) { core.Alloc(rewrite(id), resolvedInit, resolvedReg, rewrite(body)) }

    case core.Var(ref, init, capt, body) =>
      val resolvedInit = rewrite(init)
      val resolvedCapt = rewrite(capt)
      withBinding(ref) { core.Var(rewrite(ref), resolvedInit, resolvedCapt, rewrite(body)) }

    case core.Get(id, tpe, ref, capt, body) =>
      val resolvedRef = rewrite(ref)
      val resolvedCapt = rewrite(capt)
      withBinding(id) { core.Get(rewrite(id), rewrite(tpe), resolvedRef, resolvedCapt, rewrite(body)) }

  }

  override def block: PartialFunction[Block, Block] = {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
        Block.BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite,
          rewrite(body))
      }
  }

  override def rewrite(o: Operation): Operation = o match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
        Operation(name,
          tparams map rewrite,
          cparams map rewrite,
          vparams map rewrite,
          bparams map rewrite,
          rewrite(body))
      }
  }

  def apply(m: core.ModuleDecl): core.ModuleDecl =
    m match {
      case core.ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        core.ModuleDecl(path, includes, declarations, externs, definitions map rewrite, exports)
    }

  def apply(s: Stmt): Stmt = {
    rewrite(s)
  }
}

object Renamer {
  def rename(b: Block): Block = Renamer().rewrite(b)
  def rename(b: BlockLit): (BlockLit, mutable.HashMap[Id, Id]) =
    val renamer = Renamer()
    val res = renamer.rewrite(b)
    (res, renamer.renamed)
}
