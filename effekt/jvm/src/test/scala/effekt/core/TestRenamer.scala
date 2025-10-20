package effekt.core

import effekt.{ core, symbols }
import effekt.context.Context

/**
 * Freshens bound names in a given term for tests.
 * Please use this _only_ for tests. Otherwise, prefer [[effekt.core.Renamer]].
 *
 * @param names used to look up a reference by name to resolve to the same symbols.
 *              This is only used by tests to deterministically rename terms and check for
 *              alpha-equivalence.
 * @param prefix if the prefix is empty, the original name will be used as a prefix
 *
 * @param C the context is used to copy annotations from old symbols to fresh symbols
 */
class TestRenamer(names: Names = Names(Map.empty), prefix: String = "", defaultScopes: List[Map[Id, Id]] = List.empty) extends core.Tree.Rewrite {

  // list of scopes that map bound symbols to their renamed variants.
  private var scopes: List[Map[Id, Id]] = defaultScopes

  private var suffix: Int = 0

  def freshIdFor(id: Id): Id =
    suffix = suffix + 1
    val uniqueName = if prefix.isEmpty then id.name.name + "_" + suffix.toString else prefix + suffix.toString
    names.idFor(uniqueName)

  def withBindings[R](ids: List[Id])(f: => R): R =
    withMapping(ids.map { x => x -> freshIdFor(x) }.toMap)(f)

  /** Alias for withBindings(List(id)){...} */
  def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)

  def withMapping[R](ids: Map[Id, Id])(f: => R): R =
    val before = scopes
    try {
      scopes = ids :: scopes
      f
    } finally { scopes = before }

  // free variables are left untouched
  override def id: PartialFunction[core.Id, core.Id] = {
    case id => scopes.collectFirst {
      case bnds if bnds.contains(id) => bnds(id)
    }.getOrElse(id)
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {
    case core.Def(id, block, body) =>
      // can be recursive
      withBinding(id) { core.Def(rewrite(id), rewrite(block), rewrite(body)) }

    case core.Let(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Let(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      val resolvedCallee = rewrite(callee)
      val resolvedTargs = targs map rewrite
      val resolvedVargs = vargs map rewrite
      val resolvedBargs = bargs map rewrite
      withBinding(id) { core.ImpureApp(rewrite(id), resolvedCallee, resolvedTargs, resolvedVargs, resolvedBargs, rewrite(body)) }

    case core.Val(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Val(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.Alloc(id, init, reg, body) =>
      val resolvedInit = rewrite(init)
      val resolvedReg = rewrite(reg)
      withBinding(id) { core.Alloc(rewrite(id), resolvedInit, resolvedReg, rewrite(body)) }

    case core.Var(ref, init, capt, body) =>
      val resolvedInit = rewrite(init)
      // TODO: is this how we want to treat captures here?
      val resolvedCapt = freshIdFor(capt)
      withBinding(ref){ withMapping(Map(capt -> resolvedCapt)) { core.Var(rewrite(ref), resolvedInit, resolvedCapt, rewrite(body)) }}

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

  def apply(m: core.Toplevel): core.Toplevel = {
    suffix = 0
    rewrite(m)
  }

  def apply(m: core.ModuleDecl): core.ModuleDecl =
    suffix = 0
    m match {
      case core.ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        core.ModuleDecl(path, includes, declarations, externs, definitions map rewrite, exports)
    }

  def apply(s: Stmt): Stmt = {
    suffix = 0
    rewrite(s)
  }
}