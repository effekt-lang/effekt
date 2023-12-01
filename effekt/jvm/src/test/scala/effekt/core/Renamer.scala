package effekt.core

import effekt.{core, symbols}

class Renamer(names: core.Names, prefix: String = "$") extends core.Tree.Rewrite {
  var bound: List[symbols.Symbol] = Nil

  def withBindings[R](ids: List[symbols.Symbol])(f: => R): R = {
    val oldBound = bound
    bound = ids ++ bound
    val res = f
    bound = oldBound
    res
  }

  def withBinding[R](id: symbols.Symbol)(f: => R): R = withBindings(List(id))(f)

  override def id: PartialFunction[core.Id, core.Id] = { id =>
    if (bound.contains(id)) {
      names.idFor(prefix ++ (bound.length - bound.indexOf(id)).toString)
    } else id
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {
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

  override def rewrite(p: core.Param.BlockParam): core.Param.BlockParam = p match {
    case core.Param.BlockParam(id, tpe) =>
      withBinding(id) {
        core.Param.BlockParam(rewrite(id), rewrite(tpe))
      }
  }

  def apply(m: core.ModuleDecl): core.ModuleDecl = m match {
    case core.ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      core.ModuleDecl(path, imports, declarations, externs, definitions map rewrite, exports)
  }
}
