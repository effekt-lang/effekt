package effekt
package core
package optimizer

import scala.collection.mutable

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Removes local definitions that do not occur, noting on the way whether the others are called once and occur nowhere
 * else, and then inlines those with [[inlineOnce]].
 *
 * A definition comes after its rest, so a removed definition does not count what it uses.
 */
def cleanup(m: ModuleDecl): ModuleDecl = {
  val calledOnce = mutable.Map.empty[Id, Boolean]
  object removing extends Tree.Rewrite {
    override def rewrite(b: BlockVar): BlockVar = {
      calledOnce(b.id) = false
      b
    }
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) =>
        val rewrittenRest = rewrite(rest)
        if calledOnce.contains(id) then Stmt.Def(id, rewrite(block), rewrittenRest) else rewrittenRest
      case Stmt.App(callee @ BlockVar(f, _, _), targs, vargs, bargs) =>
        calledOnce(f) = !calledOnce.contains(f)
        Stmt.App(callee, targs, vargs.map(rewrite), bargs.map(rewrite))
      case other => super.rewrite(other)
    }
  }
  val removed = removing.rewrite(m)
  inlineOnce(removed, calledOnce.collect { case (f, true) => f }.toSet)
}

/**
 * Inlines the definitions in [[once]] at their only call:
 *
 *   [[ def f(x){g} = s; C[ f(a){h} ] ]]  =  C[ let x = a; def g = h; s ]
 *
 * A block argument that is a variable is substituted instead, since it may be a reference used by `get` and `put`.
 */
def inlineOnce(m: ModuleDecl, once: Set[Id]): ModuleDecl = {
  val inlined = mutable.Map.empty[Id, BlockLit]
  object inlining extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block: BlockLit, rest) if once.contains(id) =>
        inlined(id) = rewrite(block)
        rewrite(rest)
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) if inlined.contains(f) =>
        val block = inlined(f)
        val variables = (block.bparams zip bargs).collect { case (param, arg: BlockVar) => param.id -> arg }
        val types = Substitution(DB.from(block.tparams zip targs), DB.from(block.cparams zip bargs.map(_.capt)), DB.empty, DB.from(variables))
        val withBlocks = (block.bparams zip bargs).foldRight(substitute(block.body)(using types)) {
          case ((param, arg: BlockVar), rest) => rest
          case ((param, arg), rest) => Stmt.Def(param.id, rewrite(arg), rest)
        }
        (block.vparams zip vargs).foldRight(withBlocks) { case ((param, arg), rest) => Stmt.Let(param.id, rewrite(arg), rest) }
      case other => super.rewrite(other)
    }
  }
  inlining.rewrite(m)
}
