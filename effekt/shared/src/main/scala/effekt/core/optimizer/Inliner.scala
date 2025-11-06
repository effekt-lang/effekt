package effekt.core.optimizer

import effekt.core.*
import effekt.util

sealed trait InliningPolicy {
  def apply(id: Id): Boolean
}

class Unique(usage: Map[Id, Usage]) extends InliningPolicy {
  override def apply(id: Id): Boolean =
    usage.get(id).contains(Usage.Once) && usage.get(id).contains(Usage.Recursive)
}

case class Context(
  blocks: Map[Id, Block],
  exprs: Map[Id, Expr],
  //maxInlineSize: Int,
) {
  def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
  def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
}

object Context {
  def empty: Context = Context(Map.empty, Map.empty)
}

class Inliner(shouldInline: InliningPolicy) extends Tree.RewriteWithContext[Context] {

  def run(mod: ModuleDecl): ModuleDecl = {
    given Context = Context.empty
    mod match {
      case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        ModuleDecl(path, includes, declarations, externs, definitions.map { d =>
          util.trace("rewriting", util.show(d))
          val res = rewrite(d)
          util.trace("after", util.show(res))
          res
        }, exports)
    }
  }

  private def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  private def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  override def stmt(using ctx: Context): PartialFunction[Stmt, Stmt] = {
    case app @ Stmt.App(Block.BlockVar(id, tpe, capts), targs, vargs, bargs) if shouldInline(id) =>
      val BlockType.Function(tparams, cparams, vparams, bparams, result) = tpe match {
        case f: BlockType.Function => f
        case BlockType.Interface(name, targs) => ???
      }
      // TODO this is wrong, we need to bind the arguments
      util.trace("inlining", id)
      Stmt.App(blockFor(id).get, targs, vargs, bargs)
    case Stmt.Def(id, block, body) =>
      ctx.bind(id, block)
      Stmt.Def(id, block, rewrite(body))
    case Stmt.Let(id, tpe, binding, body) =>
      ctx.bind(id, binding)
      Stmt.Let(id, tpe, binding, rewrite(body))
  }

  override def expr(using Context): PartialFunction[Expr, Expr] = {
    case v @ Expr.ValueVar(id, tpe) if shouldInline(id) =>
      val e = exprFor(id)
      util.trace("inlining", id)
      e match {
        case Some(p: Expr.Make) => p
        case Some(p: Expr.Literal) => p
        case Some(p: Expr.Box) => p
        case Some(other) if other.capt.isEmpty => other
        case _ => v
      }
  }
}
