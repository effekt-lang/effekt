package effekt.core.optimizer

import effekt.core.*
import effekt.core

sealed trait InliningPolicy {
  def apply(id: Id)(using Context): Boolean
}

class Unique(usage: Map[Id, Usage]) extends InliningPolicy {
  override def apply(id: Id)(using ctx: Context): Boolean =
    usage.get(id).contains(Usage.Once) &&
      !usage.get(id).contains(Usage.Recursive) &&
      ctx.blocks.get(id).exists(_.size <= ctx.maxInlineSize)
}

class UniqueJumpSimple(usage: Map[Id, Usage]) extends InliningPolicy {
  override def apply(id: Id)(using ctx: Context): Boolean = {
    val use = usage.get(id)
    val block = ctx.blocks.get(id)
    var doInline = !usage.get(id).contains(Usage.Recursive)
    doInline &&= use.contains(Usage.Once) || block.collect {
      case effekt.core.Block.BlockLit(_, _, _, _, _: Stmt.Return) => true
      case effekt.core.Block.BlockLit(_, _, _, _, _: Stmt.App) => true
    }.isDefined
    doInline &&= block.exists(_.size <= ctx.maxInlineSize)
    doInline
  }
}

case class Context(
  blocks: Map[Id, Block],
  exprs: Map[Id, Expr],
  maxInlineSize: Int,
) {
  def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
  def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
}

object Context {
  def empty: Context = Context(Map.empty, Map.empty, 50)
}

class Inliner(shouldInline: InliningPolicy) extends Tree.RewriteWithContext[Context] {

  def run(mod: ModuleDecl): ModuleDecl = {
    given Context = Context.empty
    mod match {
      case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        ModuleDecl(path, includes, declarations, externs, definitions.map(rewrite), exports)
    }
  }

  private def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  private def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  def bindBlock(body: Stmt, bparam: BlockParam, barg: Block): Stmt =
    Stmt.Def(bparam.id, barg, body)

  def bindBlocks(body: Stmt, blocks: List[(BlockParam, Block)]): Stmt =
    blocks.foldRight(body) { case ((bp, barg), acc) =>
      bindBlock(acc, bp, barg)
    }

  def bindValue(body: Stmt, vparam: ValueParam, varg: Expr): Stmt =
    Stmt.Let(vparam.id, vparam.tpe, varg, body)

  def bindValues(body: Stmt, values: List[(ValueParam, Expr)]): Stmt =
    values.foldRight(body) { case ((vp, varg), acc) =>
      bindValue(acc, vp, varg)
    }

  override def stmt(using ctx: Context): PartialFunction[Stmt, Stmt] = {
    case app @ Stmt.App(bvar: BlockVar, targs, vargs, bargs) if shouldInline(bvar.id) =>
      //util.trace("inlining", bvar.id)
      val vas = vargs.map(rewrite)
      val bas = bargs.map(rewrite)
      blockFor(bvar.id).map {
        case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
          var b = bindValues(rewrite(body), vparams.zip(vas))
          b = bindBlocks(b, bparams.zip(bas))
          b
        case b =>
          Stmt.App(b, targs, vargs, bargs)
      }.getOrElse(Stmt.App(bvar, targs, vas, bas))
    case Stmt.Def(id, block, body) =>
      given Context = ctx.bind(id, block)
      Stmt.Def(id, rewrite(block), rewrite(body))
    case Stmt.Let(id, tpe, binding, body) =>
      given Context = ctx.bind(id, binding)
      Stmt.Let(id, tpe, rewrite(binding), rewrite(body))
  }

  override def expr(using Context): PartialFunction[Expr, Expr] = {
    case v@Expr.ValueVar(id, tpe) if shouldInline(id) =>
      val e = exprFor(id)
      //util.trace("inlining", id)
      e match {
        case Some(p: Expr.Make) => p
        case Some(p: Expr.Literal) => p
        case Some(p: Expr.Box) => p
        case Some(other) if other.capt.isEmpty => other
        case _ => v
      }
  }

  override def toplevel(using ctx: Context): PartialFunction[Toplevel, Toplevel] = {
    case Toplevel.Def(id, block) =>
      given Context = ctx.bind(id, block)
      Toplevel.Def(id, rewrite(block))
    case Toplevel.Val(id, tpe, binding) =>
      Toplevel.Val(id, tpe, rewrite(binding))
  }
}
