package effekt.core.optimizer

import effekt.core.*
import effekt.core

import scala.collection.mutable

sealed trait InliningPolicy {
  def apply(id: Id)(using Context): Boolean
}

class Unique(maxInlineSize: Int) extends InliningPolicy {
  override def apply(id: Id)(using ctx: Context): Boolean =
    ctx.usages.get(id).contains(Usage.Once) &&
      !ctx.usages.get(id).contains(Usage.Recursive) &&
      ctx.blocks.get(id).exists(_.size <= maxInlineSize)
}

class UniqueJumpSimple(maxInlineSize: Int) extends InliningPolicy {
  override def apply(id: Id)(using ctx: Context): Boolean = {
    val use = ctx.usages.get(id)
    val block = ctx.blocks.get(id)
    var doInline = !ctx.usages.get(id).contains(Usage.Recursive)
    doInline &&= use.contains(Usage.Once) || block.collect {
      case Block.BlockLit(_, _, _, _, _: Stmt.Return) => true
      case Block.BlockLit(_, _, _, _, _: Stmt.App) => true
      case Block.BlockVar(_, _, _) => true
    }.getOrElse(false)
    doInline &&= block.exists(_.size <= maxInlineSize)
    doInline
  }
}

case class Context(
  blocks: Map[Id, Block],
  exprs: Map[Id, Expr],
  usages: mutable.Map[Id, Usage]
) {
  def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
  def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
}

object Context {
  def empty(usages: Map[Id, Usage]): Context = Context(Map.empty, Map.empty, mutable.Map.from(usages))
}

class Inliner(shouldInline: InliningPolicy, usages: Map[Id, Usage]) extends Tree.RewriteWithContext[Context] {

  def run(mod: ModuleDecl): ModuleDecl = {
    given Context = Context.empty(usages)
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

  def inlineApp(b: Block.BlockLit, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])(using ctx: Context): Stmt = {
    // (1) Rename definition's blocklit to keep IDs globally unique after inlining
    val (renamedBlock @ Block.BlockLit(tparams, cparams, vparams, bparams, body), renamedIds) = Renamer.rename(b)
    // (2) Copy usage information for renamed IDs
    renamedIds.foreach { (from, to) =>
      ctx.usages.get(from).foreach { info => ctx.usages.update(to, info) }
    }
    // (3) We only need to bind block arguments that are _not_ block variables. Thus, separate block var args from the rest
    val (bvars, other) = bparams.zip(bargs).partition {
      case (_, _: Block.BlockVar) => true
      case _ => false
    }
    // (4) Substitute. Only substitute block var args, other block args are bound before the inlinee's body
    val substBody = substitutions.substitute(renamedBlock.body)(using substitutions.Substitution(
      (tparams zip targs).toMap,
      (cparams zip bargs.map(_.capt)).toMap,
      (vparams.map(_.id) zip vargs).toMap,
      bvars.map { (bp, bv) => bp.id -> bv }.toMap
    ))
    // (5) Bind all block arguments that are not block variables
    bindBlocks(substBody, other)
  }

  override def stmt(using ctx: Context): PartialFunction[Stmt, Stmt] = {
    case app @ Stmt.App(bvar: BlockVar, targs, vargs, bargs) if shouldInline(bvar.id) =>
      val vas = vargs.map(rewrite)
      val bas = bargs.map(rewrite)
      blockFor(bvar.id).map {
        case block: Block.BlockLit =>
          inlineApp(block, targs, vargs, bargs)
        case b =>
          Stmt.App(b, targs, vargs, bargs)
      }.getOrElse(Stmt.App(bvar, targs, vas, bas))
    case Stmt.Def(id, block, body) =>
      val b = rewrite(block)(using ctx)
      given Context = ctx.bind(id, b)
      Stmt.Def(id, b, rewrite(body))
    case Stmt.Let(id, tpe, binding, body) =>
      val expr = rewrite(binding)(using ctx)
      given Context = ctx.bind(id, expr)
      Stmt.Let(id, tpe, expr, rewrite(body))
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
