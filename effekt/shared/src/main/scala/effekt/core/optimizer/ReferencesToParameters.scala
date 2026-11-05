package effekt
package core
package optimizer

import scala.annotation.tailrec

object ReferencesToParameters extends Tree.Rewrite {

  def transform(m: ModuleDecl): ModuleDecl = rewrite(m)

  override def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Var(ref, init, capture, body) =>
      val rewrittenInit = rewrite(init)
      val rewrittenBody = rewrite(body)
      eliminate(ref, rewrittenInit, capture, rewrittenBody)
        .getOrElse(Stmt.Var(ref, rewrittenInit, capture, rewrittenBody))
    case other => super.rewrite(other)
  }

  private def eliminate(ref: Id, init: Expr, capture: Id, body: Stmt): Option[Stmt] = {
    val definitions = localDefinitions(body)

    @tailrec def leastFixedPoint(touching: Set[Id]): Set[Id] = {
      val next = definitions.collect {
        case (id, lit) if lit.free.contains(ref) || touching.exists(lit.free.contains) => id
      }.toSet
      if next == touching then touching else leastFixedPoint(next)
    }

    val touching = leastFixedPoint(Set.empty)
    if !Escape(ref, touching).direct(body) then return None

    val stateType = init.tpe
    val initial = Id(ref)
    val transformed = Stmt.Let(initial, init,
      Transformation(ref, stateType, touching).go(body, ValueVar(initial, stateType), false))
    Some(RemoveCapture(capture).rewrite(transformed))
  }

  private def localDefinitions(s: Stmt): Map[Id, BlockLit] = {
    object collector extends Tree.Query[Unit, Map[Id, BlockLit]] {
      def empty = Map.empty
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.Def(id, lit: BlockLit, body) => Map(id -> lit) ++ query(lit) ++ query(body)
      }
    }
    collector.query(s)(using ())
  }

  private class RemoveCapture(capture: Id) extends Tree.Rewrite {
    override def rewrite(capt: Captures): Captures = capt - capture
  }

  private case class Escape(ref: Id, touching: Set[Id]) {

    def mentions(free: Free): Boolean = free.contains(ref) || touching.exists(free.contains)
    def mentions(s: Stmt): Boolean = mentions(s.free)
    def mentions(e: Expr): Boolean = mentions(e.free)
    def mentions(b: Block): Boolean = mentions(b.free)

    def clean(vargs: List[Expr], bargs: List[Block]): Boolean =
      !vargs.exists(mentions) && !bargs.exists(mentions)

    def direct(s: Stmt): Boolean = s match {
      case Stmt.Def(id, lit: BlockLit, rest) if touching.contains(id) => direct(lit.body) && direct(rest)
      case Stmt.Def(id, block, rest) => !mentions(block) && direct(rest)
      case Stmt.Let(id, binding, rest) => !mentions(binding) && direct(rest)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        !mentions(callee) && clean(vargs, bargs) && direct(rest)
      case Stmt.Return(exprs) => !exprs.exists(mentions)
      case Stmt.Val(ids, binding, rest) => direct(binding) && direct(rest)
      case Stmt.App(BlockVar(id, _, _), targs, vargs, bargs) if touching.contains(id) => clean(vargs, bargs)
      case Stmt.If(cond, thn, els) => !mentions(cond) && direct(thn) && direct(els)
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        !mentions(scrutinee) && clauses.forall { case (_, lit) => direct(lit.body) } && default.forall(direct)
      case Stmt.Var(other, init, capture, rest) => !mentions(init) && direct(rest)
      case Stmt.Alloc(id, init, region, rest) => !mentions(init) && direct(rest)
      case Stmt.Get(id, tpe, other, capt, rest) => direct(rest)
      case Stmt.Put(other, capt, value, rest) => !mentions(value) && direct(rest)
      case Stmt.Hole(_, _) => true
      case other => !mentions(other)
    }
  }

  private case class Transformation(ref: Id, stateType: ValueType, touching: Set[Id]) {

    def mentions(s: Stmt): Boolean = s.free.contains(ref) || touching.exists(s.free.contains)

    def extended(callee: BlockVar): BlockVar = callee match {
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), capt) =>
        BlockVar(id, BlockType.Function(tparams, cparams, vparams :+ stateType, bparams, result :+ stateType), capt)
      case _ => sys error "Local definition without function type"
    }

    def results(s: Stmt): List[Id] = s.tpe.map { _ => Id("r") }

    def andReturn(s: Stmt, current: Expr): Stmt = {
      val ids = results(s)
      Stmt.Val(ids, s, Stmt.Return((ids zip s.tpe).map(ValueVar.apply) :+ current))
    }

    def dropState(call: Stmt, tpe: List[ValueType]): Stmt = {
      val ids = tpe.map { _ => Id("r") }
      Stmt.Val(ids :+ Id(ref), call, Stmt.Return((ids zip tpe).map(ValueVar.apply)))
    }

    def go(s: Stmt, current: Expr, returning: Boolean): Stmt = s match {
      case Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, body), rest) if touching.contains(id) =>
        val param = ValueParam(Id(ref), stateType)
        val lit = BlockLit(tparams, cparams, vparams :+ param, bparams, go(body, ValueVar(param.id, stateType), true))
        Stmt.Def(id, lit, go(rest, current, returning))
      case Stmt.Def(id, block, rest) => Stmt.Def(id, block, go(rest, current, returning))
      case Stmt.Let(id, binding, rest) => Stmt.Let(id, binding, go(rest, current, returning))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        Stmt.ImpureApp(id, callee, targs, vargs, bargs, go(rest, current, returning))
      case Stmt.Return(exprs) => if returning then Stmt.Return(exprs :+ current) else s
      case Stmt.Val(ids, binding, rest) if mentions(binding) =>
        val next = Id(ref)
        Stmt.Val(ids :+ next, go(binding, current, true), go(rest, ValueVar(next, stateType), returning))
      case Stmt.Val(ids, binding, rest) => Stmt.Val(ids, binding, go(rest, current, returning))
      case Stmt.App(callee @ BlockVar(id, _, _), targs, vargs, bargs) if touching.contains(id) =>
        val call = Stmt.App(extended(callee), targs, vargs :+ current, bargs)
        if returning then call else dropState(call, s.tpe)
      case Stmt.If(cond, thn, els) => Stmt.If(cond, go(thn, current, returning), go(els, current, returning))
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        Stmt.Match(scrutinee, if returning then tpe :+ stateType else tpe,
          clauses.map { case (id, lit) => (id, lit.copy(body = go(lit.body, current, returning))) },
          default.map(go(_, current, returning)))
      case Stmt.Var(other, init, capture, rest) => Stmt.Var(other, init, capture, go(rest, current, returning))
      case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, init, region, go(rest, current, returning))
      case Stmt.Get(id, tpe, other, capt, rest) if other == ref => Stmt.Let(id, current, go(rest, current, returning))
      case Stmt.Get(id, tpe, other, capt, rest) => Stmt.Get(id, tpe, other, capt, go(rest, current, returning))
      case Stmt.Put(other, capt, value, rest) if other == ref =>
        val next = Id(ref)
        Stmt.Let(next, value, go(rest, ValueVar(next, stateType), returning))
      case Stmt.Put(other, capt, value, rest) => Stmt.Put(other, capt, value, go(rest, current, returning))
      case Stmt.Hole(tpe, span) => if returning then Stmt.Hole(tpe :+ stateType, span) else s
      case other => if returning then andReturn(other, current) else other
    }
  }
}
