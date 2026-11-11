package effekt
package core
package optimizer

import scala.annotation.tailrec

/**
 * Turns a local mutable reference that does not escape into an ordinary value:
 * the local definitions that use it take its current value as an extra parameter
 * and return its new value as an extra result.
 */
object ReferencesToParameters extends Tree.Rewrite {

  def transform(m: ModuleDecl): ModuleDecl = rewrite(m)

  override def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Var(ref, init, capture, body) =>
      val rewrittenInit = rewrite(init)
      val rewrittenBody = rewrite(body)
      uses(ref, rewrittenBody) match {
        case uses if escapes(ref, uses, rewrittenBody) => Stmt.Var(ref, rewrittenInit, capture, rewrittenBody)
        case uses => eliminate(ref, rewrittenInit, capture, uses, rewrittenBody)
      }
    case other => super.rewrite(other)
  }

  /**
   * The local definitions, prompts and continuations in [[body]] that use the reference,
   * directly or through one that does.
   */
  private def uses(ref: Id, body: Stmt): Set[Id] = {
    val dependencies = dependenciesIn(body)

    @tailrec def leastFixedPoint(uses: Set[Id]): Set[Id] = {
      val next = dependencies.collect {
        case (id, Dependency(free, handler)) if (free.contains(ref) || uses.exists(free.contains)) && handler.forall(uses.contains) => id
      }.toSet
      if next == uses then uses else leastFixedPoint(next)
    }

    leastFixedPoint(Set.empty)
  }

  /**
   * Whether the reference escapes from [[body]]: it, or one of the definitions that use it,
   * occurs other than in `get` and `put`, as the callee of a call, or as a definition.
   * For example as an argument, in a closure, or under a `reset`.
   */
  private def escapes(ref: Id, uses: Set[Id], body: Stmt): Boolean = {
    def mentioned(free: Free): Boolean = mentions(ref, uses, free)
    val outside = body.free

    def escaping(s: Stmt): Boolean = s match {
      case Stmt.Def(id, lit: BlockLit, rest) if uses.contains(id) => escaping(lit.body) || escaping(rest)
      case Stmt.Def(id, block, rest) => mentioned(block.free) || escaping(rest)
      case Stmt.Let(id, binding, rest) => mentioned(binding.free) || escaping(rest)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        mentioned(callee.free) || vargs.exists(v => mentioned(v.free)) || bargs.exists(b => mentioned(b.free)) || escaping(rest)
      case Stmt.Return(exprs) => exprs.exists(e => mentioned(e.free))
      case Stmt.Val(ids, binding, rest) => escaping(binding) || escaping(rest)
      case Stmt.App(BlockVar(id, _, _), targs, vargs, bargs) if uses.contains(id) =>
        vargs.exists(v => mentioned(v.free)) || bargs.exists(b => mentioned(b.free))
      case Stmt.If(cond, thn, els) => mentioned(cond.free) || escaping(thn) || escaping(els)
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        mentioned(scrutinee.free) || clauses.exists { case (_, lit) => escaping(lit.body) } || default.exists(escaping)
      case Stmt.Var(other, init, capture, rest) => mentioned(init.free) || escaping(rest)
      case Stmt.Alloc(id, init, region, rest) => mentioned(init.free) || escaping(rest)
      case Stmt.Get(id, tpe, other, capt, rest) => escaping(rest)
      case Stmt.Put(other, capt, value, rest) => mentioned(value.free) || escaping(rest)
      case Stmt.Hole(_, _) => false
      case Stmt.Reset(BlockLit(_, _, _, List(BlockParam(prompt, _, _)), body)) if uses.contains(prompt) => escaping(body)
      case Stmt.Shift(BlockVar(prompt, _, _), _, body) if uses.contains(prompt) => escaping(body)
      case Stmt.Resume(BlockVar(k, _, _), body) if uses.contains(k) => escaping(body)
      case shift @ Stmt.Shift(prompt, BlockParam(k, BlockType.Resume(result, _), _), _) if mentioned(shift.free) =>
        val captured = StaticResumptions.Shift(shift, k, result)
        !outside.contains(prompt.id) || !captured.resumesOnly ||
          mentioned(captured.replaceResumptions { _ => Stmt.Return(Nil) }.free) ||
          resumed(k, shift.body).exists(escaping)
      case other => mentioned(other.free)
    }

    escaping(body)
  }

  /** Replaces `var ref = init; body` by passing the current value of the reference to the definitions that use it. */
  private def eliminate(ref: Id, init: Expr, capture: Id, uses: Set[Id], body: Stmt): Stmt = {
    val stateType = init.tpe
    val initial = Id(ref)
    val transformation = Transformation(ref, stateType, uses)
    val transformed = Stmt.Let(initial, init,
      transformation.dropState(transformation.go(body, ValueVar(initial, stateType)), body.tpe))
    RemoveCapture(capture).rewrite(transformed)
  }

  /** The statements [[k]] is resumed with in [[body]]. */
  private def resumed(k: Id, body: Stmt): List[Stmt] = {
    object query extends Tree.Query[Unit, List[Stmt]] {
      def empty = Nil
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.Resume(BlockVar(id, _, _), resumed) if id == k => List(resumed)
      }
    }
    query.query(body)(using ())
  }

  /** Whether [[free]] contains the reference or one of the definitions that use it. */
  private def mentions(ref: Id, uses: Set[Id], free: Free): Boolean =
    free.contains(ref) || uses.exists(free.contains)

  /**
   * What a local definition, prompt or continuation refers to: a definition and a prompt their
   * body, a continuation the prompt it is captured up to. One bound in the handler of a shift
   * only uses the reference if the continuation of that shift does.
   */
  private case class Dependency(free: Set[Id], handler: Option[Id])

  private def dependenciesIn(s: Stmt): Map[Id, Dependency] = {
    object collector extends Tree.Query[Option[Id], Map[Id, Dependency]] {
      def empty = Map.empty
      def combine = _ ++ _
      override def stmt(using handler: Option[Id]) = {
        case Stmt.Def(id, lit: BlockLit, body) => Map(id -> Dependency(lit.free.freeIds, handler)) ++ query(lit) ++ query(body)
        case Stmt.Reset(lit @ BlockLit(_, _, _, List(BlockParam(prompt, _, _)), body)) => Map(prompt -> Dependency(lit.free.freeIds, handler)) ++ query(body)
        case Stmt.Shift(BlockVar(prompt, _, _), BlockParam(k, _, _), body) => Map(k -> Dependency(Set(prompt), handler)) ++ query(body)(using Some(k))
        case Stmt.Resume(_, body) => query(body)(using None)
      }
    }
    collector.query(s)(using None)
  }

  /** Removes the capture of the eliminated reference from all capture sets. */
  private class RemoveCapture(capture: Id) extends Tree.Rewrite {
    override def rewrite(capt: Captures): Captures = capt - capture
  }

  /** Passes the current value of the reference explicitly; every transformed statement returns it as an extra result. */
  private case class Transformation(ref: Id, stateType: ValueType, uses: Set[Id]) {

    /** The type of a definition that uses the reference, with the state as extra parameter and extra result. */
    def extended(callee: BlockVar): BlockVar = callee match {
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), capt) =>
        BlockVar(id, BlockType.Function(tparams, cparams, vparams :+ stateType, bparams, result :+ stateType), capt)
      case _ => sys error "Local definition without function type"
    }

    /** `val r = s; return r, current` for a statement that does not return the state itself. */
    def andReturn(s: Stmt, current: Expr): Stmt = {
      val results = s.tpe.map { _ => Id("r") }
      Stmt.Val(results, s, Stmt.Return((results zip s.tpe).map(ValueVar.apply) :+ current))
    }

    /** `val r, state = s; return r` where [[tpe]] are the types of `r`. */
    def dropState(s: Stmt, tpe: List[ValueType]): Stmt = {
      val results = tpe.map { _ => Id("r") }
      Stmt.Val(results :+ Id(ref), s, Stmt.Return((results zip tpe).map(ValueVar.apply)))
    }

    /** Transforms [[s]] where [[current]] holds the current value of the reference. */
    def go(s: Stmt, current: Expr): Stmt = s match {
      case Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, body), rest) if uses.contains(id) =>
        val param = ValueParam(Id(ref), stateType)
        val lit = BlockLit(tparams, cparams, vparams :+ param, bparams, go(body, ValueVar(param.id, stateType)))
        Stmt.Def(id, lit, go(rest, current))
      case Stmt.Def(id, block, rest) => Stmt.Def(id, block, go(rest, current))
      case Stmt.Let(id, binding, rest) => Stmt.Let(id, binding, go(rest, current))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        Stmt.ImpureApp(id, callee, targs, vargs, bargs, go(rest, current))
      case Stmt.Return(exprs) => Stmt.Return(exprs :+ current)
      case Stmt.Val(ids, binding, rest) if mentions(ref, uses, binding.free) =>
        val next = Id(ref)
        Stmt.Val(ids :+ next, go(binding, current), go(rest, ValueVar(next, stateType)))
      case Stmt.Val(ids, binding, rest) => Stmt.Val(ids, binding, go(rest, current))
      case Stmt.App(callee @ BlockVar(id, _, _), targs, vargs, bargs) if uses.contains(id) =>
        Stmt.App(extended(callee), targs, vargs :+ current, bargs)
      case Stmt.If(cond, thn, els) => Stmt.If(cond, go(thn, current), go(els, current))
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        Stmt.Match(scrutinee, tpe :+ stateType,
          clauses.map { case (id, lit) => (id, lit.copy(body = go(lit.body, current))) },
          default.map(go(_, current)))
      case Stmt.Var(other, init, capture, rest) => Stmt.Var(other, init, capture, go(rest, current))
      case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, init, region, go(rest, current))
      case Stmt.Get(id, tpe, other, capt, rest) if other == ref => Stmt.Let(id, current, go(rest, current))
      case Stmt.Get(id, tpe, other, capt, rest) => Stmt.Get(id, tpe, other, capt, go(rest, current))
      case Stmt.Put(other, capt, value, rest) if other == ref =>
        val next = Id(ref)
        Stmt.Let(next, value, go(rest, ValueVar(next, stateType)))
      case Stmt.Put(other, capt, value, rest) => Stmt.Put(other, capt, value, go(rest, current))
      case Stmt.Hole(tpe, span) => Stmt.Hole(tpe :+ stateType, span)
      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(BlockParam(prompt, BlockType.Prompt(answer), capt)), body)) if uses.contains(prompt) =>
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(BlockParam(prompt, BlockType.Prompt(answer :+ stateType), capt)), go(body, current)))
      case Stmt.Shift(BlockVar(prompt, BlockType.Prompt(answer), promptCapt), BlockParam(k, BlockType.Resume(result, _), capt), body) if uses.contains(prompt) =>
        Stmt.Shift(BlockVar(prompt, BlockType.Prompt(answer :+ stateType), promptCapt),
          BlockParam(k, BlockType.Resume(result :+ stateType, answer :+ stateType), capt), go(body, current))
      case Stmt.Resume(BlockVar(k, BlockType.Resume(result, answer), capt), body) if uses.contains(k) =>
        Stmt.Resume(BlockVar(k, BlockType.Resume(result :+ stateType, answer :+ stateType), capt), go(body, current))
      case shift @ Stmt.Shift(prompt, BlockParam(k, BlockType.Resume(result, answer), capt), _) if mentions(ref, uses, shift.free) =>
        val retyped = BlockParam(k, BlockType.Resume(result :+ stateType, answer), capt)
        Stmt.Shift(prompt, retyped, StaticResumptions.Shift(shift, k, result).replaceResumptions { resumed =>
          Stmt.Resume(BlockVar(k, retyped.tpe, retyped.capt), go(resumed, current))
        })
      case other => andReturn(other, current)
    }
  }
}
