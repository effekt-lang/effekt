package effekt
package core
package optimizer

import scala.collection.mutable

object ReferencesToParameters extends Tree.Rewrite {

  override def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Var(ref, init, capture, body) =>
      val rewrittenInit = rewrite(init)
      val rewrittenBody = rewrite(body)
      eliminate(ref, rewrittenInit, capture, rewrittenBody).getOrElse(Stmt.Var(ref, rewrittenInit, capture, rewrittenBody))
    case other => super.rewrite(other)
  }

  def transform(m: ModuleDecl): ModuleDecl = rewrite(m)

  def eliminate(ref: Id, init: Expr, capture: Id, body: Stmt): Option[Stmt] = {
    val definitions = mutable.Map.empty[Id, BlockLit]
    collectDefinitions(body, definitions)

    val touching = mutable.Set.empty[Id]
    var changed = true
    while changed do {
      changed = false
      definitions.foreach { case (id, lit) =>
        if !touching.contains(id) && (lit.free.contains(ref) || touching.exists(lit.free.contains)) then
          touching += id
          changed = true
      }
    }

    val passing = mutable.Set.empty[Id]

    def plan(): Option[(Analysis, Set[Id], Set[Id])] = {
      val writing = mutable.Set.empty[Id]
      writing ++= passing
      var changed = true
      while changed do {
        changed = false
        definitions.foreach { case (id, lit) =>
          if touching.contains(id) && !writing.contains(id) && writes(ref, writing, lit.body) then
            writing += id
            changed = true
        }
      }

      val analysis = Analysis(ref, touching.toSet, writing.toSet, definitions.keySet.toSet)
      if !analysis.direct(body) then return None

      val returning = mutable.Set.empty[Id]
      val candidates = mutable.Set.empty[Id]
      changed = true
      while changed do {
        val before = returning.size
        analysis.returningCalls(body, false, returning, candidates)
        changed = returning.size != before
      }
      Some((analysis, returning.toSet, candidates.toSet))
    }

    var current = plan()
    if current.isEmpty then return None
    var tried = Set.empty[Id]
    var next = current.get._3
    while next.nonEmpty do {
      val candidate = next.head
      tried += candidate
      touching += candidate
      passing += candidate
      plan() match {
        case Some(result) => current = Some(result)
        case None =>
          touching -= candidate
          passing -= candidate
      }
      next = current.get._3 -- tried
    }

    val (analysis, returning, _) = current.get
    val stateType = init.tpe
    val transformation = Transformation(ref, stateType, analysis, returning)
    val initial = Id(ref)
    val transformed = Stmt.Let(initial, init, transformation.go(body, ValueVar(initial, stateType), false))
    Some(RemoveCapture(capture).rewrite(transformed))
  }

  private def writes(ref: Id, writing: collection.Set[Id], s: Stmt): Boolean = {
    object query extends Tree.Query[Unit, Boolean] {
      def empty = false
      def combine = _ || _
      override def stmt(using Unit) = {
        case Stmt.Put(other, _, _, _) if other == ref => true
        case Stmt.App(BlockVar(id, _, _), _, _, _) if writing.contains(id) => true
      }
    }
    query.query(s)(using ())
  }

  private def collectDefinitions(s: Stmt, into: mutable.Map[Id, BlockLit]): Unit = {
    object collector extends Tree.Query[Unit, Unit] {
      def empty = ()
      def combine = (_, _) => ()
      override def stmt(using Unit) = {
        case Stmt.Def(id, lit: BlockLit, body) =>
          into(id) = lit
          query(lit)
          query(body)
      }
    }
    collector.query(s)(using ())
  }

  private class RemoveCapture(capture: Id) extends Tree.Rewrite {
    override def rewrite(capt: Captures): Captures = capt - capture
  }

  private case class Analysis(ref: Id, touching: Set[Id], writing: Set[Id], local: Set[Id]) {

    def writes(s: Stmt): Boolean = ReferencesToParameters.writes(ref, writing, s)

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

    def returningCalls(s: Stmt, returning: Boolean, into: mutable.Set[Id], candidates: mutable.Set[Id]): Unit = s match {
      case Stmt.Def(id, lit: BlockLit, rest) if touching.contains(id) =>
        returningCalls(lit.body, into.contains(id), into, candidates)
        returningCalls(rest, returning, into, candidates)
      case Stmt.Def(id, block, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.Let(id, binding, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.Val(ids, binding, rest) =>
        if mentions(binding) then returningCalls(binding, writes(binding) && (returning || mentions(rest)), into, candidates)
        returningCalls(rest, returning, into, candidates)
      case Stmt.App(BlockVar(id, _, _), _, _, _) if touching.contains(id) =>
        if returning && writing.contains(id) then into += id
        if returning && !writing.contains(id) then candidates += id
      case Stmt.App(BlockVar(id, _, _), _, _, _) if local.contains(id) =>
        if returning then candidates += id
      case Stmt.If(cond, thn, els) =>
        returningCalls(thn, returning, into, candidates)
        returningCalls(els, returning, into, candidates)
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        clauses.foreach { case (_, lit) => returningCalls(lit.body, returning, into, candidates) }
        default.foreach(returningCalls(_, returning, into, candidates))
      case Stmt.Var(other, init, capture, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.Alloc(id, init, region, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.Get(id, tpe, other, capt, rest) => returningCalls(rest, returning, into, candidates)
      case Stmt.Put(other, capt, value, rest) => returningCalls(rest, returning, into, candidates)
      case _ => ()
    }
  }

  private case class Transformation(ref: Id, stateType: ValueType, analysis: Analysis, returning: Set[Id]) {

    def callee(b: BlockVar): BlockVar = b match {
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), capt) =>
        val newResult = if returning.contains(id) then result :+ stateType else result
        BlockVar(id, BlockType.Function(tparams, cparams, vparams :+ stateType, bparams, newResult), capt)
      case _ => sys error "Local definition without function type"
    }

    def andReturn(s: Stmt, current: Expr): Stmt = {
      val results = s.tpe.map { _ => Id("r") }
      Stmt.Val(results, s, Stmt.Return((results zip s.tpe).map(ValueVar.apply) :+ current))
    }

    def go(s: Stmt, current: Expr, isReturning: Boolean): Stmt = s match {
      case Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, body), rest) if analysis.touching.contains(id) =>
        val param = ValueParam(Id(ref), stateType)
        val lit = BlockLit(tparams, cparams, vparams :+ param, bparams,
          go(body, ValueVar(param.id, stateType), returning.contains(id)))
        Stmt.Def(id, lit, go(rest, current, isReturning))
      case Stmt.Def(id, block, rest) => Stmt.Def(id, block, go(rest, current, isReturning))
      case Stmt.Let(id, binding, rest) => Stmt.Let(id, binding, go(rest, current, isReturning))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        Stmt.ImpureApp(id, callee, targs, vargs, bargs, go(rest, current, isReturning))
      case Stmt.Return(exprs) =>
        if isReturning then Stmt.Return(exprs :+ current) else s
      case Stmt.Val(ids, binding, rest) if analysis.mentions(binding) && !analysis.writes(binding) =>
        Stmt.Val(ids, go(binding, current, false), go(rest, current, isReturning))
      case Stmt.Val(ids, binding, rest) if analysis.mentions(binding) && !isReturning && !analysis.mentions(rest) =>
        Stmt.Val(ids, go(binding, current, false), rest)
      case Stmt.Val(ids, binding, rest) if analysis.mentions(binding) =>
        val next = Id(ref)
        Stmt.Val(ids :+ next, go(binding, current, true), go(rest, ValueVar(next, stateType), isReturning))
      case Stmt.Val(ids, binding, rest) =>
        Stmt.Val(ids, binding, go(rest, current, isReturning))
      case Stmt.App(b @ BlockVar(id, _, _), targs, vargs, bargs) if analysis.touching.contains(id) =>
        val call = Stmt.App(callee(b), targs, vargs :+ current, bargs)
        (returning.contains(id), isReturning) match {
          case (true, true) | (false, false) => call
          case (true, false) =>
            val results = s.tpe.map { _ => Id("r") }
            Stmt.Val(results :+ Id(ref), call, Stmt.Return((results zip s.tpe).map(ValueVar.apply)))
          case (false, true) if !analysis.writing.contains(id) => andReturn(call, current)
          case (false, true) => sys error "Tail definition called in non-tail position"
        }
      case Stmt.If(cond, thn, els) =>
        Stmt.If(cond, go(thn, current, isReturning), go(els, current, isReturning))
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        Stmt.Match(scrutinee, if isReturning then tpe :+ stateType else tpe,
          clauses.map { case (id, lit) => (id, lit.copy(body = go(lit.body, current, isReturning))) },
          default.map(go(_, current, isReturning)))
      case Stmt.Var(other, init, capture, rest) => Stmt.Var(other, init, capture, go(rest, current, isReturning))
      case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, init, region, go(rest, current, isReturning))
      case Stmt.Get(id, tpe, other, capt, rest) if other == ref => Stmt.Let(id, current, go(rest, current, isReturning))
      case Stmt.Get(id, tpe, other, capt, rest) => Stmt.Get(id, tpe, other, capt, go(rest, current, isReturning))
      case Stmt.Put(other, capt, value, rest) if other == ref =>
        val next = Id(ref)
        Stmt.Let(next, value, go(rest, ValueVar(next, stateType), isReturning))
      case Stmt.Put(other, capt, value, rest) => Stmt.Put(other, capt, value, go(rest, current, isReturning))
      case Stmt.Hole(tpe, span) => if isReturning then Stmt.Hole(tpe :+ stateType, span) else s
      case other => if isReturning then andReturn(other, current) else other
    }
  }
}
