package effekt

import effekt.symbols.{ Type, ValueType, TypeVar, BlockType, Effectful, TypeApp }

import effekt.util.messages.ErrorReporter

case class Unifier(admitted: List[TypeVar], subst: Map[TypeVar, ValueType] = Map.empty) {

  def merge(t1: Type, t2: Type)(given report: ErrorReporter): Unifier = (t1, t2) match {

    case (s: TypeVar, t: ValueType) if admitted.contains(s) =>
      add(s, t)

    case (TypeApp(t1, args1), TypeApp(t2, args2)) if t1 == t2 =>
      if (args1.size != args2.size)
        report.error(s"Argument count does not match $t1 vs. $t2")

      (args1 zip args2).foldLeft(this) { case (u, (a1, a2)) => u.merge(a1, a2) }

    case (f1 @ BlockType(args1, ret1), f2 @ BlockType(args2, ret2)) =>

      if (args1.size != args2.size)
        report.error(s"Argument count does not match $f1 vs. $f2")

      (args1 zip args2).foldLeft(merge(ret1.tpe, ret2.tpe)) { case (u, (a1, a2)) => u.merge(a1, a2) }

    case (t, s) if t == s =>
      this

    case (t, s) =>
      report.error(s"Expected ${t}, but got ${s}")
      this
  }

  def add(k: TypeVar, v: ValueType)(given report: ErrorReporter) = {
    subst.get(k).foreach { v2 => if (v != v2) {
      report.error(s"${k} cannot be instantiated with ${v} and with ${v2} at the same time.")
    } }
    copy(subst = subst + (k -> v))
  }

  def substitute(t: Type): Type = t match {
    case t: ValueType => substitute(t)
    case b: BlockType => substitute(b)
  }

  def substitute(t: ValueType): ValueType = t match {
    case x: TypeVar =>
      subst.get(x).getOrElse(x)
    case TypeApp(t, args) =>
      TypeApp(t, args.map { substitute })
    case other => other
  }

  def substitute(e: Effectful): Effectful = e match {
    case Effectful(tpe, effs) => Effectful(substitute(tpe), effs)
  }

  def substitute(t: BlockType): BlockType = t match {
    case BlockType(ps, ret) => BlockType(ps.map { substitute }, this substitute ret)
  }

  def checkFullyDefined(given report: ErrorReporter) = admitted.foreach { tpe =>
    if (!subst.isDefinedAt(tpe))
      report.error(s"Couldn't infer type for ${tpe}")
  }
}
