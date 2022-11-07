package effekt
package typer

import effekt.symbols._


case class SubstitutionException(x: CaptUnificationVar, subst: Map[Capture, Captures]) extends Exception

/**
 * Substitutions not only have unification variables as keys, since we also use the same mechanics to
 * compare two types with each other (see [[TypeComparer.subFunctionType]]).
 *
 * We should **not** use these Substitutions for instantiation anymore, since this introduces fresh Unification variables.
 */
case class Substitutions(
  values: Map[TypeVar, ValueType],
  // invariant: we alway only map
  //   - a single CaptureParam -> CaptureParam
  //   - a CaptUnificationVar -> Captures
  captures: Map[CaptVar, Captures]
) {

  def isDefinedAt(t: TypeVar) = values.isDefinedAt(t)
  def isDefinedAt(c: CaptVar) = captures.isDefinedAt(c)

  def get(t: TypeVar) = values.get(t)

  def get(c: Capture): Option[Capture] = captures.get(c) map {
    case CaptureSet(cs) if cs.size == 1 => cs.head
    case other => sys error "Substitutions should map single CaptureParams to single CaptureParams, got ${other}"
  }
  def get(x: CaptUnificationVar): Option[Captures] = captures.get(x)

  // amounts to first substituting this, then other
  def updateWith(other: Substitutions): Substitutions =
    Substitutions(
      values.view.mapValues { t => other.substitute(t) }.toMap,
      captures.view.mapValues { t => other.substitute(t) }.toMap) ++ other

  // amounts to parallel substitution
  def ++(other: Substitutions): Substitutions = Substitutions(values ++ other.values, captures ++ other.captures)

  // shadowing
  private def without(tps: List[TypeVar], cps: List[Capture]): Substitutions =
    Substitutions(
      values.filterNot { case (t, _) => tps.contains(t) },
      captures.filterNot { case (t, _) => cps.contains(t) }
    )

  // TODO we DO need to distinguish between substituting unification variables for unification variables
  // and substituting concrete captures in unification variables... These are two fundamentally different operations.
  def substitute(c: Captures): Captures = c match {
    case x: CaptUnificationVar => captures.getOrElse(x, x)
    case CaptureSet(cs) => CaptureSet(cs.map {
      case x: Capture =>
        get(x).getOrElse(x)
    })
  }

  def substitute(t: ValueType): ValueType = t match {
    case ValueTypeRef(x) =>
      values.getOrElse(x, t)
    case ValueTypeApp(t, args) =>
      ValueTypeApp(t, args.map { substitute })
    case BoxedType(tpe, capt) =>
      BoxedType(substitute(tpe), substitute(capt))
  }

  def substitute(t: Effects): Effects = Effects(t.toList.map(substitute))
  def substitute(t: InterfaceType): InterfaceType = t match {
    case InterfaceType(cons, args) => InterfaceType(cons, args.map(substitute))
  }

  def substitute(t: BlockType): BlockType = t match {
    case e: InterfaceType => substitute(e)
    case b: FunctionType  => substitute(b)
  }

  def substitute(t: FunctionType): FunctionType = t match {
    case FunctionType(tps, cps, vps, bps, ret, eff) =>
      // do not substitute with types parameters bound by this function!
      val substWithout = without(tps, cps)
      FunctionType(
        tps,
        cps,
        vps map substWithout.substitute,
        bps map substWithout.substitute,
        substWithout.substitute(ret),
        substWithout.substitute(eff))
  }
}

object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty[TypeVar, ValueType], Map.empty[CaptVar, Captures])
  def apply(values: List[(TypeVar, ValueType)], captures: List[(CaptVar, Captures)]): Substitutions = Substitutions(values.toMap, captures.toMap)
  def types(keys: List[TypeVar], values: List[ValueType]): Substitutions = Substitutions((keys zip values).toMap, Map.empty)
  def captures(keys: List[CaptVar], values: List[Captures]): Substitutions = Substitutions(Map.empty, (keys zip values).toMap)
}