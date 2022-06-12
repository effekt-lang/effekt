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
    case x: TypeVar =>
      values.getOrElse(x, x)
    case ValueTypeApp(t, args) =>
      ValueTypeApp(t, args.map { substitute })
    case BoxedType(tpe, capt) =>
      BoxedType(substitute(tpe), substitute(capt))
    case other => other
  }

  // TODO implement
  def substitute(t: Effects): Effects = Effects(t.toList.map(substitute))
  def substitute(t: InterfaceType): InterfaceType = t match {
    case t: Interface => t
    case t: BuiltinEffect => t
    case BlockTypeApp(cons, args) => BlockTypeApp(cons, args.map(substitute))
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

case class BiSubstitutions(
  values: Map[TypeVar, (ValueType, ValueType)],
  // TODO make bounds concrete (CaptureSet instead of Captures)
  captures: Map[CaptVar, (Captures, Captures)]
) {

  def isDefinedAt(t: TypeVar) = values.isDefinedAt(t)
  def isDefinedAt(c: CaptVar) = captures.isDefinedAt(c)

  def get(t: TypeVar)(using p: Polarity): Option[ValueType] = (values.get(t), p) match {
    case (Some((lower, upper)), Covariant) => Some(lower)
    case (Some((lower, upper)), Contravariant) => Some(upper)
    // here we assume that both bounds are equal (has to be checked before adding to the substitution)
    // hence we can use an arbitrary bounds
    case (Some((lower, upper)), Invariant) => Some(lower)
    case (None, _) => None
  }
  def get(c: CaptVar)(using p: Polarity): Option[Captures] = (captures.get(c), p) match {
    case (Some((lower, upper)), Covariant) => Some(lower)
    case (Some((lower, upper)), Contravariant) => Some(upper)
    // here we assume that both bounds are equal (has to be checked before adding to the substitution)
    // hence we can use an arbitrary bounds
    case (Some((lower, upper)), Invariant) => Some(lower)
    case (None, _) => None
  }

  // amounts to first substituting this, then other
  def updateWith(other: BiSubstitutions): BiSubstitutions =
    substitute(other) ++ other

  // applies other to this
  def substitute(other: BiSubstitutions): BiSubstitutions =
    BiSubstitutions(
      values.view.mapValues { case (lower, upper) =>
        (other.substitute(lower)(using Covariant), other.substitute(upper)(using Contravariant))
      }.toMap,
      captures.view.mapValues { case (lower, upper) =>
        (other.substitute(lower)(using Covariant), other.substitute(upper)(using Contravariant))
      }.toMap
    )

  // amounts to parallel substitution
  def ++(other: BiSubstitutions): BiSubstitutions = BiSubstitutions(values ++ other.values, captures ++ other.captures)

  // shadowing
  private def without(tps: List[TypeVar], cps: List[Capture]): BiSubstitutions =
    BiSubstitutions(
      values.filterNot { case (t, _) => tps.contains(t) },
      captures.filterNot { case (t, _) => cps.contains(t) }
    )

  // TODO we DO need to distinguish between substituting unification variables for unification variables
  // and substituting concrete captures in unification variables... These are two fundamentally different operations.
  def substitute(c: Captures)(using Polarity): Captures =
    // TODO implement
    c

  //    c.flatMap {
  //      // we are probably instantiating a function type
  //      case x: CaptureUnificationVar if captures.keys.exists(c => c.concrete) =>
  //        throw SubstitutionException(x, captures)
  //      case c => captures.getOrElse(c, CaptureSet(c))
  //    }

  def substitute(t: ValueType)(using Polarity): ValueType = t match {
    case x: TypeVar =>
      get(x).getOrElse(x)
    case ValueTypeApp(t, args) =>
      // TODO What about aliases and their variance?
      //   should we dealias first?
      //   Right now we treat it as covariant, which is not correct.
      ValueTypeApp(t, args.map { substitute })
    case BoxedType(tpe, capt) =>
      BoxedType(substitute(tpe), substitute(capt))
    case other => other
  }

  // TODO implement
  def substitute(t: Effects)(using Polarity): Effects = Effects(t.toList.map(substitute))
  def substitute(t: InterfaceType)(using Polarity): InterfaceType = t match {
    case t: Interface => t
    case t: BuiltinEffect => t
    case BlockTypeApp(cons, args) => BlockTypeApp(cons, args.map(substitute))
  }

  def substitute(t: BlockType)(using Polarity): BlockType = t match {
    case e: InterfaceType => substitute(e)
    case b: FunctionType  => substitute(b)
  }

  def substitute(t: FunctionType)(using p: Polarity): FunctionType = t match {
    case FunctionType(tps, cps, vps, bps, ret, eff) =>
      // do not substitute with types parameters bound by this function!
      val substWithout = without(tps, cps)
      FunctionType(
        tps,
        cps,
        vps.map { param => substWithout.substitute(param)(using p.flip) },
        bps.map { param => substWithout.substitute(param)(using p.flip) },
        substWithout.substitute(ret),
        substWithout.substitute(eff))
  }
}

object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty[TypeVar, ValueType], Map.empty[CaptVar, Captures])
  def apply(values: List[(TypeVar, ValueType)], captures: List[(CaptVar, Captures)]): Substitutions = Substitutions(values.toMap, captures.toMap)
}

// TODO Mostly for backwards compat
implicit def typeMapToSubstitution(values: Map[TypeVar, ValueType]): Substitutions = Substitutions(values, Map.empty[CaptVar, Captures])
implicit def captMapToSubstitution(captures: Map[CaptVar, Captures]): Substitutions = Substitutions(Map.empty[TypeVar, ValueType], captures)

