package effekt
package typer

import effekt.symbols.*
import effekt.symbols.EffectVar.EffectSetWildcard


case class SubstitutionException(x: CaptUnificationVar, subst: Map[Capture, Captures]) extends Exception

/**
 * Substitutions not only have unification variables as keys, since we also use the same mechanics to
 * compare two types with each other (see [[TypeComparer.subFunctionType]]).
 *
 * We should **not** use these Substitutions for instantiation anymore, since this introduces fresh Unification variables.
 */
case class Substitutions(
                          values: Map[ValueTypeVar, ValueType],
                          blocks : Map[BlockTypeVar, BlockType],
                          // invariant: we alway only map
                          //   - a single CaptureParam -> CaptureParam
                          //   - a CaptUnificationVar -> Captures
                          captures: Map[CaptVar, Captures],
                          effects: Map[EffectVar, EffectsOrRef]
) {

  def isDefinedAt(t: ValueTypeVar) = values.isDefinedAt(t)
  def isDefinedAt(c: CaptVar) = captures.isDefinedAt(c)

  def get(t: ValueTypeVar) = values.get(t)

  def get(c: Capture): Option[Capture] = captures.get(c) map {
    case CaptureSet(cs) if cs.size == 1 => cs.head
    case other => sys error "Substitutions should map single CaptureParams to single CaptureParams, got ${other}"
  }
  def get(x: CaptUnificationVar): Option[Captures] = captures.get(x)

  // amounts to first substituting this, then other
  def updateWith(other: Substitutions): Substitutions =
    Substitutions(
      values.view.mapValues { t => other.substitute(t) }.toMap,
      blocks.view.mapValues {t => other.substitute(t) }.toMap,
      captures.view.mapValues { t => other.substitute(t) }.toMap,
      effects.view.mapValues { t => other.substitute(t) }.toMap) ++ other

  // amounts to parallel substitution
  def ++(other: Substitutions): Substitutions = Substitutions(values ++ other.values, blocks ++ other.blocks, captures ++ other.captures, effects ++ other.effects)

  // shadowing
  private def without(tps: List[ValueTypeVar], bps: List[BlockType], cps: List[Capture], effs: List[EffectsOrRef]): Substitutions =
    Substitutions(
      values.filterNot { case (t, _) => tps.contains(t) },
      blocks.filterNot { case (t, _) => bps.contains(t) },
      captures.filterNot { case (t, _) => cps.contains(t) },
      effects.filterNot { case (t, _) => effs.contains(t) }
    )

  // TODO we DO need to distinguish between substituting unification variables for unification variables
  // and substituting concrete captures in unification variables... These are two fundamentally different operations.
  def substitute(c: Captures): Captures = c match {
    case x: CaptureSetWildcard => captures.getOrElse(x, x)
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

  def substitute(t: EffectsOrRef): EffectsOrRef = t match {
    case x: Effects => Effects(x.toList.map(substitute))
    case EffectRef(x) => effects.getOrElse(x, t)
  }
  def substitute(t: InterfaceType): InterfaceType = t match {
    case InterfaceType(cons, args) => InterfaceType(cons, args.map(substitute))
  }

  def substitute(t: BlockType): BlockType = t match {
    case BlockTypeRef(x) => blocks.getOrElse(x, t)
    case e: InterfaceType => substitute(e)
    case b: FunctionType  => substitute(b)
  }

  def substitute(t: FunctionType): FunctionType = t match {
    case FunctionType(tps, cps, vps, bps, ret, eff) =>
      // do not substitute with types parameters bound by this function!
      val substWithout = without(tps, Nil, cps, Nil)
      val p1 = vps map substWithout.substitute
      val p2 = bps map substWithout.substitute

      val a: FunctionType = FunctionType(
        tps,
        cps,
        p1,
        p2,
        substWithout.substitute(ret),
        substWithout.substitute(eff))
      a
  }
}

object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty[ValueTypeVar, ValueType], Map.empty[BlockTypeVar, BlockType], Map.empty[CaptVar | CaptureSetWildcard, Captures], Map.empty[EffectVar, EffectsOrRef])
  def apply(values: List[(ValueTypeVar, ValueType)], blocks : List[(BlockTypeVar, BlockType)], captures: List[(CaptVar, Captures)], effects: List[(EffectVar, EffectsOrRef)]): Substitutions =
    Substitutions(values.toMap, blocks.toMap, captures.toMap, effects.toMap)
  def types(keys: List[ValueTypeVar], values: List[ValueType]): Substitutions = Substitutions((keys zip values).toMap, Map.empty, Map.empty, Map.empty)
  def blocks(keys: List[BlockTypeVar], values: List[BlockType]): Substitutions = Substitutions(Map.empty, (keys zip values).toMap, Map.empty, Map.empty)
  def captures(keys: List[CaptVar], values: List[Captures]): Substitutions = Substitutions(Map.empty, Map.empty, (keys zip values).toMap, Map.empty)
  def effects(keys: List[EffectVar], values: List[EffectsOrRef]): Substitutions = Substitutions(Map.empty, Map.empty, Map.empty, (keys zip values).toMap)
}