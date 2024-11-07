package effekt
package typer

import effekt.context.Context
import effekt.symbols._
import effekt.util.messages.ErrorMessageReifier


/**
 * All effects inferred by Typer are required to be concrete and dealiased.
 *
 * This way, we can easily compare them for equality.
 */
class ConcreteEffects private[typer] (protected val effects: List[InterfaceType]) {

  def toList: List[InterfaceType] = effects
  def toEffects: Effects = Effects(effects)

  // both are known to be concrete, no need to go through validation again
  def ++(other: ConcreteEffects): ConcreteEffects = ConcreteEffects.fromList(this.effects ++ other.effects)

  // we can use set difference since type constructors are assumed to be invariant and all unification variables
  // are substituted away.
  def --(other: ConcreteEffects): ConcreteEffects = ConcreteEffects.fromList(
    (this.effects.toSet -- other.effects.toSet).toList
  )

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: InterfaceType => Boolean): ConcreteEffects = ConcreteEffects.fromList(effects.filterNot(p))

  def canonical: List[InterfaceType] = effects.sorted(using CanonicalOrdering)

  def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
  def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)
}
object ConcreteEffects {
  // unsafe, doesn't perform check
  private def fromList(eff: List[InterfaceType]): ConcreteEffects = new ConcreteEffects(eff.distinct)

  /**
   * These smart constructors should not be used directly.
   * [[Typer.asConcrete]] should be used instead, since it performs substitution and dealiasing.
   */
  def apply(eff: List[InterfaceType])(using Context): ConcreteEffects =
    eff foreach assertConcreteEffect
    fromList(eff)

  def apply(effs: Effects)(using Context): ConcreteEffects = apply(effs.toList)

  def empty: ConcreteEffects = fromList(Nil)
}

val Pure = ConcreteEffects.empty

implicit def asConcrete(effs: Effects)(using C: Context): ConcreteEffects =
  ConcreteEffects(C.unification(effs))


/**
 * Asserts that all effects in the list are _concrete_, that is,
 * no unification variables (neither type, nor region) are allowed.
 *
 * If all effects are concrete (and we assume effect type constructors are INVARIANT):
 *   - we can use structural equality to compare them
 *   - we can use sets and hash maps
 *
 * Consequences:
 *   - If we try to add an effect that is not concrete, we should raise an "Could not infer..." error.
 *   - We need to substitute early in order to have more concrete effects.
 *   - Requiring effects to be concrete also simplifies effect-set comparison in [[TypeComparer]].
 *
 * TODO Question: should we ALWAYS require effects to be concrete, also when compared with [[TypeUnifier]]?
 */
private[typer] def assertConcreteEffects(effs: Effects)(using C: Context): Unit =
  effs.effects.foreach(assertConcreteEffect)

private[typer] def assertConcreteEffect(eff: InterfaceType)(using C: Context): Unit =
  unknowns(eff) match {
    case us if us.nonEmpty =>
      C.abort(pretty"Effects need to be fully known, but effect ${eff}'s type parameter(s) ${us.mkString(", ")} could not be inferred.\n\nMaybe try annotating them?")
    case _ => ()
  }

private[typer] def assertConcreteFunction(id: source.Id, tpe: BlockType)(using C: Context): Unit =
  unknowns(tpe) match {
    case us if us.nonEmpty =>
      C.abort(pretty"Cannot fully infer type for ${id}: ${tpe}")
    case _ => ()
  }

private type Unknown = CaptUnificationVar | UnificationVar

private def unknowns(tpe: ValueType): Set[Unknown] = tpe match {
  case ValueTypeRef(x) => unknowns(x)
  case ValueTypeApp(tpe, args) => args.flatMap(unknowns).toSet
  case BoxedType(tpe, capture) => unknowns(tpe) ++ unknowns(capture)
}

private def unknowns(tpe: TypeVar): Set[Unknown] = tpe match {
  case x: UnificationVar => Set(x)
  case x: TypeVar => Set.empty
}

private def unknowns(tpe: BlockType): Set[Unknown] = tpe match {
  case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
    vparams.flatMap(unknowns).toSet ++ bparams.flatMap(unknowns).toSet ++ unknowns(result) ++ unknowns(effects)
  case InterfaceType(tpe, args) => args.flatMap(unknowns).toSet
}

private def unknowns(capt: Captures): Set[Unknown] = capt match {
  case x @ CaptUnificationVar(role) => Set(x)
  case CaptureSet(captures) => Set.empty
}

private def unknowns(effs: Effects): Set[Unknown] = effs.toList.flatMap(unknowns).toSet
