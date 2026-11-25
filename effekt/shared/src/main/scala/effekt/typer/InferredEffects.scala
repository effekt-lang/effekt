package effekt
package typer

import effekt.context.Context
import effekt.symbols._
import effekt.util.messages.{help, ErrorMessageReifier}


/**
 * The requirements of a term on its context, as inferred, being the capabilities they were resolved to.
 *
 * A requirement enters through a capability lookup and leaves where its capability is bound,
 * therefore requirements are compared as capabilities, **never** by their types, which may still be unknown (see issue #1211).
 */
class InferredEffects private[typer] (val capabilities: List[BlockParam]) {
  def ++(other: InferredEffects): InferredEffects = InferredEffects((capabilities ++ other.capabilities).distinct)

  def --(bound: List[BlockParam]): InferredEffects = InferredEffects(capabilities.filterNot(bound.contains))

  def isEmpty: Boolean = capabilities.isEmpty
  def nonEmpty: Boolean = capabilities.nonEmpty

  def toEffects(using C: Context): Effects = Effects(capabilities.map(C.capabilityType))

  def canonical(using C: Context): List[BlockParam] = {
    assertOrderable(capabilities.map(C.capabilityType))
    capabilities.sortBy(C.capabilityType)(using CanonicalOrdering)
  }
}

// the entries of a repeated interface are ordered by their type arguments,
// which must be concrete when wanting to order the whole thing
private def assertOrderable(effs: List[InterfaceType])(using Context): Unit =
  effs.groupBy(_.typeConstructor).values.filter(_.size > 1).flatten.foreach { eff =>
    assertConcreteEffect(eff)
  }

object InferredEffects {
  def apply(capabilities: List[BlockParam]): InferredEffects = new InferredEffects(capabilities.distinct)

  def empty: InferredEffects = new InferredEffects(Nil)

  def union(effs: IterableOnce[InferredEffects]): InferredEffects = effs.iterator.foldLeft(empty)(_ ++ _)
}

val Pure = InferredEffects.empty

// declared effects in the order of their capabilities
def canonical(effs: Effects)(using C: Context): List[InterfaceType] = {
  val known = C.unification(effs).toList
  assertOrderable(known)
  known.sorted(using CanonicalOrdering)
}

/**
 * Asserts that an effect is _concrete_ (i.e., has no unification variables):
 * this is needed only where an interface occurs more than once in a set,
 * where its entries are told apart by their type arguments (see [[TypeComparer.unifyEffects]]).
 */
private[typer] def assertConcreteEffect(eff: InterfaceType)(using C: Context): Unit =
  unknowns(eff) match {
    case us if us.nonEmpty =>
      C.abort(pretty"Effects need to be fully known, but effect ${eff}'s type parameter(s) ${us.mkString(", ")} could not be inferred.") {
        help(pretty"Maybe try annotating them?")
      }
    case _ => ()
  }

private[typer] def assertConcreteFunction(id: source.Id, tpe: BlockType)(using C: Context): Unit =
  unknowns(tpe) match {
    case us if us.nonEmpty =>
      C.abort(pretty"Cannot fully infer type for ${id}: ${tpe}")
    case _ => ()
  }

private[typer] type Unknown = CaptUnificationVar | UnificationVar

private[typer] def unknowns(tpe: ValueType): Set[Unknown] = tpe match {
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
