package effekt
package symbols

import TypeVar.*

/**
 * Types
 */
sealed trait Type

/**
 * Value Types
 *
 * [[ValueType]]
 *   |
 *   |- [[ValueTypeRef]] references to type params
 *   |- [[ValueTypeApp]] references to type constructors
 *   |- [[BoxedType]] boxed block types
 */

enum ValueType extends Type {

  /**
   * Types of first-class functions
   */
  case BoxedType(tpe: BlockType, capture: Captures)

  /**
   * Reference to a type variable (we don't have type constructor polymorphism, so variables do not take arguments)
   */
  case ValueTypeRef(tvar: TypeVar)

  /**
   * Reference to a type constructor with optional type arguments
   */
  case ValueTypeApp(constructor: TypeConstructor, args: List[ValueType])
}
export ValueType.*

/**
 * [[BlockType]]
 *   |
 *   |- [[FunctionType]]
 *   |- [[InterfaceType]]
 *
 * Effects are a list of [[InterfaceType]]
 *
 * Outside of the hierarchy are
 *   [[EffectAlias]]
 * which are resolved by [[Namer]] to a list of [[InterfaceType]]s
 */
enum BlockType extends Type {

  case FunctionType(
    tparams: List[TypeParam],
    cparams: List[Capture],
    vparams: List[ValueType],
    bparams: List[BlockType],
    result: ValueType,
    effects: Effects
  )

  case InterfaceType(typeConstructor: BlockTypeConstructor, args: List[ValueType])
}
export BlockType.*

extension (i: BlockType.InterfaceType) {
  def name: Name = i.typeConstructor.name
}

/**
 * Represents effect _sets_ (no order, no duplicates) on function types.
 *
 * All effects are dealiased by namer. Each effect occurs at most once: entries are identified by their
 * interface, the type arguments of an effect are determined by unification.
 *
 * Effect sets are themselves *not* symbols, they are just aggregates.
 *
 * Member [[canonical]] computes the canonical ordering of capabilities for this set of effects.
 */
case class Effects(effects: List[BlockType.InterfaceType]) {

  lazy val toList: List[InterfaceType] = effects.distinctBy(_.typeConstructor)

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: InterfaceType => Boolean): Effects =
    Effects(effects.filterNot(p))

  def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
  def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)
  def ++(other: Effects): Effects = Effects(this.effects ++ other.effects)
  def --(other: Effects): Effects =
    filterNot { eff => other.exists { removed => removed.typeConstructor == eff.typeConstructor } }

  def size: Int = effects.size

  def distinct: Effects = Effects(toList)

  def canonical: List[InterfaceType] = CanonicalOrdering(toList)
}
object Effects {

  def apply(effs: InterfaceType*): Effects =
    new Effects(effs.toList)

  def apply(effs: Iterable[InterfaceType]): Effects =
    new Effects(effs.toList)

  def empty: Effects = new Effects(Nil)
  val Pure = empty

  def union(effs: IterableOnce[Effects]): Effects =
    new Effects(effs.iterator.flatMap { e => e.effects }.toList)
}

/**
 * The canonical ordering needs to be stable, but should also distinguish two types,
 * if they are different.
 *
 * Bugs with the canonical ordering can lead to runtime errors as observed in ticket #108
 */
object CanonicalOrdering extends Ordering[InterfaceType] {
  def compare(tpe1: InterfaceType, tpe2: InterfaceType): Int =
    tpe1.typeConstructor.id - tpe2.typeConstructor.id

  def apply(l: List[InterfaceType]): List[InterfaceType] = l.sorted(using this)

  def apply(f: FunctionType): FunctionType = f match {
    case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      val (cparamsBlocks, cparamsEffects) = cparams.splitAt(bparams.size)
      val (cparams2, effects2) = (cparamsEffects zip effects.toList).sortBy(_._2)(using this).unzip
      FunctionType(tparams, cparamsBlocks ++ cparams2, vparams, bparams, result, Effects(effects2))
  }
}


