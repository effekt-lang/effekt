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

  case BlockTypeRef(bvar: BlockTypeVar)
}
export BlockType.*

extension (i: BlockType.InterfaceType) {
  def name: Name = i.typeConstructor.name
}

/**
 * Represents effect sets on function types.
 *
 * All effects are dealiased by namer. Effects are inferred via [[typer.ConcreteEffects]] so
 * by construction all entries in the set of effects here should be concrete (no unification variables).
 *
 * Effect sets are themselves *not* symbols, they are just aggregates.
 *
 * We do not enforce entries to be distinct. This way we can substitute types and keep duplicate entries.
 * For instances { State[S], State[T] }[S -> Int, T -> Int] then becomes { State[Int], State[Int] }.
 * This is important since we need to pass two capabilities in this case.
 *
 * Member [[canonical]] computes the canonical ordering of capabilities for this set of effects.
 * Disjointness needs to be ensured manually when constructing effect sets (for instance via [[typer.ConcreteEffects]]).
 */
case class Effects(effects: List[BlockType.InterfaceType]) {

  lazy val toList: List[InterfaceType] = effects.distinct

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: InterfaceType => Boolean): Effects =
    Effects(effects.filterNot(p))

  def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
  def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)

  lazy val canonical: List[InterfaceType] = effects.sorted(using CanonicalOrdering)

  def distinct: Effects = Effects(effects.distinct)
}
object Effects {

  def apply(effs: InterfaceType*): Effects =
    new Effects(effs.toList)

  def apply(effs: Iterable[InterfaceType]): Effects =
    new Effects(effs.toList)

  def empty: Effects = new Effects(Nil)
  val Pure = empty
}

/**
 * The canonical ordering needs to be stable, but should also distinguish two types,
 * if they are different.
 *
 * Bugs with the canonical ordering can lead to runtime errors as observed in ticket #108
 */
object CanonicalOrdering extends Ordering[InterfaceType] {
  def compare(tpe1: InterfaceType, tpe2: InterfaceType): Int = compareStructural(tpe1, tpe2)

  def compareStructural(tpe1: Any, tpe2: Any): Int = (tpe1, tpe2) match {
    case (sym1: Symbol, sym2: Symbol) =>
      sym1.id - sym2.id
    case (p1: Product, p2: Product) if p1.getClass == p2.getClass =>
      (p1.productIterator zip p2.productIterator).collectFirst {
        case (child1, child2) if compareStructural(child1, child2) != 0 => compareStructural(child1, child2)
      }.getOrElse(fallback(tpe1, tpe2))
    case _ =>
      fallback(tpe1, tpe2)
  }

  def fallback(tpe1: Any, tpe2: Any): Int = tpe1.hashCode - tpe2.hashCode
}


