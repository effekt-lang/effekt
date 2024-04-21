package effekt
package symbols

import TypeVar.*

/**
 * Types
 */
sealed trait Type


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

case class Effects(effects: List[BlockType.InterfaceType]) {

  lazy val toList: List[InterfaceType] = effects.distinct

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: InterfaceType => Boolean): Effects =
    Effects(effects.filterNot(p))

  def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
  def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)

  lazy val canonical: List[InterfaceType] = ???

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
