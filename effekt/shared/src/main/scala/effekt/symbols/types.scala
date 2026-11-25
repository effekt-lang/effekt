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

  lazy val toList: List[InterfaceType] = distinctTypes(effects)

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: InterfaceType => Boolean): Effects =
    Effects(effects.filterNot(p))

  def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
  def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)
  def ++(other: Effects): Effects = Effects(this.effects ++ other.effects)

  def size: Int = effects.size

  def distinct: Effects = Effects(distinctTypes(effects))
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
    case (_: UnificationVar, _) | (_, _: UnificationVar) =>
      effekt.util.messages.INTERNAL_ERROR("Cannot compute canonical ordering, since type still contains unification variables.")
    // binders compared by position to prevent issues like #727
    case (f1: FunctionType, f2: FunctionType) if (f1.tparams, f1.cparams) != (f2.tparams, f2.cparams) &&
        f1.tparams.size == f2.tparams.size && f1.cparams.size == f2.cparams.size =>
      compareStructural(f1, renamed(f1, f2))
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

  def apply(l: List[InterfaceType]): List[InterfaceType] = l.sorted(using this)

  def apply(f: FunctionType): FunctionType = f match {
    case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      val (cparamsBlocks, cparamsEffects) = cparams.splitAt(bparams.size)
      val (cparams2, effects2) = (cparamsEffects zip effects.toList).sortBy(_._2)(using this).unzip
      FunctionType(tparams, cparamsBlocks ++ cparams2, vparams, bparams, result, Effects(effects2))
  }
}

/**
 * Equal up to binder renaming: each written function type has its own type and capture parameters,
 * where `==` would compare them by identity.
 */
def sameType(tpe1: InterfaceType, tpe2: InterfaceType): Boolean = SameType.equal(tpe1, tpe2)

// the first of each group of types that are the same as per [[sameType]]
def distinctTypes(tpes: List[InterfaceType]): List[InterfaceType] =
  tpes.foldLeft(List.empty[InterfaceType]) { (kept, t) => if (kept.exists(sameType(_, t))) kept else kept :+ t }

private object SameType {
  def equal(t1: ValueType, t2: ValueType): Boolean = (t1, t2) match {
    case (ValueTypeApp(c1, args1), ValueTypeApp(c2, args2)) => c1 == c2 && all(args1, args2)(equal)
    case (BoxedType(b1, c1), BoxedType(b2, c2)) => equal(b1, b2) && c1 == c2
    case _ => t1 == t2
  }

  def equal(t1: BlockType, t2: BlockType): Boolean = (t1, t2) match {
    case (InterfaceType(c1, args1), InterfaceType(c2, args2)) => c1 == c2 && all(args1, args2)(equal)
    case (f1 @ FunctionType(tps1, cps1, vps1, bps1, ret1, effs1), f2 @ FunctionType(tps2, cps2, _, _, _, _)) =>
      tps1.size == tps2.size && cps1.size == cps2.size && {
        val FunctionType(_, _, vps2, bps2, ret2, effs2) = renamed(f1, f2)
        all(vps1, vps2)(equal) && all(bps1, bps2)(equal) && equal(ret1, ret2) && sameSet(effs1.toList, effs2.toList)
      }
    case _ => false
  }

  // effects are a set (like in `unifyEffects`)
  private def sameSet(effs1: List[InterfaceType], effs2: List[InterfaceType]): Boolean =
    effs1.size == effs2.size && effs1.forall { e1 => effs2.exists(equal(e1, _)) }

  private def all[A](as: List[A], bs: List[A])(p: (A, A) => Boolean): Boolean =
    as.size == bs.size && (as zip bs).forall(p.tupled)
}

// `f2` with its binders renamed to those of `f1`, like in [[typer.TypeUnifier.unifyFunctionTypes]]
private def renamed(f1: FunctionType, f2: FunctionType): FunctionType = {
  val subst = effekt.typer.Substitutions(f2.tparams zip f1.tparams.map(ValueTypeRef(_)), f2.cparams zip f1.cparams.map(CaptureSet(_)))
  FunctionType(f1.tparams, f1.cparams, f2.vparams.map(subst.substitute), f2.bparams.map(subst.substitute),
    subst.substitute(f2.result), Effects(f2.effects.effects.map(subst.substitute)))
}
