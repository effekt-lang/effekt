package effekt
package typer

import effekt.symbols._
import effekt.symbols.builtins.{ TTop, TBottom }

/**
 * Given the current unification state, can we decide whether one type is a subtype of another?
 *
 * Does not influence the constraint graph.
 */
trait TypeComparer {

  // The following comparisons cannot be decided purely by inspecting the type structure
  // They require constraint information.
  def hasUpperBound(x: UnificationVar, y: ValueType): Boolean
  def hasLowerBound(x: UnificationVar, y: ValueType): Boolean
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean
  def isSubset(c1: Captures, c2: Captures): Boolean

  // TODO instead of a boolean, we could also return the explanation of the difference.
  def isEqual(tpe1: ValueType, tpe2: ValueType): Boolean = subValueType(tpe1, tpe2) && subValueType(tpe2, tpe1)

  def subValueType(tpe1: ValueType, tpe2: ValueType): Boolean = (tpe1, tpe2) match {
    case (t, s) if t == s => true
    case (_, TTop) => true
    case (TBottom, _) => true

    // Here we cannot decide whether a unification variable is a subtype; that can only
    // be done in the unifier.
    case (s: UnificationVar, t: ValueType) => hasUpperBound(s, t)
    case (s: ValueType, t: UnificationVar) => hasLowerBound(t, s)

    // For now, we treat all type constructors as invariant.
    case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2)) =>
      def sameSize = args1.size == args2.size
      def sameConstructor = isEqual(t1, t2)
      def sameArguments = (args1 zip args2) forall { case (t1, t2) => isEqual(t1, t2) }
      sameSize && sameConstructor && sameArguments

    case (BoxedType(tpe1, capt1), BoxedType(tpe2, capt2)) =>
      subBlockType(tpe1, tpe2) && isSubset(capt1, capt2)

    case (t, s) => false
  }

  def subBlockType(tpe1: BlockType, tpe2: BlockType): Boolean = (tpe1, tpe2) match {
    case (t: FunctionType, s: FunctionType) => subFunctionType(t, s)
    case (t: InterfaceType, s: InterfaceType) => subInterfaceType(t, s)
    case (t, s) => false
  }

  def subInterfaceType(tpe1: InterfaceType, tpe2: InterfaceType): Boolean = (tpe1, tpe2) match {
    case (t1: Interface, t2: Interface) => t1 == t2
    // for now block type constructors are invariant
    case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
      def sameInterfaces = subInterfaceType(c1, c2)
      def sameArgs = (targs1 zip targs2) forall { case (t1, t2) => isEqual(t1, t2) }
      sameInterfaces && sameArgs
    case _ => false
  }

  def subFunctionType(tpe1: FunctionType, tpe2: FunctionType): Boolean = (tpe1, tpe2) match {
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)
    ) =>

      if (tparams1.size != tparams2.size) return false
      if (vparams1.size != vparams2.size) return false
      if (bparams1.size != bparams2.size) return false
      if (cparams1.size != cparams2.size) return false

      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))

      def substVparams2 = vparams2 map subst.substitute
      def substBparams2 = bparams2 map subst.substitute
      def substRet2 = subst.substitute(ret2)
      def substEff2 = subst.substitute(eff2)

      def valuesOk = (vparams1 zip substVparams2) forall { case (t1, t2) => subValueType(t2, t1) }
      def blocksOk = (bparams1 zip substBparams2) forall { case (t1, t2) => subBlockType(t2, t1) }
      def returnOk = subValueType(ret1, substRet2)

      // We compare effects to be equal, since we do not have subtyping on effects
      def effectsOk = subEffects(eff1, substEff2) && subEffects(substEff2, eff1)

      valuesOk && blocksOk && returnOk && effectsOk
  }

  def subEffect(eff1: Effect, eff2: Effect): Boolean = (eff1, eff2) match {
    case (e1, e2) if e1 == e2 => true
    case (BlockTypeApp(cons1, args1), BlockTypeApp(cons2, args2)) =>
      def sameConstructor = cons1 == cons2
      // we treat effect constructors as invariant, for now.
      def sameArgs = (args1 zip args2) forall { case (t1, t2) => isEqual(t1, t2) }
      sameConstructor && sameArgs
    case _ => false
  }
  def subEffects(eff1: Effects, eff2: Effects): Boolean =
    eff1.forall(e1 => eff2.exists(e2 => subEffect(e1, e2)))
}

/**
 * A side effecting type comparer
 *
 * TODO the comparer should build up a "deconstruction trace" that can be used for better type errors.
 */
trait TypeUnifier extends TypeComparer {
  // "unification effects"
  def requireLowerBound(x: UnificationVar, tpe: ValueType): Unit
  def requireUpperBound(x: UnificationVar, tpe: ValueType): Unit
  def requireEqual(x: UnificationVar, tpe: ValueType): Unit

  def requireSubregion(lower: Captures, upper: Captures): Unit

  def abort(msg: String): Nothing
  def error(msg: String): Unit

  def unify(c1: Captures, c2: Captures)(using p: Polarity): Unit = p match {
    case Covariant => requireSubregion(c1, c2)
    case Contravariant => requireSubregion(c2, c1)
    case Invariant => requireSubregion(c1, c2); requireSubregion(c2, c1)
  }
  def unify(c1: Capture, c2: Capture)(using Polarity): Unit = unify(CaptureSet(Set(c1)), CaptureSet(Set(c2)))

  def unifyValueTypes(tpe1: ValueType, tpe2: ValueType)(using p: Polarity): Unit = (tpe1, tpe2, p) match {
    case (t, s, _) if t == s => ()
    // for now, we simply flip polarity and only cover covariant and invariant cases
    case (t, s, Contravariant) => unifyValueTypes(s, t)(using Covariant)
    case (_, TTop, Covariant) => ()
    case (TBottom, _, Covariant) => ()

    case (s: UnificationVar, t: ValueType, Covariant) => requireUpperBound(s, t)
    case (s: ValueType, t: UnificationVar, Covariant) => requireLowerBound(t, s)

    case (s: UnificationVar, t: ValueType, Invariant) => requireEqual(s, t)
    case (s: ValueType, t: UnificationVar, Invariant) => requireEqual(t, s)

    // For now, we treat all type constructors as invariant.
    case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2), _) =>
      if (args1.size != args2.size)
        abort(s"Argument count does not match $t1 vs. $t2")

      unifyValueTypes(t1, t2)(using Invariant)

      // TODO here we assume that the type constructor is covariant
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }

    case (BoxedType(tpe1, capt1), BoxedType(tpe2, capt2), p) =>
      unifyBlockTypes(tpe1, tpe2)
      unify(capt1, capt2)

    case (t: TypeAlias, _, p) => ???

    case (_, t: TypeAlias, p) => ???

    case (t, s, p) =>
      error(s"Expected ${t}, but got ${s}")
  }

  def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType)(using p: Polarity): Unit = (tpe1, tpe2) match {
    case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s)
    case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s)
    case (t, s) => error(s"Expected ${t}, but got ${s}")
  }

  def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType)(using p: Polarity): Unit = (tpe1, tpe2) match {
    case (t1: Interface, t2: Interface) => if (t1 != t2) error(s"Expected ${t1}, but got ${t2}")
    // for now block type constructors are invariant
    case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
      unifyInterfaceTypes(c1, c2)(using Invariant)
      (targs1 zip targs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }
    case _ => error(s"Kind mismatch between ${tpe1} and ${tpe2}")
  }

  def unifyEffect(eff1: Effect, eff2: Effect)(using p: Polarity): Unit = (eff1, eff2) match {
    case (e1, e2) if e1 == e2 => ()
    case (BlockTypeApp(cons1, args1), BlockTypeApp(cons2, args2)) if cons1 == cons2 =>
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }
    case _ => error(s"Mismatch between ${eff1} and ${eff2}")
  }

  def unifyEffects(eff1: Effects, eff2: Effects)(using p: Polarity): Unit = p match {
    case Covariant => if (!subEffects(eff1, eff2)) error(s"${eff1} does not subsume ${eff2}")
    case Contravariant => if (!subEffects(eff2, eff1)) error(s"${eff2} does not subsume ${eff1}")
    case Invariant => if (!(subEffects(eff1, eff2) && subEffects(eff2, eff1))) error(s"${eff2} is not equal to ${eff1}")
  }

  def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType)(using p: Polarity): Unit = (tpe1, tpe2) match {
    case (f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1), f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)) =>

      if (tparams1.size != tparams2.size)
        abort(s"Type parameter count does not match $f1 vs. $f2")

      if (vparams1.size != vparams2.size)
        abort(s"Value parameter count does not match $f1 vs. $f2")

      if (bparams1.size != bparams2.size)
        abort(s"Block parameter count does not match $f1 vs. $f2")

      if (cparams1.size != cparams2.size)
        abort(s"Capture parameter count does not match $f1 vs. $f2")

      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))

      val (substVparams2, substBparams2, substRet2) = (vparams2 map subst.substitute, bparams2 map subst.substitute, subst.substitute(ret2))

      (vparams1 zip substVparams2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using p.flip) }
      (bparams1 zip substBparams2) foreach { case (t1, t2) => unifyBlockTypes(t1, t2)(using p.flip) }
      unifyValueTypes(ret1, substRet2)
      // We compare effects to be equal, since we do not have subtyping on effects
      unifyEffects(eff1, eff2)(using Invariant)
  }

  // There are only a few users of dealiasing:
  //  1) checking for effect inclusion (`contains` in Effects)
  //  2) checking exhaustivity of pattern matching
  //  3) type comparer itself
  def dealias(tpe: ValueType): ValueType = tpe match {
    case BoxedType(tpe, capture) => BoxedType(dealias(tpe), capture)
    case ValueTypeApp(TypeAlias(name, tparams, tpe), args) =>
      val subst = (tparams zip args).toMap
      dealias(subst.substitute(tpe))
    case ValueTypeApp(cons, args) => ValueTypeApp(cons, args map dealias)
    case TypeAlias(name, tparams, tpe) => dealias(tpe)
    case tpe => tpe
  }
  // TODO implement
  def dealias(tpe: BlockType): BlockType = tpe

  // TODO implement fully
  def dealias(effs: Effects): Effects = Effects(effs.toList.flatMap(dealias))
  def dealias(eff: Effect): List[Effect] = eff match {
    case EffectAlias(name, Nil, effs) => effs.toList.flatMap(dealias)
//    case  BlockTypeApp(EffectAlias(name, tparams, effs), args) =>
//      val subst = (tparams zip args).toMap
//      dealias(subst.substitute(effs)).toList
    case e => List(e)
  }
}


/**
 * Merges two types by traversing them structurally.
 *
 * Is side effecting in that it influences the constraint graph.
 *
 * effect types need to be concrete to be mergable.
 */
trait TypeMerger extends TypeUnifier {

  /**
   * Merging captures requires introducing new unification variables and is thus deferred to [[Unification.mergeCaptures]]
   */
  def mergeCaptures(oldBound: Captures, newBound: Captures, polarity: Polarity): Captures

  // computes union or intersection, based on polarity
  def mergeValueTypes(oldBound: ValueType, newBound: ValueType, polarity: Polarity): ValueType =
    (oldBound, newBound, polarity) match {
      case (t, s, _) if t == s => t
      case (TBottom, t, Covariant) => t
      case (t, TBottom, Covariant) => t
      case (TTop, t, Contravariant) => t
      case (t, TTop, Contravariant) => t

      // reuses the type unifier implementation (will potentially register new constraints)
      case (x: UnificationVar, tpe: ValueType, p) => unifyValueTypes(x, tpe)(using p); x
      case (tpe: ValueType, x: UnificationVar, p) => unifyValueTypes(tpe, x)(using p); x

      // We can use one of them if it is more specific than the other.
      case (tpe1, tpe2, Covariant) if subValueType(tpe1, tpe2) => tpe2
      case (tpe1, tpe2, Contravariant) if subValueType(tpe1, tpe2) => tpe1
      case (tpe1, tpe2, Covariant) if subValueType(tpe2, tpe1) => tpe1
      case (tpe1, tpe2, Contravariant) if subValueType(tpe2, tpe1) => tpe2

      case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2), _) =>
        if (cons1 != cons2) abort(s"Cannot merge different constructors")
        if (args1.size != args2.size) abort(s"Different count of argument to type constructor")

        // TODO Here we assume the constructor is invariant
        val mergedArgs = (args1 zip args2).map { case (t1, t2) =>
          mergeValueTypes(t1, t2, Invariant)
        }
        ValueTypeApp(cons1, mergedArgs)

      case (BoxedType(tpe1, capt1), BoxedType(tpe2, capt2), p) =>
        BoxedType(mergeBlockTypes(tpe1, tpe2, p), mergeCaptures(capt1, capt2, p))

      case _ =>
        abort(s"Cannot merge ${oldBound} with ${newBound} at ${polarity} polarity")
    }

  def mergeBlockTypes(oldBound: BlockType, newBound: BlockType, polarity: Polarity): BlockType = (oldBound, newBound) match {
    case (t: FunctionType, s: FunctionType) => mergeFunctionTypes(t, s, polarity)
    case (t: InterfaceType, s: InterfaceType) => mergeInterfaceTypes(t, s, polarity)
    case (t, s) => abort(s"The two types ${t} and ${s} are not compatible")
  }

  def mergeInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType, polarity: Polarity): InterfaceType = (tpe1, tpe2) match {
    case (t1: Interface, t2: Interface) =>
      if (t1 != t2) abort(s"The two types ${t1} and ${t2} are not compatible")
      else t1
    // for now block type constructors are invariant
    case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
      unifyInterfaceTypes(c1, c2)(using Invariant)
      val mergedArgs = (targs1 zip targs2) map { case (t1, t2) => mergeValueTypes(t1, t2, Invariant) }
      BlockTypeApp(c1, mergedArgs)
    case _ => abort(s"Kind mismatch between ${tpe1} and ${tpe2}")
  }

  def mergeFunctionTypes(tpe1: FunctionType, tpe2: FunctionType, polarity: Polarity): FunctionType = (tpe1, tpe2) match {
    case (f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1), f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)) =>

      if (tparams1.size != tparams2.size)
        abort(s"Type parameter count does not match $f1 vs. $f2")

      if (vparams1.size != vparams2.size)
        abort(s"Value parameter count does not match $f1 vs. $f2")

      if (bparams1.size != bparams2.size)
        abort(s"Block parameter count does not match $f1 vs. $f2")

      if (cparams1.size != cparams2.size)
        abort(s"Capture parameter count does not match $f1 vs. $f2")

      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))

      val (substVparams2, substBparams2, substRet2) = (vparams2 map subst.substitute, bparams2 map subst.substitute, subst.substitute(ret2))

      val mergedVps = (vparams1 zip substVparams2) map { case (t1, t2) => mergeValueTypes(t1, t2, polarity.flip) }
      val mergedBps = (bparams1 zip substBparams2) map { case (t1, t2) => mergeBlockTypes(t1, t2, polarity.flip) }
      val mergedRet = mergeValueTypes(ret1, substRet2, polarity)

      // We compare effects to be equal, since we do not have subtyping on effects
      unifyEffects(eff1, eff2)(using Invariant)
      FunctionType(tparams1, cparams1, mergedVps, mergedBps, mergedRet, eff1)
  }

  /**
   * Compute the join of two types
   */
  def mergeLower(oldBound: ValueType, newBound: ValueType): ValueType =
    mergeValueTypes(oldBound, newBound, Covariant)

  /**
   * Compute the meet of two types
   */
  def mergeUpper(oldBound: ValueType, newBound: ValueType): ValueType =
    mergeValueTypes(oldBound, newBound, Contravariant)
}