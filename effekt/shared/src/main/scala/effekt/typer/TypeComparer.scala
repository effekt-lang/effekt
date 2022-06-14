package effekt
package typer

import effekt.symbols._
import effekt.symbols.builtins.{ TTop, TBottom }


/**
 * A side effecting type comparer
 *
 * TODO the comparer should build up a "deconstruction trace" that can be used for better type errors.
 */
trait TypeUnifier {
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

  def unifyEffect(eff1: InterfaceType, eff2: InterfaceType)(using p: Polarity): Unit = (eff1, eff2) match {
    case (e1, e2) if e1 == e2 => ()
    case (BlockTypeApp(cons1, args1), BlockTypeApp(cons2, args2)) if cons1 == cons2 =>
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }
    case _ => error(s"Mismatch between ${eff1} and ${eff2}")
  }

  def unifyEffects(eff1: Effects, eff2: Effects)(using p: Polarity): Unit =
     if (eff1.toList.toSet != eff2.toList.toSet) error(s"${eff2} is not equal to ${eff1}")

  def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType)(using p: Polarity): Unit = (tpe1, tpe2) match {
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)) =>

      if (tparams1.size != tparams2.size)
        abort(s"Type parameter count does not match $f1 vs. $f2")

      if (vparams1.size != vparams2.size)
        abort(s"Value parameter count does not match $f1 vs. $f2")

      if (bparams1.size != bparams2.size)
        abort(s"Block parameter count does not match $f1 vs. $f2")

      if (cparams1.size != cparams2.size)
        abort(s"Capture parameter count does not match $f1 vs. $f2")

      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))
      val substVParams2 = vparams2 map subst.substitute
      val substBParams2 = bparams2 map subst.substitute
      val substRet2 = subst.substitute(ret2)
      val substEffs2 = subst.substitute(eff2)

      (vparams1 zip substVParams2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using p.flip) }
      (bparams1 zip substBParams2) foreach { case (t1, t2) => unifyBlockTypes(t1, t2)(using p.flip) }

      unifyValueTypes(ret1, substRet2)

      // We compare effects to be equal, since we do not have subtyping on effects
      // TODO verify that a different ordering doesn't interact badly with capture polymorphism
      //     i.e. () => (T at {@Exc}) / {Exc, Amb} vs () => (T at {@Exc}) / {Amb, Exc}
      //   this should be ruled out by canonical ordering.
      unifyEffects(eff1, substEffs2)(using Invariant)
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
      // TODO we need to create a fresh unification variable here!
      case (x: UnificationVar, tpe: ValueType, p) => unifyValueTypes(x, tpe)(using p); x
      case (tpe: ValueType, x: UnificationVar, p) => unifyValueTypes(tpe, x)(using p); x

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
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)
    ) =>

      if (tparams1.size != tparams2.size)
        abort(s"Type parameter count does not match $f1 vs. $f2")

      if (vparams1.size != vparams2.size)
        abort(s"Value parameter count does not match $f1 vs. $f2")

      if (bparams1.size != bparams2.size)
        abort(s"Block parameter count does not match $f1 vs. $f2")

      if (cparams1.size != cparams2.size)
        abort(s"Capture parameter count does not match $f1 vs. $f2")


      // TODO potentially share code with unifyFunctionTypes and instantiate
      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))
      val substVParams2 = vparams2 map subst.substitute
      val substBParams2 = bparams2 map subst.substitute
      val substRet2 = subst.substitute(ret2)
      val substEffs2 = subst.substitute(eff2)

      val mergedVps = (vparams1 zip substVParams2) map { case (t1, t2) => mergeValueTypes(t1, t2, polarity.flip) }
      val mergedBps = (bparams1 zip substBParams2) map { case (t1, t2) => mergeBlockTypes(t1, t2, polarity.flip) }
      val mergedRet = mergeValueTypes(ret1, substRet2, polarity)

      // We compare effects to be equal, since we do not have subtyping on effects
      unifyEffects(eff1, substEffs2)(using Invariant)

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