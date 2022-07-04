package effekt
package typer

import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.typer.ErrorContext.FunctionEffects

/**
 * A side effecting type comparer
 *
 * TODO the comparer should build up a "deconstruction trace" that can be used for better type errors.
 */
trait TypeUnifier {
  // "unification effects"
  def requireLowerBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit
  def requireUpperBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit
  def requireEqual(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit

  def requireSubregion(lower: Captures, upper: Captures, ctx: ErrorContext): Unit

  def abort(msg: String, ctx: ErrorContext): Nothing
  def error(msg: String, ctx: ErrorContext): Unit
  def error(left: Type, right: Type, ctx: ErrorContext): Unit

  def unify(c1: Captures, c2: Captures, ctx: ErrorContext): Unit = ctx.polarity match {
    case Covariant     => requireSubregion(c1, c2, ctx)
    case Contravariant => requireSubregion(c2, c1, ctx)
    case Invariant     => requireSubregion(c1, c2, ctx); requireSubregion(c2, c1, ctx)
  }
  def unify(c1: Capture, c2: Capture, ctx: ErrorContext): Unit = unify(CaptureSet(Set(c1)), CaptureSet(Set(c2)), ctx)

  def unifyValueTypes(tpe1: ValueType, tpe2: ValueType, ctx: ErrorContext): Unit = (tpe1, tpe2, ctx.polarity) match {
    case (t, s, _) if t == s => ()

    case (_, TTop, Covariant) => ()
    case (TBottom, _, Covariant) => ()

    case (TTop, _, Contravariant) => ()
    case (_, TBottom, Contravariant) => ()

    case (s: UnificationVar, t: ValueType, Covariant) => requireUpperBound(s, t, ctx)
    case (s: ValueType, t: UnificationVar, Covariant) => requireLowerBound(t, s, ctx)

    case (s: UnificationVar, t: ValueType, Contravariant) => requireLowerBound(s, t, ctx)
    case (s: ValueType, t: UnificationVar, Contravariant) => requireUpperBound(t, s, ctx)

    case (s: UnificationVar, t: ValueType, Invariant) => requireEqual(s, t, ctx)
    case (s: ValueType, t: UnificationVar, Invariant) => requireEqual(t, s, ctx)

    // For now, we treat all type constructors as invariant.
    case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2), _) =>

      unifyValueTypes(t1, t2, ErrorContext.TypeConstructor(ctx))

      if (args1.size != args2.size)
        abort(pp"Argument count does not match $t1 vs. $t2", ctx) // TODO add to context

      // TODO here we assume that the type constructor is covariant
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }

    case (t @ BoxedType(tpe1, capt1), s @ BoxedType(tpe2, capt2), p) =>
      unifyBlockTypes(tpe1, tpe2, ErrorContext.BoxedTypeBlock(t, s, ctx))
      unify(capt1, capt2, ErrorContext.BoxedTypeCapture(t, s, ctx))

    case (t, s, p) =>
      error(t, s, ctx)
  }

  def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
    case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s, ctx)
    case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s, ctx)
    case (t, s) => error(t, s, ctx)
  }

  def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
    case (t1: Interface, t2: Interface) => if (t1 != t2) error(t1, t2, ctx)
    // for now block type constructors are invariant
    case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
      unifyInterfaceTypes(c1, c2, ErrorContext.TypeConstructor(ctx))
      (targs1 zip targs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }
    case _ => error(pp"Kind mismatch between ${tpe1} and ${tpe2}", ctx)
  }

  def unifyEffect(eff1: InterfaceType, eff2: InterfaceType, ctx: ErrorContext): Unit = (eff1, eff2) match {
    case (e1, e2) if e1 == e2 => ()
    case (BlockTypeApp(cons1, args1), BlockTypeApp(cons2, args2)) if cons1 == cons2 =>
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }
    case _ => error(eff1, eff2, ctx)
  }

  def unifyEffects(eff1: Effects, eff2: Effects, ctx: ErrorContext): Unit =
    if (eff1.toList.toSet != eff2.toList.toSet) error(pp"${eff2} is not equal to ${eff1}", ctx)

  def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)) =>

      if (tparams1.size != tparams2.size)
        abort(pp"Type parameter count does not match $f1 vs. $f2", ctx)

      if (vparams1.size != vparams2.size)
        abort(pp"Value parameter count does not match $f1 vs. $f2", ctx)

      if (bparams1.size != bparams2.size)
        abort(pp"Block parameter count does not match $f1 vs. $f2", ctx)

      if (cparams1.size != cparams2.size)
        abort(pp"Capture parameter count does not match $f1 vs. $f2", ctx)

      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))
      val substVParams2 = vparams2 map subst.substitute
      val substBParams2 = bparams2 map subst.substitute
      val substRet2 = subst.substitute(ret2)
      val substEffs2 = subst.substitute(eff2)

      (vparams1 zip substVParams2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.FunctionArgument(f1, f2, ctx)) }
      (bparams1 zip substBParams2) foreach { case (t1, t2) => unifyBlockTypes(t1, t2, ErrorContext.FunctionArgument(f1, f2, ctx)) }

      unifyValueTypes(ret1, substRet2, ErrorContext.FunctionReturn(ctx))

      // We compare effects to be equal, since we do not have subtyping on effects
      // TODO verify that a different ordering doesn't interact badly with capture polymorphism
      //     i.e. () => (T at {@Exc}) / {Exc, Amb} vs () => (T at {@Exc}) / {Amb, Exc}
      //   this should be ruled out by canonical ordering.
      unifyEffects(eff1, substEffs2, ErrorContext.FunctionEffects(ctx))
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
  def mergeCaptures(oldBound: Captures, newBound: Captures, ctx: ErrorContext): Captures

  // computes union or intersection, based on polarity
  def mergeValueTypes(oldBound: ValueType, newBound: ValueType, ctx: ErrorContext): ValueType =
    (oldBound, newBound, ctx.polarity) match {
      case (t, s, _) if t == s      => t
      case (TBottom, t, Covariant)  => t
      case (t, TBottom, Covariant)  => t
      case (TTop, t, Contravariant) => t
      case (t, TTop, Contravariant) => t

      // reuses the type unifier implementation (will potentially register new constraints)
      // TODO we need to create a fresh unification variable here!
      case (x: UnificationVar, tpe: ValueType, p) =>
        unifyValueTypes(x, tpe, ctx); x
      case (tpe: ValueType, x: UnificationVar, p) =>
        unifyValueTypes(tpe, x, ctx); x

      case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2), _) =>
        if (cons1 != cons2) abort(pp"Cannot merge different constructors: $cons1 vs. $cons2", ErrorContext.TypeConstructor(ctx))
        if (args1.size != args2.size) abort(pp"Different count of argument to type constructor: $oldBound vs $newBound", ctx)

        // TODO Here we assume the constructor is invariant
        val mergedArgs = (args1 zip args2).map {
          case (t1, t2) =>
            mergeValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx))
        }
        ValueTypeApp(cons1, mergedArgs)

      case (s @ BoxedType(tpe1, capt1), t @ BoxedType(tpe2, capt2), p) =>
        BoxedType(
          mergeBlockTypes(tpe1, tpe2, ErrorContext.BoxedTypeBlock(s, t, ctx)),
          mergeCaptures(capt1, capt2, ErrorContext.BoxedTypeCapture(s, t, ctx))
        )

      case _ =>
        abort(pp"Cannot merge ${oldBound} with ${newBound}", ctx)
    }

  def mergeBlockTypes(oldBound: BlockType, newBound: BlockType, ctx: ErrorContext): BlockType = (oldBound, newBound) match {
    case (t: FunctionType, s: FunctionType) => mergeFunctionTypes(t, s, ctx)
    case (t: InterfaceType, s: InterfaceType) => mergeInterfaceTypes(t, s, ctx)
    case (t, s) => abort(pp"The two types ${t} and ${s} are not compatible", ctx)
  }

  def mergeInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType, ctx: ErrorContext): InterfaceType = (tpe1, tpe2) match {
    case (t1: Interface, t2: Interface) =>
      if (t1 != t2) abort(pp"The two types ${t1} and ${t2} are not compatible", ctx)
      else t1
    // for now block type constructors are invariant
    case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
      unifyInterfaceTypes(c1, c2, ErrorContext.TypeConstructor(ctx))
      val mergedArgs = (targs1 zip targs2) map { case (t1, t2) => mergeValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }
      BlockTypeApp(c1, mergedArgs)
    case _ => abort(pp"Kind mismatch between ${tpe1} and ${tpe2}", ctx)
  }

  def mergeFunctionTypes(tpe1: FunctionType, tpe2: FunctionType, ctx: ErrorContext): FunctionType = (tpe1, tpe2) match {
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)
      ) =>

      if (tparams1.size != tparams2.size)
        abort(pp"Type parameter count does not match $f1 vs. $f2", ctx)

      if (vparams1.size != vparams2.size)
        abort(pp"Value parameter count does not match $f1 vs. $f2", ctx)

      if (bparams1.size != bparams2.size)
        abort(pp"Block parameter count does not match $f1 vs. $f2", ctx)

      if (cparams1.size != cparams2.size)
        abort(pp"Capture parameter count does not match $f1 vs. $f2", ctx)

      // TODO potentially share code with unifyFunctionTypes and instantiate
      val subst = Substitutions(tparams2 zip tparams1, cparams2 zip cparams1.map(c => CaptureSet(c)))
      val substVParams2 = vparams2 map subst.substitute
      val substBParams2 = bparams2 map subst.substitute
      val substRet2 = subst.substitute(ret2)
      val substEffs2 = subst.substitute(eff2)

      val mergedVps = (vparams1 zip substVParams2) map { case (t1, t2) => mergeValueTypes(t1, t2, ErrorContext.FunctionArgument(f1, f2, ctx)) }
      val mergedBps = (bparams1 zip substBParams2) map { case (t1, t2) => mergeBlockTypes(t1, t2, ErrorContext.FunctionArgument(f1, f2, ctx)) }
      val mergedRet = mergeValueTypes(ret1, substRet2, ErrorContext.FunctionReturn(ctx))

      // We compare effects to be equal, since we do not have subtyping on effects
      unifyEffects(eff1, substEffs2, ErrorContext.FunctionEffects(ctx))

      FunctionType(tparams1, cparams1, mergedVps, mergedBps, mergedRet, eff1)
  }
}
