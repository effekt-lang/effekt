package effekt
package typer

import effekt.symbols.*
import effekt.symbols.BlockTypeVar.{BlockTypeWildcard, BlockUnificationVar}
import effekt.symbols.EffectVar.{EffectUnificationVar, EffectSetWildcard}
import effekt.symbols.ValueTypeVar.ValueTypeWildcard
import effekt.symbols.builtins.{TBottom, TTop}
import effekt.typer.ErrorContext.FunctionEffects

/**
 * A side effecting type comparer
 *
 * TODO the comparer should build up a "deconstruction trace" that can be used for better type errors.
 */
trait TypeUnifier {
  // "unification effects"
  def requireLowerBound(x: ValueUnificationVar, tpe: ValueType, ctx: ErrorContext): Unit
  def requireLowerBound(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext): Unit
  def requireLowerBound(x: EffectUnificationVar, tpe: EffectsOrRef, ctx: ErrorContext): Unit
  def requireUpperBound(x: ValueUnificationVar, tpe: ValueType, ctx: ErrorContext): Unit
  def requireUpperBound(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext): Unit
  def requireUpperBound(x: EffectUnificationVar, tpe: EffectsOrRef, ctx: ErrorContext): Unit
  def requireEqual(x: ValueUnificationVar, tpe: ValueType, ctx: ErrorContext): Unit
  def requireEqual(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext): Unit
  def requireEqual(x: EffectUnificationVar, effs: EffectsOrRef, ctx: ErrorContext) : Unit

  def requireSubregion(lower: Captures, upper: Captures, ctx: ErrorContext): Unit

  def abort(msg: String, ctx: ErrorContext): Nothing
  def error(msg: String, ctx: ErrorContext): Unit
  def error(left: Type, right: Type, ctx: ErrorContext): Unit

  def unificationVarFromWildcard(w : ValueTypeWildcard) : ValueUnificationVar
  def unificationVarFromWildcard(w : BlockTypeWildcard) : BlockUnificationVar
  def unificationVarFromWildcard(w : EffectSetWildcard) : EffectUnificationVar

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

    case (t: ValueType, ValueTypeRef(w: ValueTypeWildcard), _) =>
      val unificationVar: ValueUnificationVar = unificationVarFromWildcard(w)
      unifyValueTypes(t, ValueTypeRef(unificationVar), ctx)

    case (ValueTypeRef(w: ValueTypeWildcard), t: ValueType, _) =>
      val unificationVar: ValueUnificationVar = unificationVarFromWildcard(w)
      unifyValueTypes(ValueTypeRef(unificationVar), t, ctx)

    case (ValueTypeRef(s: ValueUnificationVar), t: ValueType, Covariant) => requireUpperBound(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: ValueUnificationVar), Covariant) => requireLowerBound(t, s, ctx)

    case (ValueTypeRef(s: ValueUnificationVar), t: ValueType, Contravariant) => requireLowerBound(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: ValueUnificationVar), Contravariant) => requireUpperBound(t, s, ctx)

    case (ValueTypeRef(s: ValueUnificationVar), t: ValueType, Invariant) => requireEqual(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: ValueUnificationVar), Invariant) => requireEqual(t, s, ctx)

    // For now, we treat all type constructors as invariant.
    case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2), _) =>

      if (t1 != t2) { error(tpe1, tpe2, ErrorContext.TypeConstructor(ctx)) }

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
    case (t, BlockTypeRef(x : BlockTypeWildcard)) =>
      requireEqual(unificationVarFromWildcard(x), t, ctx)
    case (BlockTypeRef(x : BlockTypeWildcard), t) =>
      requireEqual(unificationVarFromWildcard(x), t, ctx)
    case (t, BlockTypeRef(x: BlockUnificationVar)) =>
      requireEqual(x, t, ctx)
    case (BlockTypeRef(x : BlockUnificationVar), t) =>
      requireEqual(x, t, ctx)
    case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s, ctx)
    case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s, ctx)
    case (t, s) => error(t, s, ctx)
  }

  def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
    // for now block type constructors are invariant
    case (InterfaceType(c1, targs1), InterfaceType(c2, targs2)) =>
      if (c1 != c2) { error(tpe1, tpe2, ErrorContext.TypeConstructor(ctx)) }
      (targs1 zip targs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }
  }

  def unifyEffects(eff1: EffectsOrRef, eff2: EffectsOrRef, ctx: ErrorContext): Unit = (eff1, eff2) match {
    case (x: Effects, y: Effects) =>
      if (x.toList.toSet != y.toList.toSet) error(pp"${y} is not equal to ${x}", ctx)

    case (EffectRef(x: EffectUnificationVar), EffectRef(y: EffectUnificationVar)) => requireEqual(x, eff2, ctx)

    case (EffectRef(x: EffectUnificationVar), y: Effects) => requireEqual(x, y, ctx)
    case (x: Effects, EffectRef(y: EffectUnificationVar)) => requireEqual(y, x, ctx)

    case (EffectRef(x: EffectSetWildcard), y) =>
      val unificationVar: EffectUnificationVar = unificationVarFromWildcard(x)
      requireEqual(unificationVar, eff2, ctx)

    case (x, EffectRef(y: EffectSetWildcard)) =>
      val unificationVar: EffectUnificationVar = unificationVarFromWildcard(y)
      requireEqual(unificationVar, eff1, ctx)

  }

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

      val targs1 = tparams1.map(ValueTypeRef.apply)

      val subst = Substitutions(tparams2 zip targs1, List(), cparams2 zip cparams1.map(c => CaptureSet(c)), List()) // Is List() the right choice?
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
      case (x @ ValueTypeRef(_: ValueUnificationVar), tpe: ValueType, p) =>
        unifyValueTypes(x, tpe, ctx); x
      case (tpe: ValueType, x @ ValueTypeRef(_: ValueUnificationVar), p) =>
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
    // for now block type constructors are invariant
    case (InterfaceType(c1, targs1), InterfaceType(c2, targs2)) =>
      if (c1 != c2) abort(pp"The two types ${ c1 } and ${ c2 } are not compatible", ErrorContext.TypeConstructor(ctx))
      val mergedConstructor = c1
      val mergedArgs = (targs1 zip targs2) map { case (t1, t2) => mergeValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }
      InterfaceType(mergedConstructor, mergedArgs)
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

      val targs1 = tparams1.map(ValueTypeRef.apply)
      val subst = Substitutions(tparams2 zip targs1, List(), cparams2 zip cparams1.map(c => CaptureSet(c)), List()) // Is List() the right choice?
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
