package effekt
package typer

import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.typer.ErrorContext.FunctionEffects
import effekt.util.messages.{ErrorMessageReifier, StructuredMessage, pluralized}

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

  def abort(issue: StructuredMessage, ctx: ErrorContext): Nothing
  def error(issue: StructuredMessage, ctx: ErrorContext): Unit
  def abort(left: Type, right: Type, ctx: ErrorContext): Nothing
  def error(left: Type, right: Type, ctx: ErrorContext): Unit

  def unifyCaptures(c1: Captures, c2: Captures, ctx: ErrorContext): Unit = ctx.polarity match {
    case Covariant     => requireSubregion(c1, c2, ctx)
    case Contravariant => requireSubregion(c2, c1, ctx)
    case Invariant     => requireSubregion(c1, c2, ctx); requireSubregion(c2, c1, ctx)
  }
  def unifyCapture(c1: Capture, c2: Capture, ctx: ErrorContext): Unit = unifyCaptures(CaptureSet(Set(c1)), CaptureSet(Set(c2)), ctx)

  def unifyValueTypes(tpe1: ValueType, tpe2: ValueType, ctx: ErrorContext): Unit = (tpe1, tpe2, ctx.polarity, ctx.coercible) match {
    case (t, s, _, _) if t == s => ()

    case (ValueTypeRef(s: UnificationVar), t: ValueType, Covariant, _) => requireUpperBound(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: UnificationVar), Covariant, _) => requireLowerBound(t, s, ctx)

    case (ValueTypeRef(s: UnificationVar), t: ValueType, Contravariant, _) => requireLowerBound(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: UnificationVar), Contravariant, _) => requireUpperBound(t, s, ctx)

    case (ValueTypeRef(s: UnificationVar), t: ValueType, Invariant, _) => requireEqual(s, t, ctx)
    case (s: ValueType, ValueTypeRef(t: UnificationVar), Invariant, _) => requireEqual(t, s, ctx)

    // coercing is the last resort...
    case (TBottom, to, Covariant, Some(coerce)) => coerce(Coercion.FromNothing(to))
    case (from, TTop, Covariant, Some(coerce)) => coerce(Coercion.ToAny(from))

    // For now, we treat all type constructors as invariant.
    case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2), _, _) =>

      if (t1 != t2) { error(tpe1, tpe2, ErrorContext.TypeConstructor(ctx)) }
      else if (args1.size != args2.size) {
        abort(pretty"Argument count does not match $t1 vs. $t2", ctx) // TODO add to context
      }

      // TODO here we assume that the type constructor is covariant
      (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx)) }

    case (t @ BoxedType(tpe1, capt1), s @ BoxedType(tpe2, capt2), _, _) =>
      unifyBlockTypes(tpe1, tpe2, ErrorContext.BoxedTypeBlock(t, s, ctx))
      unifyCaptures(capt1, capt2, ErrorContext.BoxedTypeCapture(t, s, ctx))

    case (t, s, p, _) =>
      error(t, s, ctx)
  }

  def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
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

  def unifyEffects(eff1: Effects, eff2: Effects, ctx: ErrorContext): Unit =
    val effs1 = eff1.toList.toSet
    val effs2 = eff2.toList.toSet

    // the sets are equal, so they are equal per type constructor
    val byConstructor = (effs1 ++ effs2).map { eff => eff.typeConstructor }.toList.map { c =>
      (effs1.filter { eff => eff.typeConstructor == c }, effs2.filter { eff => eff.typeConstructor == c })
    }
    def known(effs: Set[InterfaceType]) = effs.forall { eff => unknowns(eff).isEmpty }

    // We're sure that the sets are different if only one has a type constructor, or if its effects are known and differ.
    val different = byConstructor.exists { case (group1, group2) =>
      group1.isEmpty || group2.isEmpty || (known(group1) && known(group2) && group1 != group2)
    }

    if (different) {
      error(pretty"Expected the effects ${eff2} but got ${eff1}", ctx)
    } else {
      byConstructor.foreach {
        // a single effect with this type constructor equals each one on the other side
        case (group1, group2) if !(known(group1) && known(group2)) && (group1.size == 1 || group2.size == 1) =>
          for { e1 <- group1; e2 <- group2 } unifyInterfaceTypes(e1, e2, ctx)
        // TODO otherwise... who knows
        case _ => ()
      }
    }

  def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType, ctx: ErrorContext): Unit = (tpe1, tpe2) match {
    case (
      f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1, eff1),
      f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2, eff2)) =>

      if (tparams1.size != tparams2.size)
        abort(pretty"Expected ${pluralized(tparams2.size, "type parameter")} but got ${tparams1.size}", ctx)

      if (vparams1.size != vparams2.size)
        abort(pretty"Expected ${pluralized(vparams2.size, "value parameter")} but got ${vparams1.size}", ctx)

      if (bparams1.size != bparams2.size)
        abort(pretty"Expected ${pluralized(bparams2.size, "block parameter")} but got ${bparams1.size}", ctx)

      // with as many block parameters, the capture parameters differ in number only through the effects
      if (cparams1.size != cparams2.size)
        abort(pretty"Expected the effects ${eff2} but got ${eff1}", ErrorContext.FunctionEffects(ctx))

      val targs1 = tparams1.map(ValueTypeRef.apply)

      val subst = Substitutions(tparams2 zip targs1, cparams2 zip cparams1.map(c => CaptureSet(c)))
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
    (oldBound, newBound) match {
      case (t, s) if t == s => t

      case (t, TTop) => TTop
      case (TTop, t) => TTop
      case (TBottom, t) => t
      case (t, TBottom) => t

      // reuses the type unifier implementation (will potentially register new constraints)
      // TODO we need to create a fresh unification variable here!
      case (x @ ValueTypeRef(_: UnificationVar), tpe: ValueType) =>
        unifyValueTypes(x, tpe, ctx); x
      case (tpe: ValueType, x @ ValueTypeRef(_: UnificationVar)) =>
        unifyValueTypes(tpe, x, ctx); x

      case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2)) =>
        if (cons1 != cons2) abort(oldBound, newBound, ErrorContext.TypeConstructor(ctx))
        if (args1.size != args2.size) abort(pretty"Different count of argument to type constructor: $oldBound vs $newBound", ctx)

        (args1 zip args2).foreach {
          case (t1, t2) => unifyValueTypes(t1, t2, ErrorContext.TypeConstructorArgument(ctx))
        }
        ValueTypeApp(cons1, args1)

      case (s @ BoxedType(tpe1, capt1), t @ BoxedType(tpe2, capt2)) =>
        unifyBlockTypes(tpe1, tpe2, ErrorContext.BoxedTypeBlock(s, t, ctx))
        unifyCaptures(capt1, capt2, ErrorContext.BoxedTypeCapture(s, t, ctx))
        s

      case _ =>
        abort(oldBound, newBound, ctx)
    }
}
