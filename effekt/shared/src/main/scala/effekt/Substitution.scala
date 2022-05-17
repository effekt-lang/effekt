package effekt

import effekt.context.Context
import effekt.substitutions.TypeComparer
import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.util.messages.ErrorReporter

object substitutions {

  private var scopeId: Int = 0

  /**
   * Represents a node in the constraint propagation graph
   *
   *                  ┏━━━━━━━━━━━━━━━━━━┓
   *  --------------> ┃      ?MyVar      ┃ -------------->
   *   lower nodes    ┠─────────┬────────┨   upper nodes
   *  --------------> ┃  Lower  │  Upper ┃ -------------->
   *                  ┗━━━━━━━━━┷━━━━━━━━┛
   *
   * The arrows in the picture above indicate the suptyping relationship.
   * They are in fact navigatable in both directions, since new _upper_ bounds
   * need to flow through the node from right-to-left.
   */
  case class ValueTypeConstraints(
    lowerVariables: Set[UnificationVar],
    lowerType: ValueType,
    upperType: ValueType,
    upperVariables: Set[UnificationVar]
  )


  case class CaptureConstraints(lower: Set[Capture], upper: Set[Capture])

  /**
   * The state of the unification scope, used for backtracking on overload resolution
   *
   * See [[UnificationScope.backup]] and [[UnificationScope.restore]]
   */
  case class UnificationState(
    skolems: List[UnificationVar],
    valueConstraints: Map[UnificationVar, ValueTypeConstraints]
  )

  /**
   * A unification scope -- every fresh unification variable is associated with a scope.
   */
  class UnificationScope { self =>

    val id = { scopeId += 1; scopeId }

    // the state of this unification scope

    private var skolems: List[UnificationVar] = Nil
    private var capture_skolems: List[CaptureUnificationVar] = Nil
    var valueConstraints: Map[UnificationVar, ValueTypeConstraints] = Map.empty

    var valueSubstitution: Map[UnificationVar, ValueType] = Map.empty

    def dumpConstraints() =
      val colSize = 12
      val varSize = 5
      val sep = "━".repeat(colSize)
      val varSep = "━".repeat(varSize)

      println(s"┏$sep┯$sep━━━━━$varSep━━━━━$sep┯$sep┓")

      valueConstraints.foreach {
        case (x, ValueTypeConstraints(lowerVars, lower, upper, upperVars)) =>
          val lowNodes = lowerVars.mkString(", ").padTo(colSize, ' ')
          val lowType  = lower.toString.padTo(colSize, ' ')
          val variable = x.toString.padTo(varSize, ' ')
          val upType = upper.toString.padTo(colSize, ' ')
          val upNodes = upperVars.mkString(", ")
          println(s"$lowNodes │ $lowType <: $variable <: $upType │ $upNodes")
      }
      println(s"┗$sep┷$sep━━━━━$varSep━━━━━$sep┷$sep┛")

    def backup(): UnificationState = UnificationState(skolems, valueConstraints)
    def restore(state: UnificationState): Unit =
      skolems = state.skolems
      valueConstraints = state.valueConstraints

    def boundsFor(x: UnificationVar): ValueTypeConstraints =
      valueConstraints.getOrElse(x, ValueTypeConstraints(Set.empty, TBottom, TTop, Set.empty))

    def updateBounds(x: UnificationVar, bounds: ValueTypeConstraints): Unit =
      valueConstraints = valueConstraints.updated(x, bounds)
      dumpConstraints()

    def updateLowerBound(x: UnificationVar, bound: ValueType): Unit = bound match {
      case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a lower bound for ${x}"
      case _ =>
        val bounds = boundsFor(x)
        updateBounds(x, bounds.copy(lowerType = bound))
    }

    def updateUpperBound(x: UnificationVar, bound: ValueType): Unit = bound match {
      case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a upper bound for ${x}"
      case _ =>
        val bounds = boundsFor(x)
        updateBounds(x, bounds.copy(upperType = bound))
    }

    /**
     * Adds x as a lower bound to y, and y as a lower bound to x.
     */
    def connect(x: UnificationVar, y: UnificationVar): Unit =
      val boundsX = boundsFor(x)
      val boundsY = boundsFor(y)
      updateBounds(x, boundsX.copy(upperVariables = boundsX.upperVariables + y))
      updateBounds(y, boundsY.copy(lowerVariables = boundsX.lowerVariables + x))

    def fresh(role: UnificationVar.Role): UnificationVar = {
      val x = UnificationVar(role, this)
      skolems = x :: skolems
      x
    }

    def freshCaptVar(underlying: Capture): CaptureUnificationVar = {
      val x = CaptureUnificationVar(underlying, this)
      capture_skolems = x :: capture_skolems
      x
    }

    // TODO do we need to compute a bisubstitution here???
    def solveFor(rigids: List[UnificationVar], polarity: Boolean)(using C: ErrorReporter): Map[UnificationVar, ValueType] = ???


    /**
     * Checks whether t1 <: t2
     *
     * Has the side effect of registering constraints.
     */
    def requireSubtype(t1: ValueType, t2: ValueType)(using C: ErrorReporter): Unit =
      comparer.unifyValueTypes(t1, t2)

    def requireSubtype(t1: BlockType, t2: BlockType)(using C: ErrorReporter): Unit =
      sys error s"Requiring that ${t1} <:< ${t2}"

    def requireSubregion(c1: CaptureSet, c2: CaptureSet)(using C: ErrorReporter): Unit =
      sys error s"Requiring that ${c1} <:< ${c2}"


    /**
     * Given the current unification state, can we decide whether one type is a subtype of another?
     */
    def isSubtype(tpe1: ValueType, tpe2: ValueType): Boolean = false
//      object NotASubtype extends Throwable
//      object comparer extends TypeComparer {
//        def unify(c1: CaptureSet, c2: CaptureSet): Unit = ???
//        def abort(msg: String) = throw NotASubtype
//        // TODO is this correct???
//        // Check tpe <: x
//        def requireLowerBound(x: UnificationVar, tpe: ValueType) =
//          // does this terminate???
//          println(s"Checking whether ${tpe} <:< ${boundsFor(x)} (${x})")
//          if (!isSubtype(tpe, boundsFor(x).lowerType)) throw NotASubtype
//        def requireUpperBound(x: UnificationVar, tpe: ValueType) =
//          println(s"Checking whether ${tpe} >:> ${boundsFor(x)} (${x})")
//          if (!isSubtype(boundsFor(x).upperType, tpe)) throw NotASubtype
//      }
//      try { comparer.unifyValueTypes(tpe1, tpe2); return true } catch {
//        case NotASubtype => false
//      }

    /**
     * Given the current unification state, can we decide whether one effect is a subtype of another?
     *
     * Used to subtract one set of effects from another (when checking handling, or higher-order functions)
     */
    def isSubtype(e1: Effect, e2: Effect): Boolean = ???

    /**
     * Removes effects [[effs2]] from effects [[effs1]] by checking for subtypes.
     *
     * TODO check whether this is sound! It should be, since it is a conservative approximation.
     *   If it turns out two effects ARE subtypes after all, and we have not removed them, it does not
     *   compromise effect safety.
     *
     * TODO potentially dealias first...
     */
    def subtract(effs1: Effects, effs2: Effects): Effects =
      effs1.filterNot(eff1 => effs2.exists(eff2 => isSubtype(eff2, eff1)))

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: FunctionType)(using C: ErrorReporter): (List[UnificationVar], List[CaptureUnificationVar], FunctionType) = {
      val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = tpe
      val typeRigids = tparams map { t => fresh(UnificationVar.TypeVariableInstantiation(t)) }
      val captRigids = cparams map freshCaptVar
      val subst = Substitutions(
        tparams zip typeRigids,
        cparams zip captRigids.map(c => CaptureSet(c)))

      val substitutedVparams = vparams map subst.substitute
      val substitutedBparams = bparams map subst.substitute
      val substitutedReturn = subst.substitute(ret)
      val substitutedEffects = subst.substitute(eff)
      (typeRigids, captRigids, FunctionType(Nil, Nil, substitutedVparams, substitutedBparams, substitutedReturn, substitutedEffects))
    }


    // TODO the comparer should build up a "deconstruction trace" that can be used for better
    //   type errors.

    /**
     * A side effecting type comparer
     */
    def comparer(using C: ErrorReporter): TypeComparer = new TypeComparer {

      private var seen: Set[UnificationVar] = Set.empty

      def unify(c1: CaptureSet, c2: CaptureSet): Unit = ???

      def abort(msg: String) = C.abort(msg)


      /**
       * Value type [[tpe]] flows into the unification variable [[x]] as a lower bound.
       *
       *
       *                  ┏━━━━━━━━━━━━━━━━━━━━━━━┓
       *                  ┃           x           ┃
       *  --------------> ┠───────────┬───────────┨ ---------------->
       *    (1) tpe       ┃ Lower (2) │ Upper (3) ┃ (4) upper nodes
       *                  ┗━━━━━━━━━━━┷━━━━━━━━━━━┛
       *
       */
      def requireLowerBound(x: UnificationVar, tpe: ValueType) =
        println(s"Requiring that $tpe <: $x")
        if (x == tpe || seen.contains(x)) return () else seen += x

        // (1) look up the bounds for node `x` -- this can potentially add a fresh node to the network
        val ValueTypeConstraints(lowerNodes, lower, upper, upperNodes) = boundsFor(x)

        tpe match {
          // the new lower bound is a unification variable
          case y: UnificationVar =>
            if (lowerNodes contains y) return;
            connect(y, x)
            // lower bounds flow from y ---> x
            requireLowerBound(x, boundsFor(y).lowerType)
            // upper bounds flow from y <--- x
            requireUpperBound(y, upper)

          // the new lower bound is a value type
          case _ =>
            // (2) we merge the existing lower bound with the incoming type.
            val newBound = mergeLower(lower, tpe)
            updateLowerBound(x, newBound)

            // (3) we check the existing upper bound for consistency with the lower bound
            unifyValueTypes(newBound, upper)

            // (4) we propagate the incoming type to all upper nodes
            upperNodes foreach { node => requireLowerBound(node, tpe) }
        }

      /**
       * Value type [[tpe]] flows into the unification variable [[x]] as an upper bound.
       * Symmetric to [[requireLowerBound]].
       */
      def requireUpperBound(x: UnificationVar, tpe: ValueType) =
        println(s"Requiring that $x <: $tpe")
        if (x == tpe || seen.contains(x)) return () else seen += x

        // (1) look up the bounds for node `x` -- this can potentially add a fresh node to the network
        val ValueTypeConstraints(lowerNodes, lower, upper, upperNodes) = boundsFor(x)

        tpe match {
          // the new lower bound is a unification variable
          case y: UnificationVar =>
            if (upperNodes contains y) return;
            connect(x, y)
            // upper bounds flow from x <---- y
            requireUpperBound(x, boundsFor(y).upperType)
            // lower bounds flow from x ----> y
            requireLowerBound(y, boundsFor(x).lowerType)

          // the new lower bound is a value type
          case _ =>
            // (2) we merge the existing lower bound with the incoming type.
            val newBound = mergeUpper(upper, tpe)
            updateUpperBound(x, newBound)

            // (3) we check the existing upper bound for consistency with the lower bound
            unifyValueTypes(lower, newBound)

            // (4) we propagate the incoming type to all lower nodes
            lowerNodes foreach { node => requireUpperBound(node, tpe) }
        }


      /**
       * Compute the join of two types
       */
      def mergeLower(oldBound: ValueType, newBound: ValueType)(using C: ErrorReporter): ValueType =
        (oldBound, newBound) match {
          case (t, s) if t == s => t
          case (TBottom, t) => t

          case (tpe1: UnificationVar, tpe2: UnificationVar) =>
            // two unification variables, we create a fresh merge node with two incoming edges.
            val mergeNode = fresh(UnificationVar.MergeVariable)
            updateBounds(mergeNode, ValueTypeConstraints(Set(tpe1, tpe2), TBottom, TTop, Set.empty))
            // propagate existing bounds into new node
            val bounds1 = boundsFor(tpe1).lowerType
            val bounds2 = boundsFor(tpe2).lowerType
            requireLowerBound(mergeNode, bounds1)
            requireLowerBound(mergeNode, bounds2)
            mergeNode

          // We can use one of them if it is more specific than the other.
          case (tpe1, tpe2) if isSubtype(tpe1, tpe2) => tpe2
          case (tpe1, tpe2) if isSubtype(tpe2, tpe1) => tpe1

          case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2)) =>
            if (cons1 != cons2) C.abort(s"Cannot merge different constructors")
            if (args1.size != args2.size) C.abort(s"Different count of argument to type constructor")

            // TODO Here we assume the constructor is covariant
            // TODO perform analysis and then mergeUpper, lower, or require equality.
            val mergedArgs = (args1 zip args2).map { mergeLower }
            ValueTypeApp(cons1, mergedArgs)

          case _ =>
            println(s"merge of ${oldBound} and ${newBound} failed")
            C.abort(s"Cannot merge ${oldBound} with ${newBound} at positive polarity")
        }

      /**
       * Compute the meet of two types
       */
      def mergeUpper(oldBound: ValueType, newBound: ValueType)(using C: ErrorReporter): ValueType =
        (oldBound, newBound) match {
          case (t, s) if t == s => t
          case (TTop, t) => t

          case (tpe1: UnificationVar, tpe2: UnificationVar) =>
            // two unification variables, we create a fresh merge node with two incoming edges.
            val mergeNode = fresh(UnificationVar.MergeVariable)
            updateBounds(mergeNode, ValueTypeConstraints(Set(tpe1, tpe2), TBottom, TTop, Set.empty))
            // propagate existing bounds into new node
            val bounds1 = boundsFor(tpe1).upperType
            val bounds2 = boundsFor(tpe2).upperType
            requireUpperBound(mergeNode, bounds1)
            requireUpperBound(mergeNode, bounds2)
            mergeNode

          // We can use one of them if it is more specific than the other.
          case (tpe1, tpe2) if isSubtype(tpe2, tpe1) => tpe2
          case (tpe1, tpe2) if isSubtype(tpe1, tpe2) => tpe1

          case _ =>
            println(s"merge of ${oldBound} and ${newBound} failed")
            C.abort(s"Cannot merge ${oldBound} with ${newBound} at negative polarity")
        }
      }
  }

  case class SubstitutionException(x: CaptureUnificationVar, subst: Map[Capture, CaptureSet]) extends Exception

  /**
   * Substitutions not only have unification variables as keys, since we also use the same mechanics to
   * instantiate type schemes
   */
  case class Substitutions(
    values: Map[TypeVar, ValueType],
    captures: Map[Capture, CaptureSet]
  ) {

    def isDefinedAt(t: TypeVar) = values.isDefinedAt(t)
    def isDefinedAt(c: Capture) = captures.isDefinedAt(c)

    def get(t: TypeVar) = values.get(t)
    def get(c: Capture) = captures.get(c)

    // amounts to first substituting this, then other
    def updateWith(other: Substitutions): Substitutions =
      Substitutions(values.view.mapValues { t => other.substitute(t) }.toMap, captures.view.mapValues { t => other.substitute(t) }.toMap) ++ other

    // amounts to parallel substitution
    def ++(other: Substitutions): Substitutions = Substitutions(values ++ other.values, captures ++ other.captures)

    // shadowing
    private def without(tps: List[TypeVar], cps: List[Capture]): Substitutions =
      Substitutions(
        values.filterNot { case (t, _) => tps.contains(t) },
        captures.filterNot { case (t, _) => cps.contains(t) }
      )

    // TODO we DO need to distinguish between substituting unification variables for unification variables
    // and substituting concrete captures in unification variables... These are two fundamentally different operations.
    def substitute(c: CaptureSet): CaptureSet = c.flatMap {
      // we are probably instantiating a function type
      case x: CaptureUnificationVar if captures.keys.exists(c => c.concrete) =>
        throw SubstitutionException(x, captures)
      case c => captures.getOrElse(c, CaptureSet(c))
    }

    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        values.getOrElse(x, x)
      case ValueTypeApp(t, args) =>
        ValueTypeApp(t, args.map { substitute })
      case BoxedType(tpe, capt) =>
        BoxedType(substitute(tpe), substitute(capt))
      case other => other
    }

    // TODO implement
    def substitute(t: Effects): Effects = t
    def substitute(t: Effect): Effect = t

    def substitute(t: BlockType): BlockType = t match {
      case e: InterfaceType => substitute(e)
      case b: FunctionType        => substitute(b)
    }

    def substitute(t: InterfaceType): InterfaceType = t match {
      case b: Interface           => b
      case BlockTypeApp(c, targs) => BlockTypeApp(c, targs map substitute)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, cps, vps, bps, ret, eff) =>
        // do not substitute with types parameters bound by this function!
        val substWithout = without(tps, cps)
        FunctionType(
          tps,
          cps,
          vps map substWithout.substitute,
          bps map substWithout.substitute,
          substWithout.substitute(ret),
          substWithout.substitute(eff))
    }
  }

  object Substitutions {
    val empty: Substitutions = Substitutions(Map.empty[TypeVar, ValueType], Map.empty[Capture, CaptureSet])
    def apply(values: List[(TypeVar, ValueType)], captures: List[(Capture, CaptureSet)]): Substitutions = Substitutions(values.toMap, captures.toMap)
  }

  // TODO Mostly for backwards compat
  implicit def typeMapToSubstitution(values: Map[TypeVar, ValueType]): Substitutions = Substitutions(values, Map.empty[Capture, CaptureSet])
  implicit def captMapToSubstitution(captures: Map[Capture, CaptureSet]): Substitutions = Substitutions(Map.empty[TypeVar, ValueType], captures)


  trait TypeComparer {

    // "unification effects"
    def requireLowerBound(x: UnificationVar, tpe: ValueType): Unit
    def requireUpperBound(x: UnificationVar, tpe: ValueType): Unit
    def abort(msg: String): Nothing

    def unify(c1: CaptureSet, c2: CaptureSet): Unit
    def unify(c1: Capture, c2: Capture): Unit = unify(CaptureSet(Set(c1)), CaptureSet(Set(c2)))

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType): Unit = (tpe1, tpe2) match {
      case (t, s) if t == s => ()
      case (_, TTop) => ()
      case (TBottom, _) => ()

      case (s: UnificationVar, t: ValueType) => requireUpperBound(s, t)

      // occurs for example when checking the first argument of `(1 + 2) == 3` against expected
      // type `?R` (since `==: [R] (R, R) => Boolean`)
      case (s: ValueType, t: UnificationVar) => requireLowerBound(t, s)

      case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2)) =>
        if (args1.size != args2.size)
          abort(s"Argument count does not match $t1 vs. $t2")

        unifyValueTypes(t1, t2)

        // TODO here we assume that the type constructor is covariant
        (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2) }

      case (BoxedType(tpe1, capt1), BoxedType(tpe2, capt2)) =>
        unifyBlockTypes(tpe1, tpe2)
        unify(capt1, capt2)

      case (t, s) =>
        println(s"Type mismatch: ${t} is not ${s}")
        abort(s"Expected ${t}, but got ${s}")
    }

    def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType): Unit = (tpe1, tpe2) match {
      case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s)
      case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s)
      case (t, s) => abort(s"Expected ${t}, but got ${s}")
    }

    def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType): Unit = (tpe1, tpe2) match {
      case (t1: Interface, t2: Interface) => if (t1 != t2) abort(s"Expected ${t1}, but got ${t2}")
      case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
        unifyInterfaceTypes(c2, c2)
        (targs1 zip targs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2) }
      case _ => abort(s"Kind mismatch between ${tpe1} and ${tpe2}")
    }

    def unifyEffects(eff1: Effects, eff2: Effects): Unit = ???

    def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType): Unit = (tpe1, tpe2) match {
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

        (vparams1 zip substVparams2) foreach { case (t1, t2) => unifyValueTypes(t2, t1) }
        (bparams1 zip substBparams2) foreach { case (t1, t2) => unifyBlockTypes(t2, t1) }
        unifyValueTypes(ret1, substRet2)
        unifyEffects(eff1, eff2)
    }

    // There are only a few users of dealiasing:
    //  1) checking for effect inclusion (`contains` in Effects)
    //  2) checking exhaustivity of pattern matching
    //  3) type comparer itself
    def dealias(tpe: ValueType): ValueType = ???
    def dealias(tpe: Effects): Effects = ???
  }
}
