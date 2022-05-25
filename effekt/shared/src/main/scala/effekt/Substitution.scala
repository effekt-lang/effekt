package effekt

import typer._

import effekt.context.Context
import effekt.substitutions.TypeComparer
import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TInt, TTop }
import effekt.util.messages.ErrorReporter

object substitutions {

  sealed trait Polarity { def flip: Polarity }
  case object Covariant extends Polarity { def flip = Contravariant}
  case object Contravariant extends Polarity { def flip = Covariant }
  case object Invariant extends Polarity { def flip = Invariant }

  /**
   * The state of the unification scope, used for backtracking on overload resolution
   *
   * See [[Unification.backup]] and [[Unification.restore]]
   */
  case class UnificationState(
    scope: Scope,
    constraints: ConstraintSet,
    substitution: BiSubstitutions
  )

  sealed trait Scope
  case object GlobalScope extends Scope
  case class LocalScope(types: List[UnificationVar], captures: List[CaptureUnificationVar], parent: Scope) extends Scope


  /**
   * A unification scope -- every fresh unification variable is associated with a scope.
   */
  class Unification { self =>

    // Unification variables in the current scope
    // ------------------------------------------
    var scope: Scope = GlobalScope

    var substitution = BiSubstitutions(Map.empty, Map.empty)


    def isLive(x: UnificationVar): Boolean = isLive(x, scope)

    def isLive(x: UnificationVar, scope: Scope): Boolean = scope match {
      case GlobalScope => false
      case LocalScope(types, _, parent) => types.contains(x) || isLive(x, parent)
    }

    def fresh(role: UnificationVar.Role): UnificationVar = scope match {
      case GlobalScope => sys error "Cannot add unification variables to global scope"
      case s : LocalScope =>
        val x = new UnificationVar(role)
        scope = s.copy(types = x :: s.types)
        x
    }

    def freshCaptVar(underlying: Capture): CaptureUnificationVar = scope match {
      case GlobalScope => sys error "Cannot add unification variables to global scope"
      case s : LocalScope =>
        val x = CaptureUnificationVar(underlying)
        scope = s.copy(captures = x :: s.captures)
        x
    }

    // The Constraint Graph
    // --------------------
    var constraints = new ConstraintSet

    def dumpConstraints() = constraints.dumpConstraints()

    def backup(): UnificationState = UnificationState(scope, constraints.clone(), substitution)
    def restore(state: UnificationState): Unit =
      scope = state.scope
      constraints = state.constraints
      substitution = state.substitution

    def enterScope() = {
      scope = LocalScope(Nil, Nil, scope)
    }

    def leaveScope()(using ErrorReporter) = {
      val LocalScope(types, captures, parent) = scope match {
        case GlobalScope => sys error "Cannot leave global scope"
        case l : LocalScope => l
      }
      scope = parent

      println(s"Leaving scope, removing ${types}")

      constraints.dumpConstraints()

      var subst = Map.empty[TypeVar, (ValueType, ValueType)]

      constraints.remove(types.toSet) foreach {
        case (nodes, Right((lower, upper))) =>
          subst = subst ++ nodes.map { x => x -> concretizeBounds(lower, upper) }.toMap
        case (nodes, Left(repr)) =>
          subst = subst ++ nodes.map { x => x -> (repr, repr) }.toMap
      }
      val subst1 = BiSubstitutions(subst, Map.empty)

      println(subst1)

      // apply substitution to itself to remove all occurrences of skolems
      val subst2 = subst1.substitute(subst1)
      println(subst2)
      substitution = substitution.updateWith(subst2)

      // apply substitution to bounds in remaining constraints
      constraints.mapBounds { case (lower, upper) =>
        (substitution.substitute(lower)(using Covariant), substitution.substitute(upper)(using Contravariant))
      }

      constraints.dumpConstraints()
      println(substitution)
      substitution
    }


    /**
     * Checks whether t1 <: t2
     *
     * Has the side effect of registering constraints.
     *
     * TODO Covariant might not be the right polarity
     */
    def requireSubtype(t1: ValueType, t2: ValueType)(using C: ErrorReporter): Unit =
      given Polarity = Covariant;
      comparer.unifyValueTypes(
        substitution.substitute(t1),
        substitution.substitute(t2))

    def requireEqual(t1: ValueType, t2: ValueType)(using C: ErrorReporter): Unit =
      given Polarity = Invariant;
      comparer.unifyValueTypes(
        substitution.substitute(t1),
        substitution.substitute(t2))

    def requireSubtype(t1: BlockType, t2: BlockType)(using C: ErrorReporter): Unit =
      sys error s"Requiring that ${t1} <:< ${t2}"

    def requireSubregion(c1: CaptureSet, c2: CaptureSet)(using C: ErrorReporter): Unit =
      sys error s"Requiring that ${c1} <:< ${c2}"

    def join(tpes: List[ValueType])(using C: ErrorReporter): ValueType =
      tpes.foldLeft[ValueType](TBottom) { (t1, t2) => comparer.merge(t1, t2, Covariant) }

    /**
     * Given the current unification state, can we decide whether one type is a subtype of another?
     *
     * Does not influence the constraint graph.
     */
    def isSubtype(tpe1: ValueType, tpe2: ValueType): Boolean =
      val res = try { subtypingComparer.unifyValueTypes(tpe1, tpe2)(using Covariant); true } catch {
        case NotASubtype => false
      }
      println(s"Checking ${tpe1} <: ${tpe2}: $res")
      res

    /**
     * Given the current unification state, can we decide whether one effect is a subtype of another?
     *
     * Used to subtract one set of effects from another (when checking handling, or higher-order functions)
     */
    def isSubtype(e1: Effect, e2: Effect): Boolean =
      val res = try { subtypingComparer.unifyEffect(e1, e2)(using Covariant); true } catch {
        case NotASubtype => false
      }
      res

    private object NotASubtype extends Throwable
    private object subtypingComparer extends TypeComparer {
      def unify(c1: CaptureSet, c2: CaptureSet): Unit = ???
      def abort(msg: String) = throw NotASubtype
      def requireEqual(x: UnificationVar, tpe: ValueType): Unit =
        requireLowerBound(x, tpe);
        requireUpperBound(x, tpe)

      // tpe <: x
      def requireLowerBound(x: UnificationVar, tpe: ValueType) =
        tpe match {
          case y: UnificationVar =>
            if (!constraints.isSupertypeOf(x, y)) throw NotASubtype
          case tpe =>
            // it is compatible with the upper bounds on x
            unifyValueTypes(tpe, constraints.upperBound(x))(using Covariant)
        }

      // x <: tpe
      def requireUpperBound(x: UnificationVar, tpe: ValueType) =
        tpe match {
          case y: UnificationVar =>
            if (!constraints.isSubtypeOf(x, y)) throw NotASubtype
          case tpe =>
            unifyValueTypes(constraints.lowerBound(x), tpe)(using Covariant)
        }
    }


    private def concretizeBounds(lower: ValueType, upper: ValueType)(using C: ErrorReporter): (ValueType, ValueType) = (lower, upper) match {
      case (TBottom, TTop) => C.abort("Cannot infer type") // TODO move to right point
      case (TBottom, t) => (t, t)
      case (t, TTop)    => (t, t)
      case (lower, upper) => (lower, upper)
    }

    // Maybe move to type comparer??
    private def coalesceValueBounds(lower: ValueType, upper: ValueType)(using C: ErrorReporter): ValueType = (lower, upper) match {
      case (TBottom, TTop) => C.abort("Cannot infer type") // TODO move to right point
      case (TBottom, t) => t
      case (t, TTop)    => t

      // a unification variable is more precise than a concrete type
      case (x: UnificationVar, y) if !y.isInstanceOf[UnificationVar] => x
      case (x, y: UnificationVar) if !x.isInstanceOf[UnificationVar] => y

      case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2)) =>
        if (cons1 != cons2) C.abort(s"Cannot merge different constructors")
        if (args1.size != args2.size) C.abort(s"Different count of argument to type constructor")
        val mergedArgs = (args1 zip args2).map { case (t1, t2) => coalesceValueBounds(t1, t2) }
        ValueTypeApp(cons1, mergedArgs)

      case (BoxedType(b1, c1), BoxedType(b2, c2)) =>
        BoxedType(coalesceBlockBounds(b1, b2), coalesceCaptureBounds(c1, c2))

      case (lower, upper) => ???
    }

    private def coalesceBlockBounds(lower: BlockType, upper: BlockType)(using C: ErrorReporter): BlockType = ???
    private def coalesceCaptureBounds(lower: CaptureSet, upper: CaptureSet)(using C: ErrorReporter): CaptureSet = ???

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
      given Polarity = Covariant
      val effSubst1 = substitution.substitute(effs1)
      val effSubst2 = substitution.substitute(effs2)
      effSubst1.filterNot(eff1 => effSubst2.exists(eff2 => isSubtype(eff2, eff1)))

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: FunctionType, targs: List[ValueType])(using C: ErrorReporter): (List[ValueType], List[CaptureUnificationVar], FunctionType) = {
      val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = tpe

      val typeRigids = if (targs.size == tparams.size) targs else tparams map { t => fresh(UnificationVar.TypeVariableInstantiation(t)) }

      val captRigids = cparams map freshCaptVar
      val subst = Substitutions(
        tparams zip typeRigids,
        cparams zip captRigids.map(c => CaptureSet(c)))

      println(s"Type rigids: ${typeRigids}")
      println(s"Type params: ${tparams}")

      val substitutedVparams = vparams map subst.substitute
      val substitutedBparams = bparams map subst.substitute
      val substitutedReturn = subst.substitute(ret)
      val substitutedEffects = subst.substitute(eff)
      println(s"Substituted effects ${substitutedEffects}")
      (typeRigids, captRigids, FunctionType(Nil, Nil, substitutedVparams, substitutedBparams, substitutedReturn, substitutedEffects))
    }


    /**
     * TODO better name
     */
    trait UnificationComparer {
      def merge(oldBound: ValueType, newBound: ValueType, polarity: Polarity): ValueType
    }

    /**
     * A side effecting type comparer
     *
     * TODO the comparer should build up a "deconstruction trace" that can be used for better type errors.
     */
    def comparer(using C: ErrorReporter): TypeComparer with UnificationComparer = new TypeComparer with UnificationComparer {

      def unify(c1: CaptureSet, c2: CaptureSet): Unit = ???

      def abort(msg: String) = C.abort(msg)


      def requireEqual(x: UnificationVar, tpe: ValueType): Unit =
        println(s"Requiring ${x} to be equal to ${tpe}")
        requireLowerBound(x, tpe)
        requireUpperBound(x, tpe)

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

        if (!isLive(x)) sys error s"Recording constraint on variable ${x}, which is not live!"

        if (x == tpe) return ()

        tpe match {
          // the new lower bound is a unification variable
          case y: UnificationVar => connectNodes(y, x)

          // the new lower bound is a value type
          case _ =>
            // (1) look up the bounds for node `x` -- this can potentially add a fresh node to the network
            val (lower, upper) = constraints.boundsFor(x)

            // (2) we merge the existing lower bound with the incoming type.
            mergeAndUpdateLowerBound(x, tpe) foreach { merged =>

              // (3) we check the existing upper bound for consistency with the lower bound
              //     We do not have to do this for connected nodes, since type variables in
              //     the upper bounds are connected itself.
              unifyValueTypes(merged, upper)(using Covariant)
            }
        }

      /**
       * Value type [[tpe]] flows into the unification variable [[x]] as an upper bound.
       * Symmetric to [[requireLowerBound]].
       */
      def requireUpperBound(x: UnificationVar, tpe: ValueType) =

        if (!isLive(x)) sys error s"Recording constraint on variable ${x}, which is not live!"

        if (x == tpe) return ()

        tpe match {
          // the new lower bound is a unification variable
          case y: UnificationVar => connectNodes(x, y)

          // the new lower bound is a value type
          case _ =>
            // (1) look up the bounds for node `x` -- this can potentially add a fresh node to the network
            val (lower, upper) = constraints.boundsFor(x)

            // (2) we merge the existing lower bound with the incoming type.
            mergeAndUpdateUpperBound(x, tpe) foreach { merged =>

              // (3) we check the existing upper bound for consistency with the lower bound
              unifyValueTypes(lower, merged)(using Covariant)
            }
        }

      // only updates one layer of connections, not recursively since we have
      // the invariant that all transitive connections are established
      def mergeAndUpdateLowerBound(x: UnificationVar, tpe: ValueType): Option[ValueType] =
        assert (!tpe.isInstanceOf[UnificationVar])
        val lower = constraints.lowerBound(x)
        if (x == tpe || lower == tpe || tpe == TBottom) return None;
        val newBound = mergeLower(lower, tpe)
        constraints.updateLowerBound(x, newBound)
        Some(newBound)

      def mergeAndUpdateUpperBound(x: UnificationVar, tpe: ValueType): Option[ValueType] =
        assert (!tpe.isInstanceOf[UnificationVar])
        val upper = constraints.upperBound(x)
        if (x == tpe || upper == tpe || tpe == TTop) return None;
        val newBound = mergeUpper(upper, tpe)
        constraints.updateUpperBound(x, newBound)
        Some(newBound)

      def connectNodes(x: UnificationVar, y: UnificationVar): Unit =
        if (x == y || (constraints.isSubtypeOf(x, y))) return;
        requireLowerBound(y, constraints.lowerBound(x)) // TODO maybe this can be mergeAndUpdateLowerBound
        requireUpperBound(x, constraints.upperBound(y))
        constraints.connect(x, y)

      def merge(oldBound: ValueType, newBound: ValueType, polarity: Polarity): ValueType =
        (oldBound, newBound, polarity) match {
          case (t, s, _) if t == s => t
          case (TBottom, t, Covariant) => t
          case (t, TBottom, Covariant) => t
          case (TTop, t, Contravariant) => t
          case (t, TTop, Contravariant) => t

          case (tpe1: UnificationVar, tpe2: UnificationVar, _) =>
            // two unification variables, we create a fresh merge node with two incoming / outgoing edges.

            polarity match {
              case Covariant =>
                val mergeNode = fresh(UnificationVar.MergeVariable)
                connectNodes(tpe1, mergeNode)
                connectNodes(tpe2, mergeNode)
                mergeNode
              case Contravariant =>
                val mergeNode = fresh(UnificationVar.MergeVariable)
                connectNodes(mergeNode, tpe1)
                connectNodes(mergeNode, tpe2)
                mergeNode
              case Invariant =>
                // TODO does this make sense?
                connectNodes(tpe1, tpe2)
                connectNodes(tpe2, tpe1)
                tpe1
            }

          // We can use one of them if it is more specific than the other.
          case (tpe1, tpe2, Covariant) if isSubtype(tpe1, tpe2) => tpe2
          case (tpe1, tpe2, Contravariant) if isSubtype(tpe1, tpe2) => tpe1
          case (tpe1, tpe2, Covariant) if isSubtype(tpe2, tpe1) => tpe1
          case (tpe1, tpe2, Contravariant) if isSubtype(tpe2, tpe1) => tpe2

          case (ValueTypeApp(cons1, args1), ValueTypeApp(cons2, args2), _) =>
            if (cons1 != cons2) C.abort(s"Cannot merge different constructors")
            if (args1.size != args2.size) C.abort(s"Different count of argument to type constructor")

            // TODO Here we assume the constructor is invariant
            val mergedArgs = (args1 zip args2).map { case (t1, t2) =>
              merge(t1, t2, Invariant)
            }
            ValueTypeApp(cons1, mergedArgs)

          case _ =>
            C.abort(s"Cannot merge ${oldBound} with ${newBound} at ${polarity} polarity")
        }

      /**
       * Compute the join of two types
       */
      def mergeLower(oldBound: ValueType, newBound: ValueType): ValueType =
        merge(oldBound, newBound, Covariant)

      /**
       * Compute the meet of two types
       */
      def mergeUpper(oldBound: ValueType, newBound: ValueType): ValueType =
        merge(oldBound, newBound, Contravariant)
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
      Substitutions(
        values.view.mapValues { t => other.substitute(t) }.toMap,
        captures.view.mapValues { t => other.substitute(t) }.toMap) ++ other

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
    def substitute(t: Effects): Effects = Effects(t.toList.map(substitute))
    def substitute(t: Effect): Effect = t match {
      case t: Interface => t
      case t: BuiltinEffect => t
      case BlockTypeApp(cons, args) => BlockTypeApp(cons, args.map(substitute))
      case alias: EffectAlias => ???
    }

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

  case class BiSubstitutions(
    values: Map[TypeVar, (ValueType, ValueType)],
    captures: Map[Capture, (CaptureSet, CaptureSet)]
  ) {

    def isDefinedAt(t: TypeVar) = values.isDefinedAt(t)
    def isDefinedAt(c: Capture) = captures.isDefinedAt(c)

    def get(t: TypeVar)(using p: Polarity): Option[ValueType] = (values.get(t), p) match {
      case (Some((lower, upper)), Covariant) => Some(lower)
      case (Some((lower, upper)), Contravariant) => Some(upper)
      // here we assume that both bounds are equal (has to be checked before adding to the substitution)
      // hence we can use an arbitrary bounds
      case (Some((lower, upper)), Invariant) => Some(lower)
      case (None, _) => None
    }
    def get(c: Capture)(using p: Polarity): Option[CaptureSet] = (captures.get(c), p) match {
      case (Some((lower, upper)), Covariant) => Some(lower)
      case (Some((lower, upper)), Contravariant) => Some(upper)
      // here we assume that both bounds are equal (has to be checked before adding to the substitution)
      // hence we can use an arbitrary bounds
      case (Some((lower, upper)), Invariant) => Some(lower)
      case (None, _) => None
    }

    // amounts to first substituting this, then other
    def updateWith(other: BiSubstitutions): BiSubstitutions =
      substitute(other) ++ other

    // applies other to this
    def substitute(other: BiSubstitutions): BiSubstitutions =
      BiSubstitutions(
        values.view.mapValues { case (lower, upper) =>
          (other.substitute(lower)(using Covariant), other.substitute(upper)(using Contravariant))
        }.toMap,
        captures.view.mapValues { case (lower, upper) =>
          (other.substitute(lower)(using Covariant), other.substitute(upper)(using Contravariant))
        }.toMap
      )

    // amounts to parallel substitution
    def ++(other: BiSubstitutions): BiSubstitutions = BiSubstitutions(values ++ other.values, captures ++ other.captures)

    // shadowing
    private def without(tps: List[TypeVar], cps: List[Capture]): BiSubstitutions =
      BiSubstitutions(
        values.filterNot { case (t, _) => tps.contains(t) },
        captures.filterNot { case (t, _) => cps.contains(t) }
      )

    // TODO we DO need to distinguish between substituting unification variables for unification variables
    // and substituting concrete captures in unification variables... These are two fundamentally different operations.
    def substitute(c: CaptureSet)(using Polarity): CaptureSet = ???
    //    c.flatMap {
    //      // we are probably instantiating a function type
    //      case x: CaptureUnificationVar if captures.keys.exists(c => c.concrete) =>
    //        throw SubstitutionException(x, captures)
    //      case c => captures.getOrElse(c, CaptureSet(c))
    //    }

    def substitute(t: ValueType)(using Polarity): ValueType = t match {
      case x: TypeVar =>
        get(x).getOrElse(x)
      case ValueTypeApp(t, args) =>
        // TODO What about aliases and their variance?
        //   should we dealias first?
        //   Right now we treat it as covariant, which is not correct.
        ValueTypeApp(t, args.map { substitute })
      case BoxedType(tpe, capt) =>
        BoxedType(substitute(tpe), substitute(capt))
      case other => other
    }

    // TODO implement
    def substitute(t: Effects)(using Polarity): Effects = Effects(t.toList.map(substitute))
    def substitute(t: Effect)(using Polarity): Effect = t match {
      case t: Interface => t
      case t: BuiltinEffect => t
      case BlockTypeApp(cons, args) => BlockTypeApp(cons, args.map(substitute))
      case alias: EffectAlias => ???
    }

    def substitute(t: BlockType)(using Polarity): BlockType = t match {
      case e: InterfaceType => substitute(e)
      case b: FunctionType  => substitute(b)
    }

    def substitute(t: InterfaceType)(using Polarity): InterfaceType = t match {
      case b: Interface           => b
      case BlockTypeApp(c, targs) => BlockTypeApp(c, targs map substitute)
    }

    def substitute(t: FunctionType)(using p: Polarity): FunctionType = t match {
      case FunctionType(tps, cps, vps, bps, ret, eff) =>
        // do not substitute with types parameters bound by this function!
        val substWithout = without(tps, cps)
        FunctionType(
          tps,
          cps,
          vps.map { param => substWithout.substitute(param)(using p.flip) },
          bps.map { param => substWithout.substitute(param)(using p.flip) },
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
    def requireEqual(x: UnificationVar, tpe: ValueType): Unit
    def abort(msg: String): Nothing

    def unify(c1: CaptureSet, c2: CaptureSet): Unit
    def unify(c1: Capture, c2: Capture): Unit = unify(CaptureSet(Set(c1)), CaptureSet(Set(c2)))

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
        println(s"Type mismatch: ${t} is not ${s} (at polarity $p)")
        abort(s"Expected ${t}, but got ${s}")
    }

    def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType)(using p: Polarity): Unit = (tpe1, tpe2) match {
      case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s)
      case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s)
      case (t, s) => abort(s"Expected ${t}, but got ${s}")
    }

    def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType)(using p: Polarity): Unit = (tpe1, tpe2) match {
      case (t1: Interface, t2: Interface) => if (t1 != t2) abort(s"Expected ${t1}, but got ${t2}")
      // for now block type constructors are invariant
      case (BlockTypeApp(c1, targs1), BlockTypeApp(c2, targs2)) =>
        unifyInterfaceTypes(c2, c2)(using Invariant)
        (targs1 zip targs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }
      case _ => abort(s"Kind mismatch between ${tpe1} and ${tpe2}")
    }

    def unifyEffect(eff1: Effect, eff2: Effect)(using p: Polarity): Unit = (eff1, eff2) match {
      case (e1, e2) if e1 == e2 => ()
      case (BlockTypeApp(cons1, args1), BlockTypeApp(cons2, args2)) if cons1 == cons2 =>
        (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2)(using Invariant) }
      case _ => abort(s"Mismatch between ${eff1} and ${eff2}")
    }

    def unifyEffects(eff1: Effects, eff2: Effects)(using p: Polarity): Unit = ???

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
