package effekt
package typer

import effekt.context.Context
import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TInt, TTop }
import effekt.util.messages.ErrorReporter


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
 *
 * Structural comparison of types are outsourced into [[TypeComparer]], [[TypeUnifier]], and
 * [[TypeMerger]]. This way, the dependencies are a bit clearer and testability is improved.
 *
 * TODO
 *   - [ ] All incoming types need to be "normalized": substituted and dealiased.
 */
class Unification(using C: ErrorReporter) extends TypeComparer, TypeUnifier, TypeMerger { self =>

  // State of the unification engine
  // -------------------------------
  private var scope: Scope = GlobalScope
  private [typer] var substitution = BiSubstitutions(Map.empty, Map.empty)
  private var constraints = new ConstraintSet


  // Creating fresh unification variables
  // ------------------------------------
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


  // Lifecycle management
  // --------------------
  def backup(): UnificationState = UnificationState(scope, constraints.clone(), substitution)
  def restore(state: UnificationState): Unit =
    scope = state.scope
    constraints = state.constraints
    substitution = state.substitution

  def enterScope() = {
    scope = LocalScope(Nil, Nil, scope)
  }

  def leaveScope() = {
    val LocalScope(types, captures, parent) = scope match {
      case GlobalScope => sys error "Cannot leave global scope"
      case l : LocalScope => l
    }
    scope = parent

    var subst = Map.empty[TypeVar, (ValueType, ValueType)]

    constraints.remove(types.toSet) foreach {
      case (nodes, Right((lower, upper))) =>
        subst = subst ++ nodes.map { x => x -> concretizeBounds(lower, upper) }.toMap
      case (nodes, Left(repr)) =>
        subst = subst ++ nodes.map { x => x -> (repr, repr) }.toMap
    }
    val subst1 = BiSubstitutions(subst, Map.empty)

    // apply substitution to itself to remove all occurrences of skolems
    val subst2 = subst1.substitute(subst1)
    substitution = substitution.updateWith(subst2)

    // apply substitution to bounds in remaining constraints
    constraints.mapBounds { case (lower, upper) =>
      (substitution.substitute(lower)(using Covariant), substitution.substitute(upper)(using Contravariant))
    }

    // constraints.dumpConstraints()
  }

  def dumpConstraints() = constraints.dumpConstraints()


  // Registering new constraints
  // ---------------------------

  /**
   * Checks whether t1 <: t2
   *
   * Has the side effect of registering constraints.
   *
   * TODO Covariant might not be the right polarity
   */
  def requireSubtype(t1: ValueType, t2: ValueType): Unit =
    given Polarity = Covariant;
    unifyValueTypes(
      substitution.substitute(t1),
      substitution.substitute(t2))

  def requireEqual(t1: ValueType, t2: ValueType): Unit =
    given Polarity = Invariant;
    unifyValueTypes(
      substitution.substitute(t1),
      substitution.substitute(t2))

  def requireSubtype(t1: BlockType, t2: BlockType): Unit =
    given Polarity = Covariant;
    unifyBlockTypes(
      substitution.substitute(t1),
      substitution.substitute(t2))

  def requireSubregion(c1: CaptureSet, c2: CaptureSet): Unit =
    sys error s"Requiring that ${c1} <:< ${c2}"

  def join(tpes: List[ValueType]): ValueType =
    tpes.foldLeft[ValueType](TBottom) { (t1, t2) => mergeValueTypes(t1, t2, Covariant) }


  // Using collected information
  // ---------------------------

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
    effSubst1.filterNot(eff1 => effSubst2.exists(eff2 => subEffect(eff2, eff1)))

  /**
   * Instantiate a typescheme with fresh, rigid type variables
   *
   * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
   */
  def instantiate(tpe: FunctionType, targs: List[ValueType]): (List[ValueType], List[CaptureUnificationVar], FunctionType) = {
    val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = substitution.substitute(tpe)(using Covariant)

    val typeRigids = if (targs.size == tparams.size) targs else tparams map { t => fresh(UnificationVar.TypeVariableInstantiation(t)) }

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


  // Implementation Details
  // ----------------------

  def hasLowerBound(x: UnificationVar, y: ValueType): Boolean = y match {
    case y: UnificationVar => constraints.isSupertypeOf(x, y)
    // it is compatible with the upper bounds on x
    case tpe => subValueType(tpe, constraints.upperBound(x))
  }
  def hasUpperBound(x: UnificationVar, y: ValueType): Boolean = y match {
    case y: UnificationVar => constraints.isSubtypeOf(x, y)
    case tpe => subValueType(tpe, constraints.lowerBound(x))
  }
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean = constraints.isEqual(x, y)

  private def concretizeBounds(lower: ValueType, upper: ValueType): (ValueType, ValueType) = (lower, upper) match {
    case (TBottom, TTop) => C.abort("Cannot infer type") // TODO move to right point
    case (TBottom, t) => (t, t)
    case (t, TTop)    => (t, t)
    case (lower, upper) => (lower, upper)
  }

  private def isLive(x: UnificationVar): Boolean = isLive(x, scope)

  private def isLive(x: UnificationVar, scope: Scope): Boolean = scope match {
    case GlobalScope => false
    case LocalScope(types, _, parent) => types.contains(x) || isLive(x, parent)
  }

  def unify(c1: CaptureSet, c2: CaptureSet): Unit = ???

  def abort(msg: String) = C.abort(msg)


  def requireEqual(x: UnificationVar, tpe: ValueType): Unit =
    requireLowerBound(x, tpe)
    requireUpperBound(x, tpe)

  /**
   * Value type [[tpe]] flows into the unification variable [[x]] as a lower bound.
   *
   *
   *                  ┏━━━━━━━━━━━━━━━━━━━━━━━┓
   *                  ┃           x           ┃
   *  --------------> ┠───────────┬───────────┨
   *    (1) tpe       ┃ Lower (2) │ Upper (3) ┃
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
  private def mergeAndUpdateLowerBound(x: UnificationVar, tpe: ValueType): Option[ValueType] =
    assert (!tpe.isInstanceOf[UnificationVar])
    val lower = constraints.lowerBound(x)
    if (x == tpe || lower == tpe || tpe == TBottom) return None;
    val newBound = mergeLower(lower, tpe)
    constraints.updateLowerBound(x, newBound)
    Some(newBound)

  private def mergeAndUpdateUpperBound(x: UnificationVar, tpe: ValueType): Option[ValueType] =
    assert (!tpe.isInstanceOf[UnificationVar])
    val upper = constraints.upperBound(x)
    if (x == tpe || upper == tpe || tpe == TTop) return None;
    val newBound = mergeUpper(upper, tpe)
    constraints.updateUpperBound(x, newBound)
    Some(newBound)

  private def connectNodes(x: UnificationVar, y: UnificationVar): Unit =
    if (x == y || (constraints.isSubtypeOf(x, y))) return;
    requireLowerBound(y, constraints.lowerBound(x)) // TODO maybe this can be mergeAndUpdateLowerBound
    requireUpperBound(x, constraints.upperBound(y))
    constraints.connect(x, y)
}