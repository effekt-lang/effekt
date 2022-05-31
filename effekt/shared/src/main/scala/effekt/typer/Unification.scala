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
  constraints: Equivalences
)

sealed trait Scope
case object GlobalScope extends Scope
case class LocalScope(
  types: List[UnificationVar],
  captures: List[CaptureUnificationVar],
  parent: Scope) extends Scope

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
  private [typer] def substitution = constraints.subst
  private var constraints = new Equivalences


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

  // Substitution
  // ------------
  // TODO implement: should apply everything we know up to this point.
  def apply(e: Effects): Effects =
    substitution.substitute(e)

  // Lifecycle management
  // --------------------
  def backup(): UnificationState = UnificationState(scope, constraints.clone())
  def restore(state: UnificationState): Unit =
    scope = state.scope
    constraints = state.constraints

  def init() =
    scope = GlobalScope
    constraints = new Equivalences

  def enterScope() = {
    scope = LocalScope(Nil, Nil, scope)
  }

  def leaveScope() = {
    val LocalScope(types, captures, parent) = scope match {
      case GlobalScope => sys error "Cannot leave global scope"
      case l : LocalScope => l
    }
    scope = parent


    // TODO check that the substitution is defined for them.
    types foreach { x =>
      if (!constraints.subst.isDefinedAt(x))
        abort(s"The type of ${x} could not be inferred.")
    }
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
   * Instantiate a typescheme with fresh, rigid type variables
   *
   * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
   */
  def instantiate(tpe: FunctionType, targs: List[ValueType]): (List[ValueType], List[CaptureUnificationVar], FunctionType) = {
    val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = substitution.substitute(tpe)

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

  // We should ALWAYS apply the substitutions, before calling any of the methods below.

  def hasLowerBound(x: UnificationVar, y: ValueType): Boolean = y match {
    case y: UnificationVar => constraints.isEqual(x, y)
    // it is compatible with the upper bounds on x
    case tpe =>
      val knownType = constraints.typeOf(x).getOrElse {
        abort(s"Cannot compare ${x} with ${y}, since type of ${x} is not known, yet.")
      }
      subValueType(tpe, knownType)
  }
  def hasUpperBound(x: UnificationVar, y: ValueType): Boolean = y match {
    case y: UnificationVar => constraints.isEqual(x, y)
    case tpe =>
      val knownType = constraints.typeOf(x).getOrElse {
        abort(s"Cannot compare ${x} with ${y}, since type of ${x} is not known, yet.")
      }
      subValueType(tpe, knownType)
  }
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean = constraints.isEqual(x, y)

  def unify(c1: CaptureSet, c2: CaptureSet): Unit =
    println(s"Unifiying ${c1} and ${c2}")

  def abort(msg: String) = C.abort(msg)


  def requireEqual(x: UnificationVar, tpe: ValueType): Unit =
    requireLowerBound(x, tpe)
    requireUpperBound(x, tpe)

  def requireLowerBound(x: UnificationVar, tpe: ValueType) =
    given Polarity = Invariant
    constraints.learn(x, tpe)(unifyValueTypes)

  def requireUpperBound(x: UnificationVar, tpe: ValueType) =
    given Polarity = Invariant
    constraints.learn(x, tpe)(unifyValueTypes)
}
