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
  constraints: Constraints
)

sealed trait Scope
case object GlobalScope extends Scope
case class LocalScope(
  types: List[UnificationVar],
  captures: List[CaptUnificationVar],
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
class Unification(using C: ErrorReporter) extends TypeComparer, TypeUnifier, TypeMerger, TypeInstantiator { self =>

  // State of the unification engine
  // -------------------------------
  private var scope: Scope = GlobalScope
  protected var constraints = new Constraints


  // Creating fresh unification variables
  // ------------------------------------
  def fresh(role: UnificationVar.Role): UnificationVar = scope match {
    case GlobalScope => sys error "Cannot add unification variables to global scope"
    case s : LocalScope =>
      val x = new UnificationVar(role)
      scope = s.copy(types = x :: s.types)
      x
  }

  def freshCaptVar(role: CaptUnificationVar.Role): CaptUnificationVar = scope match {
    case GlobalScope => sys error "Cannot add unification variables to global scope"
    case s : LocalScope =>
      val x = CaptUnificationVar(role)
      scope = s.copy(captures = x :: s.captures)
      x
  }

  // Substitution
  // ------------
  def substitution = constraints.subst

  def apply(e: Effects): Effects =
    substitution.substitute(dealias(e))

  def apply(e: Effect): Effect =
    substitution.substitute(e)

  def apply(tpe: BlockType): BlockType =
    substitution.substitute(dealias(tpe))

  def apply(tpe: ValueType): ValueType =
    substitution.substitute(dealias(tpe))

  // Lifecycle management
  // --------------------
  def backup(): UnificationState = UnificationState(scope, constraints.clone())
  def restore(state: UnificationState): Unit =
    scope = state.scope
    constraints = state.constraints.clone()

  def init() =
    scope = GlobalScope
    constraints = new Constraints

  def enterScope() = {
    scope = LocalScope(Nil, Nil, scope)
  }

  def leaveScope(additional: List[CaptUnificationVar]) = {
    val LocalScope(types, captures, parent) = scope match {
      case GlobalScope => sys error "Cannot leave global scope"
      case l : LocalScope => l
    }
    scope = parent

    constraints.leave(types, captures ++ additional)
  }

  def dumpConstraints() =
    constraints.dumpConstraints()




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
      dealias(substitution.substitute(t1)),
      dealias(substitution.substitute(t2)))

  def requireEqual(t1: ValueType, t2: ValueType): Unit =
    given Polarity = Invariant;
    unifyValueTypes(
      dealias(substitution.substitute(t1)),
      dealias(substitution.substitute(t2)))

  def requireSubtype(t1: BlockType, t2: BlockType): Unit =
    given Polarity = Covariant;
    unifyBlockTypes(
      dealias(substitution.substitute(t1)),
      dealias(substitution.substitute(t2)))

  def requireSubregion(c1: Captures, c2: Captures): Unit = requireSubregionWithout(c1, c2, Set.empty)

  def join(tpes: ValueType*): ValueType =
    tpes.foldLeft[ValueType](TBottom) { (t1, t2) => mergeValueTypes(t1, dealias(t2), Covariant) }

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: List[CaptureParam]): Unit =
    requireSubregionWithout(lower, upper, filter.toSet)

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: Set[CaptureParam]): Unit =
    if (lower == CaptureSet()) return;
    if (lower == upper) return;
    (lower, upper) match {
      case (CaptureSet(lows), CaptureSet(ups)) =>
        val notAllowed = (ups ++ filter) -- lows
        if (notAllowed.nonEmpty) abort(s"The following captures are not allowed: ${notAllowed}")
      case (x: CaptUnificationVar, y: CaptUnificationVar) =>
        constraints.connect(x, y, filter)
      case (x: CaptUnificationVar, CaptureSet(cs)) =>
        constraints.requireUpper(cs ++ filter, x)
      case (CaptureSet(cs), x: CaptUnificationVar) =>
        constraints.requireLower(cs -- filter, x)
    }

  def without(caps: CaptUnificationVar, others: List[CaptureParam]): Captures =
    if (others.isEmpty) caps else caps match {
      case x: CaptUnificationVar =>
        val y = freshCaptVar(CaptUnificationVar.Subtraction(others, x))
        constraints.connect(y, x, others.toSet)
        y
    }

  // Using collected information
  // ---------------------------

  /**
   * Instantiate a typescheme with fresh, rigid type variables
   *
   * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
   *
   * Also returns the list of effects in canonical ordering, after dealiasing.
   *
   * TODO also create capture unification variables for (inferred) capability arguments.
   */
  def instantiate(tpe: FunctionType, targs: List[ValueType]): (List[ValueType], List[CaptUnificationVar], List[Effect], FunctionType) = {
    val position = C.focus
    val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = substitution.substitute(tpe)

    val typeRigids = if (targs.size == tparams.size) targs else tparams map { t => fresh(UnificationVar.TypeVariableInstantiation(t, position)) }

    val captRigids = cparams map { param => freshCaptVar(CaptUnificationVar.VariableInstantiation(param, position)) }

    given Instantiation = Instantiation((tparams zip typeRigids).toMap, (cparams zip captRigids).toMap)

    val substitutedVparams = vparams map instantiate
    val substitutedBparams = bparams map instantiate
    val substitutedReturn = instantiate(ret)
    val dealiasedEffs = eff.toList.flatMap(dealias).distinct

    val substitutedEffects = dealiasedEffs map instantiate

    (typeRigids, captRigids, substitutedEffects, FunctionType(Nil, Nil, substitutedVparams, substitutedBparams, substitutedReturn, Effects(substitutedEffects)))
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

  def isSubset(xs: Captures, ys: Captures): Boolean = constraints.isSubset(xs, ys)

  def abort(msg: String) = C.abort(msg)

  def error(msg: String) = C.error(msg)


  def requireEqual(x: UnificationVar, tpe: ValueType): Unit =
    requireLowerBound(x, tpe)
    requireUpperBound(x, tpe)

  def requireLowerBound(x: UnificationVar, tpe: ValueType) =
    given Polarity = Invariant
    constraints.learn(x, tpe)(unifyValueTypes)

  def requireUpperBound(x: UnificationVar, tpe: ValueType) =
    given Polarity = Invariant
    constraints.learn(x, tpe)(unifyValueTypes)

  def mergeCaptures(oldBound: Captures, newBound: Captures, polarity: Polarity): Captures = (oldBound, newBound, polarity) match {
    case (CaptureSet(xs), CaptureSet(ys), Covariant) => CaptureSet(xs intersect ys)
    case (CaptureSet(xs), CaptureSet(ys), Contravariant) => CaptureSet(xs union ys)
    case (CaptureSet(xs), CaptureSet(ys), Invariant) => if (xs == ys) oldBound else abort(s"Capture set ${xs} is not equal to ${ys}")
    case (x: CaptUnificationVar, CaptureSet(ys), p) => mergeCaptures(ys.toList, List(x), p)
    case (CaptureSet(xs), y: CaptUnificationVar, p) => mergeCaptures(xs.toList, List(y), p)
    case (x: CaptUnificationVar, y: CaptUnificationVar, p) => mergeCaptures(Nil, List(x, y), p)
  }

   /**
   * Should create a fresh unification variable bounded by the given captures
   */
  def mergeCaptures(concreteBounds: List[CaptureParam], variableBounds: List[CaptUnificationVar], polarity: Polarity): CaptUnificationVar =
    println(s"Merging captures: ${concreteBounds} and ${variableBounds} into a new unification variable")
    val newVar = freshCaptVar(CaptUnificationVar.Substitution())
    polarity match {
      case Covariant =>
        constraints.requireLower(concreteBounds.toSet, newVar)
        variableBounds.foreach { b => constraints.connect(b, newVar, Set.empty) }
      case Contravariant =>
        constraints.requireUpper(concreteBounds.toSet, newVar)
        variableBounds.foreach { b => constraints.connect(newVar, b, Set.empty) }
      case Invariant =>
        constraints.requireLower(concreteBounds.toSet, newVar)
        constraints.requireUpper(concreteBounds.toSet, newVar)
        variableBounds.foreach { b => constraints.connect(newVar, b, Set.empty) }
        variableBounds.foreach { b => constraints.connect(b, newVar, Set.empty) }
    }

    newVar
}



case class Instantiation(values: Map[TypeVar, ValueType], captures: Map[CaptureParam, CaptUnificationVar])

trait TypeInstantiator { self: Unification =>

  private def valueInstantiations(using i: Instantiation): Map[TypeVar, ValueType] = i.values
  private def captureInstantiations(using i: Instantiation): Map[CaptureParam, CaptUnificationVar] = i.captures

  private def captureParams(using Instantiation) = captureInstantiations.keys.toSet

  // shadowing
  private def without(tps: List[TypeVar], cps: List[CaptureParam])(using Instantiation): Instantiation =
    Instantiation(
      valueInstantiations.filterNot { case (t, _) => tps.contains(t) },
      captureInstantiations.filterNot { case (t, _) => cps.contains(t) }
    )

  def instantiate(c: Captures)(using Instantiation): Captures =
    val concreteCapture =
      c match {
        case CaptureSet(cs) => cs

        // Right now, we can only substitute into concrete capture sets, not unification variables
        // since we only have negation nodes, but no substitution nodes.
        //
        // we *could* use the fact that some variables cannot occur lexically in the variable (depending on the scope
        // it was introduced in).
        //
        // right now, we force solving to continue, instead of giving up.
        case x: CaptUnificationVar =>
          val capt = constraints.forceSolving(x)
          println(s"Force solving ${x} resulted in ${capt}")
          capt.captures
    }
    val contained = captureParams intersect concreteCapture // Should not contain CaptureOf
    if (contained.isEmpty) return c;

    val remaining = (concreteCapture -- captureParams).toList

    // TODO do we need to respect the polarity here? Maybe wellformedness should exclude captures in negative position?
    mergeCaptures(remaining, contained.toList.map { p => captureInstantiations(p) }, Covariant)


  def instantiate(t: ValueType)(using Instantiation): ValueType = t match {
    case x: TypeVar =>
      valueInstantiations.getOrElse(x, x)
    case ValueTypeApp(t, args) =>
      ValueTypeApp(t, args.map { instantiate })
    case BoxedType(tpe, capt) =>
      BoxedType(instantiate(tpe), instantiate(capt))
    case other => other
  }

  def instantiate(t: Effects)(using Instantiation): Effects = Effects(t.toList.map(instantiate))
  def instantiate(t: Effect)(using i: Instantiation): Effect = t match {
    case t: Interface => t
    case t: BuiltinEffect => t
    case BlockTypeApp(cons, args) => BlockTypeApp(cons, args.map(instantiate))
    case EffectAlias(name, tparams, effs) =>
      given Instantiation = without(tparams, Nil)(using i)
      EffectAlias(name, tparams, instantiate(effs))
  }

  def instantiate(t: BlockType)(using Instantiation): BlockType = t match {
    case e: InterfaceType => instantiate(e)
    case b: FunctionType  => instantiate(b)
  }

  def instantiate(t: InterfaceType)(using Instantiation): InterfaceType = t match {
    case b: Interface           => b
    case BlockTypeApp(c, targs) => BlockTypeApp(c, targs map instantiate)
    case b: BuiltinEffect => b
  }

  def instantiate(t: FunctionType)(using i: Instantiation): FunctionType = t match {
    case FunctionType(tps, cps, vps, bps, ret, eff) =>
      // do not substitute with types parameters bound by this function!
      given Instantiation = without(tps, cps)(using i)
      FunctionType(
        tps,
        cps,
        vps map instantiate,
        bps map instantiate,
        instantiate(ret),
        instantiate(eff))
  }
}