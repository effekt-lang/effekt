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
  private [typer] def substitution = constraints.subst
  private var constraints = new Equivalences
  private var captureConstraints = new CaptureConstraintGraph(Map.empty)


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
  // TODO implement: should apply everything we know up to this point.
  def apply(e: Effects): Effects =
    substitution.substitute(dealias(e))

  def apply(e: Effect): Effect =
    substitution.substitute(e)

  // Lifecycle management
  // --------------------
  def backup(): UnificationState = UnificationState(scope, constraints.clone())
  def restore(state: UnificationState): Unit =
    scope = state.scope
    constraints = state.constraints.clone()

  def init() =
    scope = GlobalScope
    constraints = new Equivalences
    captureConstraints = new CaptureConstraintGraph(Map.empty)

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
      if (!constraints.subst.isDefinedAt(x)) {
        x.role match {
          case UnificationVar.TypeVariableInstantiation(underlying, callTree) =>
            C.at(callTree) {
              C.error(s"Cannot infer type argument ${underlying}, maybe consider annotating it?")
            }
        }
      }
    }
  }

  def dumpConstraints() =
    constraints.dumpConstraints()
    captureConstraints.dumpConstraints()




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

  def requireSubregion(c1: Captures, c2: Captures): Unit =
    if (c1 == CaptureSet()) return;
    if (c1 == c2) return;
    (c1, c2) match {
      // TODO implement properly
      case (CaptureSet(cs1), CaptureSet(cs2)) =>
        val notAllowed = cs2 -- cs1
        if (notAllowed.nonEmpty) abort(s"The following captures are not allowed: ${notAllowed}")
      case (x: CaptUnificationVar, y: CaptUnificationVar) =>
        captureConstraints.connect(x, y)
      case (x: CaptUnificationVar, CaptureSet(cs)) =>
        val concrete = cs.collect {
          case c: CaptureParam => c
          case _ => ???
        }
        captureConstraints.requireUpper(concrete, x)
      case (CaptureSet(cs), x: CaptUnificationVar) =>
        val concrete = cs.collect {
          case c: CaptureParam => c
          case _ => ???
        }
        captureConstraints.requireLower(concrete, x)
    }

  def requireEqual(x: CaptUnificationVar, c: CaptureSet): Unit =
    println(s"Requiring that ${x} =:= ${c}")

  def join(tpes: ValueType*): ValueType =
    tpes.foldLeft[ValueType](TBottom) { (t1, t2) => mergeValueTypes(t1, dealias(t2), Covariant) }

  def join(caps: Captures*): Captures = ???

  def without(caps: Captures, others: List[CaptureParam]): Captures =
    caps match {
      case CaptureSet(cs) => CaptureSet(cs -- others.toSet)
      case x: CaptUnificationVar => freshCaptVar(CaptUnificationVar.Subtraction(others, x))
    }

  // Learning new subtyping / regioning information
  // ----------------------------------------------
  //def learnSub()

  // Using collected information
  // ---------------------------

  /**
   * Instantiate a typescheme with fresh, rigid type variables
   *
   * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
   *
   * Also returns the list of effects in canonical ordering, after dealiasing.
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

  def unify(c1: Captures, c2: Captures): Unit =
    println(s"Unifiying ${c1} and ${c2}")

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
}



case class Instantiation(values: Map[TypeVar, ValueType], captures: Map[CaptureParam, CaptUnificationVar])

trait TypeInstantiator {

  def abort(msg: String): Nothing

  private def valueInstantiations(using i: Instantiation): Map[TypeVar, ValueType] = i.values
  private def captureInstantiations(using i: Instantiation): Map[CaptureParam, CaptUnificationVar] = i.captures

  private def captureParams(using Instantiation) = captureInstantiations.keys.toSet

  /**
   * Should create a fresh unification variable bounded by the given captures
   */
  def mergeCaptures(concreteBounds: List[CaptureParam], variableBounds: List[CaptUnificationVar]): CaptUnificationVar =
    println(s"Merging captures: ${concreteBounds} and ${variableBounds} into a new unification variable")
    ???

  // shadowing
  private def without(tps: List[TypeVar], cps: List[CaptureParam])(using Instantiation): Instantiation =
    Instantiation(
      valueInstantiations.filterNot { case (t, _) => tps.contains(t) },
      captureInstantiations.filterNot { case (t, _) => cps.contains(t) }
    )

  // TODO we DO need to distinguish between substituting unification variables for unification variables
  // and substituting concrete captures in unification variables... These are two fundamentally different operations.
  def instantiate(c: Captures)(using Instantiation): Captures = c match {
    case CaptureSet(cs) =>
      val contained = captureParams intersect cs.asInstanceOf[Set[CaptureParam]] // Should not contain CaptureOf
      if (contained.isEmpty) return c;

      val others = (cs -- captureParams).toList.map {
        case c: CaptureParam => c
        case c: CaptureOf => ???
      }

      // TODO do we need to respect the polarity here?
      mergeCaptures(others, contained.toList.map { p => captureInstantiations(p) })

    case _ => abort("Effect polymorphic first-class functions need to be annotated, at the moment.")
  }

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