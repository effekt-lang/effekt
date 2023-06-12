package effekt
package typer

import effekt.context.Context
import effekt.source.BlockType.BlockTypeRef
import effekt.source.MatchPattern
import effekt.symbols.*
import effekt.symbols.BlockTypeVar.{BlockTypeWildcard, BlockUnificationVar}
import effekt.symbols.builtins.{TBottom, TTop}
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
   constraints: Constraints,
   valueWildcardMap : Map[TypeVar, UnificationVar],
   blockWildcardMap : Map[BlockTypeVar, BlockUnificationVar]
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
class Unification(using C: ErrorReporter) extends TypeUnifier, TypeMerger, TypeInstantiator { self =>

  // State of the unification engine
  // -------------------------------
  private var scope: Scope = GlobalScope
  protected var constraints = new Constraints
  private var valueWildcardMap = Map.empty[TypeVar, UnificationVar]
  private var blockWildcardMap = Map.empty[BlockTypeVar, BlockUnificationVar]

  // Creating fresh unification variables
  // ------------------------------------
  def fresh(underlying: TypeVar.TypeParam, call: source.Tree): UnificationVar = scope match {
    case GlobalScope => sys error "Cannot add unification variables to global scope"
    case s : LocalScope =>
      val x = new UnificationVar(underlying, call)
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
  def substitution =
    val s: Substitutions = constraints.subst
    val valueWildcardSubValue: Map[TypeVar, ValueType] = valueWildcardMap.filter((k, v) => s.values.contains(v))
      .map((k, v) => (k, s.values.get(v).get))
    val blockWildcardSubValue: Map[BlockTypeVar, BlockType] = blockWildcardMap.filter((k, v) => s.blocks.contains(v))
      .map((k, v) => (k, s.blocks.get(v).get))

    s.updateWith(Substitutions(valueWildcardSubValue, blockWildcardSubValue, s.captures))

  def apply(e: Effects): Effects =
    substitution.substitute(e)

  def apply(e: InterfaceType): InterfaceType =
    substitution.substitute(e)

  def apply(tpe: BlockType): BlockType =
    substitution.substitute(tpe)

  def apply(tpe: FunctionType): FunctionType =
    substitution.substitute(tpe)

  def apply(tpe: ValueType): ValueType =
    substitution.substitute(tpe)

  // Lifecycle management
  // --------------------
  def backup(): UnificationState = UnificationState(scope, constraints.clone(), valueWildcardMap, blockWildcardMap)
  def restore(state: UnificationState): Unit =
    scope = state.scope
    constraints = state.constraints.clone()
    valueWildcardMap = state.valueWildcardMap
    blockWildcardMap = state.blockWildcardMap

  def init() =
    scope = GlobalScope
    constraints = new Constraints
    valueWildcardMap = Map.empty[TypeVar, UnificationVar]
    blockWildcardMap = Map.empty[BlockTypeVar, BlockUnificationVar]

  def enterScope() = {
    scope = LocalScope(Nil, Nil, scope)
  }

  def forceSolve(vars: List[CaptUnificationVar]) = {
    constraints.leave(Nil, vars)
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
   */
  def requireSubtype(t1: ValueType, t2: ValueType, errorContext: ErrorContext): Unit =
    unifyValueTypes(
      substitution.substitute(t1),
      substitution.substitute(t2),
      errorContext)

  def requireSubtype(t1: BlockType, t2: BlockType, errorContext: ErrorContext): Unit =
    unifyBlockTypes(
      substitution.substitute(t1),
      substitution.substitute(t2),
      errorContext)

  def requireSubregion(c1: Captures, c2: Captures)(using C: Context): Unit =
    requireSubregion(c1, c2, ErrorContext.CaptureFlow(c1, c2, C.focus))

  def requireSubregion(c1: Captures, c2: Captures, ctx: ErrorContext): Unit = requireSubregionWithout(c1, c2, Set.empty, ctx)

  /**
   * Computes the join of all types, only called to merge the different arms of if and match
   */
  def join(tpes: ValueType*): ValueType =
    tpes.foldLeft[ValueType](TBottom) { (t1, t2) =>
      mergeValueTypes(t1, t2, ErrorContext.MergeTypes(apply(t1), apply(t2)))
    }

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: List[Capture])(using C: Context): Unit =
    requireSubregionWithout(lower, upper, filter.toSet, ErrorContext.CaptureFlow(lower, upper, C.focus))

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: List[Capture], ctx: ErrorContext): Unit =
    requireSubregionWithout(lower, upper, filter.toSet, ctx)

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: Set[Capture], ctx: ErrorContext): Unit =
    if (lower == CaptureSet()) return;
    if (lower == upper) return;
    (lower, upper) match {
      case (CaptureSet(lows), CaptureSet(ups)) =>
        val notAllowed = lows -- (ups ++ filter)
        if (notAllowed.nonEmpty)
          error(pp"The following captures are not allowed: ${CaptureSet(notAllowed)}", ctx)
      case (x: CaptUnificationVar, y: CaptUnificationVar) =>
        constraints.connect(x, y, filter)
      case (x: CaptUnificationVar, CaptureSet(cs)) =>
        constraints.requireUpper(cs ++ filter, x)
      case (CaptureSet(cs), x: CaptUnificationVar) =>
        constraints.requireLower(cs -- filter, x)
    }

  def without(caps: CaptUnificationVar, others: List[Capture]): Captures =
    if (others.isEmpty) caps else caps match {
      case x: CaptUnificationVar =>
        val y = freshCaptVar(CaptUnificationVar.Subtraction(others, x))
        constraints.connect(y, x, others.toSet)
        y
    }

  def unificationVarFromWildcard(wildcard: TypeVar): UnificationVar =
    valueWildcardMap.getOrElse(wildcard, {
      val unificationVar: UnificationVar = UnificationVar(TypeParam(NoName), null)
      valueWildcardMap = valueWildcardMap + (wildcard -> unificationVar)
      unificationVar
    })

  def unificationVarFromWildcard(wildcard: BlockTypeVar): BlockUnificationVar =
    blockWildcardMap.getOrElse(wildcard, {
      val unificationVar: BlockUnificationVar = BlockUnificationVar(TypeParam(NoName), null)
      blockWildcardMap = blockWildcardMap + (wildcard -> unificationVar)
      unificationVar
    })


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
  def instantiate(tpe: FunctionType, targs: List[ValueType], cargs: List[Captures]): (List[ValueType], List[Captures], FunctionType) = {
    val position = C.focus
    val FunctionType(tparams, cparams, vparams, bparams, ret, eff) = substitution.substitute(tpe)

    val typeRigids =
      if (targs.size == tparams.size) targs
      else tparams map { t => ValueTypeRef(fresh(t, position)) }

    if (cparams.size != (bparams.size + eff.canonical.size)) {
      sys error pp"Capture param count ${cparams.size} is not equal to bparam ${bparams.size} + controleffects ${eff.canonical.size}.\n  ${tpe}"
    }

    val captRigids =
      if (cargs.size == cparams.size) cargs
      else cparams.map { param => freshCaptVar(CaptUnificationVar.VariableInstantiation(param, position)) }

    given Instantiation = Instantiation((tparams zip typeRigids).toMap, Map.empty, (cparams zip captRigids).toMap) // Is Map.empty the right choice?

    val substitutedVparams = vparams map instantiate
    val substitutedBparams = bparams map instantiate
    val substitutedReturn = instantiate(ret)

    val substitutedEffects = instantiate(eff)

    val fun: FunctionType = FunctionType(Nil, Nil, substitutedVparams, substitutedBparams, substitutedReturn, substitutedEffects)
    (typeRigids, captRigids, fun)
  }


  // Implementation Details
  // ----------------------

  def abort(msg: String) = C.abort(msg)
  def abort(msg: String, ctx: ErrorContext) = C.abort(ErrorContext.explainInContext(msg, ctx))

  def error(msg: String) = C.error(msg)
  def error(msg: String, ctx: ErrorContext) = C.error(ErrorContext.explainInContext(msg, ctx))
  def error(left: symbols.Type, right: symbols.Type, ctx: ErrorContext) = C.error(ErrorContext.explainMismatch(left, right, ctx))

  def requireEqual(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit =
    requireLowerBound(x, tpe, ctx)
    requireUpperBound(x, tpe, ctx)

  def requireEqual(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext): Unit =
    requireLowerBound(x, tpe, ctx)
    requireUpperBound(x, tpe, ctx)

  def requireLowerBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext) =
    constraints.learn(x, tpe)((tpe1, tpe2) => unifyValueTypes(tpe1, tpe2, ErrorContext.MergeInvariant(ctx)))

  def requireLowerBound(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext) =
    constraints.learn(x, tpe)((tpe1, tpe2) => unifyBlockTypes(tpe1, tpe2, ErrorContext.MergeInvariant(ctx)))

  def requireUpperBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext) =
    constraints.learn(x, tpe)((tpe1, tpe2) => unifyValueTypes(tpe1, tpe2, ErrorContext.MergeInvariant(ctx)))

  def requireUpperBound(x: BlockUnificationVar, tpe: BlockType, ctx: ErrorContext) =
    constraints.learn(x, tpe)((tpe1, tpe2) => unifyBlockTypes(tpe1, tpe2, ErrorContext.MergeInvariant(ctx)))

  def mergeCaptures(oldBound: Captures, newBound: Captures, ctx: ErrorContext): Captures = (oldBound, newBound, ctx.polarity) match {
    case (CaptureSet(xs), CaptureSet(ys), Covariant) => CaptureSet(xs intersect ys)
    case (CaptureSet(xs), CaptureSet(ys), Contravariant) => CaptureSet(xs union ys)
    case (CaptureSet(xs), CaptureSet(ys), Invariant) => if (xs == ys) oldBound else abort(pp"Capture set ${CaptureSet(xs)} is not equal to ${CaptureSet(ys)}", ctx)
    case (x: CaptUnificationVar, CaptureSet(ys), p) if ys.isEmpty => x
    case (CaptureSet(xs), y: CaptUnificationVar, p) if xs.isEmpty => y
    case (x: CaptUnificationVar, CaptureSet(ys), p) => mergeCaptures(ys.toList, List(x), ctx)
    case (CaptureSet(xs), y: CaptUnificationVar, p) => mergeCaptures(xs.toList, List(y), ctx)
    case (x: CaptUnificationVar, y: CaptUnificationVar, p) => mergeCaptures(Nil, List(x, y), ctx)
  }

  def mergeCaptures(cs: List[Captures], ctx: ErrorContext): CaptUnificationVar =
    val (concrete, variables) = cs.partitionMap {
      case CaptureSet(xs) => Left(xs)
      case x: CaptUnificationVar => Right(x)
    }
    mergeCaptures(concrete.flatten, variables, ctx)

   /**
   * Should create a fresh unification variable bounded by the given captures
   */
  def mergeCaptures(concreteBounds: List[Capture], variableBounds: List[CaptUnificationVar], ctx: ErrorContext): CaptUnificationVar =
    val newVar = freshCaptVar(CaptUnificationVar.Substitution())
    ctx.polarity match {
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



case class Instantiation(values: Map[TypeVar, ValueType], blocks : Map[BlockTypeVar, BlockType], captures: Map[Capture, Captures])

trait TypeInstantiator { self: Unification =>

  private def valueInstantiations(using i: Instantiation): Map[TypeVar, ValueType] = i.values

  private def blockInstatiations(using i : Instantiation) : Map[BlockTypeVar, BlockType] = i.blocks
  private def captureInstantiations(using i: Instantiation): Map[Capture, Captures] = i.captures

  private def captureParams(using Instantiation) = captureInstantiations.keys.toSet

  // shadowing
  private def without(tps: List[TypeVar], bps : List[BlockTypeVar], cps: List[Capture])(using Instantiation): Instantiation =
    Instantiation(
      valueInstantiations.filterNot { case (t, _) => tps.contains(t) },
      blockInstatiations.filterNot { case (t, _) =>  tps.contains(t) },
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
          capt.captures
    }
    val contained = captureParams intersect concreteCapture // Should not contain CaptureOf
    if (contained.isEmpty) return c;

    val remaining = (concreteCapture -- captureParams).toList

    // TODO do we need to respect the polarity here? Maybe wellformedness should exclude captures in negative position?
    mergeCaptures(CaptureSet(remaining) :: contained.toList.map { p => captureInstantiations(p) }, ErrorContext.MergeCaptures())


  def instantiate(t: ValueType)(using Instantiation): ValueType = t match {
    case ValueTypeRef(x) =>
      valueInstantiations.getOrElse(x, t)
    case ValueTypeApp(t, args) =>
      ValueTypeApp(t, args.map { instantiate })
    case BoxedType(tpe, capt) =>
      BoxedType(instantiate(tpe), instantiate(capt))
  }

  def instantiate(t: Effects)(using Instantiation): Effects = Effects(t.toList.map(instantiate))

  def instantiate(t: BlockType)(using Instantiation): BlockType = t match {
    case BlockType.BlockTypeRef(x : BlockTypeVar) => blockInstatiations.getOrElse(x, t)
    case e: InterfaceType => instantiate(e)
    case b: FunctionType  => instantiate(b)
  }

  def instantiate(t: InterfaceType)(using Instantiation): InterfaceType = t match {
    case InterfaceType(c, targs) => InterfaceType(c, targs map instantiate)
  }

  def instantiate(t: FunctionType)(using i: Instantiation): FunctionType = t match {
    case FunctionType(tps, cps, vps, bps, ret, eff) =>
      // do not substitute with types parameters bound by this function!
      given Instantiation = without(tps, List(), cps)(using i) // Is List() the right choice?
      FunctionType(
        tps,
        cps,
        vps map instantiate,
        bps map instantiate,
        instantiate(ret),
        instantiate(eff))
  }
}