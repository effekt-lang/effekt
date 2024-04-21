package effekt
package typer

import effekt.context.Context
import effekt.source.MatchPattern
import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TTop }
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
class Unification(using C: ErrorReporter) extends TypeUnifier, TypeMerger, TypeInstantiator { self =>

  // State of the unification engine
  // -------------------------------
  private var scope: Scope = GlobalScope
  protected var constraints = new Constraints


  // Creating fresh unification variables
  // ------------------------------------
  def freshTypeVar(underlying: TypeVar.TypeParam, call: source.Tree): UnificationVar = ???

  def freshCaptVar(role: CaptUnificationVar.Role): CaptUnificationVar = ???

  // Substitution
  // ------------
  def substitution = constraints.subst

  def apply(e: Effects): Effects = ???

  def apply(e: InterfaceType): InterfaceType = ???

  def apply(tpe: BlockType): BlockType = ???

  def apply(tpe: FunctionType): FunctionType = ???

  def apply(tpe: ValueType): ValueType = ???

  // Lifecycle management
  // --------------------
  def backup(): UnificationState = ???
  def restore(state: UnificationState): Unit = ()

  def init() = ()

  def enterScope() = ()

  def forceSolve(vars: List[CaptUnificationVar]) = ()

  def leaveScope(additional: List[CaptUnificationVar]) = ()

  def dumpConstraints() = ()


  def requireSubtype(t1: ValueType, t2: ValueType, errorContext: ErrorContext): Unit = ()

  def requireSubtype(t1: BlockType, t2: BlockType, errorContext: ErrorContext): Unit = ()

  def requireSubregion(c1: Captures, c2: Captures)(using C: Context): Unit = ()

  def requireSubregion(c1: Captures, c2: Captures, ctx: ErrorContext): Unit = ()
  def join(tpes: ValueType*): ValueType = ???

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: List[Capture])(using C: Context): Unit = ()
  def requireSubregionWithout(lower: Captures, upper: Captures, filter: List[Capture], ctx: ErrorContext): Unit = ()

  def requireSubregionWithout(lower: Captures, upper: Captures, filter: Set[Capture], ctx: ErrorContext): Unit = ()

  def without(caps: CaptUnificationVar, others: List[Capture]): Captures = ???
  def instantiate(tpe: FunctionType, targs: List[ValueType], cargs: List[Captures]): FunctionType = ???
  def instantiateFresh(tpe: FunctionType): (List[ValueType], List[Captures], FunctionType) = ???

  def abort(msg: String) = ???
  def abort(msg: String, ctx: ErrorContext) = ???

  def error(msg: String) = ()
  def error(msg: String, ctx: ErrorContext) = ()
  def error(left: symbols.Type, right: symbols.Type, ctx: ErrorContext) = ()

  def requireEqual(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()

  def requireLowerBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()

  def requireUpperBound(x: UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()

  def mergeCaptures(oldBound: Captures, newBound: Captures, ctx: ErrorContext): Captures = ???

  def mergeCaptures(cs: List[Captures], ctx: ErrorContext): Captures = ???

  def mergeCaptures(concreteBounds: List[Capture], variableBounds: List[CaptUnificationVar], ctx: ErrorContext): Captures = ???
}



case class Instantiation(values: Map[TypeVar, ValueType], captures: Map[Capture, Captures])

trait TypeInstantiator { self: Unification =>

  private def valueInstantiations(using i: Instantiation): Map[TypeVar, ValueType] = i.values
  private def captureInstantiations(using i: Instantiation): Map[Capture, Captures] = i.captures

  private def captureParams(using Instantiation) = captureInstantiations.keys.toSet

  // shadowing
  private def without(tps: List[TypeVar], cps: List[Capture])(using Instantiation): Instantiation =
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
          capt.captures
    }
    val contained = captureParams intersect concreteCapture // Should not contain CaptureOf
    if (contained.isEmpty) return CaptureSet(concreteCapture)

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
    case e: InterfaceType => instantiate(e)
    case b: FunctionType  => instantiate(b)
  }

  def instantiate(t: InterfaceType)(using Instantiation): InterfaceType = t match {
    case InterfaceType(c, targs) => InterfaceType(c, targs map instantiate)
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
