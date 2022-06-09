package effekt
package typer

import effekt.symbols.*
import effekt.util.messages.ErrorReporter

// Auxiliary Definitions
// ---------------------

private[typer]
class Node

private[typer]
type CNode = CaptUnificationVar

// non present filter stands for "everything is passed through"
private[typer]
type Filter = Set[CaptureParam]

private[typer]
case class CaptureNodeData(
  // non present bounds represent bottom (the empty set)
  lower: Option[Set[CaptureParam]],
  // non present bounds represent top (the universal set)
  upper: Option[Set[CaptureParam]],
  lowerNodes: Map[CNode, Filter],
  upperNodes: Map[CNode, Filter]
)
private val emptyData = CaptureNodeData(None, None, Map.empty, Map.empty)

private[typer]
type CaptureConstraints = Map[CNode, CaptureNodeData]

/**
 * A network of constraints.
 *
 * We distinguish between type constraints and capture constraints.
 *
 * Type Constraints
 * ----------------
 * Right now, we are implementing a simplified version of subtyping. Whenever two
 * unification variables are required to be in a relation (subtyping, equivalence, ...) we
 * conservatively assume they have to be **equal**.
 *
 * Whenever we learn something about a variable (such as the upper type bound), we immediately
 * solve the variable and all of the other variables in the same class and compute a
 * substitution ([[typeSubstitution]]).
 *
 * To record that two variables are equal, we associate them with the same [[Node]]
 * and store them in an equivalence class [[classes]].
 *
 * Invariants:
 * - substitutions should never refer to variables that we already solved.
 *   (they should have been substituted away)
 * - all types in an equivalence class have the same substitution; thus everytime we
 *   learn something about one of them it needs to be compatible with the others.
 *
 * Capture Constraints
 * -------------------
 * Constraints on capture unification variables are represented by a network of nodes:
 *                    ┏━━━━━━━━━━━━━━━━━━┓
 *    --------------> ┃  ?MyCaptureVar   ┃ -------------->
 *     lower nodes    ┠─────────┬────────┨   upper nodes
 *    --------------> ┃  Lower  │  Upper ┃ -------------->
 *                    ┗━━━━━━━━━┷━━━━━━━━┛
 *
 * Such nodes in the graph are represented by instances of [[CaptureNodeData]].
 * Lower and upper bounds ([[CaptureNodeData.lower]], and [[CaptureNodeData.upper]]) on a
 * node are always fully known sets of type [[CaptureParam]].
 *
 * The arrows in the picture above indicate the subcapturing relationship.
 * They are in fact navigatable in both directions, since new _upper_ bounds
 * need to flow through the node from right-to-left. The are represented
 * as fields [[CaptureNodeData.lowerNodes]] and [[CaptureNodeData.lowerNodes]].
 *
 * Connections to other nodes are potentially "filtered" ([[Filter]]). That is,
 * newly learnt bounds only flow to the related nodes modulo filtering. This is
 * necessary to implement subtraction of concrete captures.
 *
 * Invariants:
 * - Intra-Consistency: Lower is always consistent to Upper
 * - Inter-Consistency: if two nodes ?S <: ?T, then the lower bound of ?S has been
 *     propagated as lower bound to ?T and the upper bound of ?T has been propagated
 *     as upper bound of ?S.
 *
 * - Transitivity: The bounds are always fully propagated to all connected nodes.
 *
 * However, for now, we do not compute equivalence classes -- we also do not establish transitive connections.
 */
class Constraints(
  /**
   * Everything we know so far -- right hand sides of substitutions are *always* concrete types.
   * They can, however, mention unification variables. e.g. `?U !-> List[?S]` is allowed.
   * The mentioned unification variables (`?S` in the example) must not have a substitution.
   * They can be part of an equivalence class.
   *
   * Once one member of the equivalence class becomes concrete, all members are assigned the same type
   * in the substitution.
   */
  private var typeSubstitution: Map[Node, ValueType] = Map.empty,

  /**
   * A map from a member in the equivalence class to the class' representative
   */
  private var classes: Map[UnificationVar, Node] = Map.empty,

  /**
   * Concrete bounds for each unification variable
   */
  private var captureConstraints: CaptureConstraints = Map.empty,

  /**
   * Everything we know so far -- right hand sides of substitutions are *always* concrete capture sets.
   * Once solved and part of the substitution, we cannot learn something about a unification variable anymore;
   * only check consistency.
   */
  private var captSubstitution: Map[CNode, CaptureSet] = Map.empty,

  /**
   * Unification variables which are not in scope anymore, but also haven't been solved, yet.
   */
  private var pendingInactive: Set[CNode] = Set.empty

)(using C: ErrorReporter) {

  /**
   * The currently known substitutions
   */
  def subst: Substitutions =
    val types = classes.flatMap[TypeVar, ValueType] { case (k, v) => typeSubstitution.get(v).map { k -> _ } }
    val captures = captSubstitution.asInstanceOf[Map[CaptVar, Captures]]
    Substitutions(types, captures)

  /**
   * Should only be called on unification variables where we do not know any types, yet
   *
   * It will *not* compare the types, but only equality imposed by constraints.
   */
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  /**
   * Retreive the potentially known type of [[x]]
   */
  def typeOf(x: UnificationVar): Option[ValueType] =
    typeOf(getNode(x))

  /**
   * Learn that unification variable [[x]] needs to be compatible with [[y]]. If there already is
   * a type for [[x]], we will invoke [[merge]] to check compatibility (and potentially error out).
   */
  def learn(x: UnificationVar, y: ValueType)(merge: (ValueType, ValueType) => Unit): Unit = {

    def learnType(x: Node, tpe: ValueType): Unit = {
      assert(!tpe.isInstanceOf[UnificationVar])
      typeOf(x) foreach { otherTpe => merge(tpe, otherTpe) }
      typeSubstitution = typeSubstitution.updated(x, tpe)
      updateSubstitution()
    }

    def connectNodes(x: Node, y: Node): Unit = {
      // Already connected
      if (x == y) return ()

      (typeOf(x), typeOf(y)) match {
        case (Some(typeOfX), Some(typeOfY)) => merge(typeOfX, typeOfY)
        case (Some(typeOfX), None) => learnType(y, typeOfX)
        case (None, Some(typeOfY)) => learnType(x, typeOfY)
        case (None, None) => ()
      }

      // create mapping to representative
      classes = classes.view.mapValues { node => if (node == x) y else node }.toMap
    }

    y match {
      case y: UnificationVar => connectNodes(getNode(x), getNode(y))
      case tpe => learnType(getNode(x), tpe)
    }
  }

  /**
   * Marks [[xs]] as pending inactive and solves them, if they do not have active bounds.
   */
  def leave(types: List[UnificationVar], capts: List[CaptUnificationVar]): Unit =
    // Check that we could infer all types of type variable instantiations.
    types.foreach {
      case x @ UnificationVar(UnificationVar.TypeVariableInstantiation(underlying, callTree)) =>
        if (!typeSubstitution.isDefinedAt(getNode(x))) C.at(callTree) {
          C.error(s"Cannot infer type argument ${underlying}, maybe consider annotating it?")
        }
      case _ => ()
    }

    pendingInactive = pendingInactive ++ capts.toSet

    var toRemove: Set[CNode] = Set.empty

    // (1) collect all nodes that can be solved
    pendingInactive foreach { n =>
      if (isInactive(n)) toRemove = toRemove + n
    }

    if (toRemove.isEmpty) return; // nothing to do

    // (2) Remove them from pending
    pendingInactive = pendingInactive -- toRemove

    // (3) Solve them
    val solved = toRemove.toList.map { n =>
      // bounds are consistent, we simply choose the lower bound as it is always concrete.
      val bounds = n.lower.getOrElse(Set.empty)
      n -> CaptureSet(bounds)
    }.toMap
    captSubstitution = captSubstitution ++ solved

    // (4) remove them from the constraint set
    captureConstraints = captureConstraints.collect {
      case (n, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) if !(toRemove.contains(n)) =>
        n -> CaptureNodeData(lower, upper, lowerNodes -- toRemove, upperNodes -- toRemove)
    }

    // (5) update the substitution
    updateSubstitution()

  override def clone(): Constraints = new Constraints(typeSubstitution, classes, captureConstraints, captSubstitution, pendingInactive)

  def dumpTypeConstraints() =
    println("\n--- Type Constraints ---")
    val cl = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    cl foreach {
      case (n, vars) => typeSubstitution.get(n) match {
        case None => println(s"{${vars.mkString(", ")}}")
        case Some(tpe) => println(s"{${vars.mkString(", ")}} --> ${tpe}")
      }
    }
    println("------------------\n")

  def dumpCaptureConstraints() =
    println("\n--- Capture Constraints ---")
    captureConstraints foreach {
      case (x, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) =>
        val prettyLower = lower.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("{}").padTo(10, ' ')
        val prettyUpper = upper.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("*").padTo(10, ' ')
        val prettyLowerNodes = lowerNodes.mkString(", ").padTo(10, ' ')
        val prettyUpperNodes = upperNodes.mkString(", ").padTo(10, ' ')
        val varName = x.toString.padTo(12, ' ')
        println(s"${prettyLowerNodes} | ${prettyLower} <: $x <: ${prettyUpper} | ${prettyUpperNodes}")
    }
    println("------------------\n")

  def dumpConstraints() =
    dumpTypeConstraints()
    dumpCaptureConstraints()

  //<editor-fold desc="Implementation Details: Capture Sets">

  private def getData(x: CNode): CaptureNodeData =
    captureConstraints.getOrElse(x, emptyData)

  /**
   * Accessing data on a node in the constraint graph.
   */
  extension (x: CNode) {
    private def lower: Option[Set[CaptureParam]] = getData(x).lower
    private def upper: Option[Set[CaptureParam]] = getData(x).upper
    private def lowerNodes: Map[CNode, Filter] = getData(x).lowerNodes
    private def upperNodes: Map[CNode, Filter] = getData(x).upperNodes
    private def lower_=(bounds: Set[CaptureParam]): Unit =
      captureConstraints = captureConstraints.updated(x, getData(x).copy(lower = Some(bounds)))
    private def upper_=(bounds: Set[CaptureParam]): Unit =
      captureConstraints = captureConstraints.updated(x, getData(x).copy(upper = Some(bounds)))
    private def addLower(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the intersection of filters
      val oldFilter = oldData.lowerNodes.getOrElse(other, Set.empty)
      val newFilter = oldFilter intersect exclude

      captureConstraints = captureConstraints.updated(x, oldData.copy(lowerNodes = oldData.lowerNodes + (other -> newFilter)))
    private def addUpper(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the intersection of filters
      val oldFilter = oldData.lowerNodes.getOrElse(other, Set.empty)
      val newFilter = oldFilter intersect exclude
      captureConstraints = captureConstraints.updated(x, oldData.copy(upperNodes = oldData.upperNodes + (other -> newFilter)))
  }

  /**
   * Computes whether a node and all of its transitive bounds are inactive.
   */
  private def isInactive(x: CNode, seen: Set[CNode] = Set.empty): Boolean =
    if ((seen contains x) || (captSubstitution isDefinedAt x)) return true;

    val selfSeen = seen + x

    val isInactiveItself = pendingInactive contains x
    val areBoundsInactive = (x.lowerNodes.keys ++ x.upperNodes.keys).forall { n => isInactive(n, selfSeen) }

    return isInactiveItself && areBoundsInactive


  private def checkConsistency(lower: Set[CaptureParam], upper: Set[CaptureParam]): Unit =
    val diff = lower -- upper
    if (diff.nonEmpty) { C.abort(s"Not allowed ${diff}") }

  private def checkEquality(xs: Set[CaptureParam], ys: Set[CaptureParam]): Unit =
    if (xs != ys) { C.abort(s"Capture set ${xs} is not equal to ${ys}") }

  // we do not necessarily need mergeLower, since we can take the free union
  private def mergeLower(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs ++ ys

  /**
   * Since we do not implement subregioning at the moment, it is safe to simply compute the
   * intersection.
   */
  private def mergeUpper(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs intersect ys


  /**
   * Adds x as a lower bound to y, and y as a lower bound to x.
   */
  def connect(x: CNode, y: CNode, exclude: Set[CaptureParam] = Set.empty): Unit =
    if (x == y /* || (y.lowerNodes contains x) */) { return () }

    // we already solved one of them? Or both?
    (captSubstitution.get(x), captSubstitution.get(y)) match {
      case (Some(CaptureSet(xs)), Some(CaptureSet(ys))) => checkConsistency(xs, ys)
      case (Some(CaptureSet(xs)), None) => requireLower(xs, y)
      case (None, Some(CaptureSet(ys))) => requireUpper(ys, x)
      case (None, None) =>
        x.addUpper(y, exclude)
        y.addLower(x, exclude) // do we need this here?

        val upperFilter = x.upperNodes(y)
        val lowerFilter = y.lowerNodes(x)

        x.lower foreach { bounds => requireLower(bounds -- upperFilter, y) }
        y.upper foreach { bounds => requireUpper(bounds -- lowerFilter, x) }
    }

  def requireLower(bounds: Set[CaptureParam], x: CNode): Unit = propagateLower(bounds, x)(using Set.empty)
  def requireUpper(bounds: Set[CaptureParam], x: CNode): Unit = propagateUpper(bounds, x)(using Set.empty)

  private def propagateLower(bounds: Set[CaptureParam], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()

    captSubstitution.get(x) foreach {
      // we already have fixed the capture set, check equality
      case CaptureSet(capt) =>
        checkEquality(capt, bounds)
        return;
    }

    given Set[CNode] = seen + x

    x.upper foreach { upperBounds => checkConsistency(bounds, upperBounds) }

    x.lower = x.lower map { existing =>
      mergeLower(existing, bounds)
    } getOrElse bounds

    x.upperNodes.foreach { case (y, filter) => propagateLower(bounds -- filter, y) }

    x.role match {
      case CaptUnificationVar.Subtraction(handled, underlying) =>
        propagateLower(bounds -- handled.toSet, underlying)
      case _ => ()
    }

  private def propagateUpper(bounds: Set[CaptureParam], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()

    captSubstitution.get(x) foreach {
      // we already have fixed the capture set, check equality
      case CaptureSet(capt) =>
        checkEquality(capt, bounds)
        return;
    }
    given Set[CNode] = seen + x

    x.lower foreach { lowerBounds => checkConsistency(lowerBounds, bounds) }

    x.upper = x.upper map { existing =>
      mergeUpper(existing, bounds)
    } getOrElse bounds

    x.lowerNodes.foreach { case (y, filter) => propagateUpper(bounds -- filter, y) }

    x.role match {
      // Does this make sense in an upper bound???
      case CaptUnificationVar.Subtraction(handled, underlying) =>
        propagateUpper(bounds -- handled.toSet, underlying)
      case _ => ()
    }

  //</editor-fold>

  //<editor-fold desc="Implementation Details: Types">

  /**
   * Updates substitution by applying it to itself.
   *
   * This way we replace {?B} -> Box[?R] with {?B} -> Box[Int] when learning ?B =:= Int
   */
  private def updateSubstitution(): Unit =
    val substitution = subst
    typeSubstitution = typeSubstitution.map { case (node, tpe) => node -> substitution.substitute(tpe) }

  private def getNode(x: UnificationVar): Node =
    classes.getOrElse(x, { val rep = new Node; classes += (x -> rep); rep })

  private def typeOf(n: Node): Option[ValueType] =
    typeSubstitution.get(n)

  //</editor-fold>
}