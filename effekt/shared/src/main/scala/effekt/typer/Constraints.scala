package effekt
package typer

import effekt.symbols.*
import effekt.symbols.BlockTypeVar.BlockUnificationVar
import effekt.util.messages.{ErrorMessageReifier, ErrorReporter}
import effekt.util.foreachAborting

// Auxiliary Definitions
// ---------------------

private[typer]
class Node

private[typer]
type CNode = CaptUnificationVar

// non present filter stands for "everything is passed through"
private[typer]
type Filter = Set[Capture]

private[typer]
case class CaptureNodeData(
  // non present bounds represent bottom (the empty set)
  lower: Option[Set[Capture]],
  // non present bounds represent top (the universal set)
  upper: Option[Set[Capture]],
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
 * substitution ([[valueTypeSubstitution]]).
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
 * node are always fully known sets of type [[Capture]].
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
  private var valueTypeSubstitution: Map[Node, ValueType] = Map.empty,

  private var blockTypeSubstitution: Map[Node, BlockType] = Map.empty,

  /**
   * A map from a member in the equivalence class to the class' representative
   */
  private var classes: Map[UnificationVar, Node] = Map.empty,

  private var blockClasses : Map[BlockUnificationVar, Node] = Map.empty,

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
    val values = classes.flatMap[TypeVar, ValueType] { case (k, v) => valueTypeSubstitution.get(v).map { k -> _ } }
    val blocks = blockClasses.flatMap[BlockTypeVar, BlockType] { case (k, v) => blockTypeSubstitution.get(v).map { k -> _ } }
    val captures = captSubstitution.asInstanceOf[Map[CaptVar, Captures]]
    Substitutions(values, blocks, captures)

  /**
   * Should only be called on unification variables where we do not know any types, yet
   *
   * It will *not* compare the types, but only equality imposed by constraints.
   */
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  def isSubset(lower: Captures, upper: Captures): Boolean =
    def knownUpper(node: CNode) = captSubstitution.get(node).map(_.captures).orElse { node.upper }
    def knownLower(node: CNode) = captSubstitution.get(node).map(_.captures).orElse { node.lower }

    (lower, upper) match {
      // (1) they are concrete, we can compare them
      case (CaptureSet(xs), CaptureSet(ys)) => xs.subsetOf(ys)
      // (2) lhs is a unification variable, we need to check whether its upper bound is smaller than ys
      case (x: CaptUnificationVar, CaptureSet(ys)) => knownUpper(x).exists(_ subsetOf ys)
      // (2) rhs is a unification variable, we need to check whether xs is smaller than its lower bound
      case (CaptureSet(xs), y: CaptUnificationVar) => knownLower(y).exists(xs subsetOf _)
      // (3) both are unification variables
      case (x: CaptUnificationVar, y: CaptUnificationVar) =>
        // either the bounds imply subcapturing ...
        val boundsImply = knownUpper(x).flatMap(xs => knownLower(y).map(ys => xs subsetOf ys)).getOrElse(false)
        // or the two are known to be related
        val areBounded = x.upperNodes.get(y).exists(f => f.isEmpty)
        return boundsImply || areBounded
    }

  /**
   * Learn that unification variable [[x]] needs to be compatible with [[y]]. If there already is
   * a type for [[x]], we will invoke [[merge]] to check compatibility (and potentially error out).
  */
  def learn(x: UnificationVar, y: ValueType)(merge: (ValueType, ValueType) => Unit): Unit = {

    def learnType(x: Node, tpe: ValueType): Unit = {
      // tpe should not be a reference to a unification variable
      valueTypeOf(x) foreach { otherTpe => merge(tpe, otherTpe) }
      valueTypeSubstitution = valueTypeSubstitution.updated(x, tpe)
      updateSubstitution()
    }

    def connectNodes(x: Node, y: Node): Unit = {
      // Already connected
      if (x == y) return ()

      (valueTypeOf(x), valueTypeOf(y)) match {
        case (Some(typeOfX), Some(typeOfY)) => merge(typeOfX, typeOfY)
        case (Some(typeOfX), None) => learnType(y, typeOfX)
        case (None, Some(typeOfY)) => learnType(x, typeOfY)
        case (None, None) => ()
      }

      // create mapping to representative
      classes = classes.view.mapValues { node => if (node == x) y else node }.toMap
    }

    y match {
      case ValueTypeRef(y: UnificationVar) => connectNodes(getNode(x), getNode(y))
      case tpe => learnType(getNode(x), tpe)
    }
  }

  def learn(x: BlockUnificationVar, y: BlockType)(merge: (BlockType, BlockType) => Unit): Unit = {

    def learnType(x: Node, tpe: BlockType): Unit = {
      // tpe should not be a reference to a unification variable
      blockTypeOf(x) foreach { otherTpe => merge(tpe, otherTpe) }
      blockTypeSubstitution = blockTypeSubstitution.updated(x, tpe)
      updateSubstitution()
    }

    def connectNodes(x: Node, y: Node): Unit = {
      // Already connected
      if (x == y) return ()

      (blockTypeOf(x), blockTypeOf(y)) match {
        case (Some(typeOfX), Some(typeOfY)) => merge(typeOfX, typeOfY)
        case (Some(typeOfX), None) => learnType(y, typeOfX)
        case (None, Some(typeOfY)) => learnType(x, typeOfY)
        case (None, None) => ()
      }

      // create mapping to representative
      blockClasses = blockClasses.view.mapValues { node => if (node == x) y else node }.toMap
    }

    y match {
      case BlockTypeRef(y: BlockUnificationVar) => 
        connectNodes(getNode(x), getNode(y))
      case tpe => 
        learnType(getNode(x), tpe)
    }
  }

  /**
   * Marks [[xs]] as pending inactive and solves them, if they do not have active bounds.
   */
  def leave(types: List[UnificationVar], capts: List[CaptUnificationVar]): Unit =
    // Check that we could infer all types of type variable instantiations.
    types.foreach {
      case x @ UnificationVar(underlying, callTree, false) =>
        if (!valueTypeSubstitution.isDefinedAt(getNode(x))) C.at(callTree) {
          C.error(s"Cannot infer type argument ${underlying}, maybe consider annotating it?")
        }
      case _ => ()
    }

    // (0) only add those to pending that haven't been solved already
    pendingInactive = pendingInactive ++ (capts.toSet -- captSubstitution.keySet)

    // (1) collect all nodes that can be solved
    val toRemove: Set[CNode] = removableNodes()

    // nothing to do
    if (toRemove.isEmpty) return;

    // (2) solve variables that can be removed
    solve(toRemove)

    // (3) update the substitution
    updateSubstitution()

  def solve(toRemove: Set[CNode]): Unit =
    // (1) Remove them from pending
    pendingInactive = pendingInactive -- toRemove

    // (2) Solve them
    val solved = toRemove.toList.map { n =>
      // bounds are consistent, we simply choose the lower bound as it is always concrete.
      val bounds = n.lower.getOrElse(Set.empty)
      n -> CaptureSet(bounds)
    }.toMap
    captSubstitution = captSubstitution ++ solved

    // (3) remove them from the constraint set
    captureConstraints = captureConstraints.collect {
      case (n, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) if !(toRemove.contains(n)) =>
        n -> CaptureNodeData(lower, upper, lowerNodes -- toRemove, upperNodes -- toRemove)
    }

  def forceSolving(node: CNode): CaptureSet =
    captSubstitution.getOrElse(node, {
      solve(Set(node))
      captSubstitution(node)
    })


  override def clone(): Constraints = new Constraints(valueTypeSubstitution, blockTypeSubstitution, classes, blockClasses, captureConstraints, captSubstitution, pendingInactive)

  def dumpTypeConstraints() =
    println("\n--- Type Constraints ---")
    val cl = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    cl foreach {
      case (n, vars) => valueTypeSubstitution.get(n) match {
        case None => println(s"{${vars.mkString(", ")}}")
        case Some(tpe) => println(s"{${vars.mkString(", ")}} --> ${tpe}")
      }
    }
    println("------------------\n")

  def dumpCaptureConstraints() =
    println("\n--- Capture Constraints ---")
    captureConstraints foreach {
      case (x, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) =>
        def printFilterNode(filtered: (CNode, Filter)): String = filtered match {
          case (n, filter) if filter.isEmpty => n.toString
          case (n, filter) => s"${n.toString}[${filter.mkString(", ")}]"
        }

        val colWidth = 25
        val prettyLower = lower.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("{}").padTo(colWidth, ' ')
        val prettyUpper = upper.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("*").padTo(colWidth, ' ')
        val prettyLowerNodes = lowerNodes.map(printFilterNode).mkString(", ").padTo(colWidth, ' ')
        val prettyUpperNodes = upperNodes.map(printFilterNode).mkString(", ").padTo(colWidth, ' ')
        val varName = x.toString.padTo(15, ' ')
        println(s"${prettyLowerNodes} | ${prettyLower} <: $varName <: ${prettyUpper} | ${prettyUpperNodes}")
    }
    println("------------------\n")
    captSubstitution.foreach {
      case (node, set) => println(node.toString.padTo(25, ' ') + " -> " + set.captures.mkString(", "))
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
    private [typer] def lower: Option[Set[Capture]] = getData(x).lower
    private [typer] def upper: Option[Set[Capture]] = getData(x).upper
    private [typer] def lowerNodes: Map[CNode, Filter] = getData(x).lowerNodes
    private [typer] def upperNodes: Map[CNode, Filter] = getData(x).upperNodes
    private def lower_=(bounds: Set[Capture]): Unit =
      captureConstraints = captureConstraints.updated(x, getData(x).copy(lower = Some(bounds)))
    private def upper_=(bounds: Set[Capture]): Unit =
      captureConstraints = captureConstraints.updated(x, getData(x).copy(upper = Some(bounds)))
    private def addLower(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the intersection of filters
      val oldFilter = oldData.lowerNodes.get(other)
      val newFilter = oldFilter.map { _ intersect exclude }.getOrElse { exclude }

      captureConstraints = captureConstraints.updated(x, oldData.copy(lowerNodes = oldData.lowerNodes + (other -> newFilter)))
    private def addUpper(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the union of filters
      val oldFilter = oldData.lowerNodes.get(other)
      val newFilter = oldFilter.map { _ union exclude }.getOrElse { exclude }
      captureConstraints = captureConstraints.updated(x, oldData.copy(upperNodes = oldData.upperNodes + (other -> newFilter)))
  }

  /**
   * Computes the set of all nodes that are inactive (that is, we left its unification scope and it
   * can be solved by unification)
   *
   * In order to do so, it checks whether the node itself is inactive and whether the transitive closure of
   * its bounds is inactive.
   */
  private def removableNodes(): Set[CNode] = {

    // The results of inactivity is cached to avoid expensive recomputation.
    var cache: Map[CNode, Boolean] = Map.empty

    /**
     * This helper function computes inactivity of nodes as the
     * transitive closure of x's bounds.
     */
    def checkInactivity(x: CNode): Unit =
      if (cache contains x) { return }

      if (captSubstitution isDefinedAt x) { cache += (x -> true); return }

      // is the node itself inactive?
      if (pendingInactive contains x) {
        cache += (x -> true)

        val allBoundsInactive = (x.lowerNodes.keys ++ x.upperNodes.keys).forall { n =>
          checkInactivity(n)
          cache(n)
        }
        cache += (x -> allBoundsInactive)
      } else {
        cache += (x -> false)
      }

    // collect all nodes that can be solved
    pendingInactive filter { n => checkInactivity(n); cache(n) }
  }


  private def checkConsistency(lower: Set[Capture], upper: Set[Capture]): Unit =
    val diff = lower -- upper
    if (diff.nonEmpty) { C.abort(pretty"Not allowed ${CaptureSet(diff)}") }

  private def checkEquality(xs: Set[Capture], ys: Set[Capture]): Unit =
    if (xs != ys) { C.abort(pretty"Capture set ${xs} is not equal to ${ys}") }

  // we do not necessarily need mergeLower, since we can take the free union
  private def mergeLower(xs: Set[Capture], ys: Set[Capture]): Set[Capture] =
    xs ++ ys

  /**
   * Since we do not implement subregioning at the moment, it is safe to simply compute the
   * intersection.
   */
  private def mergeUpper(xs: Set[Capture], ys: Set[Capture]): Set[Capture] =
    xs intersect ys


  /**
   * Adds x as a lower bound to y, and y as a lower bound to x.
   */
  def connect(x: CNode, y: CNode, exclude: Set[Capture] = Set.empty): Unit =
    if (x == y /* || (y.lowerNodes contains x) */) { return () }

    // we already solved one of them? Or both?
    (captSubstitution.get(x), captSubstitution.get(y)) match {
      case (Some(CaptureSet(xs)), Some(CaptureSet(ys))) => checkConsistency(xs, ys ++ exclude)
      case (Some(CaptureSet(xs)), None) => requireLower(xs -- exclude, y)
      case (None, Some(CaptureSet(ys))) => requireUpper(ys ++ exclude, x)
      case (None, None) =>
        // compose the filters
        x.addUpper(y, exclude)
        y.addLower(x, exclude)

        val upperFilter = x.upperNodes(y)
        val lowerFilter = y.lowerNodes(x)

        x.lower foreach { bounds => requireLower(bounds -- lowerFilter, y) }
        y.upper foreach { bounds => requireUpper(bounds ++ upperFilter, x) }
    }

  def requireLower(bounds: Set[Capture], x: CNode): Unit = propagateLower(bounds, x)(using Set.empty)
  def requireUpper(bounds: Set[Capture], x: CNode): Unit = propagateUpper(bounds, x)(using Set.empty)

  private def propagateLower(bounds: Set[Capture], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()

    captSubstitution.get(x) foreachAborting {
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


  private def propagateUpper(bounds: Set[Capture], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()

    captSubstitution.get(x) foreachAborting {
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

    x.lowerNodes.foreach { case (y, filter) => propagateUpper(bounds ++ filter, y) }

  //</editor-fold>

  //<editor-fold desc="Implementation Details: Types">

  /**
   * Updates substitution by applying it to itself.
   *
   * This way we replace {?B} -> Box[?R] with {?B} -> Box[Int] when learning ?B =:= Int
   */
  private def updateSubstitution(): Unit =
    val substitution = subst
    valueTypeSubstitution = valueTypeSubstitution.map { case (node, tpe) => node -> substitution.substitute(tpe) }

  private def getNode(x: UnificationVar): Node =
    classes.getOrElse(x, { val rep = new Node; classes += (x -> rep); rep })

  private def getNode(x: BlockUnificationVar): Node =
    blockClasses.getOrElse(x, { val rep = new Node; blockClasses += (x -> rep); rep })

  private def valueTypeOf(n: Node): Option[ValueType] =
    valueTypeSubstitution.get(n)

  private def blockTypeOf(n: Node): Option[BlockType] =
    blockTypeSubstitution.get(n)


  //</editor-fold>
}
