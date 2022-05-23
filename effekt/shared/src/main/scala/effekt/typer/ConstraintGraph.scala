package effekt
package typer

import effekt.symbols.{ UnificationVar, ValueType }
import effekt.symbols.builtins.{ TBottom, TTop }

/**
 * Represents a node in the constraint propagation graph
 *
 *                    ┏━━━━━━━━━━━━━━━━━━┓
 *    --------------> ┃ {?MyVar, ?Other} ┃ -------------->
 *     lower nodes    ┠─────────┬────────┨   upper nodes
 *    --------------> ┃  Lower  │  Upper ┃ -------------->
 *                    ┗━━━━━━━━━┷━━━━━━━━┛
 *
 * The arrows in the picture above indicate the suptyping relationship.
 * They are in fact navigatable in both directions, since new _upper_ bounds
 * need to flow through the node from right-to-left.
 *
 * Every node represents potentially a set of unification variables that
 * are assumed to be equal. Type equality constraints, invariant positions
 * and mutually recursive unification variables lead to these sets having
 * more than one member.
 *
 * We have the following invariants:
 * - Direct: all transitive lower nodes are also immediate lower nodes
 * - Intra-Consistency: Lower is always consistent to Upper
 * - Inter-Consistency: if two nodes ?S <: ?T, then the lower bound of ?S has been
 *     propagated as lower bound to ?T and the upper bound of ?T has been propagated
 *     as upper bound of ?S.
 */
private[typer]
case class NodeData(lower: Set[Node], payload: (ValueType, ValueType), upper: Set[Node])

private[typer]
class Node

/**
 * A graph of connected nodes, where each node can have multiple "upper"
 * and multiple "lower" nodes. Each node carries a payload
 *
 *    (ValueType, ValueType)
 *
 * corresponding to the lower and upper type bounds of a unification variable.
 * The graph itself establishes the following invariants:
 *
 * - Direct: all transitive (lower/higher) nodes are also immediate (lower/higher) nodes
 * - Compact: all cycles (by directness) lead to cliques. Those are represented by a single node
 *
 * The graph implementation does NOT establish invariants about the payloads (type bounds).
 * Those have to be established externally.
 */
class ConstraintGraph(
  // a map from a member in the equivalence class to the class' representative
  private var nodes: Map[UnificationVar, Node] = Map.empty,
  // bounds and payload for each node
  private var valueConstraints: Map[Node, NodeData] = Map.empty
) {

  private def getNode(x: UnificationVar): Node =
    nodes.getOrElse(x, { val rep = new Node; nodes += (x -> rep); rep })

  /**
   * Given a representative gives the list of all unification variables it is known to
   * be in the same equivalence class with.
   */
  private def variablesFor(representative: Node): Set[UnificationVar] =
    val transposed = nodes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    transposed.getOrElse(representative, Nil).toSet

  private def representativeFor(n: Node): UnificationVar =
    variablesFor(n).toList.head

  private def getBounds(x: Node): NodeData =
    valueConstraints.getOrElse(x, NodeData(Set.empty, (TBottom, TTop), Set.empty))

  private def setBounds(x: Node, bounds: NodeData): Unit =
    valueConstraints = valueConstraints.updated(x, bounds)

  def boundsFor(x: UnificationVar): (ValueType, ValueType) = getBounds(getNode(x)).payload
  def lowerBound(x: UnificationVar): ValueType = boundsFor(x)._1
  def upperBound(x: UnificationVar): ValueType = boundsFor(x)._2

  def lowerBounds(x: UnificationVar): Set[UnificationVar] =
    getBounds(getNode(x)).lower.flatMap { variablesFor }

  def upperBounds(x: UnificationVar): Set[UnificationVar] =
    getBounds(getNode(x)).upper.flatMap { variablesFor }

  def updateLowerBound(x: UnificationVar, bound: ValueType): Unit = bound match {
    case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a lower bound for ${x}"
    case _ =>
      val rep = getNode(x)
      val bounds = getBounds(rep)
      val (low, up) = bounds.payload
      setBounds(rep, bounds.copy(payload = (bound, up)))
  }

  def updateUpperBound(x: UnificationVar, bound: ValueType): Unit = bound match {
    case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a upper bound for ${x}"
    case _ =>
      val rep = getNode(x)
      val bounds = getBounds(rep)
      val (low, up) = bounds.payload
      setBounds(rep, bounds.copy(payload = (low, bound)))
  }

  /**
   * Decides whether [[x]] is a subtype of [[y]] solely by inspecting the bounds
   */
  def isSubtypeOf(x: UnificationVar, y: UnificationVar): Boolean =
    val xRepr = getNode(x)
    val yRepr = getNode(y)
    val bounds = getBounds(xRepr)
    (bounds.upper contains yRepr) || xRepr == yRepr

  def isSupertypeOf(x: UnificationVar, y: UnificationVar): Boolean =
    isSubtypeOf(y, x)

  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  def mapPayload(f: (ValueType, ValueType) => (ValueType, ValueType)): Unit =
    valueConstraints = valueConstraints.map {
      case (n, NodeData(lowerNodes, (lower, upper), upperNodes)) =>
        (n, NodeData(lowerNodes, f(lower, upper), upperNodes))
    }

  /**
   * Removes a unification variable from the graph.
   *
   * If the variable was part of an equivalence class, the remaining variables will be returned as [[Left]].
   * If the variable was the last one in an equivalence class, the node data will be returned as [[Right]].
   */
  def remove(toDelete: Set[UnificationVar]): List[(Set[UnificationVar], Either[UnificationVar, (Set[UnificationVar], (ValueType, ValueType), Set[UnificationVar])])] =
    val varToNode = toDelete.map(v => v -> getNode(v)).toMap

    // the nodes to be removed, potentially
    val nodesToDelete = varToNode.values.toSet

    nodesToDelete.toList map { node =>
      val elements = variablesFor(node)
      val remaining = elements -- toDelete
      val affected = elements intersect toDelete
      val shouldBeDeleted = remaining.isEmpty
      if (shouldBeDeleted) {
        val (vars, NodeData(low, data, up)) = deleteNode(node)
        assert(vars == affected)

        // we choose one representative for each equivalence class.
        val lower = (low -- nodesToDelete).map(representativeFor)
        val upper = (up -- nodesToDelete).map(representativeFor)
        vars -> Right((lower, data, upper))
      } else {
        // remove from mapping
        nodes = nodes.removedAll(affected)
        affected -> Left(representativeFor(node))
      }
    }

  /**
   * Adds x as a lower bound to y, and y as a lower bound to x.
   */
  def connect(x: UnificationVar, y: UnificationVar): Unit =
    val repX = getNode(x)
    val repY = getNode(y)
    connectNodes(repX, repY)

  private def addUpperBounds(x: Node, upper: Set[Node]): Unit =
    val bounds = getBounds(x)
    // we subtract [[x]] to avoid cycles in bounds
    setBounds(x, bounds.copy(upper = (bounds.upper ++ upper - x)))

  private def addLowerBounds(x: Node, lower: Set[Node]): Unit =
    val bounds = getBounds(x)
    // we subtract [[x]] to avoid cycles in bounds
    setBounds(x, bounds.copy(lower = (bounds.lower ++ lower - x)))

  private def fixupCycles(nodes: Set[Node]): Unit = nodes foreach { node =>
    val bounds = getBounds(node)
    val equals = bounds.lower intersect bounds.upper
    equals.foreach { other =>
      replaceNode(other, node)
    }
  }

  private def connectNodes(x: Node, y: Node): Unit =
    val boundsX = getBounds(x)
    val boundsY = getBounds(y)

    // Already connected
    if (boundsY.lower contains x) {
      return ()
    }

    val upper = boundsY.upper + y
    val lower = boundsX.lower + x

    lower.foreach { b => addUpperBounds(b, upper) }
    upper.foreach { b => addLowerBounds(b, lower) }

    fixupCycles(upper ++ lower)

  // used internally to add a node to an equivalence class
  private def replaceNode(old: Node, by: Node): Unit =

    valueConstraints = valueConstraints.collect {
      case (node, NodeData(low, payload, high)) if node != old =>
        (node, NodeData(
          if (low.contains(old)) ((low - old) + by) - node else low,
          payload,
          if (high.contains(old)) ((high - old) + by) - node else high
        ))
    }
    // create mapping to representative
    nodes = nodes.view.mapValues { node => if (node == old) by else node }.toMap


  private def deleteNode(x: Node): (Set[UnificationVar], NodeData) =
    val bounds = getBounds(x)
    val vars = variablesFor(x)

    // remove node itself
    valueConstraints = valueConstraints.removed(x)

    // remove from bounds
    (bounds.lower ++ bounds.upper) foreach { other =>
      val NodeData(low, data, up) = getBounds(other)
      setBounds(other, NodeData(low - x, data, up - x))
    }

    // remove from mapping
    nodes = nodes.removedAll(vars)

    (vars, bounds)

  override def clone(): ConstraintGraph =
    new ConstraintGraph(nodes, valueConstraints)

  def dumpConstraints() =
    val colSize = 12
    val varSize = 5
    val sep = "━".repeat(colSize)
    val varSep = "━".repeat(varSize)

    println(s"┏$sep┯$sep━━━━━$varSep━━━━━$sep┯$sep┓")

    def showNode(n: Node): String =
      val equiv = variablesFor(n).toList
      if (equiv.size == 1) equiv.head.toString else s"{${equiv.mkString(", ")}}"

    valueConstraints.foreach {
      case (x, NodeData(lowerVars, (lower, upper), upperVars)) =>
        val lowNodes = lowerVars.map(showNode).mkString(", ").padTo(colSize, ' ')
        val lowType  = lower.toString.padTo(colSize, ' ')
        val variable = showNode(x).padTo(varSize, ' ')
        val upType = upper.toString.padTo(colSize, ' ')
        val upNodes = upperVars.map(showNode).mkString(", ")
        println(s"$lowNodes │ $lowType <: $variable <: $upType │ $upNodes")
    }
    println(s"┗$sep┷$sep━━━━━$varSep━━━━━$sep┷$sep┛")
}