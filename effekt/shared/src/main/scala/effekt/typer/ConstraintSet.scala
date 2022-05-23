package effekt
package typer

import effekt.symbols.{ UnificationVar, ValueType }
import effekt.symbols.builtins.{ TBottom, TTop }


class ConstraintSet(
  // a map from a member in the equivalence class to the class' representative
  private var nodes: Map[UnificationVar, Node] = Map.empty,
  // concrete bounds for each node
  private var valueConstraints: Map[Node, (ValueType, ValueType)] = Map.empty
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

  private def getBounds(x: Node): (ValueType, ValueType) =
    valueConstraints.getOrElse(x, (TBottom, TTop))

  private def setBounds(x: Node, bounds: (ValueType, ValueType)): Unit =
    valueConstraints = valueConstraints.updated(x, bounds)

  def boundsFor(x: UnificationVar): (ValueType, ValueType) = getBounds(getNode(x))
  def lowerBound(x: UnificationVar): ValueType = boundsFor(x)._1
  def upperBound(x: UnificationVar): ValueType = boundsFor(x)._2

  def updateLowerBound(x: UnificationVar, bound: ValueType): Unit = bound match {
    case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a lower bound for ${x}"
    case _ =>
      val rep = getNode(x)
      val (lower, upper) = getBounds(rep)
      setBounds(rep, (bound, upper))
  }

  def updateUpperBound(x: UnificationVar, bound: ValueType): Unit = bound match {
    case y: UnificationVar => sys error s"Cannot set unification variable ${y} as a upper bound for ${x}"
    case _ =>
      val rep = getNode(x)
      val (lower, upper) = getBounds(rep)
      setBounds(rep, (lower, bound))
  }

  def isSubtypeOf(x: UnificationVar, y: UnificationVar): Boolean =
    isEqual(x, y)

  def isSupertypeOf(x: UnificationVar, y: UnificationVar): Boolean =
    isEqual(x, y)

  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  def mapBounds(f: (ValueType, ValueType) => (ValueType, ValueType)): Unit =
    valueConstraints = valueConstraints.map {
      case (n, (lower, upper)) => (n, f(lower, upper))
    }

  /**
   * Removes unification variables from the graph.
   *
   * If the variable was part of an equivalence class, the remaining variables will be returned as [[Left]].
   * If the variable was the last one in an equivalence class, the bounds will be returned as [[Right]].
   */
  def remove(toDelete: Set[UnificationVar]): List[(Set[UnificationVar], Either[UnificationVar, (ValueType, ValueType)])] =
    val varToNode = toDelete.map(v => v -> getNode(v)).toMap

    // the nodes to be removed, potentially
    val nodesToDelete = varToNode.values.toSet

    nodesToDelete.toList map { node =>
      val elements = variablesFor(node)
      val remaining = elements -- toDelete
      val affected = elements intersect toDelete
      val shouldBeDeleted = remaining.isEmpty
      if (shouldBeDeleted) {
        val (vars, bounds) = deleteNode(node)
        assert(vars == affected)
        vars -> Right(bounds)
      } else {
        // only remove from mapping
        nodes = nodes.removedAll(affected)
        affected -> Left(representativeFor(node))
      }
    }

  /**
   * Adds the two nodes into the same equivalence class
   *
   * The resulting equivalence class will have the bounds of [[y]].
   */
  def connect(x: UnificationVar, y: UnificationVar): Unit =
    val repX = getNode(x)
    val repY = getNode(y)
    connectNodes(repX, repY)

  private def connectNodes(x: Node, y: Node): Unit =
    // Already connected
    if (x == y) return ()

    // remove data on x
    valueConstraints = valueConstraints - x

    // create mapping to representative
    nodes = nodes.view.mapValues { node => if (node == x) y else node }.toMap


  private def deleteNode(x: Node): (Set[UnificationVar], (ValueType, ValueType)) =
    val bounds = getBounds(x)
    val vars = variablesFor(x)

    // remove data on x
    valueConstraints = valueConstraints.removed(x)

    // remove from mapping
    nodes = nodes.removedAll(vars)

    (vars, bounds)


  override def clone(): ConstraintSet =
    new ConstraintSet(nodes, valueConstraints)

  def dumpConstraints() =
    val colSize = 12
    val varSize = 5
    val sep = "━".repeat(colSize)
    val varSep = "━".repeat(varSize)

    println(s"┏$sep━━━━━$varSep━━━━━$sep┓")

    def showNode(n: Node): String =
      val equiv = variablesFor(n).toList
      if (equiv.size == 1) equiv.head.toString else s"{${equiv.mkString(", ")}}"

    valueConstraints.foreach {
      case (x, (lower, upper)) =>
        val lowType  = lower.toString.padTo(colSize, ' ')
        val variable = showNode(x).padTo(varSize, ' ')
        val upType = upper.toString.padTo(colSize, ' ')
        println(s" $lowType <: $variable <: $upType ")
    }
    println(s"┗$sep━━━━━$varSep━━━━━$sep┛")
}