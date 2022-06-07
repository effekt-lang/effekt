package effekt
package typer

import effekt.symbols.{ CaptUnificationVar, CaptureParam, ValueType }
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.util.messages.ErrorReporter

type CNode = CaptUnificationVar

case class CaptureNodeData(
  // non present bounds represent bottom (the empty set)
  lower: Option[Set[CaptureParam]],
  // non present bounds represent top (the universal set)
  upper: Option[Set[CaptureParam]],
  lowerNodes: Set[CNode],
  upperNodes: Set[CNode]
)

/**
 * Invariants:
 * - The bounds are always fully propagated to all nodes.
 *
 *
 * However, for now, we do not compute equivalence classes -- we also do not establish transitive connections.
 * If we wanted to, we would need to somehow push unification variables through negations / subtractions.
 */
class CaptureConstraintGraph(using C: ErrorReporter) { outer =>

  type CaptureConstraints = Map[CNode, CaptureNodeData]

  // concrete bounds for each node
  private [this] var constraintData: CaptureConstraints = Map.empty

  override def clone(): CaptureConstraintGraph =
    val copy = CaptureConstraintGraph.empty
    copy.constraintData = constraintData
    copy

  def checkConsistency(lower: Set[CaptureParam], upper: Set[CaptureParam]): Unit =
    val diff = lower -- upper
    if (diff.nonEmpty) { C.abort(s"Not allowed ${diff}") }

  // we do not necessarily need mergeLower, since we can take the free union
  def mergeLower(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs ++ ys

  // TODO implement properly
  def mergeUpper(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs intersect ys

  private val emptyData = CaptureNodeData(None, None, Set.empty, Set.empty)

  private def getData(x: CNode): CaptureNodeData =
    constraintData.getOrElse(x, emptyData)

  extension (x: CNode) {
    private def lower: Option[Set[CaptureParam]] = getData(x).lower
    private def upper: Option[Set[CaptureParam]] = getData(x).upper
    private def lowerNodes: Set[CNode] = getData(x).lowerNodes
    private def upperNodes: Set[CNode] = getData(x).upperNodes
    private def lower_=(bounds: Set[CaptureParam]): Unit =
      constraintData = constraintData.updated(x, getData(x).copy(lower = Some(bounds)))
    private def upper_=(bounds: Set[CaptureParam]): Unit =
      constraintData = constraintData.updated(x, getData(x).copy(upper = Some(bounds)))
    private def addLower(other: CNode): Unit =
      val oldData = getData(x)
      constraintData = constraintData.updated(x, oldData.copy(lowerNodes = oldData.lowerNodes + other))
    private def addUpper(other: CNode): Unit =
      val oldData = getData(x)
      constraintData = constraintData.updated(x, oldData.copy(upperNodes = oldData.upperNodes + other))
  }

  /**
   * Adds x as a lower bound to y, and y as a lower bound to x.
   */
  def connect(x: CNode, y: CNode): Unit =
    if (x == y || (y.lowerNodes contains x)) { return () }

    x.lower foreach { bounds => requireLower(bounds, y) }
    y.upper foreach { bounds => requireUpper(bounds, x) }
    x.addUpper(y)
    y.addLower(x)

  def requireLower(bounds: Set[CaptureParam], x: CNode): Unit = propagateLower(bounds, x)(using Set.empty)
  def requireUpper(bounds: Set[CaptureParam], x: CNode): Unit = propagateUpper(bounds, x)(using Set.empty)

  private def propagateLower(bounds: Set[CaptureParam], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()
    given Set[CNode] = seen + x

    x.role match {
      case CaptUnificationVar.Subtraction(handled, underlying) =>
        propagateLower(bounds -- handled.toSet, underlying)
      case _ =>
        x.upper foreach { upperBounds => checkConsistency(bounds, upperBounds) }

        x.lower = x.lower map { existing =>
          mergeLower(existing, bounds)
        } getOrElse bounds

        x.upperNodes.foreach { y => propagateLower(bounds, y) }
    }

  private def propagateUpper(bounds: Set[CaptureParam], x: CNode)(using seen: Set[CNode]): Unit =
    if (seen contains x) return()
    given Set[CNode] = seen + x

    x.role match {
      // Does this make sense in an upper bound???
      case CaptUnificationVar.Subtraction(handled, underlying) =>
        propagateUpper(bounds -- handled.toSet, underlying)
      case _ =>
        x.lower foreach { lowerBounds => checkConsistency(lowerBounds, bounds) }

        x.upper = x.upper map { existing =>
          mergeUpper(existing, bounds)
        } getOrElse bounds

        x.lowerNodes.foreach { y => propagateUpper(bounds, y) }
    }

  def dumpConstraints() =
    constraintData foreach {
      case (x, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) =>
        val prettyLower = lower.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("{}")
        val prettyUpper = upper.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("*")
        println(s"${prettyLower} <: $x <: ${prettyUpper}")
    }
}

object CaptureConstraintGraph {
  def empty(using ErrorReporter) = new CaptureConstraintGraph()
}