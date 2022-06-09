package effekt
package typer

import effekt.symbols._
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.util.messages.ErrorReporter

type CNode = CaptUnificationVar

// non present filter stands for "everything is passed through"
type Filter = Set[CaptureParam]

case class CaptureNodeData(
  // non present bounds represent bottom (the empty set)
  lower: Option[Set[CaptureParam]],
  // non present bounds represent top (the universal set)
  upper: Option[Set[CaptureParam]],
  lowerNodes: Map[CNode, Filter],
  upperNodes: Map[CNode, Filter]
)

type CaptureConstraints = Map[CNode, CaptureNodeData]

/**
 * Invariants:
 * - The bounds are always fully propagated to all nodes.
 *
 *
 * However, for now, we do not compute equivalence classes -- we also do not establish transitive connections.
 * If we wanted to, we would need to somehow push unification variables through negations / subtractions.
 */
class CaptureConstraintGraph(
  // concrete bounds for each node
  private [this] var constraintData: CaptureConstraints = Map.empty,
  // unification variables which are not in scope anymore, but also haven't been solved, yet.
  private [this] var pendingInactive: Set[CNode] = Set.empty,
   // Everything we know so far -- right hand sides of substitutions are *always* concrete capture sets.
   // Once solved and part of the substitution, we cannot learn something about a unification variable anymore;
   // only check consistency.
  private var substitution: Map[CNode, CaptureSet] = Map.empty,
)(using C: ErrorReporter) { outer =>

  private val emptyData = CaptureNodeData(None, None, Map.empty, Map.empty)

  private def getData(x: CNode): CaptureNodeData =
    constraintData.getOrElse(x, emptyData)

  extension (x: CNode) {
    private def lower: Option[Set[CaptureParam]] = getData(x).lower
    private def upper: Option[Set[CaptureParam]] = getData(x).upper
    private def lowerNodes: Map[CNode, Filter] = getData(x).lowerNodes
    private def upperNodes: Map[CNode, Filter] = getData(x).upperNodes
    private def lower_=(bounds: Set[CaptureParam]): Unit =
      constraintData = constraintData.updated(x, getData(x).copy(lower = Some(bounds)))
    private def upper_=(bounds: Set[CaptureParam]): Unit =
      constraintData = constraintData.updated(x, getData(x).copy(upper = Some(bounds)))
    private def addLower(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the intersection of filters
      val oldFilter = oldData.lowerNodes.getOrElse(other, Set.empty)
      val newFilter = oldFilter intersect exclude

      constraintData = constraintData.updated(x, oldData.copy(lowerNodes = oldData.lowerNodes + (other -> newFilter)))
    private def addUpper(other: CNode, exclude: Filter): Unit =
      val oldData = getData(x)

      // compute the intersection of filters
      val oldFilter = oldData.lowerNodes.getOrElse(other, Set.empty)
      val newFilter = oldFilter intersect exclude
      constraintData = constraintData.updated(x, oldData.copy(upperNodes = oldData.upperNodes + (other -> newFilter)))
  }

  override def clone(): CaptureConstraintGraph =
    new CaptureConstraintGraph(constraintData, pendingInactive, substitution)

  /**
   * Marks [[xs]] as pending inactive and solves them, if they do not have active bounds.
   */
  def leave(xs: List[CaptUnificationVar]): Unit =
    pendingInactive = pendingInactive ++ xs.toSet

    var toRemove: Set[CNode] = Set.empty

    // (1) collect all nodes that can be solved
    pendingInactive foreach { n =>
      if (isInactive(n)) toRemove = toRemove + n
    }

    // (2) Remove them from pending
    pendingInactive = pendingInactive -- toRemove

    // (3) Solve them
    val solved = toRemove.toList.map { n =>
      // bounds are consistent, we simply choose the lower bound as it is always concrete.
      val bounds = n.lower.getOrElse(Set.empty)
      n -> CaptureSet(bounds)
    }
    substitution = substitution ++ solved.toMap

    // (4) remove them from the constraint set
    constraintData = constraintData.collect {
      case (n, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) if !(toRemove.contains(n)) =>
        n -> CaptureNodeData(lower, upper, lowerNodes -- toRemove, upperNodes -- toRemove)
    }


  /**
   * Computes whether a node and all of its transitive bounds are inactive.
   */
  def isInactive(x: CNode, seen: Set[CNode] = Set.empty): Boolean =
    if ((seen contains x) || (substitution isDefinedAt x)) return true;

    val selfSeen = seen + x

    val isInactiveItself = pendingInactive contains x
    val areBoundsInactive = (x.lowerNodes.keys ++ x.upperNodes.keys).forall { n => isInactive(n, selfSeen) }

    return isInactiveItself && areBoundsInactive


  private def checkConsistency(lower: Set[CaptureParam], upper: Set[CaptureParam]): Unit =
    val diff = lower -- upper
    if (diff.nonEmpty) { C.abort(s"Not allowed ${diff}") }

  // we do not necessarily need mergeLower, since we can take the free union
  private def mergeLower(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs ++ ys

  /**
   * Since we do not implement subregioning at the moment, it is safe to simply compute the
   * intersection.
   */
  private def mergeUpper(xs: Set[CaptureParam], ys: Set[CaptureParam]): Set[CaptureParam] =
    xs intersect ys

  def subst: Map[CaptUnificationVar, CaptureSet] = substitution

  /**
   * Adds x as a lower bound to y, and y as a lower bound to x.
   */
  def connect(x: CNode, y: CNode, exclude: Set[CaptureParam] = Set.empty): Unit =
    if (x == y /* || (y.lowerNodes contains x) */) { return () }

    // we already solved one of them? Or both?
    (subst.get(x), subst.get(y)) match {
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
    assert (!substitution.isDefinedAt(x))
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
    assert (!substitution.isDefinedAt(x))
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

  def dumpConstraints() =
    constraintData foreach {
      case (x, CaptureNodeData(lower, upper, lowerNodes, upperNodes)) =>
        val prettyLower = lower.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("{}").padTo(10, ' ')
        val prettyUpper = upper.map { cs => "{" + cs.mkString(",") + "}" }.getOrElse("*").padTo(10, ' ')
        val prettyLowerNodes = lowerNodes.mkString(", ").padTo(10, ' ')
        val prettyUpperNodes = upperNodes.mkString(", ").padTo(10, ' ')
        val varName = x.toString.padTo(12, ' ')
        println(s"${prettyLowerNodes} | ${prettyLower} <: $x <: ${prettyUpper} | ${prettyUpperNodes}")
    }
}

object CaptureConstraintGraph {
  def empty(using ErrorReporter) = new CaptureConstraintGraph()
}