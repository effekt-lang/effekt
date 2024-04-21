package effekt
package typer

import effekt.symbols.*
import effekt.util.messages.{ ErrorReporter, ErrorMessageReifier }
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

) {

  /**
   * The currently known substitutions
   */
  def subst: Substitutions = ???

  /**
   * Should only be called on unification variables where we do not know any types, yet
   *
   * It will *not* compare the types, but only equality imposed by constraints.
   */
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean = ???

  def isSubset(lower: Captures, upper: Captures): Boolean = ???

  /**
   * Retreive the potentially known type of [[x]]
   */
  def typeOf(x: UnificationVar): Option[ValueType] = ???

  /**
   * Learn that unification variable [[x]] needs to be compatible with [[y]]. If there already is
   * a type for [[x]], we will invoke [[merge]] to check compatibility (and potentially error out).
   */
  def learn(x: UnificationVar, y: ValueType)(merge: (ValueType, ValueType) => Unit): Unit = ()

  /**
   * Marks [[xs]] as pending inactive and solves them, if they do not have active bounds.
   */
  def leave(types: List[UnificationVar], capts: List[CaptUnificationVar]): Unit = ()

  def solve(toRemove: Set[CNode]): Unit = ()

  def forceSolving(node: CNode): CaptureSet = ???


  override def clone(): Constraints = new Constraints(typeSubstitution, classes, captureConstraints, captSubstitution, pendingInactive)

  def dumpTypeConstraints() = ()

  def dumpCaptureConstraints() = ()

  def dumpConstraints() = ()
}
