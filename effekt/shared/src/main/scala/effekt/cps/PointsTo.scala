package effekt
package cps

import core.Id
import scala.collection.mutable

/** The collecting control projection of [[AbstractMachine]] (0-CFA).
 *
 *  `approximateCallResults` is useful after calling-convention lowering, where
 *  direct `Return` is present. Before lowering, setting it to `false` follows
 *  the explicit CPS continuation of a compositional call precisely. */
class PointsTo(
  module: ModuleDecl,
  approximateCallResults: Boolean = true
) extends AbstractMachine(module) {

  type Property = Unit
  type Context = Unit

  def bottom: Unit = ()
  def external: Unit = ()
  def join(left: Unit, right: Unit): Unit = ()

  def literal(value: Any, annotatedType: core.ValueType): Unit = ()
  def closure(value: Value.Closure, context: Unit): Unit = ()
  def instance(value: Value.Object, context: Unit): Unit = ()
  def constructor(value: Value.Constructor, context: Unit): Unit = ()

  def initialContext: Unit = ()
  def tick(
    call: Option[Stmt],
    callee: Value.Closure,
    arguments: List[Values],
    caller: Unit
  ): Unit = ()

  // Calling-convention lowering can introduce direct calls and returns. Their
  // results are conservatively external; this analysis only needs call targets.
  override protected def reifyContinuations: Boolean = !approximateCallResults

  private val targets = new java.util.IdentityHashMap[Stmt, mutable.Set[Id]]()
  private val opened = new java.util.IdentityHashMap[Stmt, java.lang.Boolean]()

  private def record(statement: Stmt, resolved: Set[Id], open: Boolean): Unit = {
    var set = targets.get(statement)
    if set == null then {
      set = mutable.Set.empty
      targets.put(statement, set)
    }
    set ++= resolved
    if open then opened.put(statement, java.lang.Boolean.TRUE)
  }

  override protected def observeApply(
    statement: Stmt,
    callee: Values,
    resolved: Set[Id],
    arguments: List[Values],
    context: Unit
  ): Unit = record(
    statement,
    resolved,
    callee.open || callee.values.exists {
      case closure: Value.Closure => !resolved.contains(closure.function)
      case _ => false
    })

  override protected def observeInvoke(
    statement: Stmt,
    receiver: Values,
    method: Id,
    resolved: Set[Id],
    arguments: List[Values],
    context: Unit
  ): Unit = record(
    statement,
    resolved,
    receiver.open || receiver.values.exists {
      case value: Value.Object => !value.operations.contains(method)
      case _ => false
    })

  private val escaped = mutable.Set.empty[Id]
  override protected def observeEscape(value: Values): Unit =
    value.values.foreach {
      case closure: Value.Closure => escaped += closure.function
      case value: Value.Object =>
        escaped ++= value.operations.valuesIterator.map(_.function)
      case _: Value.Constructor => ()
    }

  private val arityRigid = mutable.Set.empty[Id]
  override protected def observeArityMismatch(target: Id): Unit = arityRigid += target

  private lazy val computed: Unit = run()

  def targetsAt(statement: Stmt): Set[Id] = {
    computed
    Option(targets.get(statement)).fold(Set.empty[Id])(_.toSet)
  }

  def closedAt(statement: Stmt): Boolean = {
    computed
    opened.get(statement) == null
  }

  def escapedFunctions: Set[Id] = {
    computed
    escaped.toSet
  }

  def rigidFunctions: Set[Id] = {
    computed
    escaped.toSet ++ arityRigid
  }
}
