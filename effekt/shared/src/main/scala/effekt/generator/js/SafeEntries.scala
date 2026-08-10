package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

import java.util.IdentityHashMap
import scala.collection.mutable

/**
 * Chooses the representation of first-class entries.
 *
 * An indirect call enters its callee immediately. A raw entry may therefore
 * only be stored in a function value if every synchronous cycle reachable
 * through that value is already broken by a loop or by another safe entry.
 * Otherwise the value is represented by the eta expansion
 *
 *     (...args) => () => worker(...args)
 *
 * The analysis has two finite parts. First, a monotone points-to analysis
 * propagates functions and objects through parameters. It thereby exposes
 * higher-order paths such as
 *
 *     loop -> apply(next) -> next -> loop
 *
 * without enumerating concrete call paths. Second, strongly connected
 * components identify exactly those synchronous cycles that contain a native
 * stack frame. Installing an adapter cuts all safe incoming edges to its
 * entry. Repeating SCC decomposition terminates because every round adapts a
 * previously unadapted node from the finite program.
 */
object SafeEntries {

  private sealed abstract class Node(
    val ordinal: Int,
    val label: String,
    val cuttable: Boolean
  )

  private final class FunctionNode(
    val id: Id,
    ordinal: Int,
    cuttable: Boolean
  ) extends Node(ordinal, id.name.name, cuttable)

  private final class OperationNode(
    val operation: cps.Operation,
    ordinal: Int
  ) extends Node(ordinal, operation.name.name.name, cuttable = true)

  private final class ObjectNode(
    val methods: Map[Id, OperationNode]
  )

  private final class DataNode(
    val tag: Id,
    val arity: Int
  )

  private final class CellNode

  private final case class Info(
    node: Node,
    params: Vector[Id],
    body: cps.Stmt
  )

  /** A finite 0-CFA value: the internal allocation sites to which a value may
   *  point. Values supplied by external entries contain no internal site. */
  private final case class Value(
    functions: Set[Node],
    objects: Set[ObjectNode],
    data: Set[DataNode],
    cells: Set[CellNode]
  ) {
    def join(other: Value): Value = Value(
      functions ++ other.functions,
      objects ++ other.objects,
      data ++ other.data,
      cells ++ other.cells)
  }

  private object Value {
    val Empty: Value = Value(Set.empty, Set.empty, Set.empty, Set.empty)
    def function(node: Node): Value =
      Value(Set(node), Set.empty, Set.empty, Set.empty)
    def obj(node: ObjectNode): Value =
      Value(Set.empty, Set(node), Set.empty, Set.empty)
    def data(node: DataNode): Value =
      Value(Set.empty, Set.empty, Set(node), Set.empty)
    def cell(node: CellNode): Value =
      Value(Set.empty, Set.empty, Set.empty, Set(node))
  }

  private enum Location {
    case Variable(id: Id)
    case Cell(node: CellNode)
    case Field(node: DataNode, index: Int)
  }
  import Location.*

  private final case class Edge(
    source: Node,
    target: Node,
    safe: Boolean,
    addsFrame: Boolean
  )

  final class Result private[SafeEntries] (
    val definitions: Set[Id],
    private val operations: IdentityHashMap[cps.Operation, java.lang.Boolean],
    val adapters: Vector[String]
  ) {
    def needsAdapter(id: Id): Boolean = definitions.contains(id)

    def needsAdapter(operation: cps.Operation): Boolean =
      java.lang.Boolean.TRUE == operations.get(operation)

    def show: String = if adapters.isEmpty then "-" else adapters.mkString("\n")
  }

  def analyze(
    module: cps.ModuleDecl,
    transferOf: cps.Stmt => StackSafety.Transfer,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan,
    targetFlows: Vector[cps.GuardedEquality.TargetResult]
  ): Result = {
    var nextNode = 0
    def freshOrdinal(): Int = {
      val result = nextNode
      nextNode += 1
      result
    }

    val functions = mutable.LinkedHashMap.empty[Id, FunctionNode]
    val objectNodes = new IdentityHashMap[cps.Stmt.New, ObjectNode]()
    val infos = mutable.LinkedHashMap.empty[Node, Info]

    def function(id: Id): FunctionNode = functions.getOrElseUpdate(id, {
      val secondClass = isSecondClass(id) || defunctionalization.caseOf(id).isDefined
      new FunctionNode(id, freshOrdinal(), cuttable = !secondClass)
    })

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, _, _) =>
        functions(id) = new FunctionNode(id, freshOrdinal(), cuttable = true)
      case _: cps.ToplevelDefinition.Val => ()
    }
    targetFlows.foreach(_.localDefinitions.foreach(definition => function(definition.id)))

    def collect(stmt: cps.Stmt): Unit = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        val node = function(id)
        infos(node) = Info(node, params.toVector, body)
        collect(body)
        collect(rest)

      case statement @ cps.Stmt.New(_, _, operations, rest) =>
        val methods = operations.iterator.map { operation =>
          val node = new OperationNode(operation, freshOrdinal())
          infos(node) = Info(node, operation.params.toVector, operation.body)
          operation.name -> node
        }.toMap
        objectNodes.put(statement, new ObjectNode(methods))
        operations.foreach(operation => collect(operation.body))
        collect(rest)

      case cps.Stmt.Let(_, _, rest) => collect(rest)
      case cps.Stmt.Run(_, _, _, _, rest) => collect(rest)
      case cps.Stmt.If(_, thn, els) => collect(thn); collect(els)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => collect(clause.body) }
        default.foreach(collect)
      case cps.Stmt.Region(_, _, rest) => collect(rest)
      case cps.Stmt.Alloc(_, _, _, rest) => collect(rest)
      case cps.Stmt.Var(_, _, _, rest) => collect(rest)
      case cps.Stmt.Dealloc(_, rest) => collect(rest)
      case cps.Stmt.Get(_, _, rest) => collect(rest)
      case cps.Stmt.Put(_, _, rest) => collect(rest)
      case cps.Stmt.Reset(_, _, _, body, _, _) => collect(body)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => collect(body)
      case cps.Stmt.Resume(_, _, _, body, _, _) => collect(body)
      case _: (cps.Stmt.App | cps.Stmt.Invoke | cps.Stmt.Hole) => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        val node = function(id)
        infos(node) = Info(node, params.toVector, body)
        collect(body)
      case cps.ToplevelDefinition.Val(_, _, _, binding) => collect(binding)
    }

    val targetsByCall = new IdentityHashMap[cps.Stmt.App, cps.GuardedEquality.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach(target => targetsByCall.put(target.call, target)))

    // ---------------------------------------------------------------------
    // Finite higher-order flow

    val values = mutable.Map.empty[Location, Value].withDefaultValue(Value.Empty)
    val watchers = mutable.Map.empty[Location, mutable.ArrayBuffer[Int]]
    val actions = mutable.ArrayBuffer.empty[() => Unit]
    val pending = mutable.Queue.empty[Int]
    val queued = mutable.BitSet.empty
    val edges = mutable.LinkedHashSet.empty[Edge]
    val dataNodes = new IdentityHashMap[cps.Expr.Make, DataNode]()
    val cellNodes = new IdentityHashMap[cps.Stmt, CellNode]()

    def enqueue(action: Int): Unit =
      if queued.add(action) then pending.enqueue(action)

    def add(location: Location, incoming: Value): Unit = {
      val previous = values(location)
      val joined = previous.join(incoming)
      if joined != previous then {
        values(location) = joined
        watchers.get(location).foreach(_.foreach(enqueue))
      }
    }

    def watch(dependencies: IterableOnce[Location])(body: => Unit): Unit = {
      val action = actions.size
      actions += (() => body)
      dependencies.iterator.toSet.foreach { dependency =>
        watchers.getOrElseUpdate(dependency, mutable.ArrayBuffer.empty) += action
      }
      enqueue(action)
    }

    def dependency(expr: cps.Expr): Set[Location] = expr match {
      case cps.Expr.Variable(id) => Set(Variable(id))
      case _ => Set.empty
    }

    def dependencies(exprs: IterableOnce[cps.Expr]): Set[Location] =
      exprs.iterator.flatMap(dependency).toSet

    def dataNode(expression: cps.Expr.Make): DataNode = {
      val existing = dataNodes.get(expression)
      if existing != null then existing
      else {
        val created = new DataNode(expression.tag, expression.args.size)
        dataNodes.put(expression, created)
        expression.args.zipWithIndex.foreach { case (argument, index) =>
          watch(dependency(argument)) {
            add(Field(created, index), eval(argument))
          }
        }
        created
      }
    }

    def eval(expr: cps.Expr): Value = expr match {
      case cps.Expr.Variable(id) => values(Variable(id))
      case expression: cps.Expr.Make => Value.data(dataNode(expression))
      case _ => Value.Empty
    }

    def cellNode(statement: cps.Stmt): CellNode = {
      val existing = cellNodes.get(statement)
      if existing != null then existing
      else {
        val created = new CellNode
        cellNodes.put(statement, created)
        created
      }
    }

    functions.foreach { case (id, node) => add(Variable(id), Value.function(node)) }

    def propagate(arguments: List[cps.Expr], parameters: Vector[Id]): Unit =
      arguments.iterator.zip(parameters.iterator).foreach { case (argument, parameter) =>
        add(Variable(parameter), eval(argument))
      }

    def scan(stmt: cps.Stmt, source: Node): Unit = stmt match {
      case cps.Stmt.Def(_, _, _, rest) =>
        // Every definition body is scanned exactly once through `infos`.
        scan(rest, source)

      case statement @ cps.Stmt.New(id, _, _, rest) =>
        add(Variable(id), Value.obj(objectNodes.get(statement)))
        scan(rest, source)

      case cps.Stmt.Let(id, binding, rest) =>
        watch(dependency(binding)) { add(Variable(id), eval(binding)) }
        scan(rest, source)

      case app @ cps.Stmt.App(id, arguments, _) =>
        watch(Set(Variable(id)) ++ dependencies(arguments)) {
          val flowed = Option(targetsByCall.get(app)).iterator
            .flatMap(_.targets).flatMap(functions.get).toSet
          val targets = values(Variable(id)).functions ++ flowed
          val exact = functions.get(id)
          val transfer = transferOf(app)
          val dispatched = defunctionalization.dispatchForCallee(id).isDefined

          targets.foreach { target =>
            val syntacticallyKnown = exact.exists(_ eq target)
            val jump = dispatched || syntacticallyKnown && transfer == StackSafety.Transfer.Jump
            val safe = !dispatched && (!syntacticallyKnown || transfer == StackSafety.Transfer.Safe)
            edges += Edge(source, target, safe, addsFrame = !jump)
            propagate(arguments, infos(target).params)
          }
        }

      case cps.Stmt.Invoke(id, method, arguments) =>
        watch(Set(Variable(id)) ++ dependencies(arguments)) {
          values(Variable(id)).objects.foreach { obj =>
            obj.methods.get(method).foreach { target =>
              edges += Edge(source, target, safe = true, addsFrame = true)
              propagate(arguments, infos(target).params)
            }
          }
        }

      case cps.Stmt.Run(_, _, _, _, rest) =>
        scan(rest, source)

      case cps.Stmt.If(_, thn, els) =>
        scan(thn, source)
        scan(els, source)

      case cps.Stmt.Match(scrutinee, clauses, default) =>
        val installed = mutable.Set.empty[(DataNode, Int)]
        clauses.foreach { case (_, clause) =>
          scan(clause.body, source)
        }
        default.foreach(scan(_, source))
        watch(dependency(scrutinee)) {
          val byTag = clauses.zipWithIndex.groupMap(_._1._1)(_._2)
          eval(scrutinee).data.foreach { node =>
            byTag.getOrElse(node.tag, Nil).foreach { clauseIndex =>
              if installed.add(node -> clauseIndex) then {
                val clause = clauses(clauseIndex)._2
                clause.params.iterator.zipWithIndex.foreach { case (parameter, index) =>
                  if index < node.arity then
                    watch(List(Field(node, index))) {
                      add(Variable(parameter), values(Field(node, index)))
                    }
                }
              }
            }
          }
        }

      case cps.Stmt.Region(_, _, rest) => scan(rest, source)
      case statement @ cps.Stmt.Alloc(id, init, _, rest) =>
        val cell = cellNode(statement)
        add(Variable(id), Value.cell(cell))
        watch(dependency(init)) { add(Cell(cell), eval(init)) }
        scan(rest, source)
      case statement @ cps.Stmt.Var(id, init, _, rest) =>
        val cell = cellNode(statement)
        add(Variable(id), Value.cell(cell))
        watch(dependency(init)) { add(Cell(cell), eval(init)) }
        scan(rest, source)
      case cps.Stmt.Dealloc(_, rest) => scan(rest, source)
      case cps.Stmt.Get(ref, id, rest) =>
        val installed = mutable.Set.empty[CellNode]
        watch(List(Variable(ref))) {
          values(Variable(ref)).cells.foreach { cell =>
            if installed.add(cell) then
              watch(List(Cell(cell))) { add(Variable(id), values(Cell(cell))) }
          }
        }
        scan(rest, source)
      case cps.Stmt.Put(ref, value, rest) =>
        watch(Set(Variable(ref)) ++ dependency(value)) {
          values(Variable(ref)).cells.foreach(cell => add(Cell(cell), eval(value)))
        }
        scan(rest, source)

      case cps.Stmt.Reset(_, _, _, body, _, _) => scan(body, source)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => scan(body, source)
      case cps.Stmt.Resume(_, _, _, body, _, _) => scan(body, source)

      case _: cps.Stmt.Hole => ()
    }

    infos.valuesIterator.foreach(info => scan(info.body, info.node))

    while pending.nonEmpty do {
      val action = pending.dequeue()
      queued -= action
      actions(action)()
    }

    // ---------------------------------------------------------------------
    // Positive synchronous cycles

    val nodes = infos.keysIterator.toVector.sortBy(_.ordinal)
    val unsafe = mutable.LinkedHashSet.empty[Node]

    /** Strongly connected components of the graph left after the currently
     *  selected adapters cut their safe incoming edges. */
    def components(): Vector[Vector[Node]] = {
      val active = edges.iterator
        .filterNot(edge => edge.safe && unsafe.contains(edge.target))
        .toVector
      val outgoing = active.groupMap(_.source)(identity).withDefaultValue(Vector.empty)
      val incoming = active.groupMap(_.target)(_.source).withDefaultValue(Vector.empty)

      final case class Frame(node: Node, edges: Vector[Edge], var next: Int)
      val visited = mutable.Set.empty[Node]
      val order = mutable.ArrayBuffer.empty[Node]

      nodes.foreach { root =>
        if visited.add(root) then {
          val stack = mutable.ArrayBuffer(Frame(root, outgoing(root), 0))
          while stack.nonEmpty do {
            val frame = stack.last
            if frame.next == frame.edges.size then {
              order += frame.node
              stack.remove(stack.size - 1)
            } else {
              val target = frame.edges(frame.next).target
              frame.next += 1
              if visited.add(target) then
                stack += Frame(target, outgoing(target), 0)
            }
          }
        }
      }

      val assigned = mutable.Set.empty[Node]
      val result = mutable.ArrayBuffer.empty[Vector[Node]]
      order.reverseIterator.foreach { root =>
        if assigned.add(root) then {
          val component = mutable.ArrayBuffer.empty[Node]
          val stack = mutable.Stack(root)
          while stack.nonEmpty do {
            val node = stack.pop()
            component += node
            incoming(node).foreach { predecessor =>
              if assigned.add(predecessor) then stack.push(predecessor)
            }
          }
          result += component.toVector
        }
      }
      result.toVector
    }

    def findCuts(): Set[Node] = {
      val active = edges.iterator
        .filterNot(edge => edge.safe && unsafe.contains(edge.target))
        .toVector

      components().iterator.flatMap { component =>
        val members = component.toSet
        val internal = active.filter(edge =>
          members.contains(edge.source) && members.contains(edge.target))
        val cyclic = component.size > 1 || internal.exists(edge => edge.source eq edge.target)
        val positive = internal.exists(_.addsFrame)
        val entersThroughSafeValue = internal.exists(_.safe)

        // StackSafety already certifies the graph that contains only Direct
        // and Jump transfers. SafeEntries is responsible precisely for the
        // additional cycles obtained by entering value-level Safe edges
        // immediately. Ignoring an SCC without such an edge is important:
        // 0-CFA can merge unrelated second-class continuation states into a
        // spurious SCC, but none of those entries can or needs to be adapted.
        if cyclic && positive && entersThroughSafeValue then {
          val candidates = internal.filter(edge => edge.safe && edge.target.cuttable)
          assert(candidates.nonEmpty,
            s"direct-call cycle has no safe entry: ${component.map(_.label).mkString(" -> ")}")
          Some(candidates.minBy(_.target.ordinal).target)
        } else None
      }.toSet
    }

    var cuts = findCuts()
    while cuts.nonEmpty do {
      unsafe ++= cuts
      cuts = findCuts()
    }

    val unsafeDefinitions = unsafe.collect {
      case node: FunctionNode if node.cuttable => node.id
    }.toSet
    val unsafeOperations = new IdentityHashMap[cps.Operation, java.lang.Boolean]()
    unsafe.foreach {
      case node: OperationNode =>
        unsafeOperations.put(node.operation, java.lang.Boolean.TRUE)
      case _ => ()
    }
    val adapters = unsafe.iterator.map {
      case node: FunctionNode => s"function ${node.label}"
      case node: OperationNode => s"operation ${node.label}"
    }.toVector.sorted
    Result(unsafeDefinitions, unsafeOperations, adapters)
  }
}
