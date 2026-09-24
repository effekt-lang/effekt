package effekt
package generator
package js

import effekt.core.Id
import effekt.cps
import effekt.cps.knownArguments

import java.util.IdentityHashMap
import scala.collection.mutable

/**
 * Chooses a stack-safe implementation for every tail transfer that remains
 * after local definitions and continuation cases have become JavaScript
 * labels.
 *
 * A direct transfer retains the current JavaScript activation. Consequently,
 * direct transfers must admit a finite ranking. A closed feedback edge,
 * including an indirect one with a finite target set, can bounce at its call
 * site. An open transfer instead relies on the callee's stack-safe value
 * entry. Jumps stay in one activation and do not participate in the ranking.
 */
object StackSafety {

  enum Transfer {
    case Jump, Direct, Bounce, Safe
  }

  private final class Site(
    val stmt: cps.Stmt,
    val callee: String
  ) {
    val owners = mutable.LinkedHashSet.empty[Id]
    val sources = mutable.LinkedHashSet.empty[Id]
    var targets = Vector.empty[Id]
    var closed = false
    var known = false
    var transfer = Transfer.Safe
  }

  private[js] final case class ResidualCall(
    statement: cps.Stmt,
    sources: Set[Id],
    targets: Vector[Id],
    known: Boolean
  )

  final class Plan private[StackSafety] (
    private val transfers: IdentityHashMap[cps.Stmt, Transfer],
    val ranks: Map[Id, Int],
    private val sites: Vector[Site],
    private val loopified: Set[Id],
    private val loopMutations: Map[Id, Set[Id]],
    private val entries: EntrySafety.Result
  ) {
    private val immediateTargets: Set[Id] =
      sites.iterator
        .filter(site => site.transfer == Transfer.Direct || site.transfer == Transfer.Bounce)
        .flatMap { site =>
          site.stmt match {
            // JavaScript generation can bypass an adapter only for a
            // syntactically named entry. A closed indirect call still invokes
            // the value representation, even when its target set is known.
            case cps.Stmt.Call(cps.Callee.Function(id), _,
                _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump)
                if site.targets.contains(id) => Iterator.single(id)
            case _ => Iterator.empty
          }
        }
        .toSet

    // Every bounced application is an ordered site whose transfer was
    // rewritten to `Bounce` before `transfers` was filled, so this map is the
    // single source of truth.
    def transferOf(stmt: cps.Stmt): Transfer =
      Option(transfers.get(stmt)).getOrElse(Transfer.Safe)

    /** A stack-safe entry needs a separate immediate worker precisely when a
     *  known transfer bypasses it. Otherwise its adapter and body are one
     *  suspended function. */
    def needsWorker(id: Id): Boolean =
      entries.needsAdapter(id) && immediateTargets.contains(id)

    def needsAdapter(id: Id): Boolean = entries.needsAdapter(id)

    def needsAdapter(operation: cps.Operation): Boolean =
      entries.needsAdapter(operation)

    def adapterDefinitions: Set[Id] = entries.definitions

    def isSegmentEntry(id: Id): Boolean = entries.segmentEntries.contains(id)

    def preservesSegments(application: cps.Stmt): Boolean =
      entries.preservesSegments(application)

    def showSafeEntries: String = entries.show

    /** A JavaScript loop is useful exactly when translation turns a call from
     *  a definition's own body into a back jump to that definition. Recursive
     *  SCC membership alone is not sufficient. */
    def isLoopified(id: Id): Boolean = loopified.contains(id)

    /** Parameters whose loop registers can receive a different value on a
     *  back edge. Identity assignments do not make a parameter mutable. */
    def mutableParameters(id: Id): Set[Id] =
      loopMutations.getOrElse(id, Set.empty)

    /** Independently check the ranking certificate carried by this plan. */
    def validate(): Unit =
      sites.foreach { site =>
        transferOf(site.stmt) match {
          case Transfer.Direct =>
            assert(site.closed, s"Direct call ${site.callee} has an open target set")
            assert(site.targets.nonEmpty, s"Direct call ${site.callee} has no targets")
            site.sources.foreach { source =>
              site.targets.foreach { target =>
                assert(
                  ranks.getOrElse(source, 0) > ranks.getOrElse(target, 0),
                  s"Direct call ${name(source)} -> ${name(target)} does not decrease its stack rank")
              }
            }

          case Transfer.Bounce =>
            assert(site.closed, s"Bounced call ${site.callee} has an open target set")
            assert(site.targets.nonEmpty, s"Bounced call ${site.callee} has no targets")

          case Transfer.Jump | Transfer.Safe => ()
        }
      }

    def show: String = {
      val rankLines = ranks.toVector
        .sortBy { case (id, rank) => (-rank, name(id), id.id) }
        .map { case (id, rank) => s"  ${name(id)} = $rank" }

      val transferLines = sites.map { site =>
        val owners = site.owners.iterator.map(name).mkString(" | ")
        val source = if owners.nonEmpty then owners else "local"
        val transfer = transferOf(site.stmt)
        val target = transfer match {
          case Transfer.Jump => ""
          case _ if !site.closed =>
            val known = site.targets.map(name)
            s" [${(known :+ "?").mkString(", ")}]"
          case _ => s" [${site.targets.map(name).mkString(", ")}]"
        }
        s"  $source -> ${site.callee}: ${transfer.toString.toLowerCase}$target"
      }

      s"ranks\n${rankLines.mkString("\n")}\ntransfers\n${transferLines.mkString("\n")}"
    }
  }

  private def name(id: Id): String = id.name.name

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan
  ): Plan =
    analyze(
      module,
      isRecursive,
      isSecondClass,
      defunctionalization,
      module.definitions.map(cps.Targets.targets).toVector)

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan,
    targetFlows: Vector[cps.Targets.TargetResult]
  ): Plan =
    analyze(
      module,
      isRecursive,
      isSecondClass,
      defunctionalization,
      targetFlows,
      Set.empty,
      Map.empty)

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan,
    targetFlows: Vector[cps.Targets.TargetResult],
    directDefinitions: Set[Id],
    directEntries: Map[Id, Vector[Id]]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)
    val sitesByStmt = new IdentityHashMap[cps.Stmt, Site]()
    val orderedSites = mutable.ArrayBuffer.empty[Site]
    val nodeOrder = mutable.LinkedHashSet.empty[Id]
    // A dispatcher re-enters its owning definition through the generated
    // apply loop even if no source-level self call remains in its body.
    val loopified = mutable.LinkedHashSet.from(defunctionalization.dispatches.keys)

    def siteFor(stmt: cps.Stmt, callee: => String): Site = {
      val existing = sitesByStmt.get(stmt)
      if existing != null then existing
      else {
        val created = new Site(stmt, callee)
        sitesByStmt.put(stmt, created)
        orderedSites += created
        created
      }
    }

    // The target analysis is deliberately kept separate from the stack
    // solver. A syntactic call site denotes one grouped set of transitions:
    // it can only be direct if all of those transitions decrease the rank.
    val targetsByCall = new IdentityHashMap[cps.Stmt, cps.Targets.CallTargets]()
    val parameters = mutable.LinkedHashMap.empty[Id, Vector[Id]]

    module.definitions.zip(targetFlows).foreach { case (toplevel, flow) =>
      toplevel match {
        case cps.ToplevelDefinition.Def(id, params, _) => parameters(id) = params.toVector
        case _: cps.ToplevelDefinition.Val => ()
      }
      flow.localDefinitions.foreach(definition => parameters(definition.id) = definition.params.toVector)
      flow.callTargets.foreach(target => targetsByCall.put(target.call, target))
    }

    val loopMutations = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Id]]
    val operationActivations = new IdentityHashMap[cps.Operation, Id]()

    def operationActivation(operation: cps.Operation): Id = {
      val existing = operationActivations.get(operation)
      if existing != null then existing
      else {
        val created = Id(operation.name.name.name)
        operationActivations.put(operation, created)
        created
      }
    }

    final case class Host(owner: Id, secondClass: Set[Id], insideBody: Set[Id])
    val hosts = mutable.LinkedHashMap.empty[Id, Host]

    def recordCall(
      stmt: cps.Stmt,
      callee: String,
      owner: Id,
      jump: Boolean
    ): Unit = {
      val site = siteFor(stmt, callee)
      site.owners += owner
      nodeOrder += owner
      if !jump then site.sources += owner
    }

    def visit(
      stmt: cps.Stmt,
      owner: Id,
      secondClass: Set[Id],
      insideBody: Set[Id],
      frameCaptures: Set[Id] = Set.empty
    ): Unit = stmt match {
      case cps.Stmt.Def(id, _, body, rest) =>
        defunctionalization.caseOf(id) match {
          case Some(_) =>
            // Its body is emitted by every dispatcher that contains this case.
            visit(rest, owner, secondClass, insideBody, frameCaptures)

          case None if isSecondClass(id) =>
            val available = secondClass + id
            val inside = if isRecursive(id) then insideBody + id else insideBody
            hosts(id) = Host(owner, available, inside)
            visit(rest, owner, available, insideBody, frameCaptures)
            visit(body, owner, available, inside, frameCaptures)

          case None =>
            val available = if isRecursive(id) then Set(id) else Set.empty[Id]
            val inside = if isRecursive(id) then Set(id) else Set.empty[Id]
            hosts(id) = Host(id, available, inside)
            nodeOrder += id
            visit(body, id, available, inside, frameCaptures)
            visit(rest, owner, secondClass, insideBody, frameCaptures)
        }

      case cps.Stmt.New(_, _, operations, rest) =>
        operations.foreach { operation =>
          val activation = operationActivation(operation)
          nodeOrder += activation
          visit(operation.body, activation, Set.empty, Set.empty, frameCaptures)
        }
        visit(rest, owner, secondClass, insideBody, frameCaptures)

      case cps.Stmt.Let(_, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)

      case cps.Stmt.Call(_, _, cps.ReturnPoint.Bind(_, _, _, rest)) =>
        visit(rest, owner, secondClass, insideBody, frameCaptures)

      case call @ cps.Stmt.Call(callee, _,
          _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
        visitTransfer(call, callee, call.knownArguments, owner,
          secondClass, insideBody, frameCaptures)

      case _: cps.Stmt.Return => ()

      case cps.Stmt.Run(_, _, _, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.If(_, thn, els) =>
        visit(thn, owner, secondClass, insideBody, frameCaptures)
        visit(els, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) =>
          visit(clause.body, owner, secondClass, insideBody, frameCaptures)
        }
        default.foreach(visit(_, owner, secondClass, insideBody, frameCaptures))
      case cps.Stmt.Region(_, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Alloc(_, _, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Var(_, _, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Dealloc(_, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Get(_, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Put(_, _, rest) => visit(rest, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Reset(_, _, _, body, _, _) =>
        visit(body, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) =>
        visit(body, owner, secondClass, insideBody, frameCaptures)
      case cps.Stmt.Resume(_, _, _, body, _, _) =>
        visit(body, owner, secondClass, insideBody, frameCaptures)
      case _: cps.Stmt.Hole => ()
    }

    def visitTransfer(
      statement: cps.Stmt,
      callee: cps.Callee,
      arguments: List[cps.Expr],
      owner: Id,
      secondClass: Set[Id],
      insideBody: Set[Id],
      frameCaptures: Set[Id]
    ): Unit = callee match {
      case cps.Callee.Function(id) =>
        val dispatch = defunctionalization.dispatchFor(statement).isDefined
        val selfJump = insideBody.contains(id)
        val jump = dispatch || secondClass.contains(id) || selfJump
        if selfJump then {
          loopified += id
          val params = parameters.getOrElse(id, Vector.empty)
          val mutated = loopMutations.getOrElseUpdate(id, mutable.LinkedHashSet.empty)
          if params.size != arguments.size then mutated ++= params
          else params.zip(arguments).foreach {
            // A case capture keeps its CPS id, but JavaScript reads it from
            // the immutable frame rather than from the current loop register.
            // Hence syntactic p := p is an update precisely in this case.
            case (param, cps.Expr.Variable(argument))
                if param == argument && !frameCaptures(argument) => ()
            case (param, _) => mutated += param
          }
        }
        recordCall(statement, name(id), owner, jump)

      case cps.Callee.Method(id, method) =>
        recordCall(statement, s"${name(id)}.${name(method)}", owner, jump = false)
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, _, body) =>
        val available = if isRecursive(id) then Set(id) else Set.empty[Id]
        hosts(id) = Host(id, available, available)
        nodeOrder += id
        visit(body, id, available, available)

      case cps.ToplevelDefinition.Val(id, _, _, binding) =>
        hosts(id) = Host(id, Set.empty, Set.empty)
        nodeOrder += id
        visit(binding, id, Set.empty, Set.empty)
    }

    // Continuation cases are copied into the JavaScript function containing
    // their dispatcher. A shared case may therefore contribute transitions
    // from several source activations; the call-site decision groups them.
    defunctionalization.dispatches.values.toVector
      .sortBy(dispatch => (name(dispatch.entry), dispatch.entry.id))
      .foreach { dispatch =>
        val Host(owner, available, inside) =
          hosts.getOrElse(dispatch.entry, Host(dispatch.entry, Set.empty, Set.empty))
        dispatch.cases.foreach { continuationCase =>
          visit(
            continuationCase.body,
            owner,
            available,
            inside,
            continuationCase.captures.toSet)
        }
      }

    val definitionIds = parameters.keySet.toSet

    orderedSites.foreach { site =>
      if site.sources.isEmpty then {
        site.closed = true
        site.transfer = Transfer.Jump
      } else (site.stmt match {
        case call @ cps.Stmt.Call(callee, _,
            _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
          Some(callee -> call.knownArguments)
        case _ => None
      }) match {
        case Some((cps.Callee.Function(id), arguments)) =>
          parameters.get(id) match {
            case Some(params) if params.size == arguments.size && !isSecondClass(id) && defunctionalization.caseOf(id).isEmpty =>
              site.targets = Vector(id)
              site.closed = true
              site.known = true

            case _ =>
              Option(targetsByCall.get(site.stmt)) match {
                case Some(flow) =>
                  val ordered = flow.targets.toVector.sortBy(target => (name(target), target.id))
                  val representable = ordered.forall(target =>
                    definitionIds.contains(target) &&
                      !isSecondClass(target) &&
                      defunctionalization.caseOf(target).isEmpty)
                  site.targets = ordered
                  site.closed = flow.closed && representable && ordered.nonEmpty
                case None =>
                  site.targets = Vector.empty
                  site.closed = false
              }
          }

        case Some((_: cps.Callee.Method, _)) =>
          // Receiver-flow analysis can later turn this into a closed target
          // set. Until then invocation is an open control transfer.
          site.targets = Vector.empty
          site.closed = false

        case None => ()
      }
    }

    // Jumps are already stack neutral. Every other closed transfer is
    // provisionally immediate; EntrySafety chooses the necessary call-site
    // and entry cuts on the complete higher-order activation graph below.
    orderedSites.foreach { site =>
      if site.sources.isEmpty then site.transfer = Transfer.Jump
      else if site.closed then site.transfer = Transfer.Direct
      else site.transfer = Transfer.Safe
    }

    val transfers = new IdentityHashMap[cps.Stmt, Transfer]()
    orderedSites.foreach(site => transfers.put(site.stmt, site.transfer))
    val entrySafety = EntrySafety.analyze(
      module,
      stmt => Option(transfers.get(stmt)).getOrElse(Transfer.Safe),
      isSecondClass,
      defunctionalization,
      targetFlows,
      directDefinitions,
      directEntries,
      operationActivations,
      orderedSites.iterator.collect {
        case site if site.closed && site.sources.nonEmpty => ResidualCall(
          site.stmt, site.sources.toSet, site.targets, site.known)
      }.toVector)

    orderedSites.foreach { site =>
      if entrySafety.bouncesAt(site.stmt) then site.transfer = Transfer.Bounce
      transfers.put(site.stmt, site.transfer)
    }

    val directEdges = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Id]]
    orderedSites.filter(_.transfer == Transfer.Direct).foreach { site =>
      site.sources.foreach { source =>
        val targets = directEdges.getOrElseUpdate(source, mutable.LinkedHashSet.empty)
        targets ++= site.targets
      }
    }

    // Topologically rank the direct graph without recursive graph traversal.
    val nodes = mutable.LinkedHashSet.from(nodeOrder)
    directEdges.foreach { case (source, targets) =>
      nodes += source
      nodes ++= targets
    }
    val indegree = mutable.Map.from(nodes.iterator.map(_ -> 0))
    directEdges.valuesIterator.flatten.foreach { target => indegree(target) = indegree(target) + 1 }
    val ready = mutable.Queue.from(nodes.iterator.filter(indegree(_) == 0))
    val topological = mutable.ArrayBuffer.empty[Id]
    while ready.nonEmpty do {
      val source = ready.dequeue()
      topological += source
      directEdges.getOrElse(source, mutable.LinkedHashSet.empty).foreach { target =>
        val next = indegree(target) - 1
        indegree(target) = next
        if next == 0 then ready.enqueue(target)
      }
    }
    assert(topological.size == nodes.size, "Stack-safety planner left a direct-call cycle")

    val ranks = mutable.Map.empty[Id, Int]
    topological.reverseIterator.foreach { source =>
      val rank = directEdges.getOrElse(source, mutable.LinkedHashSet.empty)
        .iterator.map(target => ranks.getOrElse(target, 0) + 1)
        .maxOption.getOrElse(0)
      ranks(source) = rank
    }

    val plan = new Plan(
      transfers,
      ranks.toMap,
      orderedSites.toVector,
      loopified.toSet,
      loopMutations.iterator.map { case (id, params) => id -> params.toSet }.toMap,
      entrySafety)
    plan.validate()
    plan
  }
}

/**
 * Chooses the representation of first-class entries and tracks entries into
 * delimited-continuation segments.
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
 * stack frame. A closed indirect edge can be cut locally by suspending that
 * call. Otherwise an adapter cuts all safe incoming edges to its entry.
 * Repeating SCC decomposition terminates because every round removes an edge
 * or adapts a node from the finite program.
 */
private[js] object EntrySafety {

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
    body: Option[cps.Stmt]
  )

  /** A finite 0-CFA value: the internal allocation sites to which a value may
   *  point, together with segment-entry provenance. Values supplied by
   *  external entries contain no internal site. */
  private final case class Value(
    functions: Set[Node],
    objects: Set[ObjectNode],
    data: Set[DataNode],
    cells: Set[CellNode],
    segmentEntry: Boolean
  ) {
    def join(other: Value): Value = Value(
      functions ++ other.functions,
      objects ++ other.objects,
      data ++ other.data,
      cells ++ other.cells,
      segmentEntry || other.segmentEntry)

    /** Crossing an ordinary value boundary exposes a raw segment entry
     *  through its stack-safe adapter. */
    def expose: Value =
      if segmentEntry then copy(segmentEntry = false) else this
  }

  private object Value {
    val Empty: Value = Value(Set.empty, Set.empty, Set.empty, Set.empty, false)
    val SegmentEntry: Value = Empty.copy(segmentEntry = true)
    def function(node: Node): Value =
      Value(Set(node), Set.empty, Set.empty, Set.empty, false)
    def obj(node: ObjectNode): Value =
      Value(Set.empty, Set(node), Set.empty, Set.empty, false)
    def data(node: DataNode): Value =
      Value(Set.empty, Set.empty, Set(node), Set.empty, false)
    def cell(node: CellNode): Value =
      Value(Set.empty, Set.empty, Set.empty, Set(node), false)
  }

  private enum Location {
    case Variable(id: Id)
    case ReturnValue(node: Node)
    case Cell(node: CellNode)
    case Field(node: DataNode, index: Int)
  }
  import Location.*

  private final case class Edge(
    source: Node,
    target: Node,
    safe: Boolean,
    addsFrame: Boolean,
    callSite: Option[CallSite] = None
  )

  /** Identity of a syntactic application whose finite target set is closed.
   *  All abstract edges contributed by one application are cut together. */
  private final class CallSite(val application: cps.Stmt)

  final class Result private[EntrySafety] (
    val definitions: Set[Id],
    private val operations: IdentityHashMap[cps.Operation, java.lang.Boolean],
    private val bounces: IdentityHashMap[cps.Stmt, java.lang.Boolean],
    val segmentEntries: Set[Id],
    private val segmentPreservingCalls: IdentityHashMap[cps.Stmt, java.lang.Boolean],
    val adapters: Vector[String]
  ) {
    def needsAdapter(id: Id): Boolean = definitions.contains(id)

    def needsAdapter(operation: cps.Operation): Boolean =
      java.lang.Boolean.TRUE == operations.get(operation)

    def bouncesAt(application: cps.Stmt): Boolean =
      java.lang.Boolean.TRUE == bounces.get(application)

    def preservesSegments(application: cps.Stmt): Boolean =
      java.lang.Boolean.TRUE == segmentPreservingCalls.get(application)

    def show: String = if adapters.isEmpty then "-" else adapters.mkString("\n")
  }

  def analyze(
    module: cps.ModuleDecl,
    transferOf: cps.Stmt => StackSafety.Transfer,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan,
    targetFlows: Vector[cps.Targets.TargetResult],
    directDefinitions: Set[Id],
    directEntries: Map[Id, Vector[Id]],
    operationActivations: IdentityHashMap[cps.Operation, Id],
    residualCalls: Vector[StackSafety.ResidualCall]
  ): Result = {
    var nextNode = 0
    def freshOrdinal(): Int = {
      val result = nextNode
      nextNode += 1
      result
    }

    // A direct definition can have two distinct entries: its value-returning
    // worker and its CPS-facing function value. Keeping both nodes explicit
    // lets the cycle analysis decide whether the latter must suspend.
    val functions = mutable.LinkedHashMap.empty[Id, FunctionNode]
    val entries = mutable.LinkedHashMap.empty[Id, FunctionNode]
    val operationNodes = mutable.LinkedHashMap.empty[Id, OperationNode]
    val objectNodes = new IdentityHashMap[cps.Stmt.New, ObjectNode]()
    val infos = mutable.LinkedHashMap.empty[Node, Info]

    def function(id: Id): FunctionNode = functions.getOrElseUpdate(id, {
      val secondClass = isSecondClass(id) || defunctionalization.caseOf(id).isDefined
      new FunctionNode(
        id,
        freshOrdinal(),
        cuttable = !secondClass && !directDefinitions.contains(id))
    })

    def entry(id: Id): FunctionNode =
      entries.getOrElse(id, function(id))

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, _, _) =>
        functions(id) = new FunctionNode(
          id,
          freshOrdinal(),
          cuttable = !directDefinitions.contains(id))
      case _: cps.ToplevelDefinition.Val => ()
    }
    targetFlows.foreach(_.localDefinitions.foreach(definition => function(definition.id)))
    directEntries.foreach { case (id, params) =>
      val node = new FunctionNode(id, freshOrdinal(), cuttable = true)
      entries(id) = node
      infos(node) = Info(node, params, None)
    }

    def collect(stmt: cps.Stmt): Unit = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        val node = function(id)
        infos(node) = Info(node, params.toVector, Some(body))
        collect(body)
        collect(rest)

      case statement @ cps.Stmt.New(_, _, operations, rest) =>
        val methods = operations.iterator.map { operation =>
          val node = new OperationNode(operation, freshOrdinal())
          operationNodes(operationActivations.get(operation)) = node
          infos(node) = Info(node, operation.params.toVector, Some(operation.body))
          operation.name -> node
        }.toMap
        objectNodes.put(statement, new ObjectNode(methods))
        operations.foreach(operation => collect(operation.body))
        collect(rest)

      case cps.Stmt.Let(_, _, rest) => collect(rest)
      case cps.Stmt.Call(_, _, cps.ReturnPoint.Bind(_, _, _, rest)) => collect(rest)
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
      case cps.Stmt.Call(_, _, _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) |
          _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        val node = function(id)
        infos(node) = Info(node, params.toVector, Some(body))
        collect(body)
      case cps.ToplevelDefinition.Val(_, _, _, binding) => collect(binding)
    }

    val targetsByCall = new IdentityHashMap[cps.Stmt, cps.Targets.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach(target => targetsByCall.put(target.call, target)))
    val residualByCall = new IdentityHashMap[cps.Stmt, StackSafety.ResidualCall]()
    residualCalls.foreach(call => residualByCall.put(call.statement, call))

    // ---------------------------------------------------------------------
    // Finite higher-order flow

    val values = mutable.Map.empty[Location, Value].withDefaultValue(Value.Empty)
    val watchers = mutable.Map.empty[Location, mutable.ArrayBuffer[Int]]
    val actions = mutable.ArrayBuffer.empty[() => Unit]
    val pending = mutable.Queue.empty[Int]
    val queued = mutable.BitSet.empty
    val edges = mutable.LinkedHashSet.empty[Edge]
    val callSites = new IdentityHashMap[cps.Stmt, CallSite]()
    val segmentPreservingCalls = new IdentityHashMap[cps.Stmt, java.lang.Boolean]()
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
            add(Field(created, index), eval(argument).expose)
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

    functions.foreach { case (id, _) => add(Variable(id), Value.function(entry(id))) }

    def worker(node: Node): Node = node match {
      case function: FunctionNode => functions.getOrElse(function.id, function)
      case other => other
    }

    def callSite(application: cps.Stmt): CallSite = {
      val existing = callSites.get(application)
      if existing != null then existing
      else {
        val created = new CallSite(application)
        callSites.put(application, created)
        created
      }
    }

    def valueEntry(node: Node): Node = node match {
      case function: FunctionNode => entry(function.id)
      case other => other
    }

    def propagate(
      arguments: List[cps.Expr],
      parameters: Vector[Id],
      preserveSegments: Boolean = false
    ): Unit =
      arguments.iterator.zip(parameters.iterator).foreach { case (argument, parameter) =>
        val value = eval(argument)
        add(Variable(parameter), if preserveSegments then value else value.expose)
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

      case call @ cps.Stmt.Call(
            cps.Callee.Function(id), arguments,
            cps.ReturnPoint.Bind(results, _, ks, rest)) =>
        val supplied = arguments :+ ks
        val installedResults = mutable.Set.empty[Node]
        watch(Set(Variable(id)) ++ dependencies(supplied)) {
          val flowed = Option(targetsByCall.get(call)).iterator
            .flatMap(_.targets).flatMap(functions.get).toSet
          val targets = (values(Variable(id)).functions ++ flowed).map(worker)
          targets.foreach { target =>
            edges += Edge(source, target, safe = false, addsFrame = true)
            propagate(supplied, infos(target).params)
            if installedResults.add(target) then
              watch(List(ReturnValue(target))) {
                results.foreach(r => add(Variable(r), values(ReturnValue(target))))
              }
          }
        }
        scan(rest, source)

      case cps.Stmt.Call(
            cps.Callee.Method(id, method), arguments,
            cps.ReturnPoint.Bind(_, _, ks, rest)) =>
        val supplied = arguments :+ ks
        watch(Set(Variable(id)) ++ dependencies(supplied)) {
          values(Variable(id)).objects.foreach { obj =>
            obj.methods.get(method).foreach { target =>
              edges += Edge(source, target, safe = true, addsFrame = true)
              propagate(supplied, infos(target).params)
            }
          }
        }
        scan(rest, source)

      case call @ cps.Stmt.Call(callee, _,
          _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
        scanTransfer(call, callee, call.knownArguments, source)

      case cps.Stmt.Return(values) =>
        watch(dependencies(values)) {
          values.foreach(v => add(ReturnValue(source), eval(v).expose))
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
        watch(dependency(init)) { add(Cell(cell), eval(init).expose) }
        scan(rest, source)

      case statement @ cps.Stmt.Var(id, init, _, rest) =>
        val cell = cellNode(statement)
        add(Variable(id), Value.cell(cell))
        watch(dependency(init)) { add(Cell(cell), eval(init).expose) }
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
          values(Variable(ref)).cells.foreach(cell => add(Cell(cell), eval(value).expose))
        }
        scan(rest, source)

      case cps.Stmt.Reset(_, _, _, body, _, _) => scan(body, source)
      case cps.Stmt.Shift(_, _, _, k, body, _, _) =>
        add(Variable(k), Value.SegmentEntry)
        scan(body, source)
      case cps.Stmt.Resume(_, _, k, body, _, _) =>
        add(Variable(k), Value.SegmentEntry)
        scan(body, source)
      case _: cps.Stmt.Hole => ()
    }

    def scanTransfer(
      call: cps.Stmt,
      callee: cps.Callee,
      arguments: List[cps.Expr],
      source: Node
    ): Unit = callee match {
      case cps.Callee.Function(id) =>
        watch(Set(Variable(id)) ++ dependencies(arguments)) {
          val targetFlow = Option(targetsByCall.get(call))
          val flowed = targetFlow.iterator
            .flatMap(_.targets).flatMap(functions.get).map(valueEntry).toSet
          val targets = values(Variable(id)).functions.map(valueEntry) ++ flowed
          val exact = functions.get(id).map(valueEntry)
          val transfer = transferOf(call)
          val dispatched = defunctionalization.dispatchFor(call).isDefined
          val preservesSegments = exact.exists(target =>
            infos(target).params.size == arguments.size) ||
            targetFlow.exists(flow =>
              flow.closed && flow.targets.nonEmpty &&
                flow.targets.forall(target => functions.get(target)
                  .map(valueEntry).exists(node => infos(node).params.size == arguments.size)))
          if preservesSegments then
            segmentPreservingCalls.put(call, java.lang.Boolean.TRUE)

          targets.foreach { target =>
            val syntacticallyKnown = exact.exists(_ eq target)
            val jump = dispatched || syntacticallyKnown && transfer == StackSafety.Transfer.Jump
            // Only an indirect transfer enters a stack-safe value entry.
            val safe = !dispatched && !syntacticallyKnown
            // Every closed application can suspend locally. For an indirect
            // application GuardedEquality certifies the finite target set;
            // a named definition is closed by construction.
            // Closed residual calls are installed below with the source
            // activation chosen by representation planning. This matters for
            // continuation cases whose bodies move into a dispatcher.
            if !residualByCall.containsKey(call) then
              edges += Edge(source, target, safe, addsFrame = !jump)
            propagate(arguments, infos(target).params, preservesSegments)
          }
        }

      case cps.Callee.Method(id, method) =>
        watch(Set(Variable(id)) ++ dependencies(arguments)) {
          values(Variable(id)).objects.foreach { obj =>
            obj.methods.get(method).foreach { target =>
              edges += Edge(source, target, safe = true, addsFrame = true)
              propagate(arguments, infos(target).params)
            }
          }
        }
    }

    infos.valuesIterator.foreach(info => info.body.foreach(scan(_, info.node)))

    // A named direct definition's CPS value entry executes its finite-rank
    // worker and then invokes the supplied continuation. This entry is part
    // of the same graph as every other function value; it is not inherently
    // a trampoline boundary.
    directEntries.foreach { case (id, params) =>
      val adapter = entry(id)
      val direct = function(id)
      val List(ks, k) = params.takeRight(2).toList: @unchecked

      edges += Edge(adapter, direct, safe = false, addsFrame = true)
      watch(List(Variable(k), Variable(ks), ReturnValue(direct))) {
        val targets = values(Variable(k)).functions.map(valueEntry)
        targets.foreach { target =>
          edges += Edge(adapter, target, safe = true, addsFrame = true)
          val arguments = Vector(values(ReturnValue(direct)), values(Variable(ks)))
          arguments.iterator.zip(infos(target).params.iterator).foreach {
            case (argument, parameter) => add(Variable(parameter), argument)
          }
        }
      }
    }

    while pending.nonEmpty do {
      val action = pending.dequeue()
      queued -= action
      actions(action)()
    }

    residualCalls.foreach { call =>
      val site = callSite(call.statement)
      call.sources.foreach { source =>
        functions.get(source).orElse(operationNodes.get(source)).foreach { from =>
          call.targets.foreach { target =>
            val functionTarget = functions.get(target).map { node =>
              // A named transfer bypasses a direct definition's CPS adapter;
              // an indirect transfer invokes precisely that value entry.
              if call.known then node else valueEntry(node)
            }
            functionTarget.orElse(operationNodes.get(target)).foreach { to =>
              edges += Edge(
                from, to,
                safe = !call.known,
                addsFrame = true,
                callSite = Some(site))
            }
          }
        }
      }
    }

    // ---------------------------------------------------------------------
    // Positive synchronous cycles

    val nodes = infos.keysIterator.toVector.sortBy(_.ordinal)
    val unsafe = mutable.LinkedHashSet.empty[Node]
    val bounced = mutable.LinkedHashSet.empty[CallSite]

    def activeEdges: Vector[Edge] = edges.iterator.filterNot { edge =>
      edge.safe && unsafe.contains(edge.target) ||
        edge.callSite.exists(bounced.contains)
    }.toVector

    /** Strongly connected components of the graph left after the currently
     *  selected adapters cut their safe incoming edges. */
    def components(active: Vector[Edge]): Vector[Vector[Node]] = {
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

    sealed trait Cut
    final case class Bounce(site: CallSite) extends Cut
    final case class Adapt(node: Node) extends Cut

    def findCuts(): Set[Cut] = {
      val active = activeEdges
      val found = components(active)

      val componentOf = Array.fill(nextNode)(-1)
      found.zipWithIndex.foreach { case (component, index) =>
        component.foreach(node => componentOf(node.ordinal) = index)
      }

      val internalEdges = Array.fill(found.size)(mutable.ArrayBuffer.empty[Edge])
      val incoming = Array.fill(nextNode)(0)
      // A closed indirect site has one representation for its entire target
      // set, so adaptability is a property of the site group rather than of
      // any single edge in it.
      val siteAdaptable = mutable.Map.empty[CallSite, Boolean]
      active.foreach { edge =>
        val source = componentOf(edge.source.ordinal)
        if source == componentOf(edge.target.ordinal) then internalEdges(source) += edge
        if edge.safe then incoming(edge.target.ordinal) += 1
        edge.callSite.foreach { site =>
          siteAdaptable(site) =
            siteAdaptable.getOrElse(site, true) && edge.target.cuttable
        }
      }

      found.iterator.zipWithIndex.flatMap { case (component, index) =>
        val internal = internalEdges(index)
        val cyclic = component.size > 1 || internal.exists(edge => edge.source eq edge.target)
        val positive = internal.exists(_.addsFrame)
        // Only first-class entries have a runtime function value whose direct
        // invocation can introduce a JavaScript stack frame. Flow into a
        // continuation case or a labeled block is represented by a frame or
        // a jump; a Safe edge to such a node is merely a 0-CFA artifact.
        val safeEntries = internal.filter(edge => edge.safe && edge.target.cuttable)
        // Do not adapt or prefer only one alternative when another
        // alternative is a second-class continuation state.
        val adaptableEntries =
          safeEntries.filter(_.callSite.forall(siteAdaptable.getOrElse(_, true)))
        val localCuts = internal.filter(_.callSite.nonEmpty)

        // A positive cycle must be cut either at one closed application or at
        // a first-class entry reached by an open value transfer. Ignoring an
        // SCC without either kind of cut is important: 0-CFA can merge
        // unrelated second-class continuation states into a spurious SCC,
        // but none of those entries can or needs to be adapted.
        if cyclic && positive && (localCuts.nonEmpty || adaptableEntries.nonEmpty) then {
          // A site cut changes one closed invocation; an entry cut changes
          // every indirect invocation of its target. Prefer the former as the
          // least global calling-convention change. The remaining ordering is
          // only a deterministic tie-breaker; iteration removes every cycle.
          val sites = mutable.LinkedHashMap.empty[CallSite, (Int, Int)]
          adaptableEntries.foreach { edge =>
            edge.callSite.foreach { site =>
              val source = edge.source.ordinal
              val target = edge.target.ordinal
              sites(site) = sites.get(site) match {
                case Some((previousSource, previousTarget)) =>
                  math.min(previousSource, source) -> math.min(previousTarget, target)
                case None => source -> target
              }
            }
          }
          sites.iterator.minByOption(_._2).map(_._1)
            .map(Bounce.apply)
            .orElse {
              // Adapting a node cuts every safe incoming edge to it. Choose
              // the node with the smallest such footprint in the current
              // graph; this preserves the greatest number of immediate value
              // entries. Ordinal is merely a deterministic tie-breaker.
              Option.when(adaptableEntries.nonEmpty)(Adapt(
                adaptableEntries.iterator.map(_.target).toSet.minBy(node =>
                  (incoming(node.ordinal), node.ordinal))))
            }
            .orElse {
              // A cycle containing only statically known calls has no value
              // entry to adapt. Cut one of its closed applications instead.
              // Choosing the latest source matches the directed DFS order,
              // while remaining independent of map iteration order.
              localCuts.iterator.flatMap { edge =>
                edge.callSite.map(_ -> (edge.source.ordinal, edge.target.ordinal))
              }.maxByOption(_._2).map { case (site, _) => Bounce(site) }
            }
        } else None
      }.toSet
    }

    var cuts = findCuts()
    while cuts.nonEmpty do {
      cuts.foreach {
        case Bounce(site) => bounced += site
        case Adapt(node) => unsafe += node
      }
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
    val bouncedApplications = new IdentityHashMap[cps.Stmt, java.lang.Boolean]()
    bounced.foreach(site =>
      bouncedApplications.put(site.application, java.lang.Boolean.TRUE))
    val adapters = unsafe.iterator.map {
      case node: FunctionNode => s"function ${node.label}"
      case node: OperationNode => s"operation ${node.label}"
    }.toVector.sorted
    val segmentEntries = values.iterator.collect {
      case (Variable(id), value) if value.segmentEntry => id
    }.toSet
    Result(
      unsafeDefinitions,
      unsafeOperations,
      bouncedApplications,
      segmentEntries,
      segmentPreservingCalls,
      adapters)
  }
}
