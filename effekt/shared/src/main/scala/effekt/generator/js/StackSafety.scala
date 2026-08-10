package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

import java.util.IdentityHashMap
import scala.collection.mutable

/**
 * Chooses a stack-safe implementation for every tail transfer that remains
 * after local definitions and continuation cases have become JavaScript
 * labels.
 *
 * A direct transfer retains the current JavaScript activation. Consequently,
 * direct transfers must admit a finite ranking. Safe transfers use the
 * value-level calling convention: the callee either has a finite native-stack
 * bound or suspends before entering its worker. Jumps stay in one activation
 * and do not participate in the ranking.
 */
object StackSafety {

  enum Transfer {
    case Jump, Direct, Safe
  }

  private final class Site(
    val stmt: cps.Stmt,
    val callee: String
  ) {
    val owners = mutable.LinkedHashSet.empty[Id]
    val sources = mutable.LinkedHashSet.empty[Id]
    var targets = Vector.empty[Id]
    var closed = false
    var transfer = Transfer.Safe
  }

  final class Plan private[StackSafety] (
    private val transfers: IdentityHashMap[cps.Stmt, Transfer],
    val ranks: Map[Id, Int],
    private val sites: Vector[Site],
    val safeEntries: SafeEntries.Result
  ) {
    def transferOf(stmt: cps.Stmt): Transfer =
      Option(transfers.get(stmt)).getOrElse(Transfer.Safe)

    /** Independently check the ranking certificate carried by this plan. */
    def validate(): Unit =
      sites.foreach { site =>
        if site.transfer == Transfer.Direct then {
          assert(site.closed, s"Direct call ${site.callee} has an open target set")
          assert(site.targets.nonEmpty, s"Direct call ${site.callee} has no targets")
          site.sources.foreach { source =>
            site.targets.foreach { target =>
              assert(
                ranks.getOrElse(source, 0) > ranks.getOrElse(target, 0),
                s"Direct call ${name(source)} -> ${name(target)} does not decrease its stack rank")
            }
          }
        }
      }

    def show: String = {
      val rankLines = ranks.toVector
        .sortBy { case (id, rank) => (-rank, name(id), id.id) }
        .map { case (id, rank) => s"  ${name(id)} = $rank" }

      val transferLines = sites.map { site =>
        val owners = site.owners.iterator.map(name).mkString(" | ")
        val source = if owners.nonEmpty then owners else "local"
        val target = site.transfer match {
          case Transfer.Jump => ""
          case _ if !site.closed =>
            val known = site.targets.map(name)
            s" [${(known :+ "?").mkString(", ")}]"
          case _ => s" [${site.targets.map(name).mkString(", ")}]"
        }
        s"  $source -> ${site.callee}: ${site.transfer.toString.toLowerCase}$target"
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
      module.definitions.map(cps.GuardedEquality.targets).toVector)

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean,
    defunctionalization: Defunctionalization.Plan,
    targetFlows: Vector[cps.GuardedEquality.TargetResult]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)
    val sitesByStmt = new IdentityHashMap[cps.Stmt, Site]()
    val orderedSites = mutable.ArrayBuffer.empty[Site]
    val nodeOrder = mutable.LinkedHashSet.empty[Id]

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
    val targetsByCall = new IdentityHashMap[cps.Stmt.App, cps.GuardedEquality.CallTargets]()
    val parameters = mutable.LinkedHashMap.empty[Id, Int]

    module.definitions.zip(targetFlows).foreach { case (toplevel, flow) =>
      toplevel match {
        case cps.ToplevelDefinition.Def(id, params, _) => parameters(id) = params.size
        case _: cps.ToplevelDefinition.Val => ()
      }
      flow.localDefinitions.foreach(definition => parameters(definition.id) = definition.params.size)
      flow.callTargets.foreach(target => targetsByCall.put(target.call, target))
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
      insideBody: Set[Id]
    ): Unit = stmt match {
      case cps.Stmt.Def(id, _, body, rest) =>
        defunctionalization.caseOf(id) match {
          case Some(_) =>
            // Its body is emitted by every dispatcher that contains this case.
            visit(rest, owner, secondClass, insideBody)

          case None if isSecondClass(id) =>
            val available = secondClass + id
            val inside = if isRecursive(id) then insideBody + id else insideBody
            hosts(id) = Host(owner, available, inside)
            visit(rest, owner, available, insideBody)
            visit(body, owner, available, inside)

          case None =>
            val available = if isRecursive(id) then Set(id) else Set.empty[Id]
            val inside = if isRecursive(id) then Set(id) else Set.empty[Id]
            hosts(id) = Host(id, available, inside)
            nodeOrder += id
            visit(body, id, available, inside)
            visit(rest, owner, secondClass, insideBody)
        }

      case cps.Stmt.New(_, _, operations, rest) =>
        operations.foreach { operation =>
          nodeOrder += operation.name
          visit(operation.body, operation.name, Set.empty, Set.empty)
        }
        visit(rest, owner, secondClass, insideBody)

      case cps.Stmt.Let(_, _, rest) => visit(rest, owner, secondClass, insideBody)

      case app @ cps.Stmt.App(id, _, _) =>
        val dispatch = defunctionalization.dispatchForCallee(id).isDefined
        val jump = dispatch || secondClass.contains(id) || insideBody.contains(id)
        recordCall(app, name(id), owner, jump)

      case invoke @ cps.Stmt.Invoke(id, method, _) =>
        recordCall(invoke, s"${name(id)}.${name(method)}", owner, jump = false)

      case cps.Stmt.Run(_, _, _, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.If(_, thn, els) =>
        visit(thn, owner, secondClass, insideBody)
        visit(els, owner, secondClass, insideBody)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => visit(clause.body, owner, secondClass, insideBody) }
        default.foreach(visit(_, owner, secondClass, insideBody))
      case cps.Stmt.Region(_, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Alloc(_, _, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Var(_, _, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Dealloc(_, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Get(_, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Put(_, _, rest) => visit(rest, owner, secondClass, insideBody)
      case cps.Stmt.Reset(_, _, _, body, _, _) => visit(body, owner, secondClass, insideBody)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => visit(body, owner, secondClass, insideBody)
      case cps.Stmt.Resume(_, _, _, body, _, _) => visit(body, owner, secondClass, insideBody)
      case _: cps.Stmt.Hole => ()
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
          visit(continuationCase.body, owner, available, inside)
        }
      }

    val definitionIds = parameters.keySet.toSet

    orderedSites.foreach { site =>
      if site.sources.isEmpty then {
        site.closed = true
        site.transfer = Transfer.Jump
      } else site.stmt match {
        case app @ cps.Stmt.App(id, args, _) =>
          parameters.get(id) match {
            case Some(arity) if arity == args.size && !isSecondClass(id) && defunctionalization.caseOf(id).isEmpty =>
              site.targets = Vector(id)
              site.closed = true

            case _ =>
              Option(targetsByCall.get(app)) match {
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

        case _: cps.Stmt.Invoke =>
          // Receiver-flow analysis can later turn this into a closed target
          // set. Until then invocation is an open control transfer.
          site.targets = Vector.empty
          site.closed = false

        case _ => ()
      }
    }

    val candidates = orderedSites.filter(site => site.sources.nonEmpty && site.closed)
    val adjacency = mutable.LinkedHashMap.empty[Id, mutable.ArrayBuffer[(Id, Site)]]
    candidates.foreach { site =>
      site.sources.foreach { source =>
        val edges = adjacency.getOrElseUpdate(source, mutable.ArrayBuffer.empty)
        site.targets.foreach { target =>
          nodeOrder += target
          edges += target -> site
        }
      }
    }

    // Directed DFS identifies a feedback edge in every cycle. A call site is
    // grouped: if any of its possible edges is a back edge, the whole site
    // bounces. Removing those groups leaves an acyclic direct-call graph.
    enum Color { case White, Gray, Black }
    val colors = mutable.Map.empty[Id, Color].withDefaultValue(Color.White)
    val backSites = mutable.Set.empty[Site]
    final case class Frame(node: Id, var next: Int)

    nodeOrder.toVector.reverse.foreach { root =>
      if colors(root) == Color.White then {
        colors(root) = Color.Gray
        val stack = mutable.ArrayBuffer(Frame(root, 0))
        while stack.nonEmpty do {
          val frame = stack.last
          val edges = adjacency.getOrElse(frame.node, mutable.ArrayBuffer.empty)
          if frame.next >= edges.size then {
            colors(frame.node) = Color.Black
            stack.remove(stack.size - 1)
          } else {
            val (target, site) = edges(frame.next)
            frame.next += 1
            colors(target) match {
              case Color.Gray => backSites += site
              case Color.White =>
                colors(target) = Color.Gray
                stack += Frame(target, 0)
              case Color.Black => ()
            }
          }
        }
      }
    }

    orderedSites.foreach { site =>
      if site.sources.isEmpty then site.transfer = Transfer.Jump
      else if site.closed && !backSites.contains(site) then site.transfer = Transfer.Direct
      else site.transfer = Transfer.Safe
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

    val transfers = new IdentityHashMap[cps.Stmt, Transfer]()
    orderedSites.foreach(site => transfers.put(site.stmt, site.transfer))
    val safeEntries = SafeEntries.analyze(
      module,
      stmt => Option(transfers.get(stmt)).getOrElse(Transfer.Safe),
      isSecondClass,
      defunctionalization,
      targetFlows)
    val plan = new Plan(transfers, ranks.toMap, orderedSites.toVector, safeEntries)
    plan.validate()
    plan
  }
}
