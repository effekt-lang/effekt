package effekt
package generator
package js

import effekt.core.Id
import effekt.cpsds

import java.util.IdentityHashMap
import scala.annotation.tailrec

/**
 * A flat plan for defunctionalizing finite sets of local continuations while
 * translating CPSDS to JavaScript. The CPSDS tree remains the source of truth;
 * this structure only associates definitions and applications with their
 * target representation.
 */
object Defunctionalization {

  val BoundaryTag = -1

  final case class ContinuationCase(
    definition: Id,
    tag: Int,
    params: Vector[Id],
    captures: Vector[Id],
    body: cpsds.Stmt
  )

  final case class ContinuationDispatch(
    entry: Id,
    callee: Id,
    arity: Int,
    boundary: Boolean,
    targets: Set[Id],
    cases: Vector[ContinuationCase]
  )

  final class Plan private[js] (
    val cases: Map[Id, ContinuationCase],
    val dispatches: Map[Id, ContinuationDispatch],
    private val applications: Map[Id, ContinuationDispatch]
  ) {
    def caseOf(id: Id): Option[ContinuationCase] = cases.get(id)
    def dispatchFor(entry: Id): Option[ContinuationDispatch] = dispatches.get(entry)
    def dispatchForCallee(callee: Id): Option[ContinuationDispatch] = applications.get(callee)
  }

  private final case class Candidate(
    entry: Id,
    callee: Id,
    arity: Int,
    boundary: Boolean,
    targets: Set[Id],
    calls: Vector[cpsds.GuardedEquality.CallTargets]
  )

  /** Lexical scopes relevant to JavaScript label visibility. Definitions are
   *  retained even when second-class: a selected continuation case disappears
   *  during translation and therefore rebases its body into the dispatcher. */
  private enum Scope {
    case Definition(id: Id, function: Boolean)
    case Boundary(serial: Int)

    def isFunctionBoundary: Boolean = this match {
      case Definition(_, function) => function
      case Boundary(_) => true
    }
  }

  private final class Locations(
    private val definitions: Map[Id, Vector[Scope]],
    private val applications: IdentityHashMap[cpsds.Stmt, Vector[Scope]],
    isSecondClass: Id => Boolean
  ) {
    private def bodyScope(entry: Id): Option[Vector[Scope]] =
      definitions.get(entry).map(_ :+ Scope.Definition(entry, !isSecondClass(entry)))

    private def functionBoundaries(scopes: Vector[Scope]): Vector[Scope] =
      scopes.filter(_.isFunctionBoundary)

    def functionHost(entry: Id): Option[Vector[Scope]] =
      bodyScope(entry).map(functionBoundaries)

    def lexicalDepth(entry: Id): Int =
      bodyScope(entry).fold(Int.MaxValue)(_.size)

    /** Does this application end up in the JavaScript function containing the
     *  dispatcher after selected continuation definitions are replaced by
     *  their cases? */
    def visibleFrom(
      call: cpsds.Stmt,
      entry: Id,
      cases: Set[Id]
    ): Boolean = {
      val actual = Option(applications.get(call))
      val host = bodyScope(entry).map(functionBoundaries)
      (actual, host) match {
        case (Some(scopes), Some(expected)) =>
          val caseIndex = scopes.lastIndexWhere {
            case Scope.Definition(id, _) => cases.contains(id)
            case Scope.Boundary(_) => false
          }
          val effective =
            if caseIndex < 0 then functionBoundaries(scopes)
            else expected ++ functionBoundaries(scopes.drop(caseIndex + 1))
          effective == expected
        case _ => false
      }
    }

    def insideCase(call: cpsds.Stmt, cases: Set[Id]): Boolean =
      Option(applications.get(call)).exists(_.exists {
        case Scope.Definition(id, _) => cases.contains(id)
        case Scope.Boundary(_) => false
      })

    def enclosingDefinitions(call: cpsds.Stmt): Vector[Id] =
      Option(applications.get(call)).toVector.flatten.collect {
        case Scope.Definition(id, _) => id
      }

  }

  private object Locations {
    def apply(module: cpsds.ModuleDecl, isSecondClass: Id => Boolean): Locations = {
      val definitions = scala.collection.mutable.LinkedHashMap.empty[Id, Vector[Scope]]
      val applications = new IdentityHashMap[cpsds.Stmt, Vector[Scope]]()
      var nextBoundary = 0

      def boundary(): Scope = {
        val result = Scope.Boundary(nextBoundary)
        nextBoundary += 1
        result
      }

      def visit(stmt: cpsds.Stmt, scopes: Vector[Scope]): Unit = stmt match {
        case cpsds.Stmt.Def(id, _, body, rest) =>
          definitions(id) = scopes
          visit(body, scopes :+ Scope.Definition(id, !isSecondClass(id)))
          visit(rest, scopes)

        case cpsds.Stmt.New(_, _, operations, rest) =>
          operations.foreach(operation => visit(operation.body, scopes :+ boundary()))
          visit(rest, scopes)

        case cpsds.Stmt.Let(_, _, rest) => visit(rest, scopes)
        case call: cpsds.Stmt.App => applications.put(call, scopes)
        case _: cpsds.Stmt.Invoke => ()
        case cpsds.Stmt.Run(_, _, _, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.If(_, thn, els) =>
          visit(thn, scopes)
          visit(els, scopes)
        case cpsds.Stmt.Match(_, clauses, default) =>
          clauses.foreach { case (_, clause) => visit(clause.body, scopes) }
          default.foreach(visit(_, scopes))
        case cpsds.Stmt.Region(_, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.Alloc(_, _, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.Var(_, _, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.Dealloc(_, rest) => visit(rest, scopes)
        case cpsds.Stmt.Get(_, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.Put(_, _, rest) => visit(rest, scopes)
        case cpsds.Stmt.Reset(_, _, _, body, _, _) => visit(body, scopes :+ boundary())
        case cpsds.Stmt.Shift(_, _, _, _, body, _, _) => visit(body, scopes :+ boundary())
        case cpsds.Stmt.Resume(_, _, _, body, _, _) => visit(body, scopes :+ boundary())
        case _: cpsds.Stmt.Hole => ()
      }

      module.definitions.foreach {
        case cpsds.ToplevelDefinition.Def(id, _, body) =>
          definitions(id) = Vector.empty
          visit(body, Vector(Scope.Definition(id, function = true)))
        case cpsds.ToplevelDefinition.Val(id, _, _, binding) =>
          definitions(id) = Vector.empty
          visit(binding, Vector(Scope.Definition(id, function = true)))
      }

      new Locations(definitions.toMap, applications, isSecondClass)
    }
  }

  def analyze(
    module: cpsds.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean
  ): Plan = {
    val locations = Locations(module, isSecondClass)
    val allCases = scala.collection.mutable.LinkedHashMap.empty[Id, ContinuationCase]
    val allDispatches = scala.collection.mutable.LinkedHashMap.empty[Id, ContinuationDispatch]
    val allApplications = scala.collection.mutable.LinkedHashMap.empty[Id, ContinuationDispatch]
    var nextTag = 0

    module.definitions.foreach { toplevel =>
      val flow = cpsds.GuardedEquality.targets(toplevel)
      val definitions = flow.localDefinitions
      val definitionById = definitions.iterator.map(d => d.id -> d).toMap
      val localParameterOwner = definitions.iterator.flatMap { definition =>
        definition.params.iterator.map(_ -> definition.id)
      }.toMap
      val toplevelParameterOwner = toplevel match {
        case cpsds.ToplevelDefinition.Def(id, params, _) => params.map(_ -> id).toMap
        case cpsds.ToplevelDefinition.Val(id, ks, k, _) => Map(ks -> id, k -> id)
      }
      val parameterOwner = localParameterOwner ++ toplevelParameterOwner
      val calls = flow.callTargets

      val candidates = calls.groupBy(_.callee).iterator.flatMap { case (callee, sites) =>
        parameterOwner.get(callee).flatMap { entry =>
          val targets = sites.iterator.flatMap(_.targets).toSet
          val arities = sites.map(_.arity).distinct
          val boundary = sites.exists(!_.closed)

          val inhabited = sites.nonEmpty && targets.nonEmpty
          val compatible = arities.size == 1 && targets.forall { target =>
            definitionById.get(target).exists { definition =>
              !flow.escapes(target) &&
                (!flow.isRigid(target) || boundary) &&
                definition.params.size == arities.head
            }
          }

          Option.when(
            isRecursive(entry) && inhabited && compatible
          )(Candidate(entry, callee, arities.head, boundary, targets, sites))
        }
      }.toVector

      val uniqueEntries = candidates.groupBy(_.entry).values.collect {
        case Vector(candidate) => candidate
      }.toVector
      val candidateByEntry = uniqueEntries.iterator.map(candidate =>
        candidate.entry -> candidate).toMap
      val candidatesByTarget = uniqueEntries
        .flatMap(candidate => candidate.targets.map(_ -> candidate.entry))
        .groupMap(_._1)(_._2)
      val candidatesByCallee = uniqueEntries.iterator.map(candidate =>
        candidate.callee -> candidate.entry).toMap
      val adjacent = uniqueEntries.iterator.map(candidate =>
        candidate.entry -> scala.collection.mutable.LinkedHashSet.empty[Id]).toMap

      def connect(left: Id, right: Id): Unit =
        if left != right then {
          adjacent(left) += right
          adjacent(right) += left
        }

      candidatesByTarget.values.foreach { candidates =>
        candidates.headOption.foreach { first =>
          candidates.tail.foreach(other => connect(first, other))
        }
      }

      // A continuation domain must also be closed under applications made by
      // its cases. Otherwise a frame can outlive the nested dispatcher whose
      // registers would be needed to apply a captured continuation.
      calls.foreach { call =>
        candidatesByCallee.get(call.callee).foreach { callee =>
          locations.enclosingDefinitions(call.call).iterator
            .flatMap(candidatesByTarget.getOrElse(_, Vector.empty))
            .foreach(caller => connect(caller, callee))
        }
      }

      // Connected components of interacting target sets share one finite
      // continuation domain. This lets nested recursive loops pass immutable
      // frames between their structured apply loops.
      @tailrec def components(
        remaining: Vector[Candidate],
        result: Vector[Vector[Candidate]] = Vector.empty
      ): Vector[Vector[Candidate]] =
        if remaining.isEmpty then result
        else {
          val seen = scala.collection.mutable.LinkedHashSet.empty[Id]
          val pending = scala.collection.mutable.Queue(remaining.head.entry)
          while pending.nonEmpty do {
            val entry = pending.dequeue()
            if seen.add(entry) then
              adjacent(entry).filterNot(seen).foreach(pending.enqueue(_))
          }
          val component = seen.toVector.map(candidateByEntry)
          components(remaining.filterNot(candidate => seen(candidate.entry)), result :+ component)
        }

      components(uniqueEntries).foreach { component =>
        val domain = component.iterator.flatMap(_.targets).toSet
        val memberByCallee = component.iterator.map(candidate =>
          candidate.callee -> candidate).toMap
        val covered = calls.forall { call =>
          !call.targets.exists(domain) || memberByCallee.get(call.callee).exists { candidate =>
            (call.closed || candidate.boundary) && call.targets.subsetOf(domain)
          }
        }
        val callsInsideCases = calls.filter(call =>
          call.targets.exists(domain) && locations.insideCase(call.call, domain))
        val closedGroups = component.filterNot(_.boundary)
          .groupBy(candidate => locations.functionHost(candidate.entry))
          .values
          .map(_.toVector)
          .toVector
        val groups = component.filter(_.boundary).map(Vector(_)) ++ closedGroups
        val owners = groups.map { members =>
          members.minBy(candidate => locations.lexicalDepth(candidate.entry)) -> members
        }
        val eligible = component.map(_.arity).distinct.size == 1 && covered &&
          owners.forall { case (owner, members) =>
            members.flatMap(_.calls).forall(call =>
              locations.visibleFrom(call.call, owner.entry, domain)) &&
            callsInsideCases.forall(call =>
              locations.visibleFrom(call.call, owner.entry, domain))
        }

        if eligible then {
          val cases = definitions.filter(d => domain.contains(d.id)).map { definition =>
            allCases.getOrElseUpdate(definition.id, {
              val continuationCase = ContinuationCase(
                definition.id,
                nextTag,
                definition.params,
                // Second-class definitions are statically compiled labels.
                definition.captures.filterNot(isSecondClass),
                definition.body)
              nextTag += 1
              continuationCase
            })
          }

          owners.foreach { case (owner, members) =>
            val dispatch = ContinuationDispatch(
              owner.entry,
              owner.callee,
              owner.arity,
              owner.boundary,
              domain,
              cases)
            allDispatches(owner.entry) = dispatch
            members.foreach(candidate => allApplications(candidate.callee) = dispatch)
          }
        }
      }
    }

    new Plan(allCases.toMap, allDispatches.toMap, allApplications.toMap)
  }
}
