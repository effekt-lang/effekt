package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

import java.util.IdentityHashMap
import scala.annotation.tailrec

/**
 * A flat plan for defunctionalizing finite sets of local continuations while
 * translating CPS to JavaScript. The CPS tree remains the source of truth;
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
    body: cps.Stmt
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
    private val applications: IdentityHashMap[cps.Stmt.App, ContinuationDispatch],
    /** Stable local definitions referenced directly by relocated cases which
     *  must therefore retain a JavaScript function binding. */
    val firstClassRequirements: Set[Id],
    /** Definitions whose residual bodies contain a reference to themselves
     *  after continuation cases have moved to their dispatchers. */
    val reenteredDefinitions: Set[Id],
    /** Dynamic captures that are lexically available with the same identity
     *  at every dispatcher. They can be omitted unless JavaScript lowering
     *  turns their binding into a mutable loop register. */
    private val recoverableCaptures: Map[Id, Set[Id]]
  ) {
    def caseOf(id: Id): Option[ContinuationCase] = cases.get(id)
    def dispatchFor(entry: Id): Option[ContinuationDispatch] = dispatches.get(entry)
    def dispatchFor(application: cps.Stmt.App): Option[ContinuationDispatch] =
      Option(applications.get(application))

    /** Finalize frame layouts after loop lowering has identified mutable
     *  registers. A recoverable immutable binding is read from the lexical
     *  environment; every other capture remains an immutable frame field. */
    private[js] def refineFrames(mutable: Set[Id]): Plan = {
      val refinedCases = cases.view.mapValues { continuationCase =>
        val recoverable = recoverableCaptures
          .getOrElse(continuationCase.definition, Set.empty) -- mutable
        continuationCase.copy(
          captures = continuationCase.captures.filterNot(recoverable))
      }.toMap
      val refinedDispatches = dispatches.view.mapValues { dispatch =>
        dispatch.copy(cases = dispatch.cases.map(c => refinedCases(c.definition)))
      }.toMap
      val refinedApplications = new IdentityHashMap[cps.Stmt.App, ContinuationDispatch]()
      applications.forEach { (application, dispatch) =>
        refinedApplications.put(application, refinedDispatches(dispatch.entry))
      }
      new Plan(
        refinedCases,
        refinedDispatches,
        refinedApplications,
        firstClassRequirements,
        reenteredDefinitions,
        recoverableCaptures)
    }
  }

  private final case class Candidate(
    entry: Id,
    callee: Id,
    arity: Int,
    boundary: Boolean,
    targets: Set[Id],
    externalTargets: Set[Id],
    calls: Vector[cps.GuardedEquality.CallTargets]
  )

  /** Lexical scopes relevant to JavaScript label visibility. Definitions are
   *  retained even when second-class: a selected continuation case disappears
   *  during translation and therefore rebases its body into the dispatcher. */
  private enum Scope {
    case Definition(id: Id, function: Boolean)
    /** A preceding local definition visible in the remainder. `label` records
     *  whether its current representation contributes a labeled scope; the
     *  binding itself remains a potential label in either representation. */
    case Binding(id: Id, label: Boolean)
    case Boundary(serial: Int)

    def isFunctionBoundary: Boolean = this match {
      case Definition(_, function) => function
      case Binding(_, _) => false
      case Boundary(_) => true
    }

    def isStructural: Boolean = this match {
      case Definition(_, _) => true
      case Binding(_, label) => label
      case Boundary(_) => true
    }
  }

  private final class Locations(
    private val definitions: Map[Id, Vector[Scope]],
    private val applications: IdentityHashMap[cps.Stmt, Vector[Scope]],
    private val staticDefinitions: Map[Id, Set[Id]],
    isSecondClass: Id => Boolean
  ) {
    private def bodyScope(entry: Id): Option[Vector[Scope]] =
      definitions.get(entry).map(_ :+ Scope.Definition(entry, !isSecondClass(entry)))

    private def functionBoundaries(scopes: Vector[Scope]): Vector[Scope] =
      scopes.filter(_.isFunctionBoundary)

    def functionHost(entry: Id): Option[Vector[Scope]] =
      bodyScope(entry).map(functionBoundaries)

    def lexicalDepth(entry: Id): Int =
      bodyScope(entry).fold(Int.MaxValue)(_.count(_.isStructural))

    /** Definitions allocated while evaluating `entry`. Moving their bodies
     *  to a dispatcher hosted at `entry` does not cross the entry boundary. */
    def localTo(target: Id, entry: Id): Boolean =
      definitions.get(target).exists(_.exists {
        case Scope.Definition(id, _) => id == entry
        case Scope.Binding(_, _) | Scope.Boundary(_) => false
      })

    /** Does this application end up in the JavaScript function containing the
     *  dispatcher after selected continuation definitions are replaced by
     *  their cases? */
    def visibleFrom(
      call: cps.Stmt,
      entry: Id,
      cases: Set[Id]
    ): Boolean = {
      val actual = Option(applications.get(call))
      val host = bodyScope(entry).map(functionBoundaries)
      (actual, host) match {
        case (Some(scopes), Some(expected)) =>
          val caseIndex = scopes.lastIndexWhere {
            case Scope.Definition(id, _) => cases.contains(id)
            case Scope.Binding(_, _) => false
            case Scope.Boundary(_) => false
          }
          val effective =
            if caseIndex < 0 then functionBoundaries(scopes)
            else expected ++ functionBoundaries(scopes.drop(caseIndex + 1))
          // A call in a relocated case is emitted at the dispatcher. Every
          // other call must be in the definition's body. Its lexical remainder
          // runs before the definition's loop and therefore before the local
          // dispatcher, even when both share one JavaScript function.
          val dispatcherInScope = caseIndex >= 0 || scopes.exists {
            case Scope.Definition(id, _) => id == entry
            case Scope.Binding(_, _) | Scope.Boundary(_) => false
          }
          effective == expected && dispatcherInScope
        case _ => false
      }
    }

    def insideCase(call: cps.Stmt, cases: Set[Id]): Boolean =
      Option(applications.get(call)).exists(_.exists {
        case Scope.Definition(id, _) => cases.contains(id)
        case Scope.Binding(_, _) => false
        case Scope.Boundary(_) => false
      })

    def enclosingDefinitions(call: cps.Stmt): Vector[Id] =
      Option(applications.get(call)).toVector.flatten.collect {
        case Scope.Definition(id, _) => id
      }

    /** Whether a second-class definition can be named as a JavaScript label
     *  at this entry. Unlike an immutable function binding, a label cannot be
     *  captured across a JavaScript function boundary. */
    def labelVisibleAt(entry: Id, label: Id): Boolean =
      bodyScope(entry).exists { scopes =>
        // The definition named `label` is not itself a boundary in the
        // representation whose validity we are checking: lowering it would
        // replace that function scope by a label scope.
        val boundary = scopes.lastIndexWhere {
          case Scope.Definition(id, function) => function && id != label
          case Scope.Binding(_, _) => false
          case Scope.Boundary(_) => true
        }
        scopes.iterator.drop(boundary + 1).exists {
          case Scope.Definition(id, _) => id == label
          case Scope.Binding(id, _) => id == label
          case Scope.Boundary(_) => false
        }
      }

    /** Immutable definition bindings denoted by the same JavaScript value at
     *  every execution of this definition's body. These need not be stored in
     *  a continuation frame whose dispatcher is hosted here. */
    def staticAt(entry: Id): Set[Id] =
      staticDefinitions.getOrElse(entry, Set.empty)

    /** Definition bodies that lexically contain this entry, including the
     *  entry itself. A continuation case emitted at the entry becomes part of
     *  each of these residual bodies. */
    def enclosingDefinitionBodies(entry: Id): Set[Id] =
      bodyScope(entry).toVector.flatten.collect {
        case Scope.Definition(id, _) => id
      }.toSet

  }

  private object Locations {
    def apply(module: cps.ModuleDecl, isSecondClass: Id => Boolean): Locations = {
      val definitions = scala.collection.mutable.LinkedHashMap.empty[Id, Vector[Scope]]
      val applications = new IdentityHashMap[cps.Stmt, Vector[Scope]]()
      val staticDefinitions = scala.collection.mutable.LinkedHashMap.empty[Id, Set[Id]]
      var nextBoundary = 0

      def boundary(): Scope = {
        val result = Scope.Boundary(nextBoundary)
        nextBoundary += 1
        result
      }

      /** `repeated` means that evaluating the surrounding CPS body can revisit
       *  a local definition without entering a fresh JavaScript activation.
       *  Such a definition is not a stable substitute for a captured closure.
       *
       *  Local function bodies are conservatively repeated here. A later
       *  representation fixed point may turn them into labels, and keeping
       *  stability independent of that choice makes frame layouts monotone.
       */
      def visit(
        stmt: cps.Stmt,
        scopes: Vector[Scope],
        static: Set[Id],
        repeated: Boolean
      ): Unit = stmt match {
        case cps.Stmt.Def(id, _, body, rest) =>
          definitions(id) = scopes
          // The recursive binder itself always denotes the current closure (or
          // label) in its body. In the remainder it is stable only when its
          // allocation site is executed once in this JavaScript activation.
          staticDefinitions(id) = static + id
          visit(
            body,
            scopes :+ Scope.Definition(id, !isSecondClass(id)),
            static + id,
            repeated = true)
          visit(
            rest,
            scopes :+ Scope.Binding(id, isSecondClass(id)),
            static ++ Option.when(!repeated)(id),
            repeated)

        case cps.Stmt.New(_, _, operations, rest) =>
          operations.foreach(operation =>
            visit(operation.body, scopes :+ boundary(), static, repeated = false))
          visit(rest, scopes, static, repeated)

        case cps.Stmt.Let(_, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Call(_, _, _, _, _, rest) => visit(rest, scopes, static, repeated)
        case call: cps.Stmt.App => applications.put(call, scopes)
        case _: cps.Stmt.Invoke => ()
        case _: cps.Stmt.Return => ()
        case cps.Stmt.Run(_, _, _, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.If(_, thn, els) =>
          visit(thn, scopes, static, repeated)
          visit(els, scopes, static, repeated)
        case cps.Stmt.Match(_, clauses, default) =>
          clauses.foreach { case (_, clause) => visit(clause.body, scopes, static, repeated) }
          default.foreach(visit(_, scopes, static, repeated))
        case cps.Stmt.Region(_, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Alloc(_, _, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Var(_, _, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Dealloc(_, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Get(_, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Put(_, _, rest) => visit(rest, scopes, static, repeated)
        case cps.Stmt.Reset(_, _, _, body, _, _) => visit(body, scopes, static, repeated)
        case cps.Stmt.Shift(_, _, _, _, body, _, _) => visit(body, scopes, static, repeated)
        case cps.Stmt.Resume(_, _, _, body, _, _) => visit(body, scopes, static, repeated)
        case _: cps.Stmt.Hole => ()
      }

      val toplevelDefinitions = module.definitions.collect {
        case cps.ToplevelDefinition.Def(id, _, _) => id
      }.toSet
      module.definitions.foreach {
        case cps.ToplevelDefinition.Def(id, _, body) =>
          definitions(id) = Vector.empty
          staticDefinitions(id) = toplevelDefinitions
          visit(
            body,
            Vector(Scope.Definition(id, function = true)),
            toplevelDefinitions,
            repeated = false)
        case cps.ToplevelDefinition.Val(id, _, _, binding) =>
          definitions(id) = Vector.empty
          staticDefinitions(id) = toplevelDefinitions
          visit(
            binding,
            Vector(Scope.Definition(id, function = true)),
            toplevelDefinitions,
            repeated = false)
      }

      new Locations(
        definitions.toMap,
        applications,
        staticDefinitions.toMap,
        isSecondClass)
    }
  }

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean
  ): Plan =
    analyze(
      module,
      isRecursive,
      isSecondClass,
      module.definitions.map(cps.GuardedEquality.targets).toVector,
      Set.empty)

  def analyze(
    module: cps.ModuleDecl,
    isRecursive: Id => Boolean,
    isSecondClass: Id => Boolean,
    targetFlows: Vector[cps.GuardedEquality.TargetResult],
    directDefinitions: Set[Id]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)
    val locations = Locations(module, isSecondClass)
    val allCases = scala.collection.mutable.LinkedHashMap.empty[Id, ContinuationCase]
    val allDispatches = scala.collection.mutable.LinkedHashMap.empty[Id, ContinuationDispatch]
    val allApplications = new IdentityHashMap[cps.Stmt.App, ContinuationDispatch]()
    val firstClassRequirements = scala.collection.mutable.LinkedHashSet.empty[Id]
    val reenteredDefinitions = scala.collection.mutable.LinkedHashSet.empty[Id]
    var nextTag = 0

    val toplevelDefinitions = module.definitions.collect {
      case cps.ToplevelDefinition.Def(id, _, _) => id
    }.toSet
    val lexicalEnvironments = {
      val locals = targetFlows.iterator.flatMap(_.localDefinitions).map { definition =>
        // A function parameter belongs to a fresh dynamic activation on every
        // call. A second-class definition, by contrast, reuses one set of
        // registers in its enclosing activation, so an unmodified parameter
        // can be recovered there. Captures are bound outside either entry.
        val stableParameters =
          if isSecondClass(definition.id) then definition.params.toSet
          else Set.empty[Id]
        definition.id -> (stableParameters ++ definition.captures)
      }
      val toplevel = module.definitions.iterator.map {
        case cps.ToplevelDefinition.Def(id, _, _) => id -> Set.empty[Id]
        case cps.ToplevelDefinition.Val(id, _, _, _) => id -> Set.empty[Id]
      }
      (locals ++ toplevel).toMap
    }
    val definitionIds = lexicalEnvironments.keySet

    module.definitions.zip(targetFlows).foreach { case (toplevel, flow) =>
      val definitions = flow.localDefinitions
      val definitionById = definitions.iterator.map(d => d.id -> d).toMap

      // A closed direct definition denotes code, not an activation-dependent
      // closure. Relocated continuation cases can name that code directly,
      // so it is not part of their dynamic environment. Close transitively
      // over other closed direct definitions to cover small helper clusters.
      @tailrec def closeDirectDefinitions(closed: Set[Id]): Set[Id] = {
        val next = closed ++ definitions.iterator.collect {
          case definition
              if directDefinitions(definition.id) &&
                definition.captures.forall(closed) => definition.id
        }
        if next == closed then closed else closeDirectDefinitions(next)
      }
      val closedDirectDefinitions = closeDirectDefinitions(Set.empty)
      val localParameterOwner = definitions.iterator.flatMap { definition =>
        definition.params.iterator.map(_ -> definition.id)
      }.toMap
      val toplevelParameterOwner = toplevel match {
        case cps.ToplevelDefinition.Def(id, params, _) => params.map(_ -> id).toMap
        case cps.ToplevelDefinition.Val(id, ks, k, _) => Map(ks -> id, k -> id)
      }
      val parameterOwner = localParameterOwner ++ toplevelParameterOwner
      // A `Call` has an explicit lexical remainder and is not a continuation
      // application. Only terminal CPS applications can form dispatches.
      val calls = flow.callTargets.filter(_.call.isInstanceOf[cps.Stmt.App])

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
          )(Candidate(entry, callee, arities.head, boundary, targets, Set.empty, sites))
        }
      }.toVector

      val uniqueEntries = candidates.groupBy(_.entry).values.collect {
        case Vector(candidate) => candidate
      }.toVector

      /** Connected components are the maximal sets forced to share a frame
       *  representation: either an application can receive cases from both
       *  entries, or a case applies a continuation owned by another entry. */
      def components(candidates: Vector[Candidate]): Vector[Vector[Candidate]] = {
        val candidateByEntry = candidates.iterator.map(candidate =>
          candidate.entry -> candidate).toMap
        val candidatesByTarget = candidates
          .flatMap(candidate => candidate.targets.map(_ -> candidate.entry))
          .groupMap(_._1)(_._2)
        val candidatesByCallee = candidates.iterator.map(candidate =>
          candidate.callee -> candidate.entry).toMap
        val adjacent = candidates.iterator.map(candidate =>
          candidate.entry -> scala.collection.mutable.LinkedHashSet.empty[Id]).toMap

        def connect(left: Id, right: Id): Unit =
          if left != right then {
            adjacent(left) += right
            adjacent(right) += left
          }

        candidatesByTarget.values.foreach { entries =>
          entries.headOption.foreach { first =>
            entries.tail.foreach(other => connect(first, other))
          }
        }

        calls.foreach { call =>
          candidatesByCallee.get(call.callee).foreach { callee =>
            locations.enclosingDefinitions(call.call).iterator
              .flatMap(candidatesByTarget.getOrElse(_, Vector.empty))
              .foreach(caller => connect(caller, callee))
          }
        }

        @tailrec def collect(
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
            collect(
              remaining.filterNot(candidate => seen(candidate.entry)),
              result :+ component)
          }

        collect(candidates)
      }

      /** Select and materialize one closed continuation family. */
      def select(component: Vector[Candidate]): Boolean = {
        val domain = component.iterator.flatMap(_.targets).toSet
        val memberByCallee = component.iterator.map(candidate =>
          candidate.callee -> candidate).toMap
        val covered = calls.forall { call =>
          !call.targets.exists(domain) || memberByCallee.get(call.callee).exists { candidate =>
            (call.closed || candidate.boundary) &&
              call.targets.subsetOf(domain ++ candidate.externalTargets)
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
          val requiredCalls = members.flatMap(_.calls) ++ callsInsideCases
          val dominating = members.filter { candidate =>
            requiredCalls.forall(call =>
              locations.visibleFrom(call.call, candidate.entry, domain))
          }
          dominating
            .minByOption(candidate =>
              locations.lexicalDepth(candidate.entry) -> candidate.entry.id)
            .getOrElse(members.minBy(candidate =>
              locations.lexicalDepth(candidate.entry) -> candidate.entry.id)) -> members
        }
        val staticAtEveryDispatcher = owners.iterator
          .map { case (owner, _) => locations.staticAt(owner.entry) }
          .reduceOption(_ intersect _)
          .getOrElse(Set.empty)
        val capturedStaticDefinitions = definitions.iterator
          .filter(definition => domain.contains(definition.id))
          .flatMap(_.captures)
          .filter(staticAtEveryDispatcher)
          .toSet
        val requireFunction = capturedStaticDefinitions.filterNot { capture =>
          toplevelDefinitions(capture) || owners.forall {
            case (owner, _) => locations.labelVisibleAt(owner.entry, capture)
          }
        }
        val labelsAvailableAtEveryDispatcher = definitions
          .filter(definition => domain.contains(definition.id))
          .forall(definition => definition.captures.forall(capture =>
            !isSecondClass(capture) ||
              staticAtEveryDispatcher(capture) && owners.forall {
                case (owner, _) => locations.labelVisibleAt(owner.entry, capture)
              }))
        val externalTargets = component.iterator.flatMap(_.externalTargets).toSet
        val representationAvailable =
          !domain.exists(firstClassRequirements) &&
            !externalTargets.exists(allCases.contains)
        val eligible = representationAvailable &&
          component.map(_.arity).distinct.size == 1 && covered &&
          labelsAvailableAtEveryDispatcher &&
          owners.forall { case (owner, members) =>
            members.flatMap(_.calls).forall(call =>
              locations.visibleFrom(call.call, owner.entry, domain)) &&
            callsInsideCases.forall(call =>
              locations.visibleFrom(call.call, owner.entry, domain))
        }

        if eligible then {
          // A stable definition can be referenced directly instead of stored
          // in every frame. If it could not be a label at all dispatchers,
          // that choice contributes a first-class representation constraint.
          // This makes representation and layout one simultaneous solution,
          // rather than relying on the current iteration accidentally keeping
          // the definition as a function.
          firstClassRequirements ++= requireFunction ++ externalTargets
          val cases = definitions.filter(d => domain.contains(d.id)).map { definition =>
            allCases.getOrElseUpdate(definition.id, {
              val continuationCase = ContinuationCase(
                definition.id,
                nextTag,
                definition.params,
                // A frame is closure conversion relative to its dispatchers:
                // a definition binding available with the same identity at
                // every host is referenced directly; all dynamic values are
                // retained as fields.
                definition.captures
                  .filterNot(staticAtEveryDispatcher)
                  .filterNot(closedDirectDefinitions),
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
            members.flatMap(_.calls).foreach { call =>
              call.call match {
                case application: cps.Stmt.App =>
                  allApplications.put(application, dispatch)
                case _ => () // candidates contain terminal applications only
              }
            }

            // Case bodies are emitted at the dispatcher rather than at their
            // original definition sites. A reference to an enclosing
            // definition therefore becomes a residual back-edge. Captures
            // retained in the frame are field reads, not lexical references.
            val enclosing = locations.enclosingDefinitionBodies(owner.entry)
            cases.foreach { continuationCase =>
              val direct = continuationCase.body.free --
                continuationCase.params -- continuationCase.captures
              reenteredDefinitions ++= enclosing.intersect(direct)
            }
          }
        }
        eligible
      }

      components(uniqueEntries).foreach { component =>
        if !select(component) then {
          // A maximal flow component can span nested JavaScript activations.
          // Cut such a component at entry boundaries: cases allocated while
          // evaluating an entry remain local, while every incoming value uses
          // the distinguished function-valued boundary representation.
          val local = component.flatMap { candidate =>
            val (targets, external) = candidate.targets.partition(
              target => locations.localTo(target, candidate.entry))
            Option.when(targets.nonEmpty)(candidate.copy(
              boundary = candidate.boundary || external.nonEmpty,
              targets = targets,
              externalTargets = external))
          }
          val localComponents = components(local)
          // One definition cannot be a frame in one local sum and the
          // function-valued boundary of another without representation
          // conversion. Retain exactly the non-conflicting local components.
          val boundaries = localComponents.iterator
            .flatMap(_.iterator.flatMap(_.externalTargets)).toSet
          localComponents
            .filterNot(_.exists(_.targets.exists(boundaries)))
            .sortBy(_.map(candidate =>
              locations.lexicalDepth(candidate.entry) -> candidate.entry.id).min)
            .foreach(select)
        }
      }
    }

    // A case can be shared by several dispatchers. A capture is recoverable
    // only if every dispatcher can name the same lexical binding. Definition
    // bindings are handled by the representation fixed point above; this
    // late refinement concerns ordinary dynamic values only.
    val dispatchersByCase = allDispatches.valuesIterator.flatMap { dispatch =>
      dispatch.cases.iterator.map(_.definition -> dispatch.entry)
    }.toVector.groupMap(_._1)(_._2)
    val recoverableCaptures = allCases.iterator.map { case (id, continuationCase) =>
      val commonEnvironment = dispatchersByCase.getOrElse(id, Vector.empty)
        .iterator
        .map(entry => lexicalEnvironments.getOrElse(entry, Set.empty))
        .reduceOption(_ intersect _)
        .getOrElse(Set.empty)
      id -> continuationCase.captures.iterator
        .filter(commonEnvironment)
        .filterNot(definitionIds)
        .toSet
    }.toMap

    new Plan(
      allCases.toMap,
      allDispatches.toMap,
      allApplications,
      firstClassRequirements.toSet,
      reenteredDefinitions.toSet,
      recoverableCaptures)
  }
}
