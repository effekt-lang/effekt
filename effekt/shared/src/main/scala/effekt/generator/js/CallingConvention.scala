package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

import java.util.IdentityHashMap
import scala.annotation.tailrec
import scala.collection.mutable

/** Selects the value-returning calling convention.
 *
 * This analysis deliberately separates two questions:
 *
 *   1. Can the continuation parameters be erased? This is a control-flow
 *      property. Its greatest solution determines the direct CPS-IR ABI.
 *   2. Can that ABI be implemented by nested JavaScript calls? This is a
 *      stack-space property. Positive recursive components remain in CPS;
 *      finite components carry a longest-path rank that bounds their use of
 *      the JavaScript stack.
 *
 * A syntactic self-tail call has weight zero because JavaScript lowering turns
 * it into a loop. Every other direct call has weight one. The finite part of
 * this graph carries its longest-path rank as a checkable native-stack bound.
 */
object CallingConvention {

  final case class OriginalDefinition(params: List[Id])

  private def returnParameters(plan: Plan, id: Id, params: List[Id]): Option[(Id, Id)] =
    Option.when(plan.isDirect(id) && params.size >= 2)(
      params(params.size - 2) -> params.last)

  /** Lower one lexical computation according to the selected convention. */
  private def lowerStatement(
    stmt: cps.Stmt,
    returns: Option[(Id, Id)],
    plan: Plan
  ): cps.Stmt = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        val directParams =
          if plan.isDirect(id) then params.dropRight(2)
          else params
        cps.Stmt.Def(
          id,
          directParams,
          lowerStatement(body, returnParameters(plan, id, params), plan),
          lowerStatement(rest, returns, plan))
      case cps.Stmt.New(id, interface, operations, rest) =>
        cps.Stmt.New(
          id,
          interface,
          operations.map(operation => operation.copy(
            body = lowerStatement(operation.body, None, plan))),
          lowerStatement(rest, returns, plan))
      case cps.Stmt.Let(id, binding, rest) =>
        cps.Stmt.Let(id, binding, lowerStatement(rest, returns, plan))

      case call @ cps.Stmt.Call(result, callee, arguments, ks, rest)
          if plan.isDirect(call) =>
        cps.Stmt.Call(
          result,
          callee,
          arguments,
          ks,
          lowerStatement(rest, returns, plan))

      case cps.Stmt.Call(result, callee, arguments, ks, rest) =>
        val continuation = Id("k")
        val returnedKs = Id("ks")
        cps.Stmt.Def(
          continuation,
          List(result, returnedKs),
          lowerStatement(rest, returns, plan),
          cps.Stmt.App(
            callee,
            arguments ++ List(ks, cps.Expr.Variable(continuation))))

      case cps.Stmt.App(k, List(value, cps.Expr.Variable(ks)))
          if returns.contains(ks -> k) =>
        cps.Stmt.Return(value)

      case app: cps.Stmt.App => app
      case invoke: cps.Stmt.Invoke => invoke
      case returned: cps.Stmt.Return => returned
      case cps.Stmt.Run(id, callee, arguments, purity, rest) =>
        cps.Stmt.Run(
          id, callee, arguments, purity,
          lowerStatement(rest, returns, plan))
      case cps.Stmt.If(condition, thn, els) =>
        cps.Stmt.If(
          condition,
          lowerStatement(thn, returns, plan),
          lowerStatement(els, returns, plan))
      case cps.Stmt.Match(scrutinee, clauses, default) =>
        cps.Stmt.Match(
          scrutinee,
          clauses.map { case (tag, clause) =>
            tag -> clause.copy(
              body = lowerStatement(clause.body, returns, plan))
          },
          default.map(lowerStatement(_, returns, plan)))
      case cps.Stmt.Region(id, ks, rest) =>
        cps.Stmt.Region(id, ks, lowerStatement(rest, returns, plan))
      case cps.Stmt.Alloc(id, init, region, rest) =>
        cps.Stmt.Alloc(
          id, init, region,
          lowerStatement(rest, returns, plan))
      case cps.Stmt.Var(id, init, ks, rest) =>
        cps.Stmt.Var(id, init, ks, lowerStatement(rest, returns, plan))
      case cps.Stmt.Dealloc(ref, rest) =>
        cps.Stmt.Dealloc(ref, lowerStatement(rest, returns, plan))
      case cps.Stmt.Get(ref, id, rest) =>
        cps.Stmt.Get(ref, id, lowerStatement(rest, returns, plan))
      case cps.Stmt.Put(ref, value, rest) =>
        cps.Stmt.Put(ref, value, lowerStatement(rest, returns, plan))
      case cps.Stmt.Reset(p, ks, k, body, ks1, k1) =>
        cps.Stmt.Reset(
          p, ks, k,
          lowerStatement(body, None, plan),
          ks1, k1)
      case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        cps.Stmt.Shift(
          prompt, resume, ks, k,
          lowerStatement(body, None, plan),
          ks1, k1)
      case cps.Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        cps.Stmt.Resume(
          resumption, ks, k,
          lowerStatement(body, None, plan),
          ks1, k1)
      case hole: cps.Stmt.Hole => hole
  }

  /** Reify only the candidate remainders rejected by the plan. Selected calls
   *  stay compositional, and terminal applications of a selected definition's
   *  continuation become explicit `Return` statements. */
  def lower(module: cps.ModuleDecl, plan: Plan): cps.ModuleDecl = {
    val lowered = module.copy(definitions = module.definitions.map {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        val directParams =
          if plan.isDirect(id) then params.dropRight(2)
          else params
        cps.ToplevelDefinition.Def(
          id,
          directParams,
          lowerStatement(
            body,
            returnParameters(plan, id, params),
            plan))
      case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
        cps.ToplevelDefinition.Val(
          id, ks, k,
          lowerStatement(binding, None, plan))
    })
    val introduced = lowered.uses.toMap.keySet -- module.uses.toMap.keySet
    val nested = cps.BlockSinking.sinkIntroduced(lowered, introduced)
    // Lowering exposes path-static meta-continuations that were deliberately
    // unavailable while `Call` fixed the callee's control parameters.
    cps.StaticArguments.specializeCpsMetaContinuations(
      nested,
      plan.directDefinitions)
  }

  private final case class Definition(
    id: Id,
    params: Vector[Id],
    body: cps.Stmt,
    toplevel: Boolean
  ) {
    def ks: Id = params(params.size - 2)
    def k: Id = params.last
    def directParams: Vector[Id] = params.dropRight(2)
  }

  private final case class Site(
    call: cps.Stmt.Call,
    owner: Id,
    targets: Set[Id],
    closed: Boolean,
    tailSelf: Boolean
  )

  final class Plan private[CallingConvention] (
    val ranks: Map[Id, Int],
    val parameterArities: Map[Id, Map[Int, Int]],
    private val cpsEntries: Set[Id],
    private val originals: Map[Id, OriginalDefinition],
    private val sites: Map[Id, Site]
  ) {
    val directDefinitions: Set[Id] = ranks.keySet

    def isDirect(id: Id): Boolean = directDefinitions.contains(id)

    def needsCpsEntry(id: Id): Boolean = cpsEntries.contains(id)

    def original(id: Id): OriginalDefinition = originals(id)

    def isDirect(call: cps.Stmt.Call): Boolean = {
      val site = sites.get(call.id)
      site.exists(site => site.closed && site.targets.nonEmpty &&
        site.targets.forall(directDefinitions.contains)
      )
    }

    def targets(call: cps.Stmt.Call): Set[Id] =
      sites.get(call.id).fold(Set.empty[Id])(_.targets)

    /** The function-valued arguments of this call and their direct arities.
     *  All possible targets have the same map; this is precisely the ABI
     *  coherence condition for an indirect call. */
    def directArguments(call: cps.Stmt.Call): Map[Int, Int] =
      targets(call).headOption
        .fold(Map.empty[Int, Int])(id => parameterArities.getOrElse(id, Map.empty))

    def directParameterArity(id: Id, position: Int): Option[Int] =
      parameterArities.get(id).flatMap(_.get(position))

    def isFirstOrder(id: Id): Boolean =
      parameterArities.getOrElse(id, Map.empty).isEmpty

    def isTailSelf(call: cps.Stmt.Call): Boolean =
      sites.get(call.id).exists(_.tailSelf)

    def isTailRecursive(id: Id): Boolean =
      sites.valuesIterator.exists(site => site.owner == id && site.tailSelf)

    def validate(): Unit = {
      directDefinitions.foreach { source =>
        sites.valuesIterator
          .filter(_.owner == source)
          .foreach { site =>
            assert(site.closed && site.targets.nonEmpty)
            assert(site.targets.subsetOf(directDefinitions))
            assert(site.targets.iterator
              .map(id => parameterArities.getOrElse(id, Map.empty))
              .toSet.size == 1)

            if !site.tailSelf then site.targets.foreach { target =>
              assert(ranks(source) > ranks(target))
            }
          }
      }
    }

    def show: String = {
      val entries = directDefinitions.toVector
        .sortBy(id => (id.name.name, id.id))
        .map { id =>
          val direct = parameterArities.getOrElse(id, Map.empty).keySet.toVector.sorted
          val arguments = if direct.isEmpty then "" else s" [direct: ${direct.mkString(", ")}]"
          val adapter = if cpsEntries.contains(id) then " adapter" else ""
          s"  ${id.name.name} = ${ranks(id)}$arguments$adapter"
        }
      if entries.isEmpty then "-" else s"direct\n${entries.mkString("\n")}"
    }
  }

  extension [K, V](map: IdentityHashMap[K, V])
    private def valuesIterator: Iterator[V] =
      val values = map.values().iterator()
      new Iterator[V] {
        def hasNext: Boolean = values.hasNext
        def next(): V = values.next()
      }

  def analyze(
    module: cps.ModuleDecl,
    targetFlows: Vector[cps.GuardedEquality.TargetResult]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)

    val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        definitions(id) = Definition(id, params.toVector, body, toplevel = true)
      case _: cps.ToplevelDefinition.Val => ()
    }
    targetFlows.foreach(_.localDefinitions.foreach { definition =>
      definitions(definition.id) = Definition(
        definition.id,
        definition.params,
        definition.body,
        toplevel = false)
    })

    val flowed = new IdentityHashMap[cps.Stmt.Call, cps.GuardedEquality.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach { targets =>
      targets.call match {
        case call: cps.Stmt.Call => flowed.put(call, targets)
        case _ => ()
      }
    })

    def returned(stmt: cps.Stmt, result: Id, definition: Definition): Boolean = stmt match {
      case cps.Stmt.App(k, List(cps.Expr.Variable(value), cps.Expr.Variable(ks)))
          if k == definition.k && value == result && ks == definition.ks => true
      case _ => false
    }

    def resolve(call: cps.Stmt.Call): (Set[Id], Boolean) =
      definitions.get(call.callee) match {
        case Some(target) if target.params.size == call.args.size + 2 =>
          Set(target.id) -> true
        case _ =>
          Option(flowed.get(call)) match {
            case Some(result) =>
              val targets = result.targets.filter(definitions.contains)
              val compatible = targets.nonEmpty && targets.forall { id =>
                definitions(id).params.size == call.args.size + 2
              }
              targets -> (result.closed && compatible)
            case None => Set.empty[Id] -> false
          }
      }

    val sites = mutable.LinkedHashMap.empty[Id, Site]
    val callsByOwner = mutable.LinkedHashMap.empty[Id, Vector[Site]]

    /** Calling a finite-rank direct callee is valid from any computation,
     * including one which itself retains CPS. This traversal records that
     * callee-side judgment independently of the control-erasure proof below.
     * Nested definitions are analyzed under their own owner. */
    def collectSites(stmt: cps.Stmt, owner: Id): Unit = stmt match {
      case cps.Stmt.Def(_, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.New(_, _, operations, rest) =>
        operations.foreach(operation => collectSites(operation.body, owner))
        collectSites(rest, owner)
      case cps.Stmt.Let(_, _, rest) => collectSites(rest, owner)
      case call @ cps.Stmt.Call(result, _, _, _, rest) =>
        val (targets, closed) = resolve(call)
        sites(result) = Site(
          call,
          owner,
          targets,
          closed,
          targets == Set(owner) && call.callee == owner &&
            definitions.get(owner).exists(returned(rest, result, _)))
        collectSites(rest, owner)
      case cps.Stmt.Run(_, _, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.If(_, thn, els) =>
        collectSites(thn, owner)
        collectSites(els, owner)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => collectSites(clause.body, owner) }
        default.foreach(collectSites(_, owner))
      case cps.Stmt.Region(_, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Alloc(_, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Var(_, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Dealloc(_, rest) => collectSites(rest, owner)
      case cps.Stmt.Get(_, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Put(_, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Reset(_, _, _, body, _, _) => collectSites(body, owner)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => collectSites(body, owner)
      case cps.Stmt.Resume(_, _, _, body, _, _) => collectSites(body, owner)
      case _: cps.Stmt.App | _: cps.Stmt.Invoke | _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
    }

    definitions.valuesIterator.foreach(definition => collectSites(definition.body, definition.id))
    module.definitions.foreach {
      case cps.ToplevelDefinition.Val(id, _, _, binding) => collectSites(binding, id)
      case _: cps.ToplevelDefinition.Def => ()
    }

    /** The control erasure homomorphism. Nested definition and operation
     * bodies have their own conventions; only their lexical remainders are
     * part of the enclosing computation. */
    def inspect(stmt: cps.Stmt, definition: Definition): Option[Vector[Site]] = stmt match {
      case cps.Stmt.Def(_, _, _, rest) => inspect(rest, definition)
      case cps.Stmt.New(_, _, _, rest) => inspect(rest, definition)
      case cps.Stmt.Let(_, _, rest) => inspect(rest, definition)

      case call @ cps.Stmt.Call(result, _, _, _, rest) =>
        val (targets, closed) = resolve(call)
        inspect(rest, definition).map { following =>
          Site(
            call,
            definition.id,
            targets,
            closed,
            targets == Set(definition.id) && call.callee == definition.id &&
              returned(rest, result, definition)) +: following
        }

      case cps.Stmt.App(k, List(_, cps.Expr.Variable(ks)))
          if k == definition.k && ks == definition.ks => Some(Vector.empty)

      // Before convention lowering, `Return` means completion of the current
      // CPS computation, not application of this definition's continuation.
      // Treating it as an ordinary function return would change which
      // continuation receives the value.
      case cps.Stmt.Return(_) => None

      case cps.Stmt.Run(_, _, _, cps.Purity.Pure | cps.Purity.Impure, rest) =>
        inspect(rest, definition)
      case cps.Stmt.If(_, thn, els) =>
        for left <- inspect(thn, definition); right <- inspect(els, definition)
        yield left ++ right
      case cps.Stmt.Match(_, clauses, default) =>
        val branches = clauses.map(_._2.body) ++ default
        branches.foldLeft(Option(Vector.empty[Site])) { (found, branch) =>
          for before <- found; after <- inspect(branch, definition)
          yield before ++ after
        }
      case cps.Stmt.Alloc(_, _, _, rest) => inspect(rest, definition)
      case cps.Stmt.Dealloc(_, rest) => inspect(rest, definition)
      case cps.Stmt.Get(_, _, rest) => inspect(rest, definition)
      case cps.Stmt.Put(_, _, rest) => inspect(rest, definition)

      // Unknown calls and control delimiters cannot synchronously produce the
      // value expected by the direct ABI.
      case _ => None
    }

    val erasable = definitions.valuesIterator.flatMap { definition =>
      Option.when(definition.params.size >= 2) {
        inspect(definition.body, definition).map { calls =>
          callsByOwner(definition.id) = calls
          definition.id
        }
      }.flatten
    }.toSet

    /** Nodes in cyclic components of the positive (frame-adding) graph. */
    def cyclic(nodes: Set[Id]): Set[Id] = {
      val index = mutable.Map.empty[Id, Int]
      val lowlink = mutable.Map.empty[Id, Int]
      val stack = mutable.ArrayBuffer.empty[Id]
      val onStack = mutable.Set.empty[Id]
      val result = mutable.Set.empty[Id]
      var next = 0

      def successors(id: Id): Iterator[Id] =
        callsByOwner.getOrElse(id, Vector.empty).iterator
          .filterNot(_.tailSelf)
          .flatMap(_.targets)
          .filter(nodes)

      def connect(id: Id): Unit = {
        index(id) = next
        lowlink(id) = next
        next += 1
        stack += id
        onStack += id

        successors(id).foreach { target =>
          if !index.contains(target) then {
            connect(target)
            lowlink(id) = math.min(lowlink(id), lowlink(target))
          } else if onStack(target) then
            lowlink(id) = math.min(lowlink(id), index(target))
        }

        if lowlink(id) == index(id) then {
          val component = mutable.ArrayBuffer.empty[Id]
          var done = false
          while !done do {
            val member = stack.remove(stack.size - 1)
            onStack -= member
            component += member
            done = member == id
          }
          val selfCycle = component.size == 1 && successors(component.head).contains(component.head)
          if component.size > 1 || selfCycle then result ++= component
        }
      }

      nodes.foreach(id => if !index.contains(id) then connect(id))
      result.toSet
    }

    @tailrec def close(current: Set[Id]): Set[Id] = {
      val updated = current.filter { id =>
        callsByOwner.getOrElse(id, Vector.empty).forall { site =>
          site.closed && site.targets.nonEmpty && site.targets.subsetOf(current)
        }
      }
      if updated == current then current else close(updated)
    }

    // This is the greatest control-closed solution. In particular, recursion
    // is not a reason to retain continuation parameters.
    var direct = close(erasable)
    val toplevel = definitions.valuesIterator.filter(_.toplevel).map(_.id).toSet

    /** A parameter has the direct ABI exactly when it occurs as the callee of
     * a compositional call. Indirect calls additionally equate the parameter
     * conventions of all their possible targets. Other representation
     * crossings are explicit coercions in JavaScript generation; they are not
     * reasons to reject the enclosing direct definition. */
    def parameterRequirements(current: Set[Id]): (Map[Id, Map[Int, Int]], Set[Id]) = {
      val requirements = mutable.Map.from(current.iterator.map(_ -> Map.empty[Int, Int]))
      val invalid = mutable.Set.empty[Id]

      def require(id: Id, position: Int, arity: Int): Boolean =
        requirements(id).get(position) match {
          case Some(found) if found != arity =>
            invalid += id
            false
          case Some(_) => false
          case None =>
            requirements(id) = requirements(id).updated(position, arity)
            true
        }

      var changed = true
      while changed do {
        changed = false
        current.foreach { owner =>
          val definition = definitions(owner)
          val parameterIndex = definition.directParams.zipWithIndex.toMap
          callsByOwner.getOrElse(owner, Vector.empty).foreach { site =>
            parameterIndex.get(site.call.callee).foreach { position =>
              changed = require(owner, position, site.call.args.size) || changed
            }

            val byPosition = site.targets.iterator
              .flatMap(target => requirements(target))
              .toVector
              .groupMap(_._1)(_._2)
            byPosition.foreach { case (position, arities) =>
              arities.distinct match {
                case Vector(arity) =>
                  site.targets.foreach { target =>
                    changed = require(target, position, arity) || changed
                  }
                case _ => invalid += owner
              }
            }
          }
        }
      }
      requirements.toMap -> invalid.toSet
    }

    var requirements = Map.empty[Id, Map[Int, Int]]
    var stable = false
    while !stable do {
      val (nextRequirements, invalidRepresentations) = parameterRequirements(direct)
      val controlClosed = close(direct -- invalidRepresentations)

      // A value-returning JavaScript implementation is useful only when the
      // positive call graph is acyclic. Tail self calls have weight zero and
      // remain direct loops. A positive recursive component stays in CPS;
      // wrapping it in a direct entry would merely hide its CPS worker from
      // defunctionalization without removing any control representation.
      val updated = close(controlClosed -- cyclic(controlClosed))
      requirements = nextRequirements.view.filterKeys(updated.contains).toMap
      stable = updated == direct
      direct = updated
    }

    val native = direct
    val edges = native.iterator.map { source =>
      val targets = callsByOwner.getOrElse(source, Vector.empty).iterator
        .filterNot(_.tailSelf).flatMap(_.targets).filter(direct).toSet
      source -> targets
    }.toMap
    val ranks = mutable.Map.empty[Id, Int]
    def rank(id: Id): Int = ranks.getOrElseUpdate(id,
      edges.getOrElse(id, Set.empty).iterator.map { target =>
        if native.contains(target) then rank(target) + 1 else 1
      }.maxOption.getOrElse(0))
    native.foreach(rank)

    def ordinaryAll(expressions: IterableOnce[cps.Expr]): Set[Id] =
      expressions.iterator.flatMap(_.free).filter(direct).toSet

    def ordinary(expression: cps.Expr): Set[Id] =
      expression.free.intersect(direct)

    def cpsCallee(id: Id): Set[Id] =
      Option.when(direct.contains(id))(id).toSet

    def hasDirectRepresentation(expression: cps.Expr, owner: Option[Id]): Boolean =
      expression match {
        case cps.Expr.Variable(id) if direct.contains(id) => true
        case cps.Expr.Variable(id) => owner.exists { definition =>
          definitions(definition).directParams.zipWithIndex.exists {
            case (parameter, position) =>
              parameter == id &&
                requirements.getOrElse(definition, Map.empty).contains(position)
          }
        }
        case _ => false
      }

    /** Direct definitions need a CPS entry only at ordinary value boundaries.
     * Calls made by a private CPS worker are ordinary CPS calls as well. */
    def cpsReferences(stmt: cps.Stmt, owner: Option[Id]): Set[Id] = stmt match {
      case cps.Stmt.Def(_, _, _, rest) => cpsReferences(rest, owner)
      case cps.Stmt.New(_, _, operations, rest) =>
        operations.iterator.flatMap(op => cpsReferences(op.body, None)).toSet ++
          cpsReferences(rest, owner)
      case cps.Stmt.Let(_, binding, rest) =>
        ordinary(binding) ++ cpsReferences(rest, owner)

      case call @ cps.Stmt.Call(_, callee, arguments, ks, rest) =>
        val selected = sites.get(call.id).exists { site =>
          site.closed && site.targets.nonEmpty && site.targets.subsetOf(direct)
        }
        val emittedDirect = selected
        val values = if emittedDirect then {
          val directArguments = sites(call.id).targets.headOption
            .fold(Map.empty[Int, Int])(id => requirements.getOrElse(id, Map.empty))
          arguments.zipWithIndex.iterator
            .filterNot { case (argument, position) =>
              directArguments.contains(position) &&
                hasDirectRepresentation(argument, owner)
            }
            .map(_._1)
        } else arguments.iterator ++ Iterator.single(ks)
        val calleeEntry = if emittedDirect then Set.empty else cpsCallee(callee)
        calleeEntry ++ ordinaryAll(values) ++ cpsReferences(rest, owner)

      case cps.Stmt.App(callee, arguments) =>
        cpsCallee(callee) ++ ordinaryAll(arguments)
      case cps.Stmt.Invoke(receiver, _, arguments) =>
        cpsCallee(receiver) ++ ordinaryAll(arguments)
      case cps.Stmt.Return(value) => ordinary(value)
      case cps.Stmt.Run(_, callee, arguments, _, rest) =>
        cpsCallee(callee) ++ ordinaryAll(arguments) ++ cpsReferences(rest, owner)
      case cps.Stmt.If(condition, thn, els) =>
        ordinary(condition) ++ cpsReferences(thn, owner) ++ cpsReferences(els, owner)
      case cps.Stmt.Match(scrutinee, clauses, default) =>
        ordinary(scrutinee) ++
          clauses.iterator.flatMap { case (_, clause) => cpsReferences(clause.body, owner) }.toSet ++
          default.fold(Set.empty[Id])(cpsReferences(_, owner))
      case cps.Stmt.Region(_, ks, rest) => ordinary(ks) ++ cpsReferences(rest, owner)
      case cps.Stmt.Alloc(_, init, region, rest) =>
        cpsCallee(region) ++ ordinary(init) ++ cpsReferences(rest, owner)
      case cps.Stmt.Var(_, init, ks, rest) =>
        ordinaryAll(List(init, ks)) ++ cpsReferences(rest, owner)
      case cps.Stmt.Dealloc(ref, rest) => cpsCallee(ref) ++ cpsReferences(rest, owner)
      case cps.Stmt.Get(ref, _, rest) => cpsCallee(ref) ++ cpsReferences(rest, owner)
      case cps.Stmt.Put(ref, value, rest) =>
        cpsCallee(ref) ++ ordinary(value) ++ cpsReferences(rest, owner)
      case cps.Stmt.Reset(prompt, _, _, body, ks, k) =>
        cpsCallee(prompt) ++ ordinaryAll(List(ks, k)) ++ cpsReferences(body, None)
      case cps.Stmt.Shift(prompt, _, _, _, body, ks, k) =>
        cpsCallee(prompt) ++ ordinaryAll(List(ks, k)) ++ cpsReferences(body, None)
      case cps.Stmt.Resume(resumption, _, _, body, ks, k) =>
        cpsCallee(resumption) ++ ordinaryAll(List(ks, k)) ++ cpsReferences(body, None)
      case cps.Stmt.Hole(_) => Set.empty
    }

    val cpsEntries = toplevel.intersect(direct) ++ definitions.valuesIterator
      .flatMap(definition => cpsReferences(definition.body, Some(definition.id)))
      .toSet
    val originals = direct.iterator.map { id =>
      val definition = definitions(id)
      id -> OriginalDefinition(definition.params.toList)
    }.toMap

    val plan = Plan(
      ranks.toMap,
      requirements,
      cpsEntries,
      originals,
      sites.toMap)
    plan.validate()
    plan
  }
}
