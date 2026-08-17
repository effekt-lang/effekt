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
 * A closed positive-recursive local region is also a zero-cost boundary: it
 * keeps CPS internally, while its unique entry continuation becomes the
 * return case of the local defunctionalized dispatcher.
 */
object CallingConvention {

  private def apply(callee: cps.Callee, arguments: List[cps.Expr]): cps.Stmt = callee match {
    case cps.Callee.Function(id) => cps.Stmt.App(id, arguments)
    case cps.Callee.Method(receiver, method) => cps.Stmt.Invoke(receiver, method, arguments)
  }

  /** A continuation application can omit its meta-continuation after
   * parameter dropping. Keeping both shapes here makes the control-erasure
   * judgment independent of that earlier representation choice. */
  private def continuationResult(stmt: cps.Stmt): Option[(Id, cps.Expr, Option[Id])] =
    stmt match {
      case cps.Stmt.App(k, List(value)) => Some((k, value, None))
      case cps.Stmt.App(k, List(value, cps.Expr.Variable(ks))) =>
        Some((k, value, Some(ks)))
      case _ => None
    }

  final case class OriginalDefinition(params: List[Id])

  private def returnParameters(plan: Plan, id: Id, params: List[Id]): Option[(Id, Id)] =
    Option.when(plan.isDirectEntry(id) && params.size >= 2)(
      params(params.size - 2) -> params.last)

  /** Lower one lexical computation according to the selected convention. */
  private def lowerStatement(
    stmt: cps.Stmt,
    returns: Option[(Id, Id)],
    directBody: Boolean,
    plan: Plan
  ): cps.Stmt = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        val directParams =
          if plan.isDirect(id) then params.dropRight(2)
          else params
        val bodyReturns =
          if plan.inheritsReturn(id) then returns
          else returnParameters(plan, id, params)
        val bodyIsDirect =
          if plan.inheritsReturn(id) then directBody
          else plan.isDirect(id)
        cps.Stmt.Def(
          id,
          directParams,
          lowerStatement(body, bodyReturns, bodyIsDirect, plan),
          lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.New(id, interface, operations, rest) =>
        cps.Stmt.New(
          id,
          interface,
          operations.map { operation =>
            val direct = plan.isDirectOperation(id, operation.name)
            val returns = plan.operationId(id, operation.name)
              .filter(_ => direct)
              .flatMap(operationId => returnParameters(plan, operationId, operation.params))
            operation.copy(
              params = if direct then operation.params.dropRight(2) else operation.params,
              body = lowerStatement(operation.body, returns, direct, plan))
          },
          lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Let(id, binding, rest) =>
        cps.Stmt.Let(id, binding, lowerStatement(rest, returns, directBody, plan))

      // A shared join is entered only in tail position. Its declaration is
      // retained as a lexical labeled region, so the compositional call
      // becomes an ordinary second-class jump after erasing control params.
      case call @ cps.Stmt.Call(_, _, callee, arguments, _, _)
          if plan.isSharedJoin(call) =>
        apply(callee, arguments)

      case call @ cps.Stmt.Call(result, returnedKs, callee, arguments, ks, rest)
          if plan.isDirect(call) =>
        val directRest = cps.substitutions.substitute(rest)(using
          cps.substitutions.Substitution(Map(returnedKs -> ks)))
        cps.Stmt.Call(
          result,
          returnedKs,
          callee,
          arguments,
          ks,
          lowerStatement(directRest, returns, directBody, plan))

      // A positive recursive local region retains CPS internally, but its
      // closed continuation machine can return a value to a direct enclosing
      // computation. Reify that remainder with `Toplevel` as the private
      // meta-continuation; defunctionalization subsequently turns the entry
      // continuation into the return case of the local dispatch loop.
      case call @ cps.Stmt.Call(result, returnedKs, callee, arguments, _, rest)
          if plan.isMachine(call) =>
        val continuation = Id("k")
        val directRest = cps.substitutions.substitute(rest)(using
          cps.substitutions.Substitution(Map(returnedKs -> cps.Expr.Toplevel)))
        cps.Stmt.Def(
          continuation,
          List(result, returnedKs),
          lowerStatement(directRest, returns, directBody, plan),
          apply(callee, arguments ++ List(
            cps.Expr.Toplevel,
            cps.Expr.Variable(continuation))))

      // If a direct computation calls a region that retains CPS, run that
      // region to completion and continue with its ordinary result. The
      // JavaScript backend supplies fresh boundary continuations, so neither
      // removed control parameter may remain free here.
      case cps.Stmt.Call(result, returnedKs, callee, arguments, _, rest)
          if directBody =>
        val directRest = cps.substitutions.substitute(rest)(using
          cps.substitutions.Substitution(Map(returnedKs -> cps.Expr.Toplevel)))
        cps.Stmt.Call(
          result,
          returnedKs,
          callee,
          arguments,
          cps.Expr.Toplevel,
          lowerStatement(directRest, returns, directBody, plan))

      // Reifying an already-tail CPS call would introduce the eta expansion
      //
      //   def next(result, returnedKs) = k(result, ks)
      //   callee(..., ks, next)
      //
      // when the remainder simply forwards the result under the same
      // meta-continuation. Preserve the canonical tail call instead.
      case cps.Stmt.Call(result, returnedKs, callee, arguments, ks, rest)
          if continuationResult(rest).exists {
            case (_, cps.Expr.Variable(_), None) => false
            case (_, cps.Expr.Variable(returned), Some(restKs)) =>
              returned == result &&
                (restKs == returnedKs || cps.Expr.Variable(restKs) == ks)
            case _ => false
          } =>
        val (k, _, _) = continuationResult(rest).get
        apply(callee, arguments ++ List(ks, cps.Expr.Variable(k)))

      case cps.Stmt.Call(result, returnedKs, callee, arguments, ks, rest) =>
        val continuation = Id("k")
        cps.Stmt.Def(
          continuation,
          List(result, returnedKs),
          lowerStatement(rest, returns, directBody, plan),
          apply(callee, arguments ++ List(ks, cps.Expr.Variable(continuation))))

      case app: cps.Stmt.App =>
        val result = for
          (ks, k) <- returns
          (callee, value, meta) <- continuationResult(app)
          if callee == k && meta.forall(_ == ks)
        yield cps.Stmt.Return(value)
        result.getOrElse(app)
      case invoke: cps.Stmt.Invoke => invoke
      case returned: cps.Stmt.Return => returned
      case cps.Stmt.Run(id, callee, arguments, purity, rest) =>
        cps.Stmt.Run(
          id, callee, arguments, purity,
          lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.If(condition, thn, els) =>
        cps.Stmt.If(
          condition,
          lowerStatement(thn, returns, directBody, plan),
          lowerStatement(els, returns, directBody, plan))
      case cps.Stmt.Match(scrutinee, clauses, default) =>
        cps.Stmt.Match(
          scrutinee,
          clauses.map { case (tag, clause) =>
            tag -> clause.copy(
              body = lowerStatement(clause.body, returns, directBody, plan))
          },
          default.map(lowerStatement(_, returns, directBody, plan)))
      case cps.Stmt.Region(id, ks, rest) =>
        cps.Stmt.Region(id, ks, lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Alloc(id, init, region, rest) =>
        cps.Stmt.Alloc(
          id, init, region,
          lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Var(id, init, ks, rest) =>
        // A selected direct definition can contain only local variables whose
        // reference and meta-continuation dependency were proved erasable.
        // Make that erasure explicit so the lowered body does not retain the
        // removed `ks` binder as a free variable.
        val loweredKs = if directBody then cps.Expr.Toplevel else ks
        cps.Stmt.Var(id, init, loweredKs,
          lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Dealloc(ref, rest) =>
        cps.Stmt.Dealloc(ref, lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Get(ref, id, rest) =>
        cps.Stmt.Get(ref, id, lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Put(ref, value, rest) =>
        cps.Stmt.Put(ref, value, lowerStatement(rest, returns, directBody, plan))
      case cps.Stmt.Reset(p, ks, k, body, ks1, k1) =>
        cps.Stmt.Reset(
          p, ks, k,
          lowerStatement(body, None, false, plan),
          ks1, k1)
      case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        cps.Stmt.Shift(
          prompt, resume, ks, k,
          lowerStatement(body, None, false, plan),
          ks1, k1)
      case cps.Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        cps.Stmt.Resume(
          resumption, ks, k,
          lowerStatement(body, None, false, plan),
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
            plan.isDirect(id),
            plan))
      case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
        cps.ToplevelDefinition.Val(
          id, ks, k,
          lowerStatement(binding, None, false, plan))
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
    toplevel: Boolean,
    parent: Option[Id]
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
    tail: Boolean,
    tailSelf: Boolean,
    known: Boolean
  )

  final class Plan private[CallingConvention] (
    val ranks: Map[Id, Int],
    val parameterArities: Map[Id, Map[Int, Int]],
    private val cpsEntries: Set[Id],
    private val originals: Map[Id, OriginalDefinition],
    private val sites: Map[Id, Site],
    private val machineSites: Set[Id],
    val joinDefinitions: Set[Id],
    val sharedJoinDefinitions: Set[Id],
    private val joinLoops: Set[Id],
    private val inheritedReturnDefinitions: Set[Id],
    private val operations: Map[(Id, String), Id],
    private val operationNames: Map[Id, String]
  ) {
    private val operationIds = operations.values.toSet
    val directDefinitions: Set[Id] = ranks.keySet -- operationIds
    val directOperations: Set[Id] = ranks.keySet.intersect(operationIds)

    private val loopMutations: Map[Id, Set[Id]] = {
      val result = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Id]]
      sites.valuesIterator.filter(_.tailSelf).foreach { site =>
        originals.get(site.owner).foreach { original =>
          val params = original.params.dropRight(2)
          val mutated = result.getOrElseUpdate(site.owner, mutable.LinkedHashSet.empty)
          if params.size != site.call.args.size then mutated ++= params
          else params.zip(site.call.args).foreach {
            case (param, cps.Expr.Variable(argument)) if param == argument => ()
            case (param, _) => mutated += param
          }
        }
      }
      result.iterator.map { case (id, params) => id -> params.toSet }.toMap
    }

    def isDirect(id: Id): Boolean = directDefinitions.contains(id)

    private[CallingConvention] def isDirectEntry(id: Id): Boolean = ranks.contains(id)

    def isDirectOperation(objectId: Id, method: Id): Boolean =
      operations.get(objectId -> method.name.name).exists(directOperations.contains)

    def directOperation(objectId: Id, method: Id): Option[Id] =
      operations.get(objectId -> method.name.name).filter(directOperations.contains)

    private[CallingConvention] def operationId(objectId: Id, method: Id): Option[Id] =
      operations.get(objectId -> method.name.name)

    def needsCpsEntry(id: Id): Boolean = cpsEntries.contains(id)

    def original(id: Id): OriginalDefinition = originals(id)

    def isDirect(call: cps.Stmt.Call): Boolean = {
      val site = sites.get(call.id)
      site.exists(site => site.closed && site.targets.nonEmpty &&
        site.targets.forall(ranks.contains) &&
        (site.known || ranks.contains(site.owner))
      )
    }

    /** A call implemented by a closed local continuation machine rather than
     *  by nested JavaScript calls. */
    def isMachine(call: cps.Stmt.Call): Boolean =
      machineSites.contains(call.id)

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

    /** A selected local definition represented by structured control rather
     *  than by a JavaScript function. */
    def isJoin(id: Id): Boolean = joinDefinitions.contains(id)

    /** A join with several forward edges is materialized at its lexical
     * definition, so every edge can jump to the one shared body. */
    def isSharedJoin(id: Id): Boolean = sharedJoinDefinitions.contains(id)

    private[CallingConvention] def isSharedJoin(call: cps.Stmt.Call): Boolean =
      sites.get(call.id).exists(site =>
        site.targets.nonEmpty && site.targets.subsetOf(sharedJoinDefinitions))

    /** Calls to an already active join are tail transfers to the same or an
     *  enclosing loop. */
    def isJoinBackEdge(call: cps.Stmt.Call): Boolean =
      sites.get(call.id).exists(site => site.tail &&
        site.targets.nonEmpty && site.targets.subsetOf(joinDefinitions))

    def isJoinLoop(id: Id): Boolean = joinLoops.contains(id)

    /** A parameter-dropped local block executes in its enclosing direct
     * definition and therefore shares that definition's return convention. */
    def inheritsReturn(id: Id): Boolean = inheritedReturnDefinitions.contains(id)

    def isTailRecursive(id: Id): Boolean =
      sites.valuesIterator.exists(site => site.owner == id && site.tailSelf)

    /** Parameters whose direct loop registers can receive a different value
     *  on a tail-self back edge. */
    def mutableParameters(id: Id): Set[Id] =
      loopMutations.getOrElse(id, Set.empty)

    /** Whether a selected call enters this definition's value-returning
     *  implementation. Tail self calls become loop back-edges and therefore
     *  do not require a separately named worker. */
    def needsDirectWorker(id: Id): Boolean =
      !isJoin(id) && sites.valuesIterator.exists { site =>
        !site.tailSelf && site.targets.contains(id) && isDirect(site.call)
      }

    def validate(): Unit = {
      ranks.keysIterator.foreach { source =>
        sites.valuesIterator
          .filter(_.owner == source)
          .foreach { site =>
            assert(site.closed && site.targets.nonEmpty)
            if !machineSites.contains(site.call.id) then {
              assert(site.targets.forall(ranks.contains))
              assert(site.targets.iterator
                .map(id => parameterArities.getOrElse(id, Map.empty))
                .toSet.size == 1)

              if !site.tailSelf && !site.targets.subsetOf(joinDefinitions) then
                site.targets.foreach { target =>
                  assert(ranks(source) > ranks(target))
                }
            }
          }
      }
    }

    def show: String = {
      val entries = ranks.keysIterator.toVector
        .sortBy(id => (id.name.name, id.id))
        .map { id =>
          val direct = parameterArities.getOrElse(id, Map.empty).keySet.toVector.sorted
          val arguments = if direct.isEmpty then "" else s" [direct: ${direct.mkString(", ")}]"
          val adapter = if cpsEntries.contains(id) then " adapter" else ""
          val label = operationNames.getOrElse(id, id.name.name)
          s"  $label = ${ranks(id)}$arguments$adapter"
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

  /** An operation implementation is identified by its object allocation and
   *  selector. The object binder is globally fresh, so this is a stable
   *  semantic name even though operations themselves are not binders in CPS. */
  private final case class OperationInfo(
    objectId: Id,
    method: Id,
    id: Id,
    operation: cps.Operation
  )

  private final case class MethodTargets(
    targets: Set[Id],
    closed: Boolean,
    compositional: Boolean
  )

  private final case class FlowValue(
    functions: Set[Id],
    objects: Set[Id],
    open: Boolean
  ) {
    def join(other: FlowValue): FlowValue =
      FlowValue(
        functions ++ other.functions,
        objects ++ other.objects,
        open || other.open)
  }

  private object FlowValue {
    val Empty: FlowValue = FlowValue(Set.empty, Set.empty, open = false)
    val Unknown: FlowValue = FlowValue(Set.empty, Set.empty, open = true)
    def function(id: Id): FlowValue = FlowValue(Set(id), Set.empty, open = false)
    def objectAllocation(id: Id): FlowValue = FlowValue(Set.empty, Set(id), open = false)
  }

  /** Finite flow of callable and object values.
   *
   * Both kinds of values obey the same 0-CFA equations: allocations introduce
   * singleton values, aliases preserve them, and calls propagate arguments to
   * every possible parameter. Tracking them in one domain is important across
   * toplevel boundaries: a known function argument can carry known handler
   * objects into its body even when the per-definition guarded analysis cannot
   * see the caller. A method call is closed iff its receiver denotes a
   * nonempty finite set of object allocations and each allocation implements
   * the selected method. */
  private final class ValueFlow(
    module: cps.ModuleDecl,
    functions: Map[Id, Definition],
    operations: Map[(Id, String), OperationInfo],
    targetFlows: Vector[cps.GuardedEquality.TargetResult],
    externalEntries: Set[Id]
  ) {
    private val functionTargets = new IdentityHashMap[cps.Stmt, cps.GuardedEquality.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach { targets =>
      targets.call match {
        case call @ cps.Stmt.Call(_, _, cps.Callee.Function(_), _, _, _) =>
          functionTargets.put(call, targets)
        case app: cps.Stmt.App => functionTargets.put(app, targets)
        case _ => ()
      }
    })

    private val bound = mutable.Set.empty[Id]
    functions.valuesIterator.foreach(definition => bound ++= definition.params)

    private val values = mutable.Map.empty[Id, FlowValue]
      .withDefaultValue(FlowValue.Empty)
    private val escaped = mutable.Set.empty[Id]
    private var changed = false
    private var closeOpenCalls = false

    private def add(id: Id, incoming: FlowValue): Unit = {
      bound += id
      val joined = values(id).join(incoming)
      if joined != values(id) then {
        values(id) = joined
        changed = true
      }
    }

    private def value(id: Id): FlowValue =
      if bound.contains(id) then values(id) else FlowValue.Unknown

    private def eval(expr: cps.Expr): FlowValue = expr match {
      case cps.Expr.Variable(id) => value(id)
      case cps.Expr.Make(_, _, arguments) =>
        arguments.iterator.map(eval).foldLeft(FlowValue.Empty)(_ join _)
      case _ => FlowValue.Empty
    }

    private def escape(value: FlowValue): Unit = {
      val before = escaped.size
      escaped ++= value.objects
      changed ||= escaped.size != before

      // An escaped closure may be entered with arbitrary arguments. Likewise,
      // every operation of an escaped object may be invoked through the CPS
      // ABI with arbitrary arguments.
      value.functions.foreach { function =>
        functions.get(function).foreach { definition =>
          definition.params.foreach(add(_, FlowValue.Unknown))
        }
      }
      value.objects.foreach { allocation =>
        operations.iterator.foreach {
          case ((owner, _), operation) if owner == allocation =>
            operation.operation.params.foreach(add(_, FlowValue.Unknown))
          case _ => ()
        }
      }
    }

    private def propagate(arguments: List[cps.Expr], targets: Set[Id]): Unit =
      targets.foreach { target =>
        functions.get(target).foreach { definition =>
          arguments.iterator.map(eval).zip(definition.params.iterator).foreach {
            case (argument, parameter) => add(parameter, argument)
          }
        }
      }

    private def resolveFunction(
      statement: cps.Stmt,
      callee: Id,
      arity: Int
    ): (Set[Id], Boolean) =
      functions.get(callee) match {
        case Some(definition) if definition.params.size == arity => Set(callee) -> true
        case _ =>
          Option(functionTargets.get(statement)) match {
            case Some(flow) if flow.closed =>
              val targets = flow.targets.filter { target =>
                functions.get(target).exists(_.params.size == arity)
              }
              targets -> (flow.closed && targets.nonEmpty)
            case _ =>
              val calleeValue = value(callee)
              val targets = calleeValue.functions.filter { target =>
                functions.get(target).exists(_.params.size == arity)
              }
              targets -> (!calleeValue.open && targets.nonEmpty)
          }
      }

    private def methodTargets(receiver: Id, method: Id): MethodTargets = {
      val receiverValue = value(receiver)
      val found = receiverValue.objects.flatMap { allocation =>
        operations.get(allocation -> method.name.name).map(_.id)
      }
      val complete = receiverValue.objects.nonEmpty && receiverValue.objects.forall { allocation =>
        operations.contains(allocation -> method.name.name)
      }
      MethodTargets(found, !receiverValue.open && complete, compositional = false)
    }

    private val observed = new IdentityHashMap[cps.Stmt, MethodTargets]()

    private def record(
      statement: cps.Stmt,
      receiver: Id,
      method: Id,
      compositional: Boolean
    ): MethodTargets = {
      val targets = methodTargets(receiver, method).copy(compositional = compositional)
      observed.put(statement, targets)
      targets
    }

    private def scan(stmt: cps.Stmt): Unit = stmt match {
      case cps.Stmt.Def(id, _, body, rest) =>
        add(id, FlowValue.function(id))
        scan(body)
        scan(rest)

      case cps.Stmt.New(id, _, implementations, rest) =>
        add(id, FlowValue.objectAllocation(id))
        implementations.foreach(operation => scan(operation.body))
        scan(rest)

      case cps.Stmt.Let(id, binding, rest) =>
        add(id, eval(binding))
        scan(rest)

      case call @ cps.Stmt.Call(result, returnedKs, cps.Callee.Function(callee), arguments, ks, rest) =>
        val supplied = arguments ++ List(ks, cps.Expr.Abort)
        val (targets, closed) = resolveFunction(call, callee, supplied.size)
        propagate(supplied, targets)
        if closeOpenCalls && !closed then
          supplied.foreach(argument => escape(eval(argument)))
        add(result, FlowValue.Unknown)
        add(returnedKs, FlowValue.Unknown)
        scan(rest)

      case call @ cps.Stmt.Call(result, returnedKs, cps.Callee.Method(receiver, method), arguments, ks, rest) =>
        val supplied = arguments ++ List(ks, cps.Expr.Abort)
        val targets = record(call, receiver, method, compositional = true)
        propagate(supplied, targets.targets)
        if closeOpenCalls && !targets.closed then
          supplied.foreach(argument => escape(eval(argument)))
        add(result, FlowValue.Unknown)
        add(returnedKs, FlowValue.Unknown)
        scan(rest)

      case app @ cps.Stmt.App(callee, arguments) =>
        val (targets, closed) = resolveFunction(app, callee, arguments.size)
        propagate(arguments, targets)
        if closeOpenCalls && !closed then
          arguments.foreach(argument => escape(eval(argument)))

      case invoke @ cps.Stmt.Invoke(receiver, method, arguments) =>
        val targets = record(invoke, receiver, method, compositional = false)
        propagate(arguments, targets.targets)
        if closeOpenCalls && !targets.closed then
          arguments.foreach(argument => escape(eval(argument)))

      case cps.Stmt.Return(result) => escape(eval(result))

      case cps.Stmt.Run(id, _, arguments, _, rest) =>
        arguments.foreach(argument => escape(eval(argument)))
        add(id, FlowValue.Unknown)
        scan(rest)

      case cps.Stmt.If(_, thn, els) => scan(thn); scan(els)
      case cps.Stmt.Match(scrutinee, clauses, default) =>
        escape(eval(scrutinee))
        clauses.foreach { case (_, clause) =>
          clause.params.foreach(add(_, FlowValue.Unknown))
          scan(clause.body)
        }
        default.foreach(scan)
      case cps.Stmt.Region(id, ks, rest) =>
        escape(eval(ks)); add(id, FlowValue.Unknown); scan(rest)
      case cps.Stmt.Alloc(id, init, _, rest) =>
        escape(eval(init)); add(id, FlowValue.Unknown); scan(rest)
      case cps.Stmt.Var(id, init, ks, rest) =>
        escape(eval(init)); escape(eval(ks)); add(id, FlowValue.Unknown); scan(rest)
      case cps.Stmt.Dealloc(_, rest) => scan(rest)
      case cps.Stmt.Get(_, id, rest) =>
        add(id, FlowValue.Unknown); scan(rest)
      case cps.Stmt.Put(_, value, rest) => escape(eval(value)); scan(rest)
      case cps.Stmt.Reset(p, ks, k, body, ks1, k1) =>
        List(p, ks, k).foreach(add(_, FlowValue.Unknown))
        escape(eval(ks1)); escape(eval(k1)); scan(body)
      case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        escape(value(prompt))
        List(resume, ks, k).foreach(add(_, FlowValue.Unknown))
        escape(eval(ks1)); escape(eval(k1)); scan(body)
      case cps.Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        escape(value(resumption))
        List(ks, k).foreach(add(_, FlowValue.Unknown))
        escape(eval(ks1)); escape(eval(k1)); scan(body)
      case cps.Stmt.Hole(_) => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, _, _) =>
        add(id, FlowValue.function(id))
      case _: cps.ToplevelDefinition.Val => ()
    }
    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, _) if externalEntries.contains(id) =>
        params.foreach(add(_, FlowValue.Unknown))
      case _: cps.ToplevelDefinition.Def => ()
      case cps.ToplevelDefinition.Val(_, ks, k, _) =>
        List(ks, k).foreach(add(_, FlowValue.Unknown))
    }

    def scanModule(): Unit = {
      observed.clear()
      module.definitions.foreach {
        case cps.ToplevelDefinition.Def(_, _, body) => scan(body)
        case cps.ToplevelDefinition.Val(_, _, _, binding) => scan(binding)
      }
    }

    // Resolve the closed-world value equations before treating an unresolved
    // call as an open-world escape. Otherwise the first traversal would
    // permanently classify every forward reference as unknown. Closing an
    // actually open call can introduce new unknown arguments, so alternate
    // the two monotone phases to a fixed point.
    var openChanged = true
    while openChanged do {
      changed = true
      closeOpenCalls = false
      while changed do {
        changed = false
        scanModule()
      }

      changed = false
      closeOpenCalls = true
      scanModule()
      openChanged = changed
    }

    closeOpenCalls = false
    observed.clear()
    scanModule()

    def targets(statement: cps.Stmt): Option[MethodTargets] =
      Option(observed.get(statement))

    val escapedOperations: Set[Id] = escaped.iterator.flatMap { allocation =>
      operations.iterator.collect {
        case ((owner, _), operation) if owner == allocation => operation.id
      }
    }.toSet

    val cpsOperations: Set[Id] = observed.valuesIterator
      .filter(!_.compositional)
      .flatMap(_.targets)
      .toSet
  }

  def analyze(
    module: cps.ModuleDecl,
    targetFlows: Vector[cps.GuardedEquality.TargetResult],
    requiredCpsEntries: Set[Id]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)

    val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        definitions(id) = Definition(id, params.toVector, body, toplevel = true, None)
      case _: cps.ToplevelDefinition.Val => ()
    }
    targetFlows.foreach(_.localDefinitions.foreach { definition =>
      definitions(definition.id) = Definition(
        definition.id,
        definition.params,
        definition.body,
        toplevel = false,
        parent = None)
    })

    val operationInfos = mutable.LinkedHashMap.empty[(Id, String), OperationInfo]

    def collectOperations(stmt: cps.Stmt): Unit = stmt match {
      case cps.Stmt.Def(_, _, body, rest) =>
        collectOperations(body); collectOperations(rest)
      case cps.Stmt.New(objectId, _, implementations, rest) =>
        implementations.foreach { operation =>
          val id = Id(s"${operation.name.name.name}_operation")
          val info = OperationInfo(objectId, operation.name, id, operation)
          operationInfos(objectId -> operation.name.name.name) = info
          definitions(id) = Definition(
            id, operation.params.toVector, operation.body,
            toplevel = false, parent = None)
          collectOperations(operation.body)
        }
        collectOperations(rest)
      case cps.Stmt.Let(_, _, rest) => collectOperations(rest)
      case cps.Stmt.Call(_, _, _, _, _, rest) => collectOperations(rest)
      case cps.Stmt.Run(_, _, _, _, rest) => collectOperations(rest)
      case cps.Stmt.If(_, thn, els) =>
        collectOperations(thn); collectOperations(els)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => collectOperations(clause.body) }
        default.foreach(collectOperations)
      case cps.Stmt.Region(_, _, rest) => collectOperations(rest)
      case cps.Stmt.Alloc(_, _, _, rest) => collectOperations(rest)
      case cps.Stmt.Var(_, _, _, rest) => collectOperations(rest)
      case cps.Stmt.Dealloc(_, rest) => collectOperations(rest)
      case cps.Stmt.Get(_, _, rest) => collectOperations(rest)
      case cps.Stmt.Put(_, _, rest) => collectOperations(rest)
      case cps.Stmt.Reset(_, _, _, body, _, _) => collectOperations(body)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => collectOperations(body)
      case cps.Stmt.Resume(_, _, _, body, _, _) => collectOperations(body)
      case _: cps.Stmt.App | _: cps.Stmt.Invoke | _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(_, _, body) => collectOperations(body)
      case cps.ToplevelDefinition.Val(_, _, _, binding) => collectOperations(binding)
    }

    val valueFlow = ValueFlow(
      module,
      definitions.toMap,
      operationInfos.toMap,
      targetFlows,
      requiredCpsEntries ++ module.exports)

    // Lexical nesting is the dominance tree for local definitions. A transfer
    // to the same or an enclosing definition can therefore be represented by
    // a labeled continue in one JavaScript activation.
    def recordParents(stmt: cps.Stmt, owner: Option[Id]): Unit = stmt match {
      case cps.Stmt.Def(id, _, body, rest) =>
        definitions.get(id).foreach { definition =>
          definitions(id) = definition.copy(parent = owner)
        }
        recordParents(body, Some(id))
        recordParents(rest, owner)
      case cps.Stmt.New(objectId, _, implementations, rest) =>
        implementations.foreach { operation =>
          val operationId = operationInfos(objectId -> operation.name.name.name).id
          definitions.get(operationId).foreach { definition =>
            definitions(operationId) = definition.copy(parent = owner)
          }
          recordParents(operation.body, Some(operationId))
        }
        recordParents(rest, owner)
      case cps.Stmt.Let(_, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Call(_, _, _, _, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Run(_, _, _, _, rest) => recordParents(rest, owner)
      case cps.Stmt.If(_, thn, els) =>
        recordParents(thn, owner)
        recordParents(els, owner)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) =>
          recordParents(clause.body, owner)
        }
        default.foreach(recordParents(_, owner))
      case cps.Stmt.Region(_, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Alloc(_, _, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Var(_, _, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Dealloc(_, rest) => recordParents(rest, owner)
      case cps.Stmt.Get(_, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Put(_, _, rest) => recordParents(rest, owner)
      case cps.Stmt.Reset(_, _, _, body, _, _) => recordParents(body, None)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => recordParents(body, None)
      case cps.Stmt.Resume(_, _, _, body, _, _) => recordParents(body, None)
      case _: cps.Stmt.App | _: cps.Stmt.Invoke | _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
    }
    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, _, body) => recordParents(body, Some(id))
      case cps.ToplevelDefinition.Val(_, _, _, binding) => recordParents(binding, None)
    }

    val flowed = new IdentityHashMap[cps.Stmt.Call, cps.GuardedEquality.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach { targets =>
      targets.call match {
        case call: cps.Stmt.Call => flowed.put(call, targets)
        case _ => ()
      }
    })

    def returned(
      stmt: cps.Stmt,
      result: Id,
      returnedKs: Id,
      definition: Definition
    ): Boolean = continuationResult(stmt).exists {
      case (_, cps.Expr.Variable(_), None) => false
      case (k, cps.Expr.Variable(value), Some(ks)) =>
        k == definition.k && value == result &&
          (ks == definition.ks || ks == returnedKs)
      case _ => false
    }

    def resolve(call: cps.Stmt.Call): (Set[Id], Boolean) = call.callee match {
      case cps.Callee.Function(callee) =>
        definitions.get(callee) match {
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

      case cps.Callee.Method(_, _) =>
        valueFlow.targets(call).fold(Set.empty[Id] -> false) { result =>
          val compatible = result.targets.nonEmpty && result.targets.forall { id =>
            definitions(id).params.size == call.args.size + 2
          }
          result.targets -> (result.closed && compatible)
        }
    }

    def known(call: cps.Stmt.Call, closed: Boolean): Boolean = call.callee match {
      case cps.Callee.Function(id) => definitions.contains(id)
      case cps.Callee.Method(_, _) => closed
    }

    val sites = mutable.LinkedHashMap.empty[Id, Site]
    val callsByOwner = mutable.LinkedHashMap.empty[Id, Vector[Site]]

    /** Calling a finite-rank direct callee is valid from any computation,
     * including one which itself retains CPS. This traversal records that
     * callee-side judgment independently of the control-erasure proof below.
     * Nested definitions are analyzed under their own owner. */
    def collectSites(stmt: cps.Stmt, owner: Id): Unit = stmt match {
      case cps.Stmt.Def(_, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.New(_, _, _, rest) => collectSites(rest, owner)
      case cps.Stmt.Let(_, _, rest) => collectSites(rest, owner)
      case call @ cps.Stmt.Call(result, returnedKs, _, _, _, rest) =>
        val (targets, closed) = resolve(call)
        val tail = definitions.get(owner).exists { definition =>
          definition.params.size >= 2 && returned(rest, result, returnedKs, definition)
        }
        sites(result) = Site(
          call,
          owner,
          targets,
          closed,
          tail,
          targets == Set(owner) && call.callee == cps.Callee.Function(owner) && tail,
          known = known(call, closed))
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
    val escaping = module.escapes

    def stableMeta(meta: cps.Expr, stableKs: Set[Id]): Boolean = meta match {
      case cps.Expr.Variable(id) => stableKs.contains(id)
      case cps.Expr.Toplevel => true
      case _ => false
    }

    def preservesReturn(
      arguments: List[cps.Expr],
      definition: Definition,
      stableKs: Set[Id]
    ): Boolean = arguments.takeRight(2) match {
      case List(ks, cps.Expr.Variable(k)) =>
        k == definition.k && stableMeta(ks, stableKs)
      case _ => false
    }

    def isAncestor(ancestor: Id, descendant: Id): Boolean = {
      var current = Option(descendant)
      while current.nonEmpty && current.get != ancestor do
        current = definitions.get(current.get).flatMap(_.parent)
      current.contains(ancestor)
    }

    final case class Inspection(calls: Vector[Site], returnBlocks: Set[Id]) {
      def ++(other: Inspection): Inspection =
        Inspection(calls ++ other.calls, returnBlocks ++ other.returnBlocks)
    }

    val emptyInspection = Inspection(Vector.empty, Set.empty)

    def inspect(
      stmt: cps.Stmt,
      definition: Definition,
      stableKs: Set[Id],
      visiting: Set[Id] = Set.empty,
      metaWitness: Boolean = false
    ): Option[Inspection] = stmt match {
      case cps.Stmt.Def(_, _, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.New(_, _, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.Let(_, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)

      case call @ cps.Stmt.Call(result, returnedKs, _, _, ks, rest) =>
        val (targets, closed) = resolve(call)
        val followingKs = ks match {
          case cps.Expr.Variable(id) if stableKs.contains(id) => stableKs + returnedKs
          case _ => stableKs
        }
        val followingWitness = metaWitness || (ks match {
          case cps.Expr.Variable(id) => stableKs.contains(id)
          case cps.Expr.Toplevel => true
          case _ => false
        })
        inspect(rest, definition, followingKs, visiting, followingWitness).map { following =>
          val tail = returned(rest, result, returnedKs, definition)
          following.copy(calls = Site(
            call,
            definition.id,
            targets,
            closed,
            tail,
            targets == Set(definition.id) && call.callee == cps.Callee.Function(definition.id) && tail,
            known = known(call, closed)) +: following.calls)
        }

      case app: cps.Stmt.App if continuationResult(app).exists {
          case (k, _, None) => k == definition.k && metaWitness
          case (k, _, Some(ks)) => k == definition.k && stableKs.contains(ks)
        } => Some(emptyInspection)

      // Parameter dropping can turn a local CPS definition into an ordinary
      // tail-called block which closes over the enclosing continuation. Such
      // a block belongs to the same lexical control region. Revisiting it
      // closes the coinductive proof and denotes a loop, not host recursion.
      case cps.Stmt.App(id, arguments) =>
        definitions.get(id) match {
          case Some(target)
              if !target.toplevel && !escaping.contains(id) &&
                target.params.size == arguments.size &&
                isAncestor(definition.id, id) &&
                (id != definition.id || preservesReturn(arguments, definition, stableKs)) =>
            if visiting.contains(id) then
              Some(emptyInspection.copy(returnBlocks = Set(id)))
            else
              inspect(target.body, definition, stableKs, visiting + id, metaWitness)
                .map(found => found.copy(returnBlocks = found.returnBlocks + id))
          case _ => None
        }

      // Before convention lowering, `Return` means completion of the current
      // CPS computation, not application of this definition's continuation.
      // Treating it as an ordinary function return would change which
      // continuation receives the value.
      case cps.Stmt.Return(_) => None

      case cps.Stmt.Run(_, _, _, cps.Purity.Pure | cps.Purity.Impure, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.If(_, thn, els) =>
        for left <- inspect(thn, definition, stableKs, visiting, metaWitness)
            right <- inspect(els, definition, stableKs, visiting, metaWitness)
        yield left ++ right
      case cps.Stmt.Match(_, clauses, default) =>
        val branches = clauses.map(_._2.body) ++ default
        branches.foldLeft(Option(emptyInspection)) { (found, branch) =>
          for before <- found
              after <- inspect(branch, definition, stableKs, visiting, metaWitness)
          yield before ++ after
        }
      case cps.Stmt.Alloc(_, _, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.Var(id, _, ks, rest)
          if !escaping.contains(id) && stableMeta(ks, stableKs) =>
        inspect(rest, definition, stableKs, visiting, metaWitness = true)
      case cps.Stmt.Dealloc(_, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.Get(_, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)
      case cps.Stmt.Put(_, _, rest) =>
        inspect(rest, definition, stableKs, visiting, metaWitness)

      // Unknown calls and control delimiters cannot synchronously produce the
      // value expected by the direct ABI.
      case _ => None
    }

    val returnBlocksByOwner = mutable.LinkedHashMap.empty[Id, Set[Id]]
    val controlErasable = definitions.valuesIterator.flatMap { definition =>
      Option.when(definition.params.size >= 2) {
        inspect(definition.body, definition, Set(definition.ks)).map { result =>
          callsByOwner(definition.id) = result.calls
          returnBlocksByOwner(definition.id) = result.returnBlocks
          definition.id
        }
      }.flatten
    }.toSet
    val erasable = controlErasable --
      valueFlow.escapedOperations -- valueFlow.cpsOperations

    val incoming = sites.valuesIterator.toVector
      .flatMap(site => site.targets.iterator.map(_ -> site))
      .groupMap(_._1)(_._2)

    def backEdge(site: Site, target: Id): Boolean =
      site.tail && isAncestor(target, site.owner)

    // A local definition is a structured header when every incoming edge
    // enters the same lexical definition and every back edge comes from that
    // definition or one of its descendants. Reducibility requires one entry
    // *node*, not one incoming edge: sibling branches can both enter the same
    // loop header.
    val syntacticRegions = erasable.filter { id =>
      val definition = definitions(id)
      val entries = incoming.getOrElse(id, Vector.empty)
      val exact = entries.forall(site =>
        site.closed && site.known && site.targets == Set(id) &&
          site.call.callee == cps.Callee.Function(id))
      val forward = entries.filterNot(backEdge(_, id))
      !definition.toplevel && !module.escapes.contains(id) && exact &&
        forward.nonEmpty && forward.forall(site => definition.parent.contains(site.owner))
    }

    @tailrec def closeRegions(current: Set[Id]): Set[Id] = {
      val updated = current.filter { id =>
        incoming.getOrElse(id, Vector.empty).forall { site =>
          !backEdge(site, id) || site.owner == id || current.contains(site.owner)
        }
      }
      if updated == current then current else closeRegions(updated)
    }
    val structuredRegions = closeRegions(syntacticRegions)

    // A unique forward edge admits direct-style substitution at that call
    // site: its compositional remainder is the unique return point of the
    // value-returning body. With several forward edges, the CPS continuation
    // parameter is precisely what distinguishes the several remainders.
    val inlineJoins = structuredRegions.filter { id =>
      incoming.getOrElse(id, Vector.empty).count(!backEdge(_, id)) == 1
    }

    // Several forward edges can share the lexical body exactly when they are
    // tail transfers from their common parent. Their remainders are then the
    // parent's return, rather than distinct continuations that would need to
    // be represented at runtime.
    val sharedJoinCandidates = (structuredRegions -- inlineJoins).filter { id =>
      incoming.getOrElse(id, Vector.empty)
        .filterNot(backEdge(_, id))
        .forall(_.tail)
    }

    // A shared label lives in its parent's JavaScript activation. If that
    // parent retains CPS, an intervening compositional call may reify its
    // remainder as a closure; such a closure cannot jump to the label.
    def selectedJoins(direct: Set[Id]): Set[Id] =
      inlineJoins.intersect(direct) ++ sharedJoinCandidates.filter { id =>
        definitions(id).parent.exists(direct.contains)
      }.intersect(direct)

    /** Nodes on a cycle with positive total cost.
      *
      * Zero-cost edges must remain in the graph while computing components:
      * a cycle may contain both zero- and positive-cost edges. Such a mixed
      * cycle still has unbounded stack cost. An SCC is rejected precisely when
      * it contains an internal positive edge; every internal edge of an SCC
      * lies on a cycle.
      */
    def cyclic(nodes: Set[Id], zero: Site => Boolean): Set[Id] = {
      val index = mutable.Map.empty[Id, Int]
      val lowlink = mutable.Map.empty[Id, Int]
      val stack = mutable.ArrayBuffer.empty[Id]
      val onStack = mutable.Set.empty[Id]
      val result = mutable.Set.empty[Id]
      var next = 0

      def edges(id: Id): Iterator[(Site, Id)] =
        callsByOwner.getOrElse(id, Vector.empty).iterator
          .flatMap(site => site.targets.iterator.map(site -> _))
          .filter { case (_, target) => nodes.contains(target) }

      def successors(id: Id): Iterator[Id] = edges(id).map(_._2)

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
          val members = component.toSet
          val selfCycle = component.size == 1 && successors(component.head).contains(component.head)
          val positive = component.exists { source =>
            edges(source).exists { case (site, target) => members(target) && !zero(site) }
          }
          if (component.size > 1 || selfCycle) && positive then result ++= component
        }
      }

      nodes.foreach(id => if !index.contains(id) then connect(id))
      result.toSet
    }

    /** Candidate entries for a closed local continuation machine. Its one
     *  forward edge supplies the return continuation; recursive edges create
     *  the finite continuation domain handled by the local dispatcher. */
    val machineCandidates = erasable.filter { id =>
      val definition = definitions(id)
      val entries = incoming.getOrElse(id, Vector.empty)
      val exact = entries.forall(site =>
        site.closed && site.known && site.targets == Set(id) &&
          site.call.callee == cps.Callee.Function(id))
      val external = entries.filterNot(site => isAncestor(id, site.owner))
      !definition.toplevel && !module.escapes.contains(id) && exact &&
        external.size == 1 && isAncestor(external.head.owner, id)
    }.intersect(cyclic(erasable, _.tailSelf))

    /** A machine is closed when every compositional call in its body is
     *  either recursive, or enters a statically known native computation.
     *  In particular, no continuation frame crosses an unknown CPS call. */
    def machines(native: Set[Id]): Set[Id] = machineCandidates.filter { id =>
      callsByOwner.getOrElse(id, Vector.empty).forall { site =>
        site.closed && site.known && site.targets.nonEmpty &&
          (site.targets == Set(id) || site.targets.subsetOf(native))
      }
    }

    def isMachine(site: Site, machines: Set[Id]): Boolean =
      site.closed && site.targets.nonEmpty &&
        site.targets.subsetOf(machines) &&
        site.targets.forall(target =>
          target != site.owner && isAncestor(site.owner, target))

    // Every selected definition is a structured region. Cyclic components
    // need loops; acyclic components need only labeled blocks.
    val operationIds = operationInfos.valuesIterator.map(_.id).toSet
    val methodSites = sites.valuesIterator.filter(_.call.callee match {
      case cps.Callee.Method(_, _) => true
      case cps.Callee.Function(_) => false
    }).toVector

    @tailrec def close(current: Set[Id]): Set[Id] = {
      val localMachines = machines(current)
      // An object exposes one property per operation, hence one ABI. If a
      // dynamic method site can select both direct and CPS implementations,
      // every implementation at that site must retain the CPS convention.
      val incompatibleOperations = methodSites.iterator.flatMap { site =>
        val targets = site.targets.intersect(operationIds)
        Option.when(targets.exists(current) &&
          (!site.closed || !targets.subsetOf(current)))(targets.intersect(current))
      }.flatten.toSet

      val updated = (current -- incompatibleOperations).filter { id =>
        callsByOwner.getOrElse(id, Vector.empty).forall { site =>
          site.closed && site.targets.nonEmpty &&
            (site.targets.subsetOf(current) || isMachine(site, localMachines))
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
            site.call.callee.function.flatMap(parameterIndex.get).foreach { position =>
              changed = require(owner, position, site.call.args.size) || changed
            }

            val byPosition = site.targets.iterator
              .flatMap(target => requirements.getOrElse(target, Map.empty))
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
      val joins = selectedJoins(controlClosed)
      val updated = close(controlClosed -- cyclic(controlClosed, site =>
        site.tailSelf || site.tail && site.targets.nonEmpty && site.targets.subsetOf(joins)))
      requirements = nextRequirements.view.filterKeys(updated.contains).toMap
      stable = updated == direct
      direct = updated
    }

    /** Admissibility alone does not choose a calling convention. A local
     * definition needs the direct ABI only when some direct entry reaches it:
     *
     *   - a syntactically known call can enter it from either convention;
     *   - an indirect call can enter it directly only from a definition that
     *     has itself selected the direct ABI.
     *
     * Toplevel definitions are observable entries and therefore roots. The
     * least closure below is the demand counterpart of the greatest control-
     * closed solution above. In particular, merely flowing a function into an
     * indirect call in CPS code creates no direct worker plus CPS adapter.
     */
    def eligible(site: Site, candidates: Set[Id]): Boolean =
      site.closed && site.targets.nonEmpty && site.targets.subsetOf(candidates)

    val directRoots = toplevel.intersect(direct) ++
      sites.valuesIterator
        .filter(site => site.known && eligible(site, direct))
        .flatMap(_.targets)
        .toSet

    @tailrec def closeDemand(demanded: Set[Id]): Set[Id] = {
      val reached = demanded.iterator.flatMap { owner =>
        callsByOwner.getOrElse(owner, Vector.empty).iterator
          .filter(site => eligible(site, direct))
          .flatMap(_.targets)
      }.toSet
      val updated = demanded ++ reached
      if updated == demanded then demanded else closeDemand(updated)
    }

    direct = direct.intersect(closeDemand(directRoots))

    // Demand can remove the parent that made a shared lexical entry valid.
    // Re-establish control closure and acyclicity under the remaining joins;
    // this loop only removes definitions and therefore terminates.
    stable = false
    while !stable do {
      val controlClosed = close(direct)
      val joins = selectedJoins(controlClosed)
      val updated = close(controlClosed -- cyclic(controlClosed, site =>
        site.tailSelf || site.tail && site.targets.nonEmpty && site.targets.subsetOf(joins)))
      stable = updated == direct
      direct = updated
    }

    val (demandedRequirements, invalidDemanded) = parameterRequirements(direct)
    assert(invalidDemanded.isEmpty)
    requirements = demandedRequirements

    val joins = selectedJoins(direct)
    val shared = sharedJoinCandidates.intersect(joins)
    val joinLoops = joins.filter(id =>
      incoming.getOrElse(id, Vector.empty).exists(backEdge(_, id)))

    val native = direct
    val edges = native.iterator.map { source =>
      val targets = callsByOwner.getOrElse(source, Vector.empty).iterator
        .filterNot(site => site.tailSelf ||
          site.targets.nonEmpty && site.targets.subsetOf(joins))
        .flatMap(_.targets).filter(direct).toSet
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

      case call @ cps.Stmt.Call(_, _, callee, arguments, ks, rest) =>
        val selected = sites.get(call.id).exists { site =>
          eligible(site, direct) && (site.known || direct.contains(site.owner))
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
        val calleeEntry = if emittedDirect then Set.empty else cpsCallee(callee.value)
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

    val cpsEntries = requiredCpsEntries.intersect(direct) ++ definitions.valuesIterator
      .flatMap(definition => cpsReferences(definition.body, Some(definition.id)))
      .toSet
    val originals = direct.iterator.map { id =>
      val definition = definitions(id)
      id -> OriginalDefinition(definition.params.toList)
    }.toMap
    val localMachines = machines(direct)
    val machineSites = sites.valuesIterator
      .filter(site => direct.contains(site.owner) && isMachine(site, localMachines))
      .map(_.call.id)
      .toSet
    val inheritedReturns = direct.iterator
      .flatMap(id => returnBlocksByOwner.getOrElse(id, Set.empty))
      .filterNot(direct)
      .toSet

    val plan = Plan(
      ranks.toMap,
      requirements,
      cpsEntries,
      originals,
      sites.toMap,
      machineSites,
      joins,
      shared,
      joinLoops,
      inheritedReturns,
      operationInfos.valuesIterator.map { operation =>
        (operation.objectId -> operation.method.name.name) -> operation.id
      }.toMap,
      operationInfos.valuesIterator.map { operation =>
        operation.id -> s"${operation.objectId.name.name}.${operation.method.name.name}"
      }.toMap)
    plan.validate()
    plan
  }
}
