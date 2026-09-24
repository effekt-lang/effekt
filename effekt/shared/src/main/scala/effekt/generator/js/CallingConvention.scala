package effekt
package generator
package js

import effekt.core.Id
import effekt.cps
import effekt.cps.knownArguments

import java.util.IdentityHashMap
import scala.annotation.tailrec
import scala.collection.mutable

/** Recovers direct style from CPS where no control operator is required.
 *
 * Iterated CPS makes both the continuation `k` and the meta-continuation `ks`
 * explicit. A uniform JavaScript translation would consequently represent
 * ordinary functions, continuations, and join points by functions and calls,
 * even when structured control flow suffices.
 *
 * In the terminology of ''Back to Direct Style: Typed and Tight'', recovering
 * structured control amounts to synthesizing a ''continuation output''. A CPS
 * term has output `k` if it returns to the stack represented by `k`. A
 * definition is pure when its body has its continuation parameter as output;
 * that parameter can then be replaced by an ordinary return:
 *
 * {{{
 * def inc(x, ks, k) {                 def inc(x) {
 *   run y = add(x, 1)                   run y = add(x, 1)
 *   k(y, ks)                            return(y)
 * }                                   }
 * let y = inc!(41, ks, return)        let y = inc!(41)
 * }}}
 *
 * The paper gives a total translation: it inserts `suspend` and `run` when a
 * term has a nontrivial continuation output. This pass does not attempt such
 * a translation. It uses the continuation output only to identify a direct
 * fragment and retains CPS everywhere else. The result is therefore a mixed
 * CPS/direct-style program. In the direct fragment, the judgment proves that
 * `ks` and `k` can be erased.
 *
 * Effekt's CPS language adds two details to this judgment. First, every
 * return must preserve the meta-continuation `ks`. Second, all returning paths
 * must agree on their number of values. [[ResultArity]] records this number;
 * a computation without a returning path is compatible with every arity.
 * JavaScript represents zero results by `undefined`, one result by the value
 * itself, and several results by an array.
 *
 * == Higher-order control flow ==
 *
 * The local judgment above depends on the calling convention of callees. For
 * example, `f` can return directly only if its call to `g` does:
 *
 * {{{
 * def f(x, ks, k) {
 *   let y = g!(x, ks, return)
 *   k(y, ks)
 * }
 * def g(x, ks, k) { unknown(x, ks, k) }
 * }}}
 *
 * The open call to `unknown` prevents `g` from returning directly, which in
 * turn prevents `f` from doing so. Starting with all locally admissible
 * definitions and repeatedly removing definitions such as `g` and then `f`
 * computes the greatest set closed under calls. For an indirect call, the
 * finite target sets from `cps.Targets` play the role of the known callee:
 * every target must be in this set and must agree on the [[FunctionSignature]]
 * of each function-valued parameter. Method calls impose the same condition
 * on their possible operation implementations.
 *
 * == Stack safety ==
 *
 * A direct-style translation also has to respect the JavaScript stack. A
 * self-tail call implemented by a loop and a transfer implemented by a
 * lexical jump have cost zero; every other direct call has cost one. A
 * positive recursive component remains in CPS. The longest-path `rank` of
 * each remaining definition witnesses a finite bound on its stack use. A
 * closed local component may likewise remain in CPS behind an immutable
 * continuation dispatcher; its unique entry continuation is then the return
 * case of that dispatcher.
 *
 * == Direct and CPS entries ==
 *
 * When a definition is used through both calling conventions, the backend
 * emits two versions. The direct version contains the implementation; the
 * CPS version is an adapter that invokes it and forwards its result:
 *
 * {{{
 * function inc(x) { return x + 1 }
 * function inc_cps(x, ks, k) { return k(inc(x), ks) }
 * }}}
 *
 * Direct calls use `inc`; CPS calls and CPS function values use `inc_cps`.
 * Thus the two versions do not duplicate the body.
 *
 * == Demanded entries ==
 *
 * Continuation-output and stack-safety analysis determine which definitions
 * admit a direct version. They do not determine which version is needed.
 * Consider a locally defined `inc` that is only passed to an unknown CPS
 * function:
 *
 * {{{
 * def main(use, x, ks, k) {
 *   def inc(y, ks1, k1) { k1(y + 1, ks1) }
 *   use(inc, x, ks, k)
 * }
 * }}}
 *
 * Although `inc` admits the direct convention, this program needs only its
 * CPS version. If `main` also contains `let y = inc!(x, ks, return)`, that
 * call demands the direct version; because `inc` is still passed to `use`,
 * both versions are now needed. Starting with toplevel definitions, direct
 * calls, and direct higher-order arguments, a least reachability fixed point
 * retains exactly the demanded direct versions. The CPS version is retained
 * independently for definitions observed through the CPS ABI.
 *
 * == Analysis result ==
 *
 * Let `A` be the greatest set satisfying continuation-output preservation,
 * higher-order calling-convention coherence, and finite stack use. Let `R` be
 * the least subset of `A` containing the demand roots and closed under direct
 * calls. Restricting to `R` can invalidate a lexical join, so the final set
 * `D` is the greatest admissible subset of `R`. [[analyze]] records the
 * following finite judgments in a [[Plan]]:
 *
 *   - for every `d` in `D`, its result arity, stack rank, and direct
 *     higher-order parameter signatures;
 *   - the subsets of `D` requiring a separately named direct entry or a CPS
 *     adapter;
 *   - for every compositional call `c`, a [[ReturnConvention]] `return(c)`.
 *
 * `Direct` calls a value-returning implementation, `Join` jumps to a lexical
 * block, `Machine` enters a local continuation dispatcher, and `CPS` retains
 * the continuation. All later decisions about arguments, adapters, and calls
 * use these judgments.
 *
 * == Specialization and code generation ==
 *
 * The backend consumes the plan in the following phases:
 *
 *   - [[specialize]] makes calling conventions explicit in CPS. Direct
 *     definitions omit `ks` and `k`, applications of their return
 *     continuation become `Return`, direct compositional calls use
 *     `ReturnPoint.Direct`, and tail self-calls use `ReturnPoint.Jump`.
 *
 *   - `BlockSinking` and `StaticArguments` simplify the specialized CPS.
 *     They localize definitions introduced by specialization and eliminate
 *     meta-continuations that have become static.
 *
 *   - [[DefinitionPlanning]] chooses the representation of each local
 *     definition. A first-class definition remains a JavaScript function; a
 *     second-class definition becomes a labeled block or loop. A closed
 *     continuation family becomes immutable tagged frames and a local
 *     dispatch loop.
 *
 *   - [[StackSafety]] classifies each residual transfer. A `Jump` becomes a
 *     `break` or `continue`; a `Direct` transfer calls its worker immediately;
 *     a `Bounce` returns a thunk such as `() => f(x)`; and a `Safe` transfer
 *     calls the stack-safe entry exposed as a function value.
 *
 *   - [[TransformerCps]] emits JavaScript by structural recursion over the
 *     specialized CPS tree. It emits entries according to the calling-
 *     convention plan, local definitions and dispatchers according to
 *     `DefinitionPlanning`, direct calls according to their explicit return
 *     point, and residual transfers according to `StackSafety.Transfer`. It
 *     does not reconstruct any of these judgments.
 */
object CallingConvention {

  private def binding(call: cps.Stmt.Call): cps.ReturnPoint.Bind =
    call.returnsTo match {
      case bind: cps.ReturnPoint.Bind => bind
      case _: cps.ReturnPoint.Tail | _: cps.ReturnPoint.Direct | cps.ReturnPoint.Jump =>
        sys.error("Expected a compositional call")
    }

  private def jump(callee: cps.Callee, arguments: List[cps.Expr]): cps.Stmt =
    cps.Stmt.Call(callee, arguments, cps.ReturnPoint.Jump)

  /** Whether a compositional remainder merely forwards all call results. */
  private def forwards(
    stmt: cps.Stmt,
    results: List[Id],
    returnedKs: Id,
    ks: cps.Expr
  ): Option[Id] = stmt match {
    case cps.Stmt.Call(cps.Callee.Function(k), arguments, cps.ReturnPoint.Jump) =>
      val values = results.map(cps.Expr.Variable.apply)
      Option.when(
        arguments == values :+ cps.Expr.Variable(returnedKs) ||
          arguments == values :+ ks)(k)
    case _ => None
  }

  final case class OriginalDefinition(params: List[Id])

  /** The value-level ABI of a function parameter. */
  final case class FunctionSignature(arguments: Int, results: Int)

  /** Result arities form the flat lattice
   *
   *                 Conflict
   *                /   |   \
   *           Exact(0) ... Exact(n)
   *                \   |   /
   *                  Never
   *
   * `Never` denotes a computation with no returning path. It is compatible
   * with every result ABI; distinct returning arities join to `Conflict`.
   */
  enum ResultArity {
    case Never
    case Exact(size: Int)
    case Conflict

    def join(other: ResultArity): ResultArity = (this, other) match {
      case (Never, result) => result
      case (result, Never) => result
      case (Exact(left), Exact(right)) if left == right => this
      case _ => Conflict
    }

    def exact: Option[Int] = this match {
      case Exact(size) => Some(size)
      case _ => None
    }

    def accepts(size: Int): Boolean = this match {
      case Never => true
      case Exact(resultSize) => resultSize == size
      case Conflict => false
    }
  }

  /** How a compositional application realizes its remainder. */
  private enum ReturnConvention {
    case CPS, Direct, Join, Machine
  }

  private def returnParameters(plan: Plan, id: Id, params: List[Id]): Option[(Id, Id)] =
    Option.when(plan.isDirectEntry(id) && params.size >= 2)(
      params(params.size - 2) -> params.last)

  /** Specialize one lexical computation according to the selected convention. */
  private def specializeStatement(
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
          specializeStatement(body, bodyReturns, bodyIsDirect, plan),
          specializeStatement(rest, returns, directBody, plan))
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
              body = specializeStatement(operation.body, returns, direct, plan))
          },
          specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Let(id, binding, rest) =>
        cps.Stmt.Let(id, binding, specializeStatement(rest, returns, directBody, plan))

      case call @ cps.Stmt.Call(callee, arguments,
          cps.ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
        plan.returnConvention(call) match {
          // A shared join is a lexical labeled region.
          case ReturnConvention.Join => jump(callee, arguments)

          case ReturnConvention.Direct =>
            val directRest = cps.substitutions.substitute(rest)(using
              cps.substitutions.Substitution(Map(returnedKs -> ks)))
            if plan.isTailSelf(call) then jump(callee, arguments)
            else cps.Stmt.Call(callee, arguments,
              cps.ReturnPoint.Direct(
                results, specializeStatement(directRest, returns, directBody, plan)))

          // A positive recursive local region retains CPS internally. Its
          // entry continuation becomes the return case of its dispatcher.
          case ReturnConvention.Machine =>
            val continuation = Id("k")
            val directRest = cps.substitutions.substitute(rest)(using
              cps.substitutions.Substitution(Map(returnedKs -> cps.Expr.Toplevel)))
            cps.Stmt.Def(
              continuation,
              results :+ returnedKs,
              specializeStatement(directRest, returns, directBody, plan),
              jump(callee, arguments ++ List(
                cps.Expr.Toplevel,
                cps.Expr.Variable(continuation))))

          // A direct computation runs a residual CPS computation to completion.
          case ReturnConvention.CPS if directBody =>
            val directRest = cps.substitutions.substitute(rest)(using
              cps.substitutions.Substitution(Map(returnedKs -> cps.Expr.Toplevel)))
            cps.Stmt.Call(callee, arguments,
              cps.ReturnPoint.Bind(
                results, returnedKs, cps.Expr.Toplevel,
                specializeStatement(directRest, returns, directBody, plan)))

          // Otherwise reify the remainder, except for an already-tail call.
          case ReturnConvention.CPS =>
            forwards(rest, results, returnedKs, ks) match {
              case Some(k) =>
                jump(callee, arguments ++ List(ks, cps.Expr.Variable(k)))
              case None =>
                val continuation = Id("k")
                cps.Stmt.Def(
                  continuation,
                  results :+ returnedKs,
                  specializeStatement(rest, returns, directBody, plan),
                  jump(callee, arguments ++ List(ks, cps.Expr.Variable(continuation))))
            }
        }

      case call @ cps.Stmt.Call(_, _, cps.ReturnPoint.Tail(_, _)) => call

      case cps.Stmt.Call(callee, arguments, cps.ReturnPoint.Direct(results, rest)) =>
        cps.Stmt.Call(callee, arguments,
          cps.ReturnPoint.Direct(
            results, specializeStatement(rest, returns, directBody, plan)))

      case jump @ cps.Stmt.Call(cps.Callee.Function(id), arguments,
          cps.ReturnPoint.Jump) =>
        val result = for
          (ks, k) <- returns
          if id == k
        yield cps.Stmt.Return(arguments.lastOption match {
          case Some(cps.Expr.Variable(meta)) if meta == ks => arguments.dropRight(1)
          case _ => arguments
        })
        result.getOrElse(jump)
      case jump @ cps.Stmt.Call(_, _, cps.ReturnPoint.Jump) => jump
      case returned: cps.Stmt.Return => returned
      case cps.Stmt.Run(id, callee, arguments, purity, rest) =>
        cps.Stmt.Run(
          id, callee, arguments, purity,
          specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.If(condition, thn, els) =>
        cps.Stmt.If(
          condition,
          specializeStatement(thn, returns, directBody, plan),
          specializeStatement(els, returns, directBody, plan))
      case cps.Stmt.Match(scrutinee, clauses, default) =>
        cps.Stmt.Match(
          scrutinee,
          clauses.map { case (tag, clause) =>
            tag -> clause.copy(
              body = specializeStatement(clause.body, returns, directBody, plan))
          },
          default.map(specializeStatement(_, returns, directBody, plan)))
      case cps.Stmt.Region(id, ks, rest) =>
        cps.Stmt.Region(id, ks, specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Alloc(id, init, region, rest) =>
        cps.Stmt.Alloc(
          id, init, region,
          specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Var(id, init, ks, rest) =>
        // A selected direct definition can contain only local variables whose
        // reference and meta-continuation dependency were proved erasable.
        // Make that erasure explicit so the lowered body does not retain the
        // removed `ks` binder as a free variable.
        val loweredKs = if directBody then cps.Expr.Toplevel else ks
        cps.Stmt.Var(id, init, loweredKs,
          specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Dealloc(ref, rest) =>
        cps.Stmt.Dealloc(ref, specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Get(ref, id, rest) =>
        cps.Stmt.Get(ref, id, specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Put(ref, value, rest) =>
        cps.Stmt.Put(ref, value, specializeStatement(rest, returns, directBody, plan))
      case cps.Stmt.Reset(p, ks, k, body, ks1, k1) =>
        cps.Stmt.Reset(
          p, ks, k,
          specializeStatement(body, None, false, plan),
          ks1, k1)
      case cps.Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        cps.Stmt.Shift(
          prompt, resume, ks, k,
          specializeStatement(body, None, false, plan),
          ks1, k1)
      case cps.Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        cps.Stmt.Resume(
          resumption, ks, k,
          specializeStatement(body, None, false, plan),
          ks1, k1)
      case hole: cps.Stmt.Hole => hole
  }

  /** Reify only the candidate remainders rejected by the plan. Selected calls
   *  stay compositional, and terminal applications of a selected definition's
   *  continuation become explicit `Return` statements. */
  def specialize(module: cps.ModuleDecl, plan: Plan): cps.ModuleDecl = {
    module.copy(definitions = module.definitions.map {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        val directParams =
          if plan.isDirect(id) then params.dropRight(2)
          else params
        cps.ToplevelDefinition.Def(
          id,
          directParams,
          specializeStatement(
            body,
            returnParameters(plan, id, params),
            plan.isDirect(id),
            plan))
      case cps.ToplevelDefinition.Val(id, ks, k, binding) =>
        cps.ToplevelDefinition.Val(
          id, ks, k,
          specializeStatement(binding, None, false, plan))
    })
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

  private enum Representation {
    case Function(loop: Boolean)
    case Join(shared: Boolean, loop: Boolean)

    /** Whether this definition is represented by structured control rather
     *  than by a JavaScript function. */
    def isJoin: Boolean = this match {
      case Join(_, _) => true
      case Function(_) => false
    }

    /** Whether the representation repeats, as a loop back edge. */
    def loops: Boolean = this match {
      case Function(loop) => loop
      case Join(_, loop) => loop
    }
  }

  private final case class Direct(
    rank: Int,
    results: ResultArity,
    needsCpsEntry: Boolean,
    representation: Representation
  )

  private final case class EntryPlan(
    original: OriginalDefinition,
    parameters: Map[Int, FunctionSignature],
    direct: Option[Direct],
    inheritsReturn: Boolean,
    mutableParameters: Set[Id],
    needsDirectWorker: Boolean
  )

  private final case class ApplicationPlan(
    site: Site,
    returns: ReturnConvention
  )

  final class Plan private[CallingConvention] (
    private val entries: Map[Id, EntryPlan],
    private val applications: Map[Id, ApplicationPlan],
    private val cpsArgumentSignatures: Map[cps.Callee, Map[Int, FunctionSignature]],
    private val operations: Map[(Id, String), Id],
    private val operationNames: Map[Id, String]
  ) {
    val ranks: Map[Id, Int] = entries.collect {
      case (id, EntryPlan(_, _, Some(direct), _, _, _)) => id -> direct.rank
    }
    val parameterSignatures: Map[Id, Map[Int, FunctionSignature]] =
      entries.view.mapValues(_.parameters).filter(_._2.nonEmpty).toMap
    val resultArities: Map[Id, ResultArity] = entries.collect {
      case (id, EntryPlan(_, _, Some(direct), _, _, _)) => id -> direct.results
    }

    private val operationIds = operations.values.toSet
    val directDefinitions: Set[Id] = ranks.keySet -- operationIds
    val directOperations: Set[Id] = ranks.keySet.intersect(operationIds)
    val joinDefinitions: Set[Id] = entries.collect {
      case (id, EntryPlan(_, _, Some(Direct(_, _, _, Representation.Join(_, _))), _, _, _)) => id
    }.toSet
    val sharedJoinDefinitions: Set[Id] = entries.collect {
      case (id, EntryPlan(_, _, Some(Direct(_, _, _, Representation.Join(true, _))), _, _, _)) => id
    }.toSet

    def isDirect(id: Id): Boolean = directDefinitions.contains(id)

    private[CallingConvention] def isDirectEntry(id: Id): Boolean =
      entries.get(id).exists(_.direct.nonEmpty)

    private def representation(id: Id): Option[Representation] =
      entries.get(id).flatMap(_.direct).map(_.representation)

    def isDirectOperation(objectId: Id, method: Id): Boolean =
      operations.get(objectId -> method.name.name).exists(directOperations.contains)

    def directOperation(objectId: Id, method: Id): Option[Id] =
      operations.get(objectId -> method.name.name).filter(directOperations.contains)

    private[js] def operationId(objectId: Id, method: Id): Option[Id] =
      operations.get(objectId -> method.name.name)

    def needsCpsEntry(id: Id): Boolean =
      entries(id).direct.exists(_.needsCpsEntry)

    def original(id: Id): OriginalDefinition = entries(id).original

    def cpsArguments(callee: cps.Callee): Map[Int, FunctionSignature] =
      cpsArgumentSignatures.getOrElse(callee, Map.empty)

    /** A call implemented by a closed local continuation machine rather than
     *  by nested JavaScript calls. */
    def isMachine(call: cps.Stmt.Call): Boolean =
      returnConvention(call) == ReturnConvention.Machine

    private def application(call: cps.Stmt.Call): Option[ApplicationPlan] =
      applications.get(binding(call).returnedKs)

    /** The function-valued arguments of this call and their direct signatures.
     *  All possible targets have the same map; this is precisely the ABI
     *  coherence condition for an indirect call. */
    def directArguments(targets: Set[Id]): Map[Int, FunctionSignature] =
      targets.headOption
        .flatMap(entries.get)
        .fold(Map.empty[Int, FunctionSignature])(_.parameters)

    def directParameterSignature(id: Id, position: Int): Option[FunctionSignature] =
      entries.get(id).flatMap(_.parameters.get(position))

    def resultArity(id: Id): ResultArity = resultArities(id)

    def isTailSelf(call: cps.Stmt.Call): Boolean =
      application(call).exists(_.site.tailSelf)

    /** A selected local definition represented by structured control rather
     *  than by a JavaScript function. */
    def isJoin(id: Id): Boolean = joinDefinitions.contains(id)

    /** A join with several forward edges is materialized at its lexical
     * definition, so every edge can jump to the one shared body. */
    def isSharedJoin(id: Id): Boolean = sharedJoinDefinitions.contains(id)

    private[CallingConvention] def returnConvention(call: cps.Stmt.Call): ReturnConvention =
      application(call).fold(ReturnConvention.CPS)(_.returns)

    def isJoinLoop(id: Id): Boolean =
      representation(id).exists(r => r.isJoin && r.loops)

    /** A parameter-dropped local block executes in its enclosing direct
     * definition and therefore shares that definition's return convention. */
    def inheritsReturn(id: Id): Boolean =
      entries.get(id).exists(_.inheritsReturn)

    def isTailRecursive(id: Id): Boolean =
      representation(id).exists(r => !r.isJoin && r.loops)

    /** Parameters whose direct loop registers can receive a different value
     *  on a tail-self back edge. */
    def mutableParameters(id: Id): Set[Id] =
      entries.get(id).fold(Set.empty[Id])(_.mutableParameters)

    /** Whether a selected call enters this definition's value-returning
     *  implementation. Tail self calls become loop back-edges and therefore
     *  do not require a separately named worker. */
    def needsDirectWorker(id: Id): Boolean =
      entries.get(id).exists(_.needsDirectWorker)

    def validate(): Unit = {
      assert(resultArities.keySet == ranks.keySet)
      assert(resultArities.valuesIterator.forall(_ != ResultArity.Conflict))
      val applicationsByOwner = applications.values.groupBy(_.site.owner)
      ranks.keysIterator.foreach { source =>
        applicationsByOwner.getOrElse(source, Nil)
          .foreach { application =>
            val site = application.site
            assert(site.closed && site.targets.nonEmpty)
            if application.returns != ReturnConvention.Machine then {
              assert(site.targets.forall(ranks.contains))
              assert(site.targets.iterator
                .map(id => entries(id).parameters)
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
      val lines = ranks.keysIterator.toVector
        .sortBy(id => (id.name.name, id.id))
        .map { id =>
          val entry = entries(id)
          val direct = entry.parameters.keySet.toVector.sorted
          val arguments = if direct.isEmpty then "" else s" [direct: ${direct.mkString(", ")}]"
          val adapter = if entry.direct.exists(_.needsCpsEntry) then " adapter" else ""
          val label = operationNames.getOrElse(id, id.name.name)
          s"  $label = ${entry.direct.get.rank}$arguments$adapter"
        }
      if lines.isEmpty then "-" else s"direct\n${lines.mkString("\n")}"
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
    targetFlows: Vector[cps.Targets.TargetResult],
    externalEntries: Set[Id]
  ) {
    private val functionTargets = new IdentityHashMap[cps.Stmt, cps.Targets.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach { targets =>
      functionTargets.put(targets.call, targets)
    })

    private val bound = mutable.Set.empty[Id]
    functions.valuesIterator.foreach(definition => bound ++= definition.params)

    private val values = mutable.Map.empty[Id, FlowValue]
      .withDefaultValue(FlowValue.Empty)
    private val escaped = mutable.Set.empty[Id]
    private val predecessors = mutable.Map.empty[Id, mutable.Set[Id]]
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
          arguments.iterator.zip(definition.params.iterator).foreach {
            case (argument, parameter) =>
              add(parameter, eval(argument))
              argument match {
                case cps.Expr.Variable(id) => predecessors.getOrElseUpdate(parameter, mutable.Set.empty) += id
                case _ => ()
              }
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
        binding match {
          case cps.Expr.Variable(source) => predecessors.getOrElseUpdate(id, mutable.Set.empty) += source
          case _ => ()
        }
        scan(rest)

      case call @ cps.Stmt.Call(cps.Callee.Function(callee), arguments,
          cps.ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
        val supplied = arguments ++ List(ks, cps.Expr.Abort)
        val (targets, closed) = resolveFunction(call, callee, supplied.size)
        observed.put(call, MethodTargets(targets, closed, compositional = true))
        propagate(supplied, targets)
        if closeOpenCalls && !closed then
          supplied.foreach(argument => escape(eval(argument)))
        results.foreach(add(_, FlowValue.Unknown))
        add(returnedKs, FlowValue.Unknown)
        scan(rest)

      case call @ cps.Stmt.Call(cps.Callee.Method(receiver, method), arguments,
          cps.ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
        val supplied = arguments ++ List(ks, cps.Expr.Abort)
        val targets = record(call, receiver, method, compositional = true)
        propagate(supplied, targets.targets)
        if closeOpenCalls && !targets.closed then
          supplied.foreach(argument => escape(eval(argument)))
        results.foreach(add(_, FlowValue.Unknown))
        add(returnedKs, FlowValue.Unknown)
        scan(rest)

      case call @ cps.Stmt.Call(cps.Callee.Function(callee), _,
          _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
        val supplied = call.knownArguments
        val (targets, closed) = resolveFunction(call, callee, supplied.size)
        observed.put(call, MethodTargets(targets, closed, compositional = false))
        propagate(supplied, targets)
        if closeOpenCalls && !closed then
          supplied.foreach(argument => escape(eval(argument)))

      case call @ cps.Stmt.Call(cps.Callee.Method(receiver, method), _,
          _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
        val supplied = call.knownArguments
        val targets = record(call, receiver, method, compositional = false)
        propagate(supplied, targets.targets)
        if closeOpenCalls && !targets.closed then
          supplied.foreach(argument => escape(eval(argument)))

      case call @ cps.Stmt.Call(cps.Callee.Function(callee), arguments,
          cps.ReturnPoint.Direct(results, rest)) =>
        val (targets, closed) = resolveFunction(call, callee, arguments.size)
        observed.put(call, MethodTargets(targets, closed, compositional = true))
        propagate(arguments, targets)
        if closeOpenCalls && !closed then arguments.foreach(argument => escape(eval(argument)))
        results.foreach(add(_, FlowValue.Unknown))
        scan(rest)

      case call @ cps.Stmt.Call(cps.Callee.Method(receiver, method), arguments,
          cps.ReturnPoint.Direct(results, rest)) =>
        val targets = record(call, receiver, method, compositional = true)
        propagate(arguments, targets.targets)
        if closeOpenCalls && !targets.closed then
          arguments.foreach(argument => escape(eval(argument)))
        results.foreach(add(_, FlowValue.Unknown))
        scan(rest)

      case cps.Stmt.Return(results) => results.foreach(r => escape(eval(r)))

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

    def functionValues(id: Id): Set[Id] = {
      val flow = value(id)
      if !callableValues(id) || flow.open || flow.objects.nonEmpty then Set.empty else flow.functions
    }

    // Values in aggregates are tracked for escape, but are not themselves
    // callable. Demand a function representation only along variable flows
    // leading to a function application.
    private lazy val callableValues: Set[Id] = {
      val result = mutable.Set.from(calls.keysIterator.flatMap(_.function))
      val pending = mutable.Queue.from(result)
      while pending.nonEmpty do
        predecessors.get(pending.dequeue()).iterator.flatten.foreach { source =>
          if result.add(source) then pending.enqueue(source)
        }
      result.toSet
    }

    val cpsCallees: Set[Id] = {
      val iterator = observed.entrySet().iterator()
      val result = mutable.Set.empty[Id]
      while iterator.hasNext do {
        val entry = iterator.next()
        if !entry.getValue.compositional then
          result ++= entry.getKey.asInstanceOf[cps.Stmt.Call].callee.function
      }
      result.toSet
    }

    val calls: Map[cps.Callee, Set[Id]] = {
      val result = mutable.Map.empty[cps.Callee, Set[Id]]
      val iterator = observed.entrySet().iterator()
      while iterator.hasNext do {
        val entry = iterator.next()
        val call = entry.getKey.asInstanceOf[cps.Stmt.Call]
        result(call.callee) = result.getOrElse(call.callee, Set.empty) ++ entry.getValue.targets
      }
      result.toMap
    }

    val escapedOperations: Set[Id] = escaped.iterator.flatMap { allocation =>
      operations.iterator.collect {
        case ((owner, _), operation) if owner == allocation => operation.id
      }
    }.toSet

    val cpsOperations: Set[Id] = observed.valuesIterator
      .filter(!_.compositional)
      .flatMap(_.targets)
      .toSet.intersect(operations.valuesIterator.map(_.id).toSet)
  }

  def analyze(
    module: cps.ModuleDecl,
    targetFlows: Vector[cps.Targets.TargetResult],
    requiredCpsEntries: Set[Id]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)

    val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    val operationInfos = mutable.LinkedHashMap.empty[(Id, String), OperationInfo]

    /** Index entries and their lexical parent in one structural traversal. */
    def collect(stmt: cps.Stmt, parent: Option[Id]): Unit = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        definitions(id) = Definition(
          id, params.toVector, body, toplevel = false, parent)
        collect(body, Some(id))
        collect(rest, parent)
      case cps.Stmt.New(objectId, _, implementations, rest) =>
        implementations.foreach { operation =>
          val id = Id(s"${operation.name.name.name}_operation")
          val info = OperationInfo(objectId, operation.name, id, operation)
          operationInfos(objectId -> operation.name.name.name) = info
          definitions(id) = Definition(
            id, operation.params.toVector, operation.body,
            toplevel = false, parent)
          collect(operation.body, Some(id))
        }
        collect(rest, parent)
      case cps.Stmt.Let(_, _, rest) => collect(rest, parent)
      case cps.Stmt.Call(_, _, cps.ReturnPoint.Bind(_, _, _, rest)) =>
        collect(rest, parent)
      case cps.Stmt.Run(_, _, _, _, rest) => collect(rest, parent)
      case cps.Stmt.If(_, thn, els) =>
        collect(thn, parent); collect(els, parent)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => collect(clause.body, parent) }
        default.foreach(collect(_, parent))
      case cps.Stmt.Region(_, _, rest) => collect(rest, parent)
      case cps.Stmt.Alloc(_, _, _, rest) => collect(rest, parent)
      case cps.Stmt.Var(_, _, _, rest) => collect(rest, parent)
      case cps.Stmt.Dealloc(_, rest) => collect(rest, parent)
      case cps.Stmt.Get(_, _, rest) => collect(rest, parent)
      case cps.Stmt.Put(_, _, rest) => collect(rest, parent)
      case cps.Stmt.Reset(_, _, _, body, _, _) => collect(body, None)
      case cps.Stmt.Shift(_, _, _, _, body, _, _) => collect(body, None)
      case cps.Stmt.Resume(_, _, _, body, _, _) => collect(body, None)
      case _: cps.Stmt.Call | _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, body) =>
        definitions(id) = Definition(
          id, params.toVector, body, toplevel = true, parent = None)
        collect(body, Some(id))
      case cps.ToplevelDefinition.Val(_, _, _, binding) => collect(binding, None)
    }

    val valueFlow = ValueFlow(
      module,
      definitions.toMap,
      operationInfos.toMap,
      targetFlows,
      requiredCpsEntries ++ module.exports)

    def returned(
      stmt: cps.Stmt,
      results: List[Id],
      returnedKs: Id,
      definition: Definition
    ): Boolean = stmt match {
      case cps.Stmt.Call(cps.Callee.Function(k), arguments, cps.ReturnPoint.Jump)
          if k == definition.k =>
        val values = results.map(cps.Expr.Variable.apply)
        arguments == values :+ cps.Expr.Variable(definition.ks) ||
          arguments == values :+ cps.Expr.Variable(returnedKs)
      case _ => false
    }

    def resolve(call: cps.Stmt.Call): (Set[Id], Boolean) = call.callee match {
      case cps.Callee.Function(callee) =>
        definitions.get(callee) match {
          case Some(target) if target.params.size == call.args.size + 2 =>
            Set(target.id) -> true
          case _ =>
            valueFlow.targets(call) match {
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

    // The meta-continuation binder identifies the call even when it returns
    // no values. Result vectors cannot serve as keys: all empty ones coincide.
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
      case call @ cps.Stmt.Call(_, _,
          cps.ReturnPoint.Bind(results, returnedKs, _, rest)) =>
        val (targets, closed) = resolve(call)
        val tail = definitions.get(owner).exists { definition =>
          definition.params.size >= 2 && returned(rest, results, returnedKs, definition)
        }
        sites(returnedKs) = Site(
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
      case _: cps.Stmt.Call | _: cps.Stmt.Return | _: cps.Stmt.Hole => ()
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

    final case class Inspection(
      calls: Vector[Site],
      returnBlocks: Set[Id],
      resultArity: ResultArity
    ) {
      def ++(other: Inspection): Inspection =
        Inspection(
          calls ++ other.calls,
          returnBlocks ++ other.returnBlocks,
          resultArity.join(other.resultArity))
    }

    val emptyInspection = Inspection(Vector.empty, Set.empty, ResultArity.Never)

    /** Number of ordinary values returned to this definition's continuation.
     * Parameter dropping may already have removed the meta-continuation. */
    def returnArity(
      jump: cps.Stmt.Call,
      definition: Definition,
      stableKs: Set[Id],
      metaWitness: Boolean
    ): Option[Int] = jump match {
      case cps.Stmt.Call(cps.Callee.Function(k), arguments, cps.ReturnPoint.Jump)
          if k == definition.k =>
        arguments.lastOption match {
          case Some(cps.Expr.Variable(ks)) if stableKs.contains(ks) =>
            Some(arguments.size - 1)
          case _ if metaWitness => Some(arguments.size)
          case _ => None
        }
      case _ => None
    }

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

      case call @ cps.Stmt.Call(_, _,
          cps.ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
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
          val tail = returned(rest, results, returnedKs, definition)
          following.copy(calls = Site(
            call,
            definition.id,
            targets,
            closed,
            tail,
            targets == Set(definition.id) && call.callee == cps.Callee.Function(definition.id) && tail,
            known = known(call, closed)) +: following.calls)
        }

      // Parameter dropping can turn a local CPS definition into an ordinary
      // tail-called block which closes over the enclosing continuation. Such
      // a block belongs to the same lexical control region. Revisiting it
      // closes the coinductive proof and denotes a loop, not host recursion.
      case jump @ cps.Stmt.Call(cps.Callee.Function(id), arguments,
          cps.ReturnPoint.Jump) =>
        returnArity(jump, definition, stableKs, metaWitness) match {
          case Some(arity) =>
            Some(emptyInspection.copy(resultArity = ResultArity.Exact(arity)))
          case None => definitions.get(id) match {
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
        }

      // Before convention specialization, `Return` means completion of the current
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
    val resultAritiesByOwner = mutable.LinkedHashMap.empty[Id, ResultArity]
    val controlErasable = definitions.valuesIterator.flatMap { definition =>
      Option.when(definition.params.size >= 2) {
        inspect(definition.body, definition, Set(definition.ks))
          // A CPS definition has one continuation signature. A definition
          // without a return path is polymorphic in its result arity.
          .filter(_.resultArity != ResultArity.Conflict)
          .map { result =>
            callsByOwner(definition.id) = result.calls
            returnBlocksByOwner(definition.id) = result.returnBlocks
            resultAritiesByOwner(definition.id) = result.resultArity
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

    // A native region never reifies a compositional remainder as a CPS
    // closure. Only within such a region can syntactic back edges reliably
    // become JavaScript jumps rather than calls across function boundaries.
    def nativeRegion(id: Id, direct: Set[Id]): Boolean =
      callsByOwner.getOrElse(id, Vector.empty).forall { site =>
        site.closed && site.targets.nonEmpty && site.targets.subsetOf(direct)
      }

    def nativeSelf(site: Site, direct: Set[Id]): Boolean =
      site.tailSelf && nativeRegion(site.owner, direct)

    def selectedJoins(direct: Set[Id]): Set[Id] =
      (inlineJoins ++ sharedJoinCandidates.filter { id =>
        definitions(id).parent.exists(parent =>
          direct.contains(parent) && nativeRegion(parent, direct))
      }).intersect(direct).filter(nativeRegion(_, direct))

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
    def parameterRequirements(current: Set[Id]): (Map[Id, Map[Int, FunctionSignature]], Set[Id]) = {
      val requirements = mutable.Map.from(current.iterator.map(_ -> Map.empty[Int, FunctionSignature]))
      val invalid = mutable.Set.empty[Id]

      def require(id: Id, position: Int, signature: FunctionSignature): Boolean =
        requirements(id).get(position) match {
          case Some(found) if found != signature =>
            invalid += id
            false
          case Some(_) => false
          case None =>
            requirements(id) = requirements(id).updated(position, signature)
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
              changed = require(owner, position,
                FunctionSignature(
                  site.call.args.size,
                  binding(site.call).results.size)) || changed
            }

            val byPosition = site.targets.iterator
              .flatMap(target => requirements.getOrElse(target, Map.empty))
              .toVector
              .groupMap(_._1)(_._2)
            byPosition.foreach { case (position, arities) =>
              arities.distinct match {
                case Vector(signature) =>
                  site.targets.foreach { target =>
                    changed = require(target, position, signature) || changed
                  }
                case _ => invalid += owner
              }
            }
          }
        }
      }
      requirements.toMap -> invalid.toSet
    }

    var requirements = Map.empty[Id, Map[Int, FunctionSignature]]
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
        nativeSelf(site, controlClosed) || site.tail && site.targets.nonEmpty && site.targets.subsetOf(joins)))
      requirements = nextRequirements.view.filterKeys(updated.contains).toMap
      stable = updated == direct
      direct = updated
    }

    /** A CPS entry may still receive direct function values. The entire
     * parameter domain must have one bounded direct signature, not merely
     * the targets observed at a particular application. */
    def cpsParameters(current: Set[Id]): Map[Id, Map[Int, FunctionSignature]] = {
      val result = mutable.Map.from(definitions.valuesIterator.filterNot(d => current(d.id)).map { definition =>
        val positions = definition.params.zipWithIndex.flatMap { case (parameter, position) =>
          val targets = valueFlow.functionValues(parameter)
          val signatures = targets.toVector.flatMap { target =>
            resultAritiesByOwner.get(target).flatMap(_.exact).map { results =>
              FunctionSignature(definitions(target).params.size - 2, results)
            }
          }
          Option.when(!valueFlow.cpsCallees(parameter) && targets.nonEmpty && targets.subsetOf(current) &&
            signatures.size == targets.size && signatures.distinct.size == 1)(position -> signatures.head)
        }.toMap
        definition.id -> positions
      })
      // Every possible target of an indirect CPS call must agree. A direct
      // definition's CPS adapter retains its ordinary, unspecialized ABI.
      var changed = true
      while changed do {
        changed = false
        valueFlow.calls.values.foreach { targets =>
          val conventions = targets.toVector.map(id => result.getOrElse(id, Map.empty))
          val common = conventions.reduceOption { (left, right) =>
            left.filter { case (position, signature) => right.get(position).contains(signature) }
          }.getOrElse(Map.empty)
          targets.filter(result.contains).foreach { target =>
            if result(target) != common then {
              result(target) = common
              changed = true
            }
          }
        }
      }
      result.toMap.filter(_._2.nonEmpty)
    }

    /** Admissibility alone does not choose a calling convention. A local
     * definition needs the direct ABI only when some direct entry reaches it:
     *
     *   - a syntactically known call can enter it from either convention;
     *   - a closed parameter domain can select its direct value representation
     *     independently of the enclosing definition's result convention.
     *
     * Toplevel definitions are observable entries and therefore roots. The
     * least closure below is the demand counterpart of the greatest control-
     * closed solution above.
     */
    def eligible(site: Site, candidates: Set[Id]): Boolean =
      site.closed && site.targets.nonEmpty && site.targets.subsetOf(candidates)

    def parameterTargets(parameters: Map[Id, Map[Int, FunctionSignature]]): Set[Id] =
      parameters.iterator.flatMap { case (id, positions) =>
        positions.keysIterator.flatMap(position => valueFlow.functionValues(definitions(id).params(position)))
      }.toSet

    val directRoots = toplevel.intersect(direct) ++ parameterTargets(cpsParameters(direct)) ++
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
        nativeSelf(site, controlClosed) || site.tail && site.targets.nonEmpty && site.targets.subsetOf(joins)))
      stable = updated == direct
      direct = updated
    }

    val (demandedRequirements, invalidDemanded) = parameterRequirements(direct)
    assert(invalidDemanded.isEmpty)
    val cpsRequirements = cpsParameters(direct)
    requirements = demandedRequirements ++ cpsRequirements
    val directValues = parameterTargets(cpsRequirements)
    val cpsArguments = valueFlow.calls.iterator.map { case (callee, targets) =>
      callee -> targets.headOption.fold(Map.empty[Int, FunctionSignature]) { target =>
        cpsRequirements.getOrElse(target, Map.empty)
      }
    }.toMap

    val joins = selectedJoins(direct)
    val shared = sharedJoinCandidates.intersect(joins)
    val joinLoops = joins.filter(id =>
      incoming.getOrElse(id, Vector.empty).exists(backEdge(_, id)))

    val localMachines = machines(direct)
    val machineSites = sites.valuesIterator
      .filter(site => direct.contains(site.owner) && isMachine(site, localMachines))
      .map(site => binding(site.call).returnedKs)
      .toSet
    val directParameterValues = requirements.iterator.flatMap { case (id, positions) =>
      positions.keysIterator.map(definitions(id).params)
    }.toSet
    val returnConventions = sites.iterator.map { case (returnedKs, site) =>
      val convention =
        if site.targets.nonEmpty && site.targets.subsetOf(shared) then
          ReturnConvention.Join
        else if site.closed && site.targets.nonEmpty &&
            site.targets.forall(direct.contains) &&
            (site.known || direct.contains(site.owner) ||
              site.call.callee.function.exists(directParameterValues.contains)) then
          ReturnConvention.Direct
        else if machineSites.contains(returnedKs) then
          ReturnConvention.Machine
        else ReturnConvention.CPS
      returnedKs -> convention
    }.toMap

    val native = direct
    val edges = native.iterator.map { source =>
      val targets = callsByOwner.getOrElse(source, Vector.empty).iterator
        .filterNot(site => nativeSelf(site, direct) ||
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
          definitions(definition).params.zipWithIndex.exists {
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

      case call @ cps.Stmt.Call(callee, arguments,
          cps.ReturnPoint.Bind(_, _, ks, rest)) =>
        val returnedKs = binding(call).returnedKs
        val directCall = returnConventions(returnedKs) match {
          case ReturnConvention.Direct | ReturnConvention.Join => true
          case ReturnConvention.CPS | ReturnConvention.Machine => false
        }
        val values = if directCall then {
          val directArguments = sites(returnedKs).targets.headOption
            .fold(Map.empty[Int, FunctionSignature])(id =>
              requirements.getOrElse(id, Map.empty))
          arguments.zipWithIndex.iterator
            .filterNot { case (argument, position) =>
              directArguments.contains(position) &&
                hasDirectRepresentation(argument, owner)
            }
            .map(_._1)
        } else arguments.zipWithIndex.iterator.filterNot { case (argument, position) =>
          cpsArguments.getOrElse(callee, Map.empty).contains(position) &&
            hasDirectRepresentation(argument, owner)
        }.map(_._1) ++ Iterator.single(ks)
        val calleeEntry = if directCall then Set.empty else cpsCallee(callee.value)
        calleeEntry ++ ordinaryAll(values) ++ cpsReferences(rest, owner)

      case call @ cps.Stmt.Call(callee, _,
          _: cps.ReturnPoint.Tail | cps.ReturnPoint.Jump) =>
        val arguments = call.knownArguments.zipWithIndex.iterator.filterNot { case (argument, position) =>
          cpsArguments.getOrElse(callee, Map.empty).contains(position) &&
            hasDirectRepresentation(argument, owner)
        }.map(_._1)
        cpsCallee(callee.value) ++ ordinaryAll(arguments)
      case cps.Stmt.Call(_, arguments, cps.ReturnPoint.Direct(_, rest)) =>
        ordinaryAll(arguments) ++ cpsReferences(rest, owner)
      case cps.Stmt.Return(values) => ordinaryAll(values)
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
    val inheritedReturns = direct.iterator
      .flatMap(id => returnBlocksByOwner.getOrElse(id, Set.empty))
      .filterNot(direct)
      .toSet

    val applications = sites.iterator.map { case (returnedKs, site) =>
      returnedKs -> ApplicationPlan(
        site.copy(tailSelf = nativeSelf(site, direct)),
        returnConventions(returnedKs))
    }.toMap

    // Both facts below are properties of the application set, not of a single
    // entry. Collecting them in one pass keeps plan construction linear.
    val tailSelfSites = applications.valuesIterator.map(_.site).filter(_.tailSelf).toVector
    val tailSelfOwners = tailSelfSites.iterator.map(_.owner).toSet
    val directWorkerTargets = applications.valuesIterator.flatMap { application =>
      if application.returns == ReturnConvention.Direct && !application.site.tailSelf
      then application.site.targets.iterator
      else Iterator.empty
    }.toSet

    val loopMutations = {
      val result = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Id]]
      tailSelfSites.foreach { site =>
        val params = definitions(site.owner).directParams
        val mutated = result.getOrElseUpdate(site.owner, mutable.LinkedHashSet.empty)
        if params.size != site.call.args.size then mutated ++= params
        else params.zip(site.call.args).foreach {
          case (param, cps.Expr.Variable(argument)) if param == argument => ()
          case (param, _) => mutated += param
        }
      }
      result.iterator.map { case (id, params) => id -> params.toSet }.toMap
    }

    val entryPlans = definitions.iterator.map { case (id, definition) =>
      val representation =
        if joins(id) then Representation.Join(shared(id), joinLoops(id))
        else Representation.Function(tailSelfOwners.contains(id))
      val needsDirectWorker = !joins(id) &&
        (directValues(id) || directWorkerTargets.contains(id))

      id -> EntryPlan(
        OriginalDefinition(definition.params.toList),
        requirements.getOrElse(id, Map.empty),
        Option.when(direct(id))(Direct(
          ranks(id), resultAritiesByOwner(id), cpsEntries(id), representation)),
        inheritedReturns(id),
        loopMutations.getOrElse(id, Set.empty),
        needsDirectWorker)
    }.toMap

    val plan = Plan(
      entryPlans,
      applications,
      cpsArguments,
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
