package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

/**
 * Jointly chooses the runtime representation of local definitions.
 *
 * Syntactic escape analysis is necessarily conservative before
 * defunctionalization: a continuation closure appears to store all of its
 * free definitions. Once that closure becomes an immutable continuation
 * frame, only the fields in its residual frame layout are runtime values.
 * This analysis repeatedly removes definitions no longer demanded as values
 * and adds back-edges created by case relocation until definition kinds and
 * continuation layouts are simultaneously stable.
 *
 * The abstract state is the finite product
 *
 *     (definitions represented as functions, residually recursive definitions)
 *
 * ordered by reverse inclusion in the first component and inclusion in the
 * second. Frame layouts only discard labels and common static bindings, so a
 * removed function demand cannot be reintroduced; relocating more cases can
 * only add residual back-edges. Consequently at most two changes per
 * definition are possible.
 */
object DefinitionPlanning {

  final case class Kind(isRecursive: Boolean, isFirstClass: Boolean) {
    def isSecondClass: Boolean = !isFirstClass
  }

  final case class Plan(
    kinds: Map[Id, Kind],
    defunctionalization: Defunctionalization.Plan
  )

  def analyze(
    module: cps.ModuleDecl,
    targetFlows: Vector[cps.GuardedEquality.TargetResult],
    requiredFunctions: Set[Id] = Set.empty,
    directDefinitions: Set[Id] = Set.empty
  ): Plan = {
    require(module.definitions.size == targetFlows.size)

    val uses = module.uses.toMap
    val definitions = uses.keySet
    var recursive = definitions.filter(id => uses.get(id).exists(_.contains(id)))
    var functions = (module.escapes ++ requiredFunctions).intersect(definitions)

    while true do {
      def isSecondClass(id: Id): Boolean =
        definitions.contains(id) && !functions.contains(id)

      val defunctionalization = Defunctionalization.analyze(
        module,
        recursive.contains,
        isSecondClass,
        targetFlows,
        directDefinitions)
      val required = runtimeFunctionValues(
        module,
        definitions,
        defunctionalization) ++ defunctionalization.firstClassRequirements ++ requiredFunctions
      val residualRecursive = recursive ++ defunctionalization.reenteredDefinitions

      // This is the monotonicity invariant that makes the representation
      // computation a finite fixed point rather than a heuristic loop.
      require(
        required.subsetOf(functions),
        "residual representation introduced a new function-value demand")

      if required == functions && residualRecursive == recursive then {
        val kinds = definitions.iterator.map { id =>
          id -> Kind(recursive.contains(id), functions.contains(id))
        }.toMap
        return Plan(kinds, defunctionalization)
      }
      functions = required
      recursive = residualRecursive
    }

    throw new AssertionError("unreachable")
  }

  /** Definitions whose runtime values are required after continuation cases
   *  have been replaced by their residual frame layouts. */
  private def runtimeFunctionValues(
    module: cps.ModuleDecl,
    definitions: Set[Id],
    defunctionalization: Defunctionalization.Plan
  ): Set[Id] = {
    val cases = defunctionalization.cases.keySet

    def definitionsIn(ids: IterableOnce[Id]): Set[Id] =
      ids.iterator.filter(id => definitions.contains(id) && !cases.contains(id)).toSet

    def free(expressions: IterableOnce[cps.Expr]): Set[Id] =
      expressions.iterator.flatMap(_.free).toSet

    /** The two observations needed from a term after choosing its residual
     *  representation. `free` describes its lexical dependencies;
     *  `functions` is the subset of definition bindings used as values.
     *
     *  This is the ordinary free-variable and escape algebra, except for a
     *  continuation case: its dynamic captures occur at frame construction,
     *  while its body reads those captures from fields at the dispatcher.
     */
    final case class Summary(free: Set[Id], functions: Set[Id])

    def operation(op: cps.Operation): Summary = {
      val body = statement(op.body)
      val captured = body.free -- op.params
      Summary(captured, body.functions ++ definitionsIn(captured))
    }

    def clause(clause: cps.Clause): Summary = {
      val body = statement(clause.body)
      body.copy(free = body.free -- clause.params)
    }

    def statement(stmt: cps.Stmt): Summary = stmt match {
      case cps.Stmt.Def(id, params, body, rest) =>
        val bodySummary = statement(body)
        val restSummary = statement(rest)
        defunctionalization.caseOf(id) match {
          case Some(continuationCase) =>
            val captures = continuationCase.captures.toSet
            Summary(
              // The case body is placed at the dispatcher. Its parameters and
              // frame fields are bound there; frame construction occurs here.
              (bodySummary.free -- params - id -- captures) ++
                captures ++ (restSummary.free - id),
              bodySummary.functions ++ restSummary.functions ++
                definitionsIn(captures))

          case None =>
            val nested = bodySummary.functions ++ restSummary.functions
            val closure = Option.when(nested.contains(id)) {
              definitionsIn(bodySummary.free -- params - id)
            }.getOrElse(Set.empty)
            Summary(
              (bodySummary.free -- params - id) ++ (restSummary.free - id),
              nested ++ closure)
        }

      case cps.Stmt.New(id, _, operations, rest) =>
        val bodies = operations.map(operation)
        val continuation = statement(rest)
        Summary(
          bodies.iterator.flatMap(_.free).toSet ++ (continuation.free - id),
          bodies.iterator.flatMap(_.functions).toSet ++ continuation.functions)

      case cps.Stmt.Let(id, binding, rest) =>
        val continuation = statement(rest)
        Summary(
          binding.free ++ (continuation.free - id),
          definitionsIn(binding.free) ++ continuation.functions)

      case cps.Stmt.Call(ids, returnedKs, callee, arguments, ks, rest) =>
        val argumentFree = free(arguments) ++ ks.free
        val continuation = statement(rest)
        val boundary = ks match {
          case cps.Expr.Toplevel => Set(callee.value)
          case _ => Set.empty[Id]
        }
        Summary(
          argumentFree ++ (continuation.free -- ids.toSet - returnedKs) + callee.value,
          definitionsIn(argumentFree ++ boundary) ++ continuation.functions)

      case cps.Stmt.App(callee, arguments) =>
        val argumentFree = free(arguments)
        Summary(argumentFree + callee, definitionsIn(argumentFree))
      case cps.Stmt.Invoke(receiver, _, arguments) =>
        val argumentFree = free(arguments)
        Summary(argumentFree + receiver, definitionsIn(argumentFree))

      case cps.Stmt.Return(values) =>
        val valuesFree = free(values)
        Summary(valuesFree, definitionsIn(valuesFree))

      case cps.Stmt.Run(id, callee, arguments, cps.Purity.Async, rest) =>
        val argumentFree = free(arguments)
        val continuation = statement(rest)
        Summary(
          argumentFree ++ (continuation.free - id) + callee,
          definitionsIn(argumentFree ++ continuation.free) ++ continuation.functions)
      case cps.Stmt.Run(id, callee, arguments, _, rest) =>
        val argumentFree = free(arguments)
        val continuation = statement(rest)
        Summary(
          argumentFree ++ (continuation.free - id) + callee,
          definitionsIn(argumentFree) ++ continuation.functions)

      case cps.Stmt.If(condition, thn, els) =>
        val left = statement(thn)
        val right = statement(els)
        Summary(
          condition.free ++ left.free ++ right.free,
          definitionsIn(condition.free) ++ left.functions ++ right.functions)

      case cps.Stmt.Match(scrutinee, clauses, default) =>
        val branches = clauses.map { case (_, body) => clause(body) } ++ default.map(statement)
        Summary(
          scrutinee.free ++ branches.iterator.flatMap(_.free).toSet,
          definitionsIn(scrutinee.free) ++ branches.iterator.flatMap(_.functions).toSet)

      case cps.Stmt.Region(id, ks, rest) =>
        val continuation = statement(rest)
        Summary(ks.free ++ (continuation.free - id), continuation.functions)
      case cps.Stmt.Alloc(id, init, region, rest) =>
        val continuation = statement(rest)
        Summary(
          init.free ++ (continuation.free - id) + region,
          definitionsIn(init.free) ++ continuation.functions)
      case cps.Stmt.Var(id, init, ks, rest) =>
        val continuation = statement(rest)
        Summary(
          init.free ++ ks.free ++ (continuation.free - id),
          definitionsIn(init.free) ++ continuation.functions)
      case cps.Stmt.Dealloc(ref, rest) =>
        val continuation = statement(rest)
        continuation.copy(free = continuation.free + ref)
      case cps.Stmt.Get(ref, id, rest) =>
        val continuation = statement(rest)
        continuation.copy(free = (continuation.free - id) + ref)
      case cps.Stmt.Put(ref, value, rest) =>
        val continuation = statement(rest)
        Summary(
          value.free ++ continuation.free + ref,
          definitionsIn(value.free) ++ continuation.functions)

      case cps.Stmt.Reset(prompt, meta, continuation, body, ks, k) =>
        val nested = statement(body)
        Summary(
          (nested.free - prompt - meta - continuation) ++ ks.free ++ k.free,
          nested.functions ++ definitionsIn(ks.free ++ k.free))
      case cps.Stmt.Shift(prompt, resume, meta, continuation, body, ks, k) =>
        val nested = statement(body)
        Summary(
          (nested.free - resume - meta - continuation) ++ ks.free ++ k.free + prompt,
          nested.functions ++ definitionsIn(ks.free ++ k.free))
      case cps.Stmt.Resume(resumption, meta, continuation, body, ks, k) =>
        val nested = statement(body)
        Summary(
          (nested.free - meta - continuation) ++ ks.free ++ k.free + resumption,
          nested.functions ++ definitionsIn(ks.free ++ k.free))

      case _: cps.Stmt.Hole => Summary(Set.empty, Set.empty)
    }

    module.definitions.iterator.flatMap {
      case cps.ToplevelDefinition.Def(id, _, body) =>
        statement(body).functions + id
      case cps.ToplevelDefinition.Val(_, _, _, binding) =>
        statement(binding).functions
    }.toSet
  }
}
