package effekt
package cpsds

import core.Id

import java.util.IdentityHashMap
import scala.annotation.tailrec
import scala.collection.mutable


/**
 * Parameter dropping for local functions.
 *
 * `GuardedEquality` supplies the two semantic facts used here:
 *
 *   - which expression in a closure's lexical scope equals a parameter on
 *     every invocation, and
 *   - which local functions may meet at each call site.
 *
 * This file only chooses a uniform calling convention, computes parameters
 * that become dead together, and applies the resulting rewrite.
 */
object ParameterDropping {

  case class DropInfo(
    /** True at positions removed from a local function. */
    functions: Map[Id, Vector[Boolean]],

    /** Replacements for used parameters and aliases. */
    bindings: Map[Id, Expr],

    private val calls: IdentityHashMap[Stmt, Vector[Boolean]] = IdentityHashMap()
  ) {
    def show: String =
      bindings.toList.sortBy(_._1.id).map { case (id, expr) =>
        s"${util.show(id)} !-> ${util.show(expr)}"
      }.mkString("\n")

    def substitute(id: Id): Expr = bindings.getOrElse(id, Expr.Variable(id))

    private[ParameterDropping] def callMask(call: Stmt): Vector[Boolean] =
      Option(calls.get(call)).getOrElse {
        sys.error("Every call must have a parameter-dropping mask")
      }
  }


  // -----------------------------------------------------------------------
  // Calling conventions and dead parameters

  private final case class Conventions(
    groups: Vector[Vector[Id]],
    groupOf: Map[Id, Int],
    blocked: Set[Int]
  )

  private final class Analysis(flow: GuardedEquality.Result, body: Stmt) {
    private val definitions = flow.definitions.iterator.map(d => d.id -> d).toMap
    private val order = flow.definitions.map(_.id)
    private val reconstruction = flow.facts.iterator.map { facts =>
      facts.id -> facts.entry
    }.toMap

    private def conventions(): Conventions = {
      val neighbours = order.iterator.map(id => id -> mutable.Set.empty[Id]).toMap

      // Functions that can occur at the same call site require one signature.
      flow.targetsAt.valuesIterator.foreach { targets =>
        val local = targets.filter(definitions.contains)
        local.foreach(from => neighbours(from) ++= local.filterNot(_ == from))
      }

      @tailrec def reachable(todo: List[Id], found: Set[Id]): Set[Id] = todo match {
        case Nil => found
        case id :: rest if found.contains(id) => reachable(rest, found)
        case id :: rest => reachable(neighbours(id).toList ::: rest, found + id)
      }

      @tailrec def partition(
        remaining: List[Id],
        assigned: Set[Id],
        result: Vector[Vector[Id]]
      ): Vector[Vector[Id]] = remaining match {
        case Nil => result
        case id :: rest if assigned.contains(id) => partition(rest, assigned, result)
        case id :: rest =>
          val group = reachable(List(id), Set.empty)
          partition(rest, assigned ++ group, result :+ order.filter(group))
      }

      val groups = partition(order.toList, Set.empty, Vector.empty)
      val groupOf = groups.zipWithIndex.flatMap { case (members, group) =>
        members.map(_ -> group)
      }.toMap

      val unsafeTargets = flow.rigidSites.iterator.flatMap { site =>
        flow.targetsAt.getOrElse(site, Set.empty)
      }
      val blockedFunctions = (flow.rigidFunctions.iterator ++ unsafeTargets)
        .filter(definitions.contains)
        .toSet
      val incompatibleGroups = groups.iterator.zipWithIndex.collect {
        case (members, group)
            if members.map(definitions(_).params.size).distinct.size != 1 => group
      }.toSet

      Conventions(
        groups,
        groupOf,
        blockedFunctions.map(groupOf) ++ incompatibleGroups)
    }

    private def siteMasks(
      conventions: Conventions,
      groupMasks: Map[Int, Vector[Boolean]]
    ): Map[Int, Vector[Boolean]] =
      flow.sites.iterator.zipWithIndex.map { case (call, site) =>
        val targets = flow.targetsAt.getOrElse(site, Set.empty).filter(definitions.contains)
        val mask =
          if targets.isEmpty || flow.rigidSites.contains(site) then
            Vector.fill(call.arity)(false)
          else {
            val group = conventions.groupOf(targets.head)
            assert(targets.forall(conventions.groupOf(_) == group))
            val groupMask = groupMasks(group)
            Vector.tabulate(call.arity)(index => groupMask.lift(index).getOrElse(false))
          }
        site -> mask
      }.toMap

    /**
     * Free variables after applying hypothetical drop masks.
     *
     * Uses are recorded before removing the parameters bound by their own
     * definition. Starting from the greatest convention therefore also drops
     * mutually dead argument chains.
     */
    private def parameterUses(
      functionMasks: Map[Id, Vector[Boolean]],
      callMasks: Map[Int, Vector[Boolean]]
    ): Map[Id, Vector[Boolean]] = {
      val result = mutable.Map.empty[Id, Vector[Boolean]]

      def visit(stmt: Stmt): Set[Id] = stmt match {
        case Stmt.Def(id, params, functionBody, rest) =>
          val bodyFree = visit(functionBody)
          result(id) = params.map(bodyFree.contains).toVector

          val mask = functionMasks(id)
          val replacements = reconstruction(id)
          val outerFree = params.zipWithIndex.foldLeft(bodyFree) {
            case (free, (param, index)) =>
              val withReplacement =
                if mask.lift(index).getOrElse(false) && free.contains(param) then
                  replacements.lift(index).flatten.fold(free) { replacement =>
                    (free - param) ++ replacement.free
                  }
                else free
              withReplacement - param
          }

          (outerFree - id) ++ (visit(rest) - id)

        case Stmt.New(id, _, operations, rest) =>
          val operationFree = operations.iterator.flatMap { operation =>
            visit(operation.body) -- operation.params
          }.toSet
          operationFree ++ (visit(rest) - id)

        case Stmt.Let(id, binding, rest) => binding.free ++ (visit(rest) - id)

        case app @ Stmt.App(id, args, _) =>
          val mask = callMasks(flow.siteOf(app))
          Set(id) ++ args.zipWithIndex.iterator.collect {
            case (arg, index) if !mask.lift(index).getOrElse(false) => arg.free
          }.flatten.toSet

        case Stmt.Invoke(id, _, args) => Set(id) ++ args.flatMap(_.free)

        case Stmt.Run(id, callee, args, _, rest) =>
          Set(callee) ++ args.flatMap(_.free) ++ (visit(rest) - id)

        case Stmt.If(cond, thn, els) => cond.free ++ visit(thn) ++ visit(els)

        case Stmt.Match(scrutinee, clauses, default) =>
          scrutinee.free ++ clauses.iterator.flatMap { case (_, clause) =>
            visit(clause.body) -- clause.params
          }.toSet ++ default.map(visit).getOrElse(Set.empty)

        case Stmt.Region(id, ks, rest) => ks.free ++ (visit(rest) - id)

        case Stmt.Alloc(id, init, region, rest) =>
          init.free + region ++ (visit(rest) - id)

        case Stmt.Var(id, init, ks, rest) =>
          init.free ++ ks.free ++ (visit(rest) - id)

        case Stmt.Dealloc(ref, rest) => visit(rest) + ref
        case Stmt.Get(ref, id, rest) => (visit(rest) - id) + ref
        case Stmt.Put(ref, value, rest) => visit(rest) ++ value.free + ref

        case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
          (visit(resetBody) -- Set(p, ks, k)) ++ ks1.free ++ k1.free

        case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
          (visit(shiftBody) -- Set(resume, ks, k)) ++ ks1.free ++ k1.free + prompt

        case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
          (visit(resumeBody) -- Set(ks, k)) ++ ks1.free ++ k1.free + resumption

        case Stmt.Hole(_) => Set.empty
      }

      visit(body)
      result.toMap
    }

    private def normalize(bindings: Map[Id, Expr]): Map[Id, Expr] = {
      def go(expr: Expr, seen: Set[Id]): Expr = expr match {
        case Expr.Variable(id) if bindings.contains(id) && !seen.contains(id) =>
          go(bindings(id), seen + id)
        case Expr.Make(data, tag, args) => Expr.Make(data, tag, args.map(go(_, seen)))
        case other => other
      }

      bindings.map { case (id, expression) => id -> go(expression, Set(id)) }
    }

    def result(): DropInfo = {
      val convention = conventions()

      // Begin with the greatest convention; facts can only be removed.
      val initialMasks = convention.groups.zipWithIndex.map { case (members, group) =>
        val arity = definitions(members.head).params.size
        group -> Vector.fill(arity)(!convention.blocked.contains(group))
      }.toMap

      @tailrec def greatestFixedPoint(
        groupMasks: Map[Int, Vector[Boolean]]
      ): Map[Int, Vector[Boolean]] = {
        val functionMasks = order.iterator.map { id =>
          id -> groupMasks(convention.groupOf(id))
        }.toMap
        val calls = siteMasks(convention, groupMasks)
        val uses = parameterUses(functionMasks, calls)

        val updated = convention.groups.zipWithIndex.map { case (members, group) =>
          val before = groupMasks(group)
          val after = before.indices.map { index =>
            before(index) && members.forall { id =>
              val isUsed = uses.get(id).flatMap(_.lift(index)).getOrElse(false)
              !isUsed || reconstruction(id)(index).isDefined
            }
          }.toVector
          group -> after
        }.toMap

        if updated == groupMasks then groupMasks else greatestFixedPoint(updated)
      }

      val groupMasks = greatestFixedPoint(initialMasks)
      val functionMasks = order.iterator.map { id =>
        id -> groupMasks(convention.groupOf(id))
      }.toMap
      val masksAtCalls = siteMasks(convention, groupMasks)
      val finalUses = parameterUses(functionMasks, masksAtCalls)

      val parameterBindings = flow.definitions.iterator.flatMap { definition =>
        definition.params.zipWithIndex.flatMap { case (param, index) =>
          val dropped = functionMasks(definition.id)(index)
          val used = finalUses(definition.id)(index)
          if dropped && used then {
            val replacement = reconstruction(definition.id)(index).getOrElse {
              sys.error(s"Dropped used parameter ${util.show(param)} has no replacement")
            }
            Some(param -> replacement)
          } else None
        }
      }.toMap

      val callMasks = new IdentityHashMap[Stmt, Vector[Boolean]]()
      flow.sites.zipWithIndex.foreach { case (call, site) =>
        callMasks.put(call.stmt, masksAtCalls(site))
      }

      DropInfo(
        functionMasks,
        normalize(parameterBindings ++ flow.bindings),
        callMasks)
    }
  }


  // -----------------------------------------------------------------------
  // Public analyses

  def solve(toplevel: ToplevelDefinition): DropInfo = toplevel match {
    case ToplevelDefinition.Def(_, _, body) =>
      val flow = GuardedEquality.analyze(toplevel)
      Analysis(flow, body).result()
    case _: ToplevelDefinition.Val => DropInfo(Map.empty, Map.empty)
  }
  // -----------------------------------------------------------------------
  // Rewriting

  private def transform(expr: Expr, info: DropInfo): Expr = expr match {
    case Expr.Variable(id) => info.substitute(id)
    case Expr.Literal(_, _) => expr
    case Expr.Make(data, tag, args) => Expr.Make(data, tag, args.map(transform(_, info)))
    case Expr.Abort | Expr.Return | Expr.Toplevel => expr
  }

  private def transform(stmt: Stmt, info: DropInfo): Stmt = stmt match {
    case Stmt.Def(id, params, functionBody, rest) =>
      val mask = info.functions(id)
      val kept = params.zipWithIndex.collect {
        case (param, index) if !mask.lift(index).getOrElse(false) => param
      }
      Stmt.Def(id, kept, transform(functionBody, info), transform(rest, info))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(transform(_, info)), transform(rest, info))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, transform(binding, info), transform(rest, info))

    case app @ Stmt.App(id, args, canBeDirect) =>
      val callee = info.substitute(id) match {
        case Expr.Variable(callee) => callee
        case other => sys.error(s"A call target cannot be replaced by ${util.show(other)}")
      }
      val mask = info.callMask(app)
      val kept = args.zipWithIndex.collect {
        case (argument, index) if !mask.lift(index).getOrElse(false) =>
          transform(argument, info)
      }
      Stmt.App(callee, kept, canBeDirect)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(transform(_, info)))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(transform(_, info)), purity, transform(rest, info))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond, info), transform(thn, info), transform(els, info))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        transform(scrutinee, info),
        clauses.map { case (tag, clause) => tag -> transform(clause, info) },
        default.map(transform(_, info)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, transform(ks, info), transform(rest, info))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, transform(init, info), region, transform(rest, info))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, transform(init, info), transform(ks, info), transform(rest, info))

    case Stmt.Dealloc(ref, rest) => Stmt.Dealloc(ref, transform(rest, info))
    case Stmt.Get(ref, id, rest) => Stmt.Get(ref, id, transform(rest, info))
    case Stmt.Put(ref, value, rest) => Stmt.Put(ref, transform(value, info), transform(rest, info))

    case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
      Stmt.Reset(p, ks, k, transform(resetBody, info), transform(ks1, info), transform(k1, info))

    case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
      Stmt.Shift(
        prompt, resume, ks, k,
        transform(shiftBody, info), transform(ks1, info), transform(k1, info))

    case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
      Stmt.Resume(
        resumption, ks, k,
        transform(resumeBody, info), transform(ks1, info), transform(k1, info))

    case _: Stmt.Hole => stmt
  }

  private def transform(operation: Operation, info: DropInfo): Operation =
    Operation(operation.name, operation.params, transform(operation.body, info))

  private def transform(clause: Clause, info: DropInfo): Clause =
    Clause(clause.params, transform(clause.body, info))

  private def transform(toplevel: ToplevelDefinition): ToplevelDefinition = toplevel match {
    case definition @ ToplevelDefinition.Def(id, params, body) =>
      ToplevelDefinition.Def(id, params, transform(body, solve(definition)))
    case value: ToplevelDefinition.Val => value
  }

  def transform(module: ModuleDecl): ModuleDecl =
    module.copy(definitions = module.definitions.map(transform))
}
