package effekt
package cpsds

import core.Id

import java.util.IdentityHashMap
import scala.annotation.tailrec
import scala.collection.mutable


/**
 * Parameter dropping for local functions.
 *
 * The analysis keeps calls relational: a row contains the called closure, its
 * captured environment, and the complete argument vector. In particular, it
 * never projects
 *
 *   { (inc, 1), (dec, 0) }
 *
 * to the cartesian approximation
 *
 *   { inc, dec } x { 0, 1 }.
 *
 * A parameter can be dropped when, for every abstract invocation of the
 * closure, it is equal to an expression visible at the function's definition.
 * Calls through several possible functions use the intersection of their drop
 * masks, so all possible callees retain one uniform calling convention.
 *
 * The value domain is finite. Terms are variables or pure expressions already
 * present in the program; every constructed value is immediately projected
 * back onto that vocabulary. Closure contexts contain only such terms,
 * definition sites, or unknown. Consequently the relational worklist reaches
 * a fixed point without widening or an arbitrary fact budget.
 */
object ParameterDropping {

  case class DropInfo(
    // true at positions that are removed from a local function
    functions: Map[Id, Vector[Boolean]],
    // replacements for used parameters; dead parameters have no replacement
    bindings: Map[Id, Expr],
    private val calls: IdentityHashMap[Stmt, Vector[Boolean]] = IdentityHashMap()
  ) {
    def show: String =
      bindings.toList.sortBy(_._1.id).map {
        case (id, expr) => s"${util.show(id)} !-> ${util.show(expr)}"
      }.mkString("\n")

    def substitute(id: Id): Expr = bindings.getOrElse(id, Expr.Variable(id))

    private[ParameterDropping] def callMask(call: Stmt): Vector[Boolean] =
      Option(calls.get(call)).getOrElse {
        sys.error("Every call must have a parameter-dropping mask")
      }
  }


  // -------------------------------------------------------------------------
  // Finite relational abstract values

  private enum AbstractValue {
    /** A variable or a pure expression from the program's finite vocabulary. */
    case Term(expr: Expr)

    /** A local closure allocated in an abstract lexical environment. */
    case Closure(address: Address)

    /** An opaque value, retaining only local functions enclosed in the value. */
    case Opaque(functions: Set[Id])
  }

  private val UnknownValue = AbstractValue.Opaque(Set.empty)

  private enum ContextValue {
    case Term(expr: Expr)
    case Function(id: Id)
    case Unknown
  }

  /** A closure site paired with its finite observational capture profile. */
  private case class Address(function: Id, context: Vector[ContextValue])

  private type Env = Map[Id, AbstractValue]

  private case class Definition(
    id: Id,
    params: Vector[Id],
    body: Stmt,
    visible: Vector[Id],
    singleAllocation: Boolean
  )

  private type Arguments = Vector[AbstractValue]

  private case class Entry(
    address: Address,
    captures: Env,
    arguments: Arguments
  )

  private case class BindingFacts(
    scope: Vector[Id],
    observations: mutable.Set[(Env, AbstractValue)] = mutable.Set.empty
  )

  private case class Site(stmt: Stmt, callee: Id, arity: Int)


  // -------------------------------------------------------------------------
  // Collecting semantics

  private final class Analysis(toplevelParams: List[Id], body: Stmt) {

    private val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    private val terms = mutable.Set.empty[Expr]
    private val callees = mutable.Set.empty[Id]
    private val bindings = mutable.Map.empty[Id, BindingFacts]
    private val sitesByStmt = new IdentityHashMap[Stmt, Integer]()
    private val sites = mutable.ArrayBuffer.empty[Site]

    // Closure(address, captures) and Call(address, arguments) are kept as
    // separate relations and joined incrementally in both directions.
    private val closureEnvironments = mutable.Map.empty[Address, mutable.Set[Env]]
    private val callsByAddress = mutable.Map.empty[Address, mutable.Set[Arguments]]

    private val entryQueue = mutable.Queue.empty[Entry]
    private val entriesByFunction = mutable.Map.empty[Id, mutable.Set[Entry]]

    private val targetsBySite = mutable.Map.empty[Int, mutable.Set[Id]]
    private val rigidSites = mutable.Set.empty[Int]
    private val rigidFunctions = mutable.Set.empty[Id]

    collect(body, toplevelParams.toVector, repeated = false)

    private def extend(scope: Vector[Id], ids: IterableOnce[Id]): Vector[Id] =
      scope ++ ids

    private def registerSite(stmt: Stmt, callee: Id, arity: Int): Unit = {
      if sitesByStmt.get(stmt) == null then {
        val site = sites.size
        sitesByStmt.put(stmt, site)
        sites += Site(stmt, callee, arity)
      }
    }

    /** Add a pure expression and all of its subexpressions to the vocabulary. */
    private def remember(expr: Expr): Unit = expr match {
      case _: Expr.Variable => ()
      case term @ Expr.Make(_, _, args) =>
        terms += term
        args.foreach(remember)
      case term => terms += term
    }

    /** Register definitions, scopes, and call sites independently of reachability. */
    private def collect(stmt: Stmt, scope: Vector[Id], repeated: Boolean): Unit = stmt match {
      case Stmt.Def(id, params, functionBody, rest) =>
        definitions.get(id) match {
          case Some(previous) =>
            assert(previous.params == params.toVector && previous.body == functionBody)
          case None =>
            definitions(id) = Definition(id, params.toVector, functionBody, scope, !repeated)
        }
        collect(functionBody, extend(extend(scope, List(id)), params), repeated = true)
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.New(id, interface, operations, rest) =>
        operations.foreach { op => collect(op.body, extend(scope, op.params), repeated = true) }
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.Let(id, binding, rest) =>
        remember(binding)
        bindings(id) = BindingFacts(scope)
        collect(rest, extend(scope, List(id)), repeated)

      case app @ Stmt.App(id, args, direct) =>
        callees += id
        args.foreach(remember)
        registerSite(app, id, args.size)

      case Stmt.Invoke(id, method, args) =>
        callees += id
        args.foreach(remember)

      case Stmt.Run(id, callee, args, purity, rest) =>
        callees += callee
        args.foreach(remember)
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.If(cond, thn, els) =>
        remember(cond)
        collect(thn, scope, repeated)
        collect(els, scope, repeated)

      case Stmt.Match(scrutinee, clauses, default) =>
        remember(scrutinee)
        clauses.foreach { case (_, clause) =>
          collect(clause.body, extend(scope, clause.params), repeated)
        }
        default.foreach(collect(_, scope, repeated))

      case Stmt.Region(id, ks, rest) =>
        remember(ks)
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.Alloc(id, init, region, rest) =>
        remember(init)
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.Var(id, init, ks, rest) =>
        remember(init)
        remember(ks)
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.Dealloc(ref, rest) => collect(rest, scope, repeated)

      case Stmt.Get(ref, id, rest) =>
        collect(rest, extend(scope, List(id)), repeated)

      case Stmt.Put(ref, value, rest) =>
        remember(value)
        collect(rest, scope, repeated)

      case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        collect(resetBody, extend(scope, List(p, ks, k)), repeated = true)

      case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        collect(shiftBody, extend(scope, List(resume, ks, k)), repeated = true)

      case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        collect(resumeBody, extend(scope, List(ks, k)), repeated = true)

      case Stmt.Hole(span) => ()
    }

    private def siteOf(stmt: Stmt): Int =
      val site = sitesByStmt.get(stmt)
      assert(site != null, "all call sites are registered before analysis")
      site.intValue

    private def restrict(env: Env, scope: Vector[Id]): Env =
      scope.iterator.map(id => id -> env(id)).toMap

    private def reify(value: AbstractValue): Option[Expr] = value match {
      case AbstractValue.Term(expr) => Some(expr)
      case AbstractValue.Closure(address) => None
      case AbstractValue.Opaque(_) => None
    }

    private def contextValue(value: AbstractValue): ContextValue = value match {
      case AbstractValue.Closure(address) => ContextValue.Function(address.function)
      case AbstractValue.Term(expr) => ContextValue.Term(expr)
      case AbstractValue.Opaque(_) => ContextValue.Unknown
    }

    private def address(info: Definition, captures: Env): Address =
      Address(info.id, info.visible.map { id =>
        contextValue(captures(id))
      })

    /** Project an expression immediately onto the finite vocabulary. */
    private def observe(expr: Expr): AbstractValue = expr match {
      case variable: Expr.Variable => AbstractValue.Term(variable)
      case _ if terms.contains(expr) => AbstractValue.Term(expr)
      case _ => UnknownValue
    }

    private def enclosedFunctions(value: AbstractValue): Set[Id] = value match {
      case AbstractValue.Closure(address) => Set(address.function)
      case AbstractValue.Opaque(functions) => functions
      case AbstractValue.Term(_) => Set.empty
    }

    private def eval(expr: Expr, env: Env): AbstractValue = expr match {
      case Expr.Variable(id) => env.getOrElse(id, UnknownValue)
      case literal: Expr.Literal => observe(literal)
      case Expr.Make(data, tag, args) =>
        val values = args.map(eval(_, env))
        val functions = values.iterator.flatMap(enclosedFunctions).toSet
        if functions.nonEmpty then AbstractValue.Opaque(functions)
        else {
          val expressions = values.map(reify)
          if expressions.forall(_.isDefined) then
            observe(Expr.Make(data, tag, expressions.map(_.get)))
          else UnknownValue
        }
      case Expr.Abort => observe(Expr.Abort)
      case Expr.Return => observe(Expr.Return)
      case Expr.Toplevel => observe(Expr.Toplevel)
    }

    private def opaqueArguments(function: Id): Arguments =
      Vector.fill(definitions(function).params.size)(UnknownValue)

    /**
     * A function crossing a rigid boundary may be invoked with arbitrary
     * arguments. Its signature is fixed, and its body must therefore also be
     * analyzed from an opaque entry.
     */
    private def makeRigid(function: Id): Unit =
      if rigidFunctions.add(function) then {
        closureEnvironments.foreach { case (address, environments) =>
          if address.function == function then
            environments.foreach { captures =>
              addEntry(address, captures, opaqueArguments(function))
            }
        }
      }

    /** Values crossing a rigid boundary may be invoked with their old signature. */
    private def escape(value: AbstractValue): Unit =
      enclosedFunctions(value).foreach(makeRigid)

    private def addEntry(address: Address, captures: Env, args: Arguments): Unit = {
      val info = definitions(address.function)
      if args.size != info.params.size then
        makeRigid(info.id)
      else {
        val entry = Entry(address, captures, args)
        val functionEntries = entriesByFunction.getOrElseUpdate(info.id, mutable.Set.empty)
        if functionEntries.add(entry) then entryQueue.enqueue(entry)
      }
    }

    private def addClosure(address: Address, captures: Env): Unit = {
      val environments = closureEnvironments.getOrElseUpdate(address, mutable.Set.empty)
      if environments.add(captures) then {
        callsByAddress.get(address).foreach { calls =>
          calls.foreach(args => addEntry(address, captures, args))
        }
        if rigidFunctions.contains(address.function) then
          addEntry(address, captures, opaqueArguments(address.function))
      }
    }

    private def addCall(site: Int, address: Address, args: Arguments): Unit = {
      targetsBySite.getOrElseUpdate(site, mutable.Set.empty) += address.function

      definitions.get(address.function) match {
        case Some(info) if info.params.size == args.size => ()
        case Some(info) =>
          rigidSites += site
          makeRigid(info.id)
        case None => rigidSites += site
      }

      val calls = callsByAddress.getOrElseUpdate(address, mutable.Set.empty)
      if calls.add(args) then {
        closureEnvironments.get(address).foreach { environments =>
          environments.foreach(captures => addEntry(address, captures, args))
        }
      }
    }

    private def bindOpaque(env: Env, ids: IterableOnce[Id]): Env =
      env ++ ids.iterator.map(_ -> UnknownValue)

    private def execute(stmt: Stmt, env: Env): Unit = {
      stmt match {
        case Stmt.Def(id, params, functionBody, rest) =>
          val info = definitions(id)
          val captures = restrict(env, info.visible)
          val closureAddress = address(info, captures)
          addClosure(closureAddress, captures)
          execute(rest, env.updated(id, AbstractValue.Closure(closureAddress)))

        case Stmt.New(id, interface, operations, rest) =>
          // Operation signatures are rigid, but local definitions inside an
          // operation can still capture its opaque parameters.
          operations.foreach { operation =>
            val operationEnv = bindOpaque(env, operation.params)
            execute(operation.body, operationEnv)
          }
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.Let(id, binding, rest) =>
          val value = eval(binding, env)
          val facts = bindings(id)
          facts.observations += restrict(env, facts.scope) -> value
          execute(rest, env.updated(id, value))

        case app @ Stmt.App(id, args, direct) =>
          val site = siteOf(app)
          val arguments = args.map(eval(_, env)).toVector
          eval(Expr.Variable(id), env) match {
            case AbstractValue.Closure(address) => addCall(site, address, arguments)
            case other =>
              rigidSites += site
              escape(other)
              arguments.foreach(escape)
          }

        case Stmt.Invoke(id, method, args) =>
          escape(eval(Expr.Variable(id), env))
          args.foreach(arg => escape(eval(arg, env)))

        case Stmt.Run(id, callee, args, purity, rest) =>
          escape(eval(Expr.Variable(callee), env))
          args.foreach(arg => escape(eval(arg, env)))
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.If(cond, thn, els) =>
          execute(thn, env)
          execute(els, env)

        case Stmt.Match(scrutinee, clauses, default) =>
          escape(eval(scrutinee, env))
          clauses.foreach { case (_, clause) =>
            val clauseEnv = bindOpaque(env, clause.params)
            execute(clause.body, clauseEnv)
          }
          default.foreach(execute(_, env))

        case Stmt.Region(id, ks, rest) =>
          escape(eval(ks, env))
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.Alloc(id, init, region, rest) =>
          escape(eval(init, env))
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.Var(id, init, ks, rest) =>
          escape(eval(init, env))
          escape(eval(ks, env))
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.Dealloc(ref, rest) => execute(rest, env)

        case Stmt.Get(ref, id, rest) =>
          execute(rest, env.updated(id, UnknownValue))

        case Stmt.Put(ref, value, rest) =>
          escape(eval(value, env))
          execute(rest, env)

        case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
          escape(eval(ks1, env))
          escape(eval(k1, env))
          val bodyEnv = bindOpaque(env, List(p, ks, k))
          execute(resetBody, bodyEnv)

        case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
          escape(eval(Expr.Variable(prompt), env))
          escape(eval(ks1, env))
          escape(eval(k1, env))
          val bodyEnv = bindOpaque(env, List(resume, ks, k))
          execute(shiftBody, bodyEnv)

        case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
          escape(eval(Expr.Variable(resumption), env))
          escape(eval(ks1, env))
          escape(eval(k1, env))
          val bodyEnv = bindOpaque(env, List(ks, k))
          execute(resumeBody, bodyEnv)

        case Stmt.Hole(span) => ()
      }
    }

    private def analyze(): Unit = {
      val initialEnv = toplevelParams.iterator.map { id =>
        id -> AbstractValue.Term(Expr.Variable(id))
      }.toMap

      execute(body, initialEnv)

      @tailrec def saturate(): Unit =
        if entryQueue.nonEmpty then {
          val entry = entryQueue.dequeue()
          val info = definitions(entry.address.function)
          val parameterEnv = info.params.zip(entry.arguments).toMap
          val env = entry.captures ++ parameterEnv +
            (info.id -> AbstractValue.Closure(entry.address))
          execute(info.body, env)
          saturate()
        }

      saturate()

      // Even unreachable direct calls constrain the one syntactically named
      // local function. This also makes the rewrite total on dead code.
      sites.zipWithIndex.foreach { case (Site(_, callee, arity), site) =>
        definitions.get(callee).foreach { info =>
          targetsBySite.getOrElseUpdate(site, mutable.Set.empty) += callee
          if info.params.size != arity then rigidSites += site
        }
      }
    }


    // -----------------------------------------------------------------------
    // Reconstructible parameters

    private def definitelyEqual(left: AbstractValue, right: AbstractValue): Boolean =
      (left, right) match {
        case (AbstractValue.Term(x), AbstractValue.Term(y)) => x == y
        case (AbstractValue.Closure(a), AbstractValue.Closure(b)) =>
          a == b && definitions(a.function).singleAllocation
        case _ => false
      }

    private def commonExpression(
      rows: Set[(Env, AbstractValue)],
      visible: Vector[Id],
      allowNonVariable: Boolean
    ): Option[Expr] = {
      if rows.isEmpty then return None

      // Prefer the nearest visible variable. The replacement is interpreted in
      // each closure's own captured environment.
      visible.reverseIterator.find { candidate =>
        rows.forall { case (environment, value) =>
          environment.get(candidate).exists(definitelyEqual(value, _))
        }
      }.map(Expr.Variable.apply).orElse {
        val converted = rows.iterator.map { case (_, value) => reify(value) }.toVector

        // Every row must have a reification. Dropping the opaque rows here would
        // turn { 0, ? } into the unsound singleton { 0 }.
        if converted.forall(_.isDefined) then {
          converted.map(_.get).distinct.toList match {
            case expression :: Nil
                if expression.free.subsetOf(visible.toSet) &&
                   (expression.isInstanceOf[Expr.Variable] || allowNonVariable) =>
              Some(expression)
            case _ => None
          }
        } else None
      }
    }

    private def reconstruction(info: Definition, index: Int): Option[Expr] = {
      val rows = entriesByFunction.get(info.id).fold(Set.empty[Entry])(_.toSet).map { row =>
        row.captures -> row.arguments(index)
      }
      commonExpression(
        rows,
        info.visible,
        allowNonVariable = !callees.contains(info.params(index)))
    }

    private def bindingReconstruction(facts: BindingFacts): Option[Expr] =
      commonExpression(facts.observations.toSet, facts.scope, allowNonVariable = true)


    // -----------------------------------------------------------------------
    // Uniform calling conventions and dead arguments

    private case class Conventions(
      groups: Vector[Vector[Id]],
      groupOf: Map[Id, Int],
      blocked: Set[Int]
    )

    private def conventions(): Conventions = {
      val order = definitions.keysIterator.toVector
      val neighbours = order.iterator.map(id => id -> mutable.Set.empty[Id]).toMap

      // Functions that can occur at the same call site need one convention.
      targetsBySite.valuesIterator.foreach { targets =>
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

      val unsafeTargets = rigidSites.iterator.flatMap { site =>
        targetsBySite.get(site).iterator.flatMap(_.iterator)
      }
      val blockedFunctions = (rigidFunctions.iterator ++ unsafeTargets)
        .filter(definitions.contains)
        .toSet
      val incompatibleGroups = groups.iterator.zipWithIndex.collect {
        case (members, group)
            if members.map(definitions(_).params.size).distinct.size != 1 => group
      }.toSet
      val blocked = blockedFunctions.map(groupOf) ++ incompatibleGroups

      Conventions(groups, groupOf, blocked)
    }

    private def siteMasks(
      conventions: Conventions,
      groupMasks: Map[Int, Vector[Boolean]]
    ): Map[Int, Vector[Boolean]] = sites.iterator.zipWithIndex.map { case (Site(_, _, arity), site) =>
      val targets = targetsBySite.get(site).fold(Set.empty[Id])(_.toSet).filter(definitions.contains)
      val mask =
        if targets.isEmpty || rigidSites.contains(site) then
          Vector.fill(arity)(false)
        else {
          val group = conventions.groupOf(targets.head)
          assert(targets.forall(conventions.groupOf(_) == group))
          val groupMask = groupMasks(group)
          Vector.tabulate(arity)(i => groupMask.lift(i).getOrElse(false))
        }
      site -> mask
    }.toMap

    /**
     * Free variables after applying the current hypothetical drop masks.
     * Parameter uses are recorded before binding/removing the parameters of the
     * definition itself. This realizes the greatest fixed point for chains of
     * arguments that become dead together.
     */
    private def parameterUses(
      functionMasks: Map[Id, Vector[Boolean]],
      callMasks: Map[Int, Vector[Boolean]],
      reconstruct: Map[Id, Vector[Option[Expr]]]
    ): Map[Id, Vector[Boolean]] = {

      val result = mutable.Map.empty[Id, Vector[Boolean]]

      def visit(stmt: Stmt): Set[Id] = stmt match {
        case Stmt.Def(id, params, functionBody, rest) =>
          val bodyFree = visit(functionBody)
          result(id) = params.map(bodyFree.contains).toVector

          val mask = functionMasks(id)
          val replacements = reconstruct(id)

          val outerFree = params.zipWithIndex.foldLeft(bodyFree) {
            case (free, (param, index)) =>
              val withReplacement =
                if mask.lift(index).getOrElse(false) && free.contains(param) then {
                  replacements.lift(index).flatten.fold(free) { replacement =>
                    (free - param) ++ replacement.free
                  }
                } else free
              withReplacement - param
          }

          (outerFree - id) ++ (visit(rest) - id)

        case Stmt.New(id, interface, operations, rest) =>
          val operationFree = operations.iterator.flatMap { operation =>
            visit(operation.body) -- operation.params
          }.toSet
          operationFree ++ (visit(rest) - id)

        case Stmt.Let(id, binding, rest) => binding.free ++ (visit(rest) - id)

        case app @ Stmt.App(id, args, direct) =>
          val mask = callMasks(siteOf(app))
          Set(id) ++ args.zipWithIndex.iterator.collect {
            case (arg, index) if !mask.lift(index).getOrElse(false) => arg.free
          }.flatten.toSet

        case Stmt.Invoke(id, method, args) => Set(id) ++ args.flatMap(_.free)

        case Stmt.Run(id, callee, args, purity, rest) =>
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

        case Stmt.Hole(span) => Set.empty
      }

      visit(body)
      result.toMap
    }

    private def normalizeBindings(bindings: Map[Id, Expr]): Map[Id, Expr] = {
      def normalize(expr: Expr, seen: Set[Id]): Expr = expr match {
        case Expr.Variable(id) if bindings.contains(id) && !seen.contains(id) =>
          normalize(bindings(id), seen + id)
        case Expr.Make(data, tag, args) =>
          Expr.Make(data, tag, args.map(normalize(_, seen)))
        case other => other
      }

      bindings.map { case (id, expression) => id -> normalize(expression, Set(id)) }
    }

    def result(): DropInfo = {
      analyze()

      val reconstruct = definitions.valuesIterator.map { info =>
        info.id -> Vector.tabulate(info.params.size)(reconstruction(info, _))
      }.toMap

      val convention = conventions()

      // Begin with the greatest possible convention and remove positions that
      // cannot be justified. This also removes mutually dead argument cycles.
      val initialMasks = convention.groups.zipWithIndex.map { case (members, group) =>
        val arity = definitions(members.head).params.size
        group -> Vector.fill(arity)(!convention.blocked.contains(group))
      }.toMap

      @tailrec def greatestFixedPoint(
        groupMasks: Map[Int, Vector[Boolean]]
      ): Map[Int, Vector[Boolean]] = {
        val functionMasks = definitions.keysIterator.map { id =>
          id -> groupMasks(convention.groupOf(id))
        }.toMap
        val calls = siteMasks(convention, groupMasks)
        val uses = parameterUses(functionMasks, calls, reconstruct)

        val updated = convention.groups.zipWithIndex.map { case (members, group) =>
          val before = groupMasks(group)
          val after = before.indices.map { index =>
            before(index) && members.forall { id =>
              val isUsed = uses.get(id).flatMap(_.lift(index)).getOrElse(false)
              !isUsed || reconstruct(id)(index).isDefined
            }
          }.toVector
          group -> after
        }.toMap

        if updated == groupMasks then groupMasks else greatestFixedPoint(updated)
      }

      val groupMasks = greatestFixedPoint(initialMasks)
      val functionMasks = definitions.keysIterator.map { id =>
        id -> groupMasks(convention.groupOf(id))
      }.toMap
      val masksAtCalls = siteMasks(convention, groupMasks)
      val finalUses = parameterUses(functionMasks, masksAtCalls, reconstruct)

      val parameterBindings = definitions.valuesIterator.flatMap { info =>
        info.params.zipWithIndex.flatMap { case (param, index) =>
          val dropped = functionMasks(info.id)(index)
          val used = finalUses(info.id)(index)
          if dropped && used then {
            val replacement = reconstruct(info.id)(index).getOrElse {
              sys.error(s"Dropped used parameter ${util.show(param)} has no replacement")
            }
            Some(param -> replacement)
          } else None
        }
      }.toMap

      val letBindings = bindings.iterator.flatMap { case (id, facts) =>
        bindingReconstruction(facts).map(id -> _)
      }.toMap

      val rawBindings = parameterBindings ++ letBindings

      val callMaskTable = new IdentityHashMap[Stmt, Vector[Boolean]]()
      sites.zipWithIndex.foreach { case (Site(stmt, _, _), site) =>
        callMaskTable.put(stmt, masksAtCalls(site))
      }

      DropInfo(
        functionMasks,
        normalizeBindings(rawBindings),
        callMaskTable)
    }
  }


  // -------------------------------------------------------------------------
  // Public analysis entry point

  def solve(toplevel: ToplevelDefinition): DropInfo = toplevel match {
    case ToplevelDefinition.Def(id, params, body) =>
      Analysis(params, body).result()
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      DropInfo(Map.empty, Map.empty)
  }


  // -------------------------------------------------------------------------
  // Rewriting

  private def transform(expr: Expr, info: DropInfo): Expr = expr match {
    case Expr.Variable(id) => info.substitute(id)
    case Expr.Literal(value, tpe) => expr
    case Expr.Make(data, tag, args) => Expr.Make(data, tag, args.map(transform(_, info)))
    case Expr.Abort => expr
    case Expr.Return => expr
    case Expr.Toplevel => expr
  }

  private def transform(stmt: Stmt, info: DropInfo): Stmt = stmt match {
    case Stmt.Def(id, params, body, rest) =>
      val mask = info.functions(id)
      val keptParams = params.zipWithIndex.collect {
        case (param, index) if !mask.lift(index).getOrElse(false) => param
      }
      Stmt.Def(id, keptParams, transform(body, info), transform(rest, info))

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

      val keptArgs = args.zipWithIndex.collect {
        case (argument, index) if !mask.lift(index).getOrElse(false) =>
          transform(argument, info)
      }
      Stmt.App(callee, keptArgs, canBeDirect)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(transform(_, info)))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(transform(_, info)), purity, transform(rest, info))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond, info), transform(thn, info), transform(els, info))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        transform(scrutinee, info),
        clauses.map { case (id, clause) => id -> transform(clause, info) },
        default.map(transform(_, info)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, transform(ks, info), transform(rest, info))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, transform(init, info), region, transform(rest, info))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, transform(init, info), transform(ks, info), transform(rest, info))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, transform(rest, info))

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, transform(rest, info))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, transform(value, info), transform(rest, info))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, transform(body, info), transform(ks1, info), transform(k1, info))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, transform(body, info), transform(ks1, info), transform(k1, info))

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      Stmt.Resume(resumption, ks, k, transform(body, info), transform(ks1, info), transform(k1, info))

    case Stmt.Hole(span) => stmt
  }

  private def transform(operation: Operation, info: DropInfo): Operation = operation match {
    case Operation(name, params, body) => Operation(name, params, transform(body, info))
  }

  private def transform(clause: Clause, info: DropInfo): Clause = clause match {
    case Clause(params, body) => Clause(params, transform(body, info))
  }

  private def transform(top: ToplevelDefinition): ToplevelDefinition = top match {
    case ToplevelDefinition.Def(id, params, body) =>
      val info = solve(top)
      ToplevelDefinition.Def(id, params, transform(body, info))
    case ToplevelDefinition.Val(id, ks, k, binding) => top
  }

  def transform(module: ModuleDecl): ModuleDecl = module match {
    case ModuleDecl(includes, declarations, externs, definitions, exports) =>
      ModuleDecl(
        includes,
        declarations,
        externs,
        definitions.map(transform),
        exports)
  }
}
