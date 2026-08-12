package effekt
package cps

import core.Id

import java.util.IdentityHashMap
import scala.collection.mutable


/**
 * Must-equalities relative to a closure allocation.
 *
 * Instead of enumerating concrete abstract environments, the analysis names
 * values by variables visible at the observed definition. Calls through a
 * helper are partitioned by the helper position that contains the observed
 * closure. This is the guard that retains correlations such as
 *
 *   apply(inc, 1) | apply(dec, 0)
 *
 * without constructing a product of closure environments and arguments.
 * Each guarded function entry is a vector in a flat equality domain; joining
 * two entries keeps exactly the origins on which they agree.
 */
object GuardedEquality {

  /** Syntactic closure information shared by clients of the target analysis. */
  final case class LocalDefinition(
    id: Id,
    params: Vector[Id],
    body: Stmt,
    captures: Vector[Id]
  )

  /** The finite set of local definitions that can be called at one application. */
  final case class CallTargets(
    call: Stmt,
    callee: Id,
    arity: Int,
    targets: Set[Id],
    closed: Boolean
  )

  final case class TargetResult(
    localDefinitions: Vector[LocalDefinition],
    callTargets: Vector[CallTargets],
    rigidFunctions: Set[Id],
    escapedFunctions: Set[Id]
  ) {
    def isRigid(function: Id): Boolean = rigidFunctions.contains(function)
    def escapes(function: Id): Boolean = escapedFunctions.contains(function)
  }

  /** The two projections needed by static-argument specialization share the
   *  same metadata and target analysis. Keeping them together avoids solving
   *  the call graph twice. */
  final case class RecursiveAnalysis(
    staticParameters: Map[Id, Vector[Boolean]],
    targetFlows: Vector[TargetResult]
  )

  final case class FunctionFacts(
    id: Id,
    params: Vector[Id],
    entry: Vector[Option[Expr]],
    recursive: Vector[Boolean]
  )

  private[cps] final case class Definition(
    id: Id,
    params: Vector[Id],
    body: Stmt,
    rest: Stmt,
    scope: Vector[Id],
    scopeValues: Vector[Origin],
    captures: Vector[Id]
  )

  private[cps] final case class CallSite(
    stmt: Stmt,
    callee: Id,
    arity: Int
  )

  final class Result private[GuardedEquality] (
    val facts: Vector[FunctionFacts],
    private[cps] val definitions: Vector[Definition],
    private[cps] val sites: Vector[CallSite],
    private[cps] val targetsAt: Map[Int, Set[Id]],
    private[cps] val rigidSites: Set[Int],
    private[cps] val rigidFunctions: Set[Id],
    private[cps] val bindings: Map[Id, Expr],
    private val sitesByStmt: IdentityHashMap[Stmt, Integer]
  ) {
    private[cps] def siteOf(stmt: Stmt): Int = {
      val site = sitesByStmt.get(stmt)
      assert(site != null, "all call sites are registered before analysis")
      site.intValue
    }

    def show: String = facts.map { function =>
      val entries = function.params.zip(function.entry).collect {
        case (param, Some(value)) => s"${name(param)} = ${GuardedEquality.show(value)}"
      }
      val statics = function.params.zip(function.recursive).collect {
        case (param, true) => name(param)
      }
      s"${name(function.id)}:\n" +
        s"  entry ${if entries.isEmpty then "-" else entries.mkString(", ")}\n" +
        s"  recursive ${if statics.isEmpty then "-" else statics.mkString(", ")}"
    }.mkString("\n")
  }


  // -----------------------------------------------------------------------
  // Finite equality origins

  private[cps] enum Origin {
    /** The value of a variable in the observed closure's lexical activation. */
    case Symbol(id: Id)

    /** A pure term from the finite syntactic vocabulary of the program. */
    case Term(expr: Expr)

    /** The particular closure allocation relative to which we are analyzing. */
    case Observed

    /** A closure together with its relative lexical environment. */
    case Closure(function: Id, captures: Vector[Origin])

    /** No equality is known. `mayBeObserved` retains a lost closure guard. */
    case Unknown(mayBeObserved: Boolean)
  }

  private val Unknown = Origin.Unknown(false)

  private def mayBeObserved(origin: Origin): Boolean = origin match {
    case Origin.Observed => true
    case Origin.Closure(_, captures) => captures.exists(mayBeObserved)
    case Origin.Unknown(mayBeObserved) => mayBeObserved
    case _ => false
  }

  private def containsObserved(origin: Origin): Boolean = origin match {
    case Origin.Observed => true
    case Origin.Closure(_, captures) => captures.exists(containsObserved)
    case _ => false
  }

  private def knownEqual(left: Origin, right: Origin): Boolean = (left, right) match {
    case (Origin.Unknown(_), _) | (_, Origin.Unknown(_)) => false
    case _ => left == right
  }

  /** Meet in the flat must-equality domain. */
  private def meet(left: Origin, right: Origin): Origin = (left, right) match {
    case _ if left == right => left
    case (Origin.Closure(f, xs), Origin.Closure(g, ys))
        if f == g && xs.size == ys.size =>
      Origin.Closure(f, xs.zip(ys).map(meet))
    case _ => Origin.Unknown(mayBeObserved(left) || mayBeObserved(right))
  }

  private def reify(origin: Origin): Option[Expr] = origin match {
    case Origin.Symbol(id) => Some(Expr.Variable(id))
    case Origin.Term(expr) => Some(expr)
    case _ => None
  }

  private def name(id: Id): String = id.name.name

  private def show(expr: Expr): String = expr match {
    case Expr.Variable(id) => name(id)
    case Expr.Literal(value: String, _) => s"\"$value\""
    case Expr.Literal(value, _) => value.toString
    case Expr.Make(_, tag, args) =>
      s"${name(tag)}(${args.map(show).mkString(", ")})"
    case Expr.Abort => "abort"
    case Expr.Toplevel => "toplevel"
  }


  // -----------------------------------------------------------------------
  // Syntactic metadata

  private final class Metadata(toplevelParams: List[Id], val body: Stmt) {
    val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    val terms = mutable.Set.empty[Expr]
    val callees = mutable.Set.empty[Id]
    val bindings = mutable.LinkedHashMap.empty[Id, Expr]
    val sites = mutable.ArrayBuffer.empty[CallSite]
    val sitesByStmt = new IdentityHashMap[Stmt, Integer]()

    private val initialScope = toplevelParams.toVector
    private val initialValues = initialScope.iterator.map(id => id -> Origin.Symbol(id)).toMap

    collect(body, initialScope, initialValues)

    private def remember(expr: Expr): Unit = expr match {
      case _: Expr.Variable => ()
      case term @ Expr.Make(_, _, args) =>
        terms += term
        args.foreach(remember)
      case term => terms += term
    }

    private def eval(expr: Expr, env: Map[Id, Origin]): Origin = expr match {
      case Expr.Variable(id) => env.getOrElse(id, Unknown)
      case literal: Expr.Literal => Origin.Term(literal)
      case expression @ Expr.Make(data, tag, args) =>
        val values = args.map(eval(_, env))
        val rebuilt = values.map(reify)
        if rebuilt.forall(_.isDefined) then {
          val term = Expr.Make(data, tag, rebuilt.map(_.get))
          if terms.contains(term) || term == expression then Origin.Term(term) else Unknown
        } else Origin.Unknown(values.exists(mayBeObserved))
      case Expr.Abort => Origin.Term(Expr.Abort)
      case Expr.Toplevel => Origin.Term(Expr.Toplevel)
    }

    private def registerSite(stmt: Stmt, callee: Id, arity: Int): Unit =
      if sitesByStmt.get(stmt) == null then {
        val site = sites.size
        sitesByStmt.put(stmt, site)
        sites += CallSite(stmt, callee, arity)
      }

    private def symbols(ids: IterableOnce[Id]): Map[Id, Origin] =
      ids.iterator.map(id => id -> Origin.Symbol(id)).toMap

    private def collect(
      stmt: Stmt,
      scope: Vector[Id],
      env: Map[Id, Origin]
    ): Unit = stmt match {
      case Stmt.Def(id, params, functionBody, rest) =>
        val free = functionBody.free
        val captures = scope.filter(free)
        val closure = Origin.Closure(id, captures.map(env.getOrElse(_, Unknown)))
        definitions(id) = Definition(
          id,
          params.toVector,
          functionBody,
          rest,
          scope,
          scope.map(env.getOrElse(_, Unknown)),
          captures)

        val bodyScope = scope ++ Vector(id) ++ params
        val bodyEnv = env + (id -> closure) ++ symbols(params)
        collect(functionBody, bodyScope, bodyEnv)
        collect(rest, scope :+ id, env + (id -> closure))

      case Stmt.New(id, _, operations, rest) =>
        operations.foreach { operation =>
          collect(
            operation.body,
            scope ++ operation.params,
            env ++ symbols(operation.params))
        }
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.Let(id, binding, rest) =>
        remember(binding)
        val value = eval(binding, env)
        value match {
          case Origin.Symbol(alias) => bindings(id) = Expr.Variable(alias)
          case Origin.Term(expression) => bindings(id) = expression
          case _ => ()
        }
        collect(rest, scope :+ id, env + (id -> value))

      case call @ Stmt.Call(result, id, args, ks, rest) =>
        callees += id
        args.foreach(remember)
        remember(ks)
        registerSite(call, id, args.size + 2)
        collect(rest, scope :+ result, env + (result -> Unknown))

      case app @ Stmt.App(id, args) =>
        callees += id
        args.foreach(remember)
        registerSite(app, id, args.size)

      case Stmt.Invoke(id, _, args) =>
        callees += id
        args.foreach(remember)

      case Stmt.Return(value) =>
        remember(value)

      case Stmt.Run(id, callee, args, _, rest) =>
        callees += callee
        args.foreach(remember)
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.If(cond, thn, els) =>
        remember(cond)
        collect(thn, scope, env)
        collect(els, scope, env)

      case Stmt.Match(scrutinee, clauses, default) =>
        remember(scrutinee)
        clauses.foreach { case (_, clause) =>
          collect(
            clause.body,
            scope ++ clause.params,
            env ++ symbols(clause.params))
        }
        default.foreach(collect(_, scope, env))

      case Stmt.Region(id, ks, rest) =>
        remember(ks)
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.Alloc(id, init, _, rest) =>
        remember(init)
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.Var(id, init, ks, rest) =>
        remember(init)
        remember(ks)
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.Dealloc(_, rest) => collect(rest, scope, env)

      case Stmt.Get(_, id, rest) =>
        collect(rest, scope :+ id, env + (id -> Unknown))

      case Stmt.Put(_, value, rest) =>
        remember(value)
        collect(rest, scope, env)

      case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        val params = List(p, ks, k)
        collect(resetBody, scope ++ params, env ++ symbols(params))

      case Stmt.Shift(_, resume, ks, k, shiftBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        val params = List(resume, ks, k)
        collect(shiftBody, scope ++ params, env ++ symbols(params))

      case Stmt.Resume(_, ks, k, resumeBody, ks1, k1) =>
        remember(ks1)
        remember(k1)
        val params = List(ks, k)
        collect(resumeBody, scope ++ params, env ++ symbols(params))

      case Stmt.Hole(_) => ()
    }
  }


  // -----------------------------------------------------------------------
  // Finite target flow (0-CFA)

  private final case class TargetValue(functions: Set[Id], unknown: Boolean) {
    def join(other: TargetValue): TargetValue =
      TargetValue(functions ++ other.functions, unknown || other.unknown)
  }

  private object TargetValue {
    val Empty = TargetValue(Set.empty, unknown = false)
    val Unknown = TargetValue(Set.empty, unknown = true)
    def function(id: Id): TargetValue = TargetValue(Set(id), unknown = false)
  }

  private final class TargetAnalysis(meta: Metadata, toplevelParams: List[Id]) {
    private val definitions = meta.definitions
    private val allocations = mutable.Map.empty[Id, Vector[TargetValue]]
    private val arguments = mutable.Map.empty[Id, Vector[TargetValue]]
    private val processed = mutable.Map.empty[Id, Vector[TargetValue]]
    private val queue = mutable.Queue.empty[Id]
    private val queued = mutable.Set.empty[Id]

    private val mutableTargets = mutable.Map.empty[Int, mutable.Set[Id]]
    private val mutableRigidSites = mutable.Set.empty[Int]
    private val mutableRigidFunctions = mutable.Set.empty[Id]
    private val mutableEscapedFunctions = mutable.Set.empty[Id]

    execute(meta.body, toplevelParams.iterator.map(_ -> TargetValue.Unknown).toMap)
    registerDirectTargets()
    saturate()

    val targetsAt: Map[Int, Set[Id]] =
      mutableTargets.iterator.map((site, targets) => site -> targets.toSet).toMap
    val rigidSites: Set[Int] = mutableRigidSites.toSet
    val rigidFunctions: Set[Id] = mutableRigidFunctions.toSet
    val escapedFunctions: Set[Id] = mutableEscapedFunctions.toSet

    /** Transitive closure of the closure-capture graph. The target fixed point
     *  is complete before relative equality queries begin, so this graph is
     *  immutable and can be shared by every observed-closure analysis. */
    private lazy val transitivelyCaptured: Map[Id, Set[Id]] = {
      val direct = allocations.iterator.map { case (function, captures) =>
        function -> captures.iterator.flatMap(_.functions).toSet
      }.toMap

      definitions.keysIterator.map { function =>
        val found = mutable.Set.empty[Id]
        val todo = mutable.Queue.from(direct.getOrElse(function, Set.empty))

        while todo.nonEmpty do {
          val current = todo.dequeue()
          if found.add(current) then
            todo.enqueueAll(direct.getOrElse(current, Set.empty))
        }

        function -> found.toSet
      }.toMap
    }

    /** Which captures may transitively enclose a particular local closure. */
    def capturesMayContain(function: Id, observed: Id): Vector[Boolean] =
      allocations.get(function).fold(Vector.empty) { captures =>
        captures.map { value =>
          value.functions.exists { target =>
            target == observed ||
              transitivelyCaptured.getOrElse(target, Set.empty).contains(observed)
          }
        }
      }

    private def siteOf(stmt: Stmt): Int = {
      val site = meta.sitesByStmt.get(stmt)
      assert(site != null)
      site.intValue
    }

    private def merge(
      previous: Option[Vector[TargetValue]],
      values: Vector[TargetValue]
    ): Vector[TargetValue] = previous match {
      case None => values
      case Some(before) =>
        if before.size != values.size then before
        else before.zip(values).map(_ join _)
    }

    private def schedule(id: Id): Unit =
      if allocations.contains(id) && arguments.contains(id) && queued.add(id) then
        queue.enqueue(id)

    private def addAllocation(id: Id, values: Vector[TargetValue]): Unit = {
      val updated = merge(allocations.get(id), values)
      if allocations.get(id).forall(_ != updated) then {
        allocations(id) = updated
        schedule(id)
      }
    }

    private def addArguments(id: Id, values: Vector[TargetValue]): Unit = {
      val info = definitions(id)
      if values.size != info.params.size then makeRigid(id)
      else {
        val updated = merge(arguments.get(id), values)
        if arguments.get(id).forall(_ != updated) then {
          arguments(id) = updated
          schedule(id)
        }
      }
    }

    private def makeRigid(id: Id): Unit =
      if definitions.contains(id) && mutableRigidFunctions.add(id) then
        addArguments(id, Vector.fill(definitions(id).params.size)(TargetValue.Unknown))

    private def escape(value: TargetValue): Unit =
      value.functions.foreach { function =>
        mutableEscapedFunctions += function
        makeRigid(function)
      }

    private def eval(expr: Expr, env: Map[Id, TargetValue]): TargetValue = expr match {
      case Expr.Variable(id) => env.getOrElse(id, TargetValue.Unknown)
      case _: Expr.Literal => TargetValue.Empty
      case Expr.Make(_, _, args) =>
        args.iterator.map(eval(_, env)).foldLeft(TargetValue.Empty)(_ join _)
      case Expr.Abort | Expr.Toplevel => TargetValue.Empty
    }

    private def unknowns(ids: IterableOnce[Id]): Map[Id, TargetValue] =
      ids.iterator.map(_ -> TargetValue.Unknown).toMap

    private def call(
      site: Int,
      callee: TargetValue,
      args: Vector[TargetValue]
    ): Unit = {
      val targets = mutableTargets.getOrElseUpdate(site, mutable.Set.empty)
      targets ++= callee.functions.filter(definitions.contains)

      callee.functions.filter(definitions.contains).foreach(addArguments(_, args))

      if callee.unknown then {
        mutableRigidSites += site
        callee.functions.foreach(makeRigid)
        args.foreach(escape)
      }
    }

    private def execute(stmt: Stmt, env: Map[Id, TargetValue]): Unit = stmt match {
      case Stmt.Def(id, _, _, rest) =>
        val info = definitions(id)
        addAllocation(id, info.captures.map(env.getOrElse(_, TargetValue.Unknown)))
        execute(rest, env + (id -> TargetValue.function(id)))

      case Stmt.New(id, _, operations, rest) =>
        operations.foreach { operation =>
          execute(operation.body, env ++ unknowns(operation.params))
        }
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.Let(id, binding, rest) =>
        execute(rest, env + (id -> eval(binding, env)))

      case application @ Stmt.Call(result, id, args, ks, rest) =>
        call(
          siteOf(application),
          env.getOrElse(id, TargetValue.Unknown),
          args.map(eval(_, env)).toVector :+
            eval(ks, env) :+ TargetValue.Unknown)
        execute(rest, env + (result -> TargetValue.Unknown))

      case app @ Stmt.App(id, args) =>
        call(siteOf(app), env.getOrElse(id, TargetValue.Unknown), args.map(eval(_, env)).toVector)

      case Stmt.Invoke(id, _, args) =>
        escape(env.getOrElse(id, TargetValue.Unknown))
        args.foreach(arg => escape(eval(arg, env)))

      case Stmt.Return(value) =>
        escape(eval(value, env))

      case Stmt.Run(id, callee, args, _, rest) =>
        escape(env.getOrElse(callee, TargetValue.Unknown))
        args.foreach(arg => escape(eval(arg, env)))
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.If(_, thn, els) =>
        execute(thn, env)
        execute(els, env)

      case Stmt.Match(scrutinee, clauses, default) =>
        escape(eval(scrutinee, env))
        clauses.foreach { case (_, clause) =>
          execute(clause.body, env ++ unknowns(clause.params))
        }
        default.foreach(execute(_, env))

      case Stmt.Region(id, ks, rest) =>
        escape(eval(ks, env))
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.Alloc(id, init, _, rest) =>
        escape(eval(init, env))
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.Var(id, init, ks, rest) =>
        escape(eval(init, env))
        escape(eval(ks, env))
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.Dealloc(_, rest) => execute(rest, env)

      case Stmt.Get(_, id, rest) =>
        execute(rest, env + (id -> TargetValue.Unknown))

      case Stmt.Put(_, value, rest) =>
        escape(eval(value, env))
        execute(rest, env)

      case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
        escape(eval(ks1, env))
        escape(eval(k1, env))
        execute(resetBody, env ++ unknowns(List(p, ks, k)))

      case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
        escape(env.getOrElse(prompt, TargetValue.Unknown))
        escape(eval(ks1, env))
        escape(eval(k1, env))
        execute(shiftBody, env ++ unknowns(List(resume, ks, k)))

      case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
        escape(env.getOrElse(resumption, TargetValue.Unknown))
        escape(eval(ks1, env))
        escape(eval(k1, env))
        execute(resumeBody, env ++ unknowns(List(ks, k)))

      case Stmt.Hole(_) => ()
    }

    private def saturate(): Unit =
      while queue.nonEmpty do {
        val id = queue.dequeue()
        queued -= id
        val info = definitions(id)
        val input = allocations(id) ++ arguments(id)
        if processed.get(id).forall(_ != input) then {
          processed(id) = input
          val env = (info.captures ++ info.params).zip(input).toMap +
            (id -> TargetValue.function(id))
          execute(info.body, env)
        }
      }

    private def registerDirectTargets(): Unit =
      meta.sites.zipWithIndex.foreach { case (site, index) =>
        definitions.get(site.callee).foreach { info =>
          mutableTargets.getOrElseUpdate(index, mutable.Set.empty) += info.id
          if site.arity != info.params.size then {
            mutableRigidSites += index
            makeRigid(info.id)
          }
        }
      }
  }


  // -----------------------------------------------------------------------
  // Guarded relative equality queries

  private final class EntryObservation(
    info: Definition,
    scopeValues: Vector[Origin],
    callees: Set[Id]
  ) {
    private val candidates = Array.fill(info.params.size)(info.scope.indices.toVector)
    private val commonTerms = Array.fill[Option[Expr]](info.params.size)(None)
    private var seen = false
    var unsafe = false

    def observe(args: Vector[Origin]): Unit = {
      if args.size != info.params.size then {
        unsafe = true
        return
      }

      args.indices.foreach { index =>
        val value = args(index)
        candidates(index) = candidates(index).filter { candidate =>
          knownEqual(value, scopeValues(candidate))
        }

        val term = value match {
          case Origin.Term(expr) => Some(expr)
          case _ => None
        }
        commonTerms(index) =
          if !seen then term else commonTerms(index).filter(term.contains)
      }
      seen = true
    }

    def result(safe: Boolean): Vector[Option[Expr]] =
      if !seen || !safe then Vector.fill(info.params.size)(None)
      else Vector.tabulate(info.params.size) { index =>
        candidates(index).lastOption
          .map(candidate => Expr.Variable(info.scope(candidate)))
          .orElse {
            commonTerms(index).filter { expression =>
              !callees.contains(info.params(index)) || expression.isInstanceOf[Expr.Variable]
            }
          }
      }
  }

  private final class RecursiveObservation(info: Definition) {
    private val invariant = Array.fill(info.params.size)(true)
    private var seen = false
    var unsafe = false

    def observe(args: Vector[Origin], direct: Boolean): Unit = {
      seen = true
      if !direct || args.size != info.params.size then {
        java.util.Arrays.fill(invariant, false)
      } else {
        args.indices.foreach { index =>
          if args(index) != Origin.Symbol(info.params(index)) then invariant(index) = false
        }
      }
    }

    def result(safe: Boolean): Vector[Boolean] =
      if !seen || !safe then Vector.fill(info.params.size)(false)
      else invariant.toVector
  }

  private final case class SummaryKey(
    function: Id,
    guard: Option[(Int, Id)]
  )

  private final class RelativeAnalysis(
    meta: Metadata,
    targets: TargetAnalysis,
    observed: Definition,
    knownBindings: Map[Id, Expr]
  ) {
    private val summaries = mutable.Map.empty[SummaryKey, Vector[Origin]]
    private val queue = mutable.Queue.empty[SummaryKey]
    private val queued = mutable.Set.empty[SummaryKey]

    private def siteOf(stmt: Stmt): Int = {
      val site = meta.sitesByStmt.get(stmt)
      assert(site != null)
      site.intValue
    }

    private def eval(expr: Expr, env: Map[Id, Origin]): Origin = expr match {
      case Expr.Variable(id) => env.getOrElse(id, Unknown)
      case literal: Expr.Literal => Origin.Term(literal)
      case expression @ Expr.Make(data, tag, args) =>
        val values = args.map(eval(_, env))
        val rebuilt = values.map(reify)
        if rebuilt.forall(_.isDefined) then {
          val term = Expr.Make(data, tag, rebuilt.map(_.get))
          if meta.terms.contains(term) || term == expression then Origin.Term(term)
          else Unknown
        } else Origin.Unknown(values.exists(mayBeObserved))
      case Expr.Abort => Origin.Term(Expr.Abort)
      case Expr.Toplevel => Origin.Term(Expr.Toplevel)
    }

    private def lexicalValue(origin: Origin): Origin = origin match {
      case Origin.Symbol(id) =>
        knownBindings.get(id).fold(origin)(lexicalExpression)
      case Origin.Closure(function, captures) =>
        Origin.Closure(function, captures.map(lexicalValue))
      case other => other
    }

    private def lexicalExpression(expr: Expr): Origin = expr match {
      case Expr.Variable(id) => lexicalValue(Origin.Symbol(id))
      case literal: Expr.Literal => Origin.Term(literal)
      case Expr.Make(data, tag, args) =>
        val values = args.map(lexicalExpression)
        val rebuilt = values.map(reify)
        if rebuilt.forall(_.isDefined) then
          Origin.Term(Expr.Make(data, tag, rebuilt.map(_.get)))
        else Origin.Unknown(values.exists(mayBeObserved))
      case Expr.Abort => Origin.Term(Expr.Abort)
      case Expr.Toplevel => Origin.Term(Expr.Toplevel)
    }

    private lazy val scopeValues = observed.scopeValues.map(lexicalValue)

    private val canBeCalled =
      targets.targetsAt.valuesIterator.exists(_.contains(observed.id))

    private def unknowns(ids: IterableOnce[Id]): Map[Id, Origin] =
      ids.iterator.map(_ -> Unknown).toMap

    private def merge(previous: Vector[Origin], incoming: Vector[Origin]): Vector[Origin] =
      if previous.size != incoming.size then previous.map(_ => Origin.Unknown(true))
      else previous.zip(incoming).map(meet)

    /** Partition entries by every position known to contain the observed closure. */
    private def enqueue(function: Id, values: Vector[Origin]): Unit = {
      val guards = values.zipWithIndex.flatMap {
        case (Origin.Observed, position) => Some(position -> observed.id)
        case (Origin.Closure(target, _), position) if containsObserved(values(position)) =>
          Some(position -> target)
        case _ => None
      }
      val keys =
        if guards.nonEmpty then guards.map(guard => SummaryKey(function, Some(guard)))
        else Vector(SummaryKey(function, None))

      keys.foreach { key =>
        val updated = summaries.get(key).fold(values)(merge(_, values))
        if summaries.get(key).forall(_ != updated) then {
          summaries(key) = updated
          if queued.add(key) then queue.enqueue(key)
        }
      }
    }

    private def captures(
      target: Definition,
      env: Map[Id, Origin],
      precise: Boolean
    ): Vector[Origin] =
      if precise then target.captures.map(env.getOrElse(_, Unknown))
      else {
        val mayContain = targets.capturesMayContain(target.id, observed.id)
        Vector.tabulate(target.captures.size) { index =>
          Origin.Unknown(mayContain.lift(index).getOrElse(false))
        }
      }

    /** Analyze any local closure that an opaque context may invoke. */
    private def escape(origins: IterableOnce[Origin], markUnsafe: () => Unit): Unit =
      origins.iterator.foreach {
        case Origin.Observed => markUnsafe()
        case Origin.Closure(function, captures) if meta.definitions.contains(function) =>
          val arity = meta.definitions(function).params.size
          enqueue(function, captures ++ Vector.fill(arity)(Unknown))
        case Origin.Unknown(true) => markUnsafe()
        case _ => ()
      }

    private def executeCall(
      application: Stmt,
      id: Id,
      arguments: Vector[Origin],
      env: Map[Id, Origin],
      onObservedCall: (Vector[Origin], Boolean) => Unit,
      markUnsafe: () => Unit
    ): Unit = {
      val site = siteOf(application)
      val callee = env.getOrElse(id, Unknown)
      val possibleTargets = targets.targetsAt.getOrElse(site, Set.empty)

      def invoke(target: Definition, preciseCaptures: Boolean): Unit =
        if target.id == observed.id then
          onObservedCall(arguments, id == observed.id)
        else
          enqueue(target.id, captures(target, env, preciseCaptures) ++ arguments)

      def invokePossible(includeObserved: Boolean): Unit = {
        if includeObserved && possibleTargets.contains(observed.id) then
          onObservedCall(arguments, false)

        possibleTargets.iterator.filterNot(_ == observed.id).foreach { target =>
          meta.definitions.get(target).foreach(invoke(_, preciseCaptures = false))
        }

        if targets.rigidSites.contains(site) then escape(arguments, markUnsafe)
      }

      callee match {
        case Origin.Observed => onObservedCall(arguments, id == observed.id)
        case Origin.Closure(target, closureCaptures) if meta.definitions.contains(target) =>
          if target == observed.id then
            onObservedCall(arguments, id == observed.id)
          else
            enqueue(target, closureCaptures ++ arguments)
        case Origin.Symbol(target) if meta.definitions.contains(target) =>
          invoke(meta.definitions(target), preciseCaptures = true)

        case Origin.Symbol(_) => invokePossible(includeObserved = true)

        case Origin.Unknown(mayBeObserved) => invokePossible(mayBeObserved)

        case _ =>
          if targets.rigidSites.contains(site) then escape(arguments, markUnsafe)
      }
    }

    private def execute(
      stmt: Stmt,
      env: Map[Id, Origin],
      onObservedCall: (Vector[Origin], Boolean) => Unit,
      markUnsafe: () => Unit
    ): Unit = stmt match {
      case Stmt.Def(id, _, _, rest) if id == observed.id =>
        // A repeated allocation is analyzed relative to its own lexical scope.
        val fresh = env ++ observed.scope.zip(scopeValues) + (id -> Origin.Observed)
        execute(rest, fresh, onObservedCall, markUnsafe)

      case Stmt.Def(id, _, _, rest) =>
        val info = meta.definitions(id)
        val closure = Origin.Closure(id, info.captures.map(env.getOrElse(_, Unknown)))
        execute(rest, env + (id -> closure), onObservedCall, markUnsafe)

      case Stmt.New(id, _, operations, rest) =>
        operations.foreach { operation =>
          execute(
            operation.body,
            env ++ unknowns(operation.params),
            onObservedCall,
            markUnsafe)
        }
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.Let(id, binding, rest) =>
        execute(rest, env + (id -> eval(binding, env)), onObservedCall, markUnsafe)

      case application @ Stmt.Call(result, id, args, ks, rest) =>
        val arguments =
          args.map(eval(_, env)).toVector :+ eval(ks, env) :+ Unknown
        executeCall(application, id, arguments, env, onObservedCall, markUnsafe)
        execute(
          rest,
          env + (result -> Unknown),
          onObservedCall,
          markUnsafe)

      case app @ Stmt.App(id, args) =>
        executeCall(
          app, id, args.map(eval(_, env)).toVector, env,
          onObservedCall, markUnsafe)

      case Stmt.Invoke(id, _, args) =>
        escape(env.get(id).iterator ++ args.iterator.map(eval(_, env)), markUnsafe)

      case Stmt.Return(value) =>
        escape(Iterator(eval(value, env)), markUnsafe)

      case Stmt.Run(id, callee, args, _, rest) =>
        escape(env.get(callee).iterator ++ args.iterator.map(eval(_, env)), markUnsafe)
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.If(_, thn, els) =>
        execute(thn, env, onObservedCall, markUnsafe)
        execute(els, env, onObservedCall, markUnsafe)

      case Stmt.Match(scrutinee, clauses, default) =>
        escape(Iterator(eval(scrutinee, env)), markUnsafe)
        clauses.foreach { case (_, clause) =>
          execute(
            clause.body,
            env ++ unknowns(clause.params),
            onObservedCall,
            markUnsafe)
        }
        default.foreach(execute(_, env, onObservedCall, markUnsafe))

      case Stmt.Region(id, ks, rest) =>
        escape(Iterator(eval(ks, env)), markUnsafe)
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.Alloc(id, init, _, rest) =>
        escape(Iterator(eval(init, env)), markUnsafe)
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.Var(id, init, ks, rest) =>
        escape(Iterator(eval(init, env), eval(ks, env)), markUnsafe)
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.Dealloc(_, rest) => execute(rest, env, onObservedCall, markUnsafe)

      case Stmt.Get(_, id, rest) =>
        execute(rest, env + (id -> Unknown), onObservedCall, markUnsafe)

      case Stmt.Put(_, value, rest) =>
        escape(Iterator(eval(value, env)), markUnsafe)
        execute(rest, env, onObservedCall, markUnsafe)

      case Stmt.Reset(p, ks, k, resetBody, ks1, k1) =>
        escape(Iterator(eval(ks1, env), eval(k1, env)), markUnsafe)
        execute(resetBody, env ++ unknowns(List(p, ks, k)), onObservedCall, markUnsafe)

      case Stmt.Shift(prompt, resume, ks, k, shiftBody, ks1, k1) =>
        escape(
          Iterator(env.getOrElse(prompt, Unknown), eval(ks1, env), eval(k1, env)),
          markUnsafe)
        execute(
          shiftBody,
          env ++ unknowns(List(resume, ks, k)),
          onObservedCall,
          markUnsafe)

      case Stmt.Resume(resumption, ks, k, resumeBody, ks1, k1) =>
        escape(
          Iterator(env.getOrElse(resumption, Unknown), eval(ks1, env), eval(k1, env)),
          markUnsafe)
        execute(resumeBody, env ++ unknowns(List(ks, k)), onObservedCall, markUnsafe)

      case Stmt.Hole(_) => ()
    }

    private def drain(
      onObservedCall: (Vector[Origin], Boolean) => Unit,
      markUnsafe: () => Unit
    ): Unit =
      while queue.nonEmpty do {
        val key = queue.dequeue()
        queued -= key
        val info = meta.definitions(key.function)
        val values = summaries(key)
        val captured = values.take(info.captures.size)
        val self =
          if info.id == observed.id then Origin.Observed
          else Origin.Closure(info.id, captured)
        val env = (info.captures ++ info.params).zip(values).toMap + (info.id -> self)
        execute(info.body, env, onObservedCall, markUnsafe)
      }

    def entries(): Vector[Option[Expr]] = {
      if !canBeCalled || targets.rigidFunctions.contains(observed.id) then
        return Vector.fill(observed.params.size)(None)

      val observation = EntryObservation(observed, scopeValues, meta.callees.toSet)

      def onCall(args: Vector[Origin], direct: Boolean): Unit = {
        observation.observe(args)
        enqueue(
          observed.id,
          observed.captures.map { capture =>
            val index = observed.scope.indexOf(capture)
            if index >= 0 then scopeValues(index) else Unknown
          } ++ args)
      }

      val start = observed.scope.zip(scopeValues).toMap +
        (observed.id -> Origin.Observed)
      execute(observed.rest, start, onCall, () => observation.unsafe = true)
      drain(onCall, () => observation.unsafe = true)

      val safe = !targets.rigidFunctions.contains(observed.id) && !observation.unsafe
      observation.result(safe)
    }

    def recursive(): Vector[Boolean] = {
      if !canBeCalled || targets.rigidFunctions.contains(observed.id) then
        return Vector.fill(observed.params.size)(false)

      summaries.clear()
      queue.clear()
      queued.clear()

      val observation = RecursiveObservation(observed)
      val captures = observed.captures.iterator.map(_ -> Unknown).toMap
      val parameters = observed.params.iterator.map(id => id -> Origin.Symbol(id)).toMap
      val start = captures ++ parameters + (observed.id -> Origin.Observed)

      def onCall(args: Vector[Origin], direct: Boolean): Unit =
        observation.observe(args, direct)

      execute(observed.body, start, onCall, () => observation.unsafe = true)
      drain(onCall, () => observation.unsafe = true)

      val safe = !targets.rigidFunctions.contains(observed.id) && !observation.unsafe
      observation.result(safe)
    }
  }


  // -----------------------------------------------------------------------
  // Public entry points

  private def analyze(toplevelParams: List[Id], body: Stmt): Result = {
    val meta = Metadata(toplevelParams, body)
    val targets = TargetAnalysis(meta, toplevelParams)

    val knownBindings = mutable.LinkedHashMap.empty[Id, Expr]
    val facts = meta.definitions.valuesIterator.map { definition =>
      val relative = RelativeAnalysis(meta, targets, definition, knownBindings.toMap)
      val facts = FunctionFacts(
        definition.id,
        definition.params,
        relative.entries(),
        relative.recursive())
      definition.params.zip(facts.entry).foreach {
        case (param, Some(value)) => knownBindings(param) = value
        case _ => ()
      }
      facts
    }.toVector

    Result(
      facts,
      meta.definitions.valuesIterator.toVector,
      meta.sites.toVector,
      targets.targetsAt,
      targets.rigidSites,
      targets.rigidFunctions,
      meta.bindings.toMap,
      meta.sitesByStmt)
  }

  private def recursive(
    meta: Metadata,
    targets: TargetAnalysis
  ): Map[Id, Vector[Boolean]] =
    meta.definitions.valuesIterator.map { definition =>
      val relative = RelativeAnalysis(meta, targets, definition, Map.empty)
      definition.id -> relative.recursive()
    }.toMap

  private def targetResult(meta: Metadata, targets: TargetAnalysis): TargetResult = {
    val definitions = meta.definitions.valuesIterator.map { definition =>
      LocalDefinition(
        definition.id,
        definition.params,
        definition.body,
        definition.captures)
    }.toVector
    val calls = meta.sites.zipWithIndex.map { case (site, index) =>
      CallTargets(
        site.stmt,
        site.callee,
        site.arity,
        targets.targetsAt.getOrElse(index, Set.empty),
        closed = !targets.rigidSites.contains(index))
    }.toVector
    TargetResult(definitions, calls, targets.rigidFunctions, targets.escapedFunctions)
  }

  /** The finite call-target projection, without solving relative equalities
   *  for every local definition. */
  def targets(toplevel: ToplevelDefinition): TargetResult = {
    val (params, body) = toplevel match {
      case ToplevelDefinition.Def(_, params, body) => (params, body)
      case ToplevelDefinition.Val(_, ks, k, binding) => (List(ks, k), binding)
    }
    val meta = Metadata(params, body)
    val targets = TargetAnalysis(meta, params)
    targetResult(meta, targets)
  }

  def analyze(toplevel: ToplevelDefinition): Result = toplevel match {
    case ToplevelDefinition.Def(_, params, body) => analyze(params, body)
    case ToplevelDefinition.Val(_, ks, k, binding) => analyze(List(ks, k), binding)
  }

  /** Recursive must-equalities and call targets without computing entry
   *  equalities. Both projections use one target-analysis solution. */
  def analyzeRecursion(module: ModuleDecl): RecursiveAnalysis = {
    val statics = mutable.LinkedHashMap.empty[Id, Vector[Boolean]]
    val flows = module.definitions.map { toplevel =>
      val (params, body) = toplevel match {
        case ToplevelDefinition.Def(_, params, body) => (params, body)
        case ToplevelDefinition.Val(_, ks, k, binding) => (List(ks, k), binding)
      }
      val meta = Metadata(params, body)
      val targets = TargetAnalysis(meta, params)
      statics ++= recursive(meta, targets)
      targetResult(meta, targets)
    }.toVector
    RecursiveAnalysis(statics.toMap, flows)
  }
}
