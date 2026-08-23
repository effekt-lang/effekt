package effekt
package cps

import core.Id

import scala.collection.mutable


/** Path-dependent must equalities for local function parameters.
 *
 *  The analysis focuses one local closure allocation at a time. Whenever that
 *  closure is carried through a helper, the helper's entry is partitioned by
 *  the position and outer shape carrying it. The abstract store consequently
 *  keeps `apply(inc, 1)` and `apply(dec, 0)` separate.
 *
 *  The collecting component of [[AbstractMachine]] represents closures and
 *  their environments. The client property below therefore only needs the
 *  finite, flat domain of equality origins.
 */
object GuardedEquality {

  final case class FunctionFacts(
    id: Id,
    params: Vector[Id],
    entry: Vector[Option[Expr]]
  )

  private final case class Definition(
    id: Id,
    params: Vector[Id],
    rest: Stmt,
    scope: Vector[(Id, Seed)],
    captures: Vector[Id],
    recursive: Boolean
  )

  final case class Result(facts: Vector[FunctionFacts]) {
    def show: String = facts.map { function =>
      val entries = function.params.zip(function.entry).collect {
        case (param, Some(value)) => s"${name(param)} = ${showExpr(value)}"
      }
      s"${name(function.id)}:\n" +
        s"  entry ${if entries.isEmpty then "-" else entries.mkString(", ")}"
    }.mkString("\n")
  }


  // Finite equality origins

  private enum Origin {
    case Bottom
    case Equal(expr: Expr)
    case Unknown
  }

  /** Finite syntax used only to seed a focus's lexical environment. Runtime
   *  closures are represented exclusively by the machine's values. */
  private enum Seed {
    case Equal(expr: Expr)
    case Closure(function: Id, captures: Vector[Seed])
    case Unknown
  }

  /** The complete finite shape through which the focus enters a call. Keeping
   *  all witnesses together prevents one occurrence from contaminating
   *  another when the same closure is passed in several positions. */
  private type Guard = Set[(Int, Id)]
  private val Unguarded: Guard = Set.empty

  private enum Evidence {
    case Unseen
    case Equal(witnesses: Vector[Set[Expr]])
    case Unknown
  }

  private def meet(left: Origin, right: Origin): Origin = (left, right) match {
    case (Origin.Bottom, other) => other
    case (other, Origin.Bottom) => other
    case (a, b) if a == b => a
    case _ => Origin.Unknown
  }

  private def reify(origin: Origin): Option[Expr] = origin match {
    case Origin.Equal(expr) => Some(expr)
    case _ => None
  }

  private def name(id: Id): String = id.name.name

  private def showExpr(expr: Expr): String = expr match {
    case Expr.Variable(id) => name(id)
    case Expr.Literal(value: String, _) => s"\"$value\""
    case Expr.Literal(value, _) => value.toString
    case Expr.Make(_, tag, args) =>
      s"${name(tag)}(${args.map(showExpr).mkString(", ")})"
    case Expr.Abort => "abort"
    case Expr.Toplevel => "toplevel"
  }


  // Syntactic focus points

  private final class Metadata(toplevelParams: List[Id], body: Stmt) {
    val definitions = mutable.LinkedHashMap.empty[Id, Definition]
    val terms = mutable.Set.empty[Expr]
    val bindings = mutable.LinkedHashMap.empty[Id, Expr]

    private val initialScope = toplevelParams.toVector
    private val initialValues = initialScope.iterator.map { id =>
      id -> Seed.Equal(Expr.Variable(id))
    }.toMap

    collect(body, initialScope, initialValues)

    private def remember(expr: Expr): Unit = expr match {
      case _: Expr.Variable => ()
      case term @ Expr.Make(_, _, args) => terms += term; args.foreach(remember)
      case term => terms += term
    }

    private def reifySeed(seed: Seed): Option[Expr] = seed match {
      case Seed.Equal(expr) => Some(expr)
      case _ => None
    }

    private def eval(expr: Expr, environment: Map[Id, Seed]): Seed = expr match {
      case Expr.Variable(id) => environment.getOrElse(id, Seed.Unknown)
      case literal: Expr.Literal => Seed.Equal(literal)
      case expression @ Expr.Make(data, tag, arguments) =>
        val rebuilt = arguments.map(eval(_, environment)).map(reifySeed)
        if rebuilt.forall(_.isDefined) then {
          val term = Expr.Make(data, tag, rebuilt.map(_.get))
          if terms.contains(term) || term == expression then Seed.Equal(term)
          else Seed.Unknown
        } else Seed.Unknown
      case Expr.Abort => Seed.Equal(Expr.Abort)
      case Expr.Toplevel => Seed.Equal(Expr.Toplevel)
    }

    private def symbols(ids: IterableOnce[Id]): Map[Id, Seed] =
      ids.iterator.map(id => id -> Seed.Equal(Expr.Variable(id))).toMap

    private def collect(
      statement: Stmt,
      scope: Vector[Id],
      environment: Map[Id, Seed]
    ): Unit = statement match {
      case Stmt.Def(id, params, body, rest) =>
        val captures = scope.filter(body.free)
        val value = Seed.Closure(id, captures.map(environment.getOrElse(_, Seed.Unknown)))
        definitions(id) = Definition(
          id, params.toVector, rest,
          scope.map(variable => variable -> environment.getOrElse(variable, Seed.Unknown)),
          captures, body.free(id))
        collect(body, scope ++ Vector(id) ++ params, environment + (id -> value) ++ symbols(params))
        collect(rest, scope :+ id, environment + (id -> value))

      case Stmt.New(id, _, operations, rest) =>
        operations.foreach { operation =>
          collect(operation.body, scope ++ operation.params, environment ++ symbols(operation.params))
        }
        collect(rest, scope :+ id, environment + (id -> Seed.Unknown))

      case Stmt.Let(id, binding, rest) =>
        remember(binding)
        val value = eval(binding, environment)
        value match {
          case Seed.Equal(expression) => bindings(id) = expression
          case _ => ()
        }
        collect(rest, scope :+ id, environment + (id -> value))

      case Stmt.Call(results, returnedKs, _, args, ks, rest) =>
        args.foreach(remember); remember(ks)
        collect(rest, scope ++ results :+ returnedKs,
          environment ++ results.map(_ -> Seed.Unknown) + (returnedKs -> Seed.Unknown))

      case Stmt.App(_, args) => args.foreach(remember)

      case Stmt.Invoke(_, _, args) => args.foreach(remember)
      case Stmt.Return(values) => values.foreach(remember)

      case Stmt.Run(id, _, args, _, rest) =>
        args.foreach(remember)
        collect(rest, scope :+ id, environment + (id -> Seed.Unknown))

      case Stmt.If(condition, thn, els) =>
        remember(condition); collect(thn, scope, environment); collect(els, scope, environment)

      case Stmt.Match(scrutinee, clauses, default) =>
        remember(scrutinee)
        clauses.foreach { case (_, clause) =>
          collect(clause.body, scope ++ clause.params, environment ++ symbols(clause.params))
        }
        default.foreach(collect(_, scope, environment))

      case Stmt.Region(id, ks, rest) =>
        remember(ks); collect(rest, scope :+ id, environment + (id -> Seed.Unknown))
      case Stmt.Alloc(id, init, _, rest) =>
        remember(init); collect(rest, scope :+ id, environment + (id -> Seed.Unknown))
      case Stmt.Var(id, init, ks, rest) =>
        remember(init); remember(ks)
        collect(rest, scope :+ id, environment + (id -> Seed.Unknown))
      case Stmt.Dealloc(_, rest) => collect(rest, scope, environment)
      case Stmt.Get(_, id, rest) =>
        collect(rest, scope :+ id, environment + (id -> Seed.Unknown))
      case Stmt.Put(_, value, rest) => remember(value); collect(rest, scope, environment)

      case Stmt.Reset(p, ks, k, body, ks1, k1) =>
        remember(ks1); remember(k1)
        collect(body, scope ++ Vector(p, ks, k), environment ++ symbols(List(p, ks, k)))
      case Stmt.Shift(_, resume, ks, k, body, ks1, k1) =>
        remember(ks1); remember(k1)
        collect(body, scope ++ Vector(resume, ks, k), environment ++ symbols(List(resume, ks, k)))
      case Stmt.Resume(_, ks, k, body, ks1, k1) =>
        remember(ks1); remember(k1)
        collect(body, scope ++ Vector(ks, k), environment ++ symbols(List(ks, k)))
      case Stmt.Hole(_) => ()
    }
  }


  // Focused abstract machine

  private final class Focused(
    module: ModuleDecl,
    metadata: Metadata,
    observed: Definition
  ) extends AbstractMachine(module) {

    type Property = Origin
    type Context = Guard

    def bottom: Origin = Origin.Bottom
    def external: Origin = Origin.Unknown
    def join(left: Origin, right: Origin): Origin = meet(left, right)

    def literal(value: Any, annotatedType: core.ValueType): Origin =
      Origin.Equal(Expr.Literal(value, annotatedType))
    def closure(value: Value.Closure, context: Guard): Origin = Origin.Unknown
    def instance(value: Value.Object, context: Guard): Origin = Origin.Unknown

    def constructor(value: Value.Constructor, context: Guard): Origin = {
      val fields = value.fields.map(address => reify(valueAt(address).property))
      if fields.forall(_.isDefined) then {
        val term = Expr.Make(value.tpe, value.tag, fields.map(_.get))
        if metadata.terms.contains(term) then Origin.Equal(term) else Origin.Unknown
      } else Origin.Unknown
    }

    def initialContext: Guard = Unguarded

    private def containsObserved(value: Values): Boolean = {
      val seen = mutable.Set.empty[Address]

      def addressContains(address: Address): Boolean =
        seen.add(address) && valueContains(valueAt(address))
      def closureContains(closure: Value.Closure): Boolean =
        closure.function == observed.id || closure.environment.valuesIterator.exists(addressContains)
      def valueContains(value: Values): Boolean = value.values.exists {
        case closure: Value.Closure => closureContains(closure)
        case value: Value.Object =>
          value.operations.valuesIterator.exists(closureContains)
        case value: Value.Constructor => value.fields.exists(addressContains)
      }

      valueContains(value)
    }

    /** The outermost finite shape witnessing that a value carries the focus. */
    private def carriers(value: Values): Set[Id] =
      value.values.flatMap {
        case closure: Value.Closure if closure.function == observed.id =>
          Set(observed.id)
        case closure: Value.Closure if closure.environment.valuesIterator.exists { address =>
          containsObserved(valueAt(address))
        } => Set(closure.function)
        case value: Value.Object =>
          value.operations.valuesIterator.collect {
            case operation if operation.function == observed.id => observed.id
            case operation if operation.environment.valuesIterator.exists { address =>
              containsObserved(valueAt(address))
            } => operation.function
          }.toSet
        case value: Value.Constructor if value.fields.exists { address =>
          containsObserved(valueAt(address))
        } => Set(value.tag)
        case _ => Set.empty
      }

    def tick(
      call: Option[Stmt],
      callee: Value.Closure,
      arguments: List[Values],
      caller: Guard
    ): Guard = {
      val captures = metadata.definitions.get(callee.function).toList.flatMap { definition =>
        definition.captures.toList.map { id =>
          valueAt(callee.environment.getOrElse(id, Address.External))
        }
      }
      (captures ++ arguments).zipWithIndex.flatMap { case (value, position) =>
        carriers(value).map(position -> _)
      }.toSet
    }

    override protected def reifyContinuations: Boolean = false
    override protected def registerNestedDefinitions: Boolean = true
    override protected def entryPoints: List[(Id, List[Values])] = Nil

    private def lexicalValue(seed: Seed): Values = seed match {
      // A symbolic lexical value has unknown representation: it can itself be
      // called, while still denoting the same value within the focused scope.
      case Seed.Equal(expression @ Expr.Variable(_)) =>
        Values(Set.empty, open = true, Origin.Equal(expression))
      // The equality property remembers the constructor term, but it is not a
      // collecting constructor value with field addresses. Keep its runtime
      // representation open so that matching still explores every case.
      case Seed.Equal(expression: Expr.Make) =>
        Values(Set.empty, open = true, Origin.Equal(expression))
      case Seed.Equal(expression) =>
        Values(Set.empty, open = false, Origin.Equal(expression))
      case Seed.Unknown =>
        Values(Set.empty, open = true, Origin.Unknown)
      case Seed.Closure(function, _) =>
        val environment = metadata.definitions.get(function).toList.flatMap { definition =>
          val captures = definition.captures.map(id => id -> Address.Binding(id, Unguarded))
          if definition.recursive then
            captures :+ (function -> Address.Binding(function, Unguarded))
          else captures
        }.toMap
        closuresOf(function, environment, Unguarded)
    }

    private lazy val lexicalScope = observed.scope.map { case (id, seed) =>
      id -> lexicalValue(seed)
    }

    override protected def initialStates: List[InitialState] = {
      val bindings = mutable.LinkedHashMap.empty[Id, Values]

      def bind(id: Id, seed: Seed): Unit =
        if !bindings.contains(id) then {
          seed match {
            case Seed.Closure(function, captures) =>
              metadata.definitions.get(function).foreach { definition =>
                definition.captures.zip(captures).foreach(bind)
              }
            case _ => ()
          }
          bindings(id) = lexicalValue(seed)
        }

      observed.scope.foreach(bind)
      val scope = observed.scope.toMap
      val self = Seed.Closure(observed.id, observed.captures.map { capture =>
        scope.getOrElse(capture, Seed.Unknown)
      })
      bind(observed.id, self)
      List(InitialState(observed.rest, bindings.toMap, Unguarded))
    }

    private var evidence: Evidence = Evidence.Unseen

    private def knownEqual(left: Values, right: Values): Boolean = {
      val equalOrigin = left.property match {
        case Origin.Bottom | Origin.Unknown => false
        case origin => origin == right.property
      }
      val equalFlow =
        !left.open && !right.open &&
          left.values.nonEmpty && left.values == right.values
      equalOrigin || equalFlow
    }

    private def witnesses(value: Values): Set[Expr] = {
      val lexical = lexicalScope.collect {
        case (id, candidate) if knownEqual(value, candidate) => Expr.Variable(id)
      }
      val term = value.property match {
        case Origin.Equal(expression) => List(expression)
        case _ => Nil
      }
      (lexical ++ term).toSet
    }

    private def observe(arguments: List[Values]): Unit = {
      if arguments.size != observed.params.size then {
        evidence = Evidence.Unknown
        return
      }

      val current = arguments.map(witnesses).toVector
      evidence = evidence match {
        case Evidence.Unseen => Evidence.Equal(current)
        case Evidence.Equal(previous) =>
          Evidence.Equal(previous.zip(current).map(_ intersect _))
        case Evidence.Unknown => Evidence.Unknown
      }
    }

    private def result: Vector[Option[Expr]] = evidence match {
      case Evidence.Unseen | Evidence.Unknown =>
        Vector.fill(observed.params.size)(None)
      case Evidence.Equal(entries) =>
        val lexical = observed.scope.map(_._1).reverseIterator.map(Expr.Variable.apply).toVector
        entries.map { witnesses =>
          lexical.find(witnesses).orElse(witnesses.find {
            case _: Expr.Variable => false
            case _ => true
          })
        }
    }

    override protected def observeApply(
      statement: Stmt,
      callee: Values,
      targets: Set[Id],
      arguments: List[Values],
      context: Guard
    ): Unit =
      if targets.contains(observed.id) then {
        observe(arguments)
        if callee.open then evidence = Evidence.Unknown
      }

    override protected def observeEscape(value: Values): Unit =
      // `AbstractMachine.escape` enters known escaping wrappers with external
      // arguments. Only escaping the focused closure itself loses the guard.
      if value.values.exists {
        case closure: Value.Closure => closure.function == observed.id
        case _ => false
      } then evidence = Evidence.Unknown

    override protected def observeArityMismatch(target: Id): Unit =
      if target == observed.id then evidence = Evidence.Unknown

    def entries: Vector[Option[Expr]] = {
      run()
      result
    }
  }


  // Public entry points

  private def lexical(seed: Seed, bindings: Map[Id, Expr]): Seed = seed match {
    case Seed.Equal(Expr.Variable(id)) =>
      bindings.get(id).fold(seed)(expression => lexicalExpression(expression, bindings))
    case Seed.Closure(function, captures) =>
      Seed.Closure(function, captures.map(lexical(_, bindings)))
    case other => other
  }

  private def lexicalExpression(expression: Expr, bindings: Map[Id, Expr]): Seed = expression match {
    case variable: Expr.Variable => lexical(Seed.Equal(variable), bindings)
    case literal: Expr.Literal => Seed.Equal(literal)
    case Expr.Make(data, tag, arguments) =>
      val values = arguments.map(lexicalExpression(_, bindings))
      val expressions = values.map {
        case Seed.Equal(expression) => Some(expression)
        case _ => None
      }
      if expressions.forall(_.isDefined) then
        Seed.Equal(Expr.Make(data, tag, expressions.map(_.get)))
      else Seed.Unknown
    case Expr.Abort => Seed.Equal(Expr.Abort)
    case Expr.Toplevel => Seed.Equal(Expr.Toplevel)
  }

  private def normalize(expression: Expr, bindings: Map[Id, Expr]): Expr =
    lexicalExpression(expression, bindings) match {
      case Seed.Equal(equal) => equal
      case _ => expression
    }

  def analyze(toplevel: ToplevelDefinition): Result = {
    val (params, body) = toplevel match {
      case ToplevelDefinition.Def(_, parameters, body) => parameters -> body
      case ToplevelDefinition.Val(_, ks, k, body) => List(ks, k) -> body
    }
    val module = ModuleDecl(Nil, Nil, Nil, List(toplevel), Nil)
    val metadata = Metadata(params, body)
    val lexicalBindings = metadata.bindings.toMap
    val knownBindings = mutable.LinkedHashMap.from(lexicalBindings)
    val facts = metadata.definitions.valuesIterator.map { definition =>
      val bindings = knownBindings.toMap
      val scope = definition.scope.map { case (id, seed) =>
        id -> lexical(seed, bindings)
      }
      val entry = Focused(module, metadata, definition.copy(scope = scope)).entries.map {
        // Retain the nearest lexical representative. Equalities established
        // for enclosing function parameters guide the analysis, but replacing
        // that representative would unnecessarily widen the closure's scope.
        _.map(normalize(_, lexicalBindings))
      }
      definition.params.zip(entry).foreach {
        case (parameter, Some(expression)) => knownBindings(parameter) = expression
        case _ => ()
      }
      FunctionFacts(definition.id, definition.params, entry)
    }.toVector

    Result(facts)
  }

}
