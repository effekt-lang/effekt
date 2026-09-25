package effekt
package cps

import core.Id
import scala.collection.mutable

/** The identity of a syntax node, independent of its structural equality. */
final class Site[T <: AnyRef](val value: T) {
  private val hash = System.identityHashCode(value)

  override def hashCode(): Int = hash

  override def equals(other: Any): Boolean = other match {
    case site: Site[?] => value eq site.value
    case _ => false
  }
}

/** Flow analysis for CPS.
 *
 *  The corresponding concrete machine has states
 *
 *    < statement, environment, store, time >
 *
 *  and values containing closures `<definition, environment>`, objects, and
 *  constructors. The analysis allocates one address per CPS binder and joins
 *  values stored at the same address. The store is globalized;
 *  states that read a growing address are reconsidered until the least fixed
 *  point is reached.
 *
 *  CPS continuations are ordinary closures. A compositional [[Stmt.Call]] can
 *  reify its remainder as such a closure. Analyses that run after calling-
 *  convention lowering may instead approximate the direct-style result by an
 *  external value. Delimited control is deliberately opaque: values crossing
 *  it escape and values introduced by it are external.
 *
 *  Besides collecting closures, objects, and constructors, the analysis
 *  records call targets, escaping functions, and the flow of values between
 *  bindings. These are precisely the observations needed by the CPS passes;
 *  there is deliberately no client-defined machine component.
 */
final class FlowAnalysis(
  module: ModuleDecl,
  roots: Option[Set[Id]] = None,
  reifyCalls: Boolean = false
) {

  enum Address {
    case Binding(id: Id)
    case Field(site: Site[Expr], index: Int)
    case Cell(id: Id)
    case External
  }

  case class Values(
    values: Set[Value],
    open: Boolean,
    sources: Set[Address]
  )

  enum Value {
    case Closure(
      function: Id,
      parameters: List[Id],
      body: Site[Stmt],
      environment: Map[Id, Address]
    )
    case Object(interface: Id, operations: Map[Id, Closure])
    case Constructor(
      tpe: core.ValueType.Data,
      tag: Id,
      fields: List[Address]
    )
  }
  object Value {

    def closure(
      function: Id,
      parameters: List[Id],
      body: Site[Stmt],
      environment: Map[Id, Address]
    ): Values = {
      val value: Closure = Closure(function, parameters, body, environment)
      Values(Set(value), open = false, Set.empty)
    }

    def instance(
      interface: Id,
      operations: Map[Id, Closure]
    ): Values = {
      val value: Object = Object(interface, operations)
      Values(Set(value), open = false, Set.empty)
    }

    def constructor(
      tpe: core.ValueType.Data,
      tag: Id,
      fields: List[Address]
    ): Values = {
      val value: Constructor = Constructor(tpe, tag, fields)
      Values(Set(value), open = false, Set.empty)
    }
  }

  final case class CallFlow(targets: Set[Id], open: Boolean)

  private val flows = new java.util.IdentityHashMap[Stmt, CallFlow]()
  private val escaped = mutable.Set.empty[Id]
  private val arityRigid = mutable.Set.empty[Id]
  private val demanded = mutable.Set.empty[Address]
  private val predecessors = mutable.Map.empty[Address, mutable.Set[Address]]
  private val successors = mutable.Map.empty[Address, mutable.Set[Address]]
  private val recursiveObjects = mutable.Set.empty[Id]
  private val calls = new java.util.IdentityHashMap[Stmt.Call, java.lang.Boolean]()

  private lazy val computed: Unit = execute()

  def flowAt(statement: Stmt): CallFlow = {
    computed
    Option(flows.get(statement)).getOrElse(CallFlow(Set.empty, open = false))
  }

  def targetsAt(statement: Stmt): Set[Id] = flowAt(statement).targets
  def closedAt(statement: Stmt): Boolean = !flowAt(statement).open

  def callFlows: Map[Site[Stmt], CallFlow] = {
    computed
    val result = mutable.Map.empty[Site[Stmt], CallFlow]
    val iterator = flows.entrySet().iterator()
    while iterator.hasNext do {
      val entry = iterator.next()
      result(Site(entry.getKey)) = entry.getValue
    }
    result.toMap
  }

  def escapedFunctions: Set[Id] = { computed; escaped.toSet }
  def rigidFunctions: Set[Id] = { computed; escaped.toSet ++ arityRigid }
  def demandedAddresses: Set[Address] = { computed; demanded.toSet }
  def addressFlows: Map[Address, Set[Address]] = {
    computed
    successors.iterator.map((source, targets) => source -> targets.toSet).toMap
  }
  def selfReferentialObjects: Set[Id] = { computed; recursiveObjects.toSet }
  def calledFunctions: Set[Id] = {
    computed
    val result = mutable.Set.empty[Id]
    val iterator = flows.values().iterator()
    while iterator.hasNext do result ++= iterator.next().targets
    result.toSet
  }
  def compositionalCalls: List[Stmt.Call] = {
    computed
    val result = mutable.ListBuffer.empty[Stmt.Call]
    val iterator = calls.keySet().iterator()
    while iterator.hasNext do result += iterator.next()
    result.toList
  }
  def valueAt(address: Address): Values = { computed; read(address) }
  def parameterListsOf(function: Id): List[List[Id]] = {
    computed
    blocks.get(function).fold(Nil)(_.toList.map(_.parameters))
  }
  def reifiedContinuationOf(call: Stmt.Call): Option[Id] = {
    computed
    Option(continuations.get(call))
  }


  // Private machine implementation

  private type Environment = Map[Id, Address]
  private case class Block(parameters: List[Id], body: Site[Stmt])

  /** A statement is a control-flow node. Reusing the same statement object
   *  consequently represents a join with several incoming edges. */
  private case class State(
    site: Site[Stmt],
    environment: Environment
  ) {
    // we cache the hash for performance reasons
    private val hash = 31 * site.hashCode + environment.hashCode

    override def hashCode(): Int = hash
  }

  private val blocks = mutable.Map.empty[Id, mutable.ListBuffer[Block]]
  private val store = mutable.Map.empty[Address, Values]

  private val pending = mutable.Queue.empty[State]
  private val scheduled = mutable.Set.empty[State]
  private val reached = mutable.Set.empty[State]
  private val readers = mutable.Map.empty[Address, mutable.Set[State]]
  private val dependencies = mutable.Map.empty[State, mutable.Set[Address]]
  private var current: Option[State] = None

  private def define(id: Id, parameters: List[Id], body: Stmt): Block = {
    val definitions = blocks.getOrElseUpdate(id, mutable.ListBuffer.empty)
    definitions.find(_.body.value eq body).getOrElse {
      val definition = Block(parameters, Site(body))
      definitions += definition
      definition
    }
  }

  private def schedule(state: State): Unit =
    if scheduled.add(state) then pending.enqueue(state)

  private def reach(state: State): Unit =
    if !reached.contains(state) then schedule(state)

  private def clearDependencies(state: State): Unit =
    dependencies.remove(state).foreach { addresses =>
      addresses.foreach { address =>
        readers.get(address).foreach { states =>
          states -= state
          if states.isEmpty then readers -= address
        }
      }
    }

  private lazy val bottomValue =
    Values(Set.empty, open = false, Set.empty)
  private lazy val externalValue =
    Values(Set.empty, open = true, Set.empty)

  private def joinValue(left: Values, right: Values): Values =
    Values(
      left.values ++ right.values,
      left.open || right.open,
      left.sources ++ right.sources)

  private def subsumesValue(bigger: Values, smaller: Values): Boolean =
    smaller.values.subsetOf(bigger.values) &&
      (bigger.open || !smaller.open) &&
      smaller.sources.subsetOf(bigger.sources)

  private def read(address: Address): Values = {
    val value = address match {
      case Address.External => externalValue
      case _ =>
        current.foreach { state =>
          readers.getOrElseUpdate(address, mutable.Set.empty) += state
          dependencies.getOrElseUpdate(state, mutable.Set.empty) += address
        }
        store.getOrElse(address, bottomValue)
    }
    value.copy(sources = Set(address))
  }

  private def write(address: Address, incoming: Values): Unit = {
    address match {
      case _: Address.Binding => incoming.sources.foreach(flow(_, address))
      case _ => ()
    }
    val value = incoming.copy(sources = Set.empty)
    address match {
    case Address.External => escape(value)
    case _ =>
      val previous = store.getOrElse(address, bottomValue)
      if !subsumesValue(previous, value) then {
        store(address) = joinValue(previous, value)
        readers.get(address).foreach(_.foreach(schedule))
      }
    }
  }

  private def binding(id: Id): Address = Address.Binding(id)

  private def flow(source: Address, target: Address): Unit = {
    val incoming = predecessors.getOrElseUpdate(target, mutable.Set.empty)
    if incoming.add(source) then {
      successors.getOrElseUpdate(source, mutable.Set.empty) += target
      if demanded(target) then demand(List(source))
    }
  }

  private def demand(addresses: IterableOnce[Address]): Unit = {
    val pending = mutable.Queue.empty[Address]
    addresses.iterator.foreach { address =>
      if demanded.add(address) then pending.enqueue(address)
    }
    while pending.nonEmpty do {
      predecessors.get(pending.dequeue()).foreach { sources =>
        sources.foreach { source =>
          if demanded.add(source) then pending.enqueue(source)
        }
      }
    }
  }

  private def record(statement: Stmt, targets: Set[Id], open: Boolean): Unit = {
    val previous = Option(flows.get(statement)).getOrElse(CallFlow(Set.empty, open = false))
    flows.put(statement, CallFlow(previous.targets ++ targets, previous.open || open))
    statement match {
      case call @ Stmt.Call(_, _, ReturnPoint.Bind(_, _, _, _)) =>
        calls.put(call, java.lang.Boolean.TRUE)
      case _ => ()
    }
  }

  private def capture(body: Stmt, environment: Environment): Environment =
    body.free.iterator.flatMap { id => environment.get(id).map(id -> _) }.toMap

  private val continuations = new java.util.IdentityHashMap[Stmt, Id]()

  private def reifyContinuation(call: Stmt): Id = {
    val previous = continuations.get(call)
    if previous != null then previous
    else {
      val result = Id("k")
      continuations.put(call, result)
      result
    }
  }

  private def eval(
    expression: Expr,
    environment: Environment
  ): Values = expression match {
    case Expr.Variable(id) => read(environment.getOrElse(id, Address.External))
    case Expr.Literal(value, tpe) =>
      Values(Set.empty, open = false, Set.empty)
    case make @ Expr.Make(data, tag, arguments) =>
      val site = Site[Expr](make)
      val fields = arguments.zipWithIndex.map { (argument, index) =>
        val address = Address.Field(site, index)
        write(address, eval(argument, environment))
        address
      }
      Value.constructor(data, tag, fields)
    case Expr.Abort | Expr.Toplevel => externalValue
  }

  private def applyClosure(
    target: Value.Closure,
    arguments: List[Values]
  ): Unit = {
    if target.parameters.size != arguments.size then
      arityRigid += target.function

    val parameters = target.parameters.zip(arguments).map { (parameter, value) =>
      val address = binding(parameter)
      write(address, value)
      parameter -> address
    }

    reach(State(target.body, target.environment ++ parameters))
  }

  private def escape(value: Values): Unit = {
    val seenValues = mutable.Set.empty[Values]
    val seenAddresses = mutable.Set.empty[Address]
    val seenClosures = mutable.Set.empty[Value.Closure]

    def visitClosure(value: Value.Closure): Unit =
      if seenClosures.add(value) then
        applyClosure(
          value,
          List.fill(value.parameters.size)(externalValue))

    def visit(value: Values): Unit =
      if seenValues.add(value) then {
        value.values.foreach {
          case closure: Value.Closure =>
            escaped += closure.function
            visitClosure(closure)
          case objectValue: Value.Object =>
            escaped ++= objectValue.operations.valuesIterator.map(_.function)
            objectValue.operations.valuesIterator.foreach(visitClosure)
          case constructor: Value.Constructor =>
            constructor.fields.foreach { address =>
              if seenAddresses.add(address) then visit(read(address))
            }
        }
      }

    visit(value)
  }

  private def applyValue(
    statement: Stmt,
    callee: Values,
    arguments: List[Values]
  ): Unit = {
    val targets = callee.values.collect { case closure: Value.Closure => closure }
    record(statement, targets.map(_.function), callee.open)
    targets.foreach(applyClosure(_, arguments))

    if callee.open then arguments.foreach(escape)
  }

  private def invoke(
    statement: Stmt,
    receiver: Values,
    method: Id,
    arguments: List[Values]
  ): Unit = {
    val objects = receiver.values.collect { case value: Value.Object => value }
    val targets = objects.flatMap(_.operations.get(method))
    demand(receiver.sources)
    record(statement, targets.map(_.function),
      receiver.open || objects.exists(!_.operations.contains(method)))
    targets.foreach(applyClosure(_, arguments))

    if receiver.open || objects.exists(!_.operations.contains(method))
    then arguments.foreach(escape)
  }

  private def unknown(
    ids: IterableOnce[Id],
    environment: Environment
  ): Environment = ids.iterator.foldLeft(environment) { (result, id) =>
    val address = binding(id)
    write(address, externalValue)
    result + (id -> address)
  }

  private def step(state: State): Unit = {
    val statement = state.site.value
    val environment = state.environment

    def next(statement: Stmt, environment: Environment = environment): Unit =
      reach(State(Site(statement), environment))

    statement match {
      case Stmt.Def(id, parameters, body, rest) =>
        val definition = define(id, parameters, body)
        val address = binding(id)
        val scope = environment + (id -> address)
        val captured = capture(body, scope)
        write(address, Value.closure(
          id,
          definition.parameters,
          definition.body,
          captured))
        next(rest, scope)

      case Stmt.New(id, interface, operations, rest) =>
        val address = binding(id)
        val scope = environment + (id -> address)
        val implementations = operations.map { operation =>
          val definition = define(operation.name, operation.params, operation.body)
          val implementation: Value.Closure = Value.Closure(
            operation.name,
            definition.parameters,
            definition.body,
            capture(operation.body, scope))
          operation.name -> implementation
        }.toMap
        val value = Value.instance(interface, implementations)
        write(address, value)
        if operations.exists(_.body.free.contains(id)) then recursiveObjects += id
        next(rest, scope)

      case Stmt.Let(id, expression, rest) =>
        val address = binding(id)
        write(address, eval(expression, environment))
        next(rest, environment + (id -> address))

      case call @ Stmt.Call(callee, arguments,
          ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
        val values = arguments.map(eval(_, environment))
        val meta = eval(ks, environment)
        val continuation =
          if reifyCalls then {
            val id = reifyContinuation(call)
            val definition = define(id, results :+ returnedKs, rest)
            Value.closure(
              id,
              definition.parameters,
              definition.body,
              capture(rest, environment))
          } else externalValue
        val supplied = values ++ List(meta, continuation)

        callee match {
          case Callee.Function(id) =>
            applyValue(
              call,
              read(environment.getOrElse(id, Address.External)),
              supplied)
          case Callee.Method(receiver, method) =>
            invoke(
              call,
              read(environment.getOrElse(receiver, Address.External)),
              method,
              supplied)
        }

        if !reifyCalls then
          next(rest, unknown(results :+ returnedKs, environment))

      case call @ Stmt.Call(callee, arguments, ReturnPoint.Direct(results, rest)) =>
        val supplied = arguments.map(eval(_, environment))
        callee match {
          case Callee.Function(id) =>
            applyValue(call, read(environment.getOrElse(id, Address.External)), supplied)
          case Callee.Method(receiver, method) =>
            invoke(call, read(environment.getOrElse(receiver, Address.External)), method, supplied)
        }
        next(rest, unknown(results, environment))

      case call @ Stmt.Call(callee, _, _: ReturnPoint.Tail | ReturnPoint.Jump) =>
        val supplied = call.knownArguments.map(eval(_, environment))
        callee match {
          case Callee.Function(id) =>
            applyValue(call, read(environment.getOrElse(id, Address.External)), supplied)
          case Callee.Method(receiver, method) =>
            invoke(call, read(environment.getOrElse(receiver, Address.External)), method, supplied)
        }

      case Stmt.Return(values) =>
        values.foreach(value => escape(eval(value, environment)))

      case Stmt.Run(id, _, arguments, _, rest) =>
        arguments.foreach(argument => escape(eval(argument, environment)))
        next(rest, unknown(List(id), environment))

      case Stmt.If(_, thn, els) =>
        next(thn)
        next(els)

      case Stmt.Match(scrutinee, clauses, default) =>
        val value = eval(scrutinee, environment)
        demand(value.sources)

        value.values.foreach {
          case constructor: Value.Constructor =>
            clauses.find(_._1 == constructor.tag) match {
              case Some((_, Clause(parameters, body))) =>
                val bindings = parameters.zip(constructor.fields).map { (parameter, field) =>
                  val address = binding(parameter)
                  write(address, read(field))
                  parameter -> address
                }
                next(body, environment ++ bindings)
              case None => default.foreach(next(_))
            }
          case _ => ()
        }

        if value.open then {
          clauses.foreach { case (_, Clause(parameters, body)) =>
            next(body, unknown(parameters, environment))
          }
          default.foreach(next(_))
        }

      case Stmt.Region(id, ks, rest) =>
        escape(eval(ks, environment))
        next(rest, unknown(List(id), environment))

      case Stmt.Alloc(id, init, _, rest) =>
        val address = Address.Cell(id)
        write(address, eval(init, environment))
        next(rest, environment + (id -> address))

      case Stmt.Var(id, init, ks, rest) =>
        escape(eval(ks, environment))
        val address = Address.Cell(id)
        write(address, eval(init, environment))
        next(rest, environment + (id -> address))

      case Stmt.Dealloc(_, rest) => next(rest)

      case Stmt.Get(reference, id, rest) =>
        val address = binding(id)
        write(address, read(environment.getOrElse(reference, Address.External)))
        next(rest, environment + (id -> address))

      case Stmt.Put(reference, value, rest) =>
        write(
          environment.getOrElse(reference, Address.External),
          eval(value, environment))
        next(rest)

      case Stmt.Reset(prompt, ks, k, body, ks1, k1) =>
        escape(eval(ks1, environment))
        escape(eval(k1, environment))
        next(body, unknown(List(prompt, ks, k), environment))

      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        escape(read(environment.getOrElse(prompt, Address.External)))
        escape(eval(ks1, environment))
        escape(eval(k1, environment))
        next(body, unknown(List(resume, ks, k), environment))

      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        escape(read(environment.getOrElse(resumption, Address.External)))
        escape(eval(ks1, environment))
        escape(eval(k1, environment))
        next(body, unknown(List(ks, k), environment))

      case Stmt.Hole(_) => ()
    }
  }

  private def execute(): Unit = {
    val global: Environment = module.definitions.collect {
      case ToplevelDefinition.Def(id, _, _) => id -> binding(id)
    }.toMap
    val entries = mutable.Map.empty[Id, Value.Closure]

    module.definitions.foreach {
      case ToplevelDefinition.Def(id, parameters, body) =>
        val definition = define(id, parameters, body)
        val environment = capture(body, global)
        val value: Value.Closure = Value.Closure(
          id,
          definition.parameters,
          definition.body,
          environment)
        entries(id) = value
        write(binding(id), Value.closure(
          id,
          definition.parameters,
          definition.body,
          environment))

      case ToplevelDefinition.Val(id, ks, k, body) =>
        val definition = define(id, List(ks, k), body)
        entries(id) = Value.Closure(
          id,
          definition.parameters,
          definition.body,
          capture(body, global))
    }

    val rootIds = roots.getOrElse(entries.keySet.toSet)
    val initializers = module.definitions.collect {
      case ToplevelDefinition.Val(id, _, _, _) => id
    }.toSet
    val entryPoints = rootIds ++ module.exports ++ initializers

    entryPoints.foreach { id =>
      val arity = module.definitions.collectFirst {
        case ToplevelDefinition.Def(`id`, parameters, _) => parameters.size
        case ToplevelDefinition.Val(`id`, _, _, _) => 2
      }.getOrElse(0)
      val arguments = List.fill(arity)(externalValue)
      entries.get(id) match {
        case Some(value) => applyClosure(value, arguments)
        case None =>
          for
            definitions <- blocks.get(id)
            definition <- definitions
          do {
            val self = binding(id)
            val environment = definition.body.value.free.iterator.map { free =>
              free -> (if free == id then self else Address.External)
            }.toMap
            val closure: Value.Closure = Value.Closure(
              id,
              definition.parameters,
              definition.body,
              environment)
            write(self, Value.closure(
              id,
              definition.parameters,
              definition.body,
              environment))
            applyClosure(closure, arguments)
          }
      }
    }

    while pending.nonEmpty do {
      val state = pending.dequeue()
      scheduled -= state
      reached += state
      clearDependencies(state)
      current = Some(state)
      try step(state)
      finally current = None
    }
  }

}
