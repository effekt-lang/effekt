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

/** A globally store-widened abstract machine for CPS.
 *
 *  The corresponding concrete machine has states
 *
 *    < statement, environment, store, time >
 *
 *  and values containing closures `<definition, environment>`, objects, and
 *  constructors. Its allocator is fresh. This machine is obtained by choosing
 *  a finite [[Context]], allocating addresses from syntax and context, and
 *  joining values stored at the same address. The store is then globalized;
 *  states that read a growing address are reconsidered until the least fixed
 *  point is reached.
 *
 *  CPS continuations are ordinary closures. A compositional [[Stmt.Call]] can
 *  reify its remainder as such a closure. Analyses that run after calling-
 *  convention lowering may instead approximate the direct-style result by an
 *  external value. Delimited control is deliberately opaque: values crossing
 *  it escape and values introduced by it are external.
 *
 *  A client supplies only an additional abstract property. The collecting
 *  control component is fixed here, so a property join can never accidentally
 *  discard a possible closure, operation, or constructor.
 */
abstract class AbstractMachine(module: ModuleDecl) {

  type Property
  type Context

  enum Address {
    case Binding(id: Id, context: Context)
    case Field(site: Site[Expr], index: Int, context: Context)
    case Cell(id: Id, context: Context)
    case External
  }

  case class Values(
    values: Set[Value],
    open: Boolean,
    property: Property
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
      environment: Map[Id, Address],
      context: Context
    ): Values = {
      val value: Closure = Closure(function, parameters, body, environment)
      Values(
        Set(value), open = false,
        AbstractMachine.this.closure(value, context))
    }

    def instance(
      interface: Id,
      operations: Map[Id, Closure],
      context: Context
    ): Values = {
      val value: Object = Object(interface, operations)
      Values(
        Set(value), open = false,
        AbstractMachine.this.instance(value, context))
    }

    def constructor(
      tpe: core.ValueType.Data,
      tag: Id,
      fields: List[Address],
      context: Context
    ): Values = {
      val value: Constructor = Constructor(tpe, tag, fields)
      Values(
        Set(value), open = false,
        AbstractMachine.this.constructor(value, context))
    }
  }

  def bottom: Property
  def external: Property
  def join(left: Property, right: Property): Property
  def subsumes(bigger: Property, smaller: Property): Boolean =
    join(bigger, smaller) == bigger

  def literal(value: Any, annotatedType: core.ValueType): Property
  def closure(value: Value.Closure, context: Context): Property
  def instance(value: Value.Object, context: Context): Property
  def constructor(value: Value.Constructor, context: Context): Property

  def initialContext: Context
  def tick(
    call: Option[Stmt],
    callee: Value.Closure,
    arguments: List[Values],
    caller: Context
  ): Context

  protected def observeApply(
    statement: Stmt,
    callee: Values,
    targets: Set[Id],
    arguments: List[Values],
    context: Context
  ): Unit = ()

  protected def observeInvoke(
    statement: Stmt,
    receiver: Values,
    method: Id,
    targets: Set[Id],
    arguments: List[Values],
    context: Context
  ): Unit = ()

  protected def observeMatch(
    statement: Stmt.Match,
    scrutinee: Values,
    context: Context
  ): Unit = ()

  protected def observeNew(
    statement: Stmt.New,
    instance: Values,
    context: Context
  ): Unit = ()

  protected def observeEscape(value: Values): Unit = ()
  protected def observeArityMismatch(target: Id): Unit = ()

  /** Analysis-specific information attached while a value crosses an address.
   *  The defaults leave the collecting semantics unchanged. */
  protected def readValue(address: Address, value: Values): Values = value
  protected def writeValue(address: Address, value: Values): Values = value

  /** Reify a CPS `Call` remainder. Turning this off is a sound widening used
   *  after direct calling conventions have been introduced. */
  protected def reifyContinuations: Boolean = true

  /** Register nested blocks before they are reached. This does not make their
   *  closures flow anywhere. */
  protected def registerNestedDefinitions: Boolean = false

  /** An additional initial configuration supplied by a focused analysis. */
  protected final case class InitialState(
    statement: Stmt,
    bindings: Map[Id, Values],
    context: Context
  )

  protected def entryPoints: List[(Id, List[Values])] =
    module.definitions.map {
      case ToplevelDefinition.Def(id, parameters, _) =>
        id -> List.fill(parameters.size)(externalValue)
      case ToplevelDefinition.Val(id, _, _, _) =>
        id -> List.fill(2)(externalValue)
    }

  protected def initialStates: List[InitialState] = Nil

  protected final def run(): Unit = execute()

  protected def valueAt(address: Address): Values = read(address)

  protected def parameterValue(id: Id, context: Context): Values =
    read(binding(id, context))

  protected final def parameterListsOf(function: Id): List[List[Id]] =
    blocks.get(function).fold(Nil)(_.toList.map(_.parameters))

  protected final def closuresOf(
    function: Id,
    environment: Map[Id, Address],
    context: Context
  ): Values = blocks.get(function).fold(bottomValue) { definitions =>
    definitions.foldLeft(bottomValue) { (result, definition) =>
      joinValue(result, Value.closure(
        function,
        definition.parameters,
        definition.body,
        environment,
        context))
    }
  }

  protected final def reifiedContinuationOf(call: Stmt.Call): Option[Id] =
    Option(continuations.get(call))


  // Private machine implementation

  private type Environment = Map[Id, Address]
  private case class Block(parameters: List[Id], body: Site[Stmt])

  /** A statement is a control-flow node. Reusing the same statement object
   *  consequently represents a join with several incoming edges. */
  private case class State(
    site: Site[Stmt],
    environment: Environment,
    context: Context
  ) {
    // we cache the hash for performance reasons
    private val hash = {
      var result = site.hashCode
      result = 31 * result + environment.hashCode
      31 * result + context.hashCode
    }

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
    Values(Set.empty, open = false, bottom)
  private lazy val externalValue =
    Values(Set.empty, open = true, external)

  private def joinValue(left: Values, right: Values): Values =
    Values(
      left.values ++ right.values,
      left.open || right.open,
      join(left.property, right.property))

  private def subsumesValue(bigger: Values, smaller: Values): Boolean =
    smaller.values.subsetOf(bigger.values) &&
      (bigger.open || !smaller.open) &&
      subsumes(bigger.property, smaller.property)

  private def read(address: Address): Values = address match {
    case Address.External => readValue(address, externalValue)
    case _ =>
      current.foreach { state =>
        readers.getOrElseUpdate(address, mutable.Set.empty) += state
        dependencies.getOrElseUpdate(state, mutable.Set.empty) += address
      }
      readValue(address, store.getOrElse(address, bottomValue))
  }

  private def write(address: Address, incoming: Values): Unit = {
    val value = writeValue(address, incoming)
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

  private def binding(id: Id, context: Context): Address =
    Address.Binding(id, context)

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
    environment: Environment,
    context: Context
  ): Values = expression match {
    case Expr.Variable(id) => read(environment.getOrElse(id, Address.External))
    case Expr.Literal(value, tpe) =>
      Values(Set.empty, open = false, literal(value, tpe))
    case make @ Expr.Make(data, tag, arguments) =>
      val site = Site[Expr](make)
      val fields = arguments.zipWithIndex.map { (argument, index) =>
        val address = Address.Field(site, index, context)
        write(address, eval(argument, environment, context))
        address
      }
      Value.constructor(data, tag, fields, context)
    case Expr.Abort | Expr.Toplevel => externalValue
  }

  private def applyClosure(
    target: Value.Closure,
    arguments: List[Values],
    caller: Context,
    call: Option[Stmt]
  ): Unit = {
    if target.parameters.size != arguments.size then
      observeArityMismatch(target.function)

    val context = tick(call, target, arguments, caller)
    val parameters = target.parameters.zip(arguments).map { (parameter, value) =>
      val address = binding(parameter, context)
      write(address, value)
      parameter -> address
    }

    reach(State(target.body, target.environment ++ parameters, context))
  }

  private def escape(value: Values): Unit = {
    val seenValues = mutable.Set.empty[Values]
    val seenAddresses = mutable.Set.empty[Address]
    val seenClosures = mutable.Set.empty[Value.Closure]

    def visitClosure(value: Value.Closure): Unit =
      if seenClosures.add(value) then
        applyClosure(
          value,
          List.fill(value.parameters.size)(externalValue),
          initialContext,
          None)

    def visit(value: Values): Unit =
      if seenValues.add(value) then {
        observeEscape(value)
        value.values.foreach {
          case closure: Value.Closure => visitClosure(closure)
          case objectValue: Value.Object =>
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
    arguments: List[Values],
    context: Context
  ): Unit = {
    val targets = callee.values.collect { case closure: Value.Closure => closure }
    observeApply(statement, callee, targets.map(_.function), arguments, context)
    targets.foreach(applyClosure(_, arguments, context, Some(statement)))

    if callee.open then arguments.foreach(escape)
  }

  private def invoke(
    statement: Stmt,
    receiver: Values,
    method: Id,
    arguments: List[Values],
    context: Context
  ): Unit = {
    val objects = receiver.values.collect { case value: Value.Object => value }
    val targets = objects.flatMap(_.operations.get(method))
    observeInvoke(statement, receiver, method, targets.map(_.function), arguments, context)
    targets.foreach(applyClosure(_, arguments, context, Some(statement)))

    if receiver.open || objects.exists(!_.operations.contains(method))
    then arguments.foreach(escape)
  }

  private def unknown(
    ids: IterableOnce[Id],
    environment: Environment,
    context: Context
  ): Environment = ids.iterator.foldLeft(environment) { (result, id) =>
    val address = binding(id, context)
    write(address, externalValue)
    result + (id -> address)
  }

  private def step(state: State): Unit = {
    val statement = state.site.value
    val environment = state.environment
    val context = state.context

    def next(statement: Stmt, environment: Environment = environment): Unit =
      reach(State(Site(statement), environment, context))

    statement match {
      case Stmt.Def(id, parameters, body, rest) =>
        val definition = define(id, parameters, body)
        val address = binding(id, context)
        val scope = environment + (id -> address)
        val captured = capture(body, scope)
        write(address, Value.closure(
          id,
          definition.parameters,
          definition.body,
          captured,
          context))
        next(rest, scope)

      case allocation @ Stmt.New(id, interface, operations, rest) =>
        val address = binding(id, context)
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
        val value = Value.instance(interface, implementations, context)
        write(address, value)
        observeNew(allocation, value, context)
        next(rest, scope)

      case Stmt.Let(id, expression, rest) =>
        val address = binding(id, context)
        write(address, eval(expression, environment, context))
        next(rest, environment + (id -> address))

      case call @ Stmt.Call(results, returnedKs, callee, arguments, ks, rest) =>
        val values = arguments.map(eval(_, environment, context))
        val meta = eval(ks, environment, context)
        val continuation =
          if reifyContinuations then {
            val id = reifyContinuation(call)
            val definition = define(id, results :+ returnedKs, rest)
            Value.closure(
              id,
              definition.parameters,
              definition.body,
              capture(rest, environment),
              context)
          } else externalValue
        val supplied = values ++ List(meta, continuation)

        callee match {
          case Callee.Function(id) =>
            applyValue(
              call,
              read(environment.getOrElse(id, Address.External)),
              supplied,
              context)
          case Callee.Method(receiver, method) =>
            invoke(
              call,
              read(environment.getOrElse(receiver, Address.External)),
              method,
              supplied,
              context)
        }

        if !reifyContinuations then
          next(rest, unknown(results :+ returnedKs, environment, context))

      case application @ Stmt.App(id, arguments) =>
        applyValue(
          application,
          read(environment.getOrElse(id, Address.External)),
          arguments.map(eval(_, environment, context)),
          context)

      case invocation @ Stmt.Invoke(id, method, arguments) =>
        invoke(
          invocation,
          read(environment.getOrElse(id, Address.External)),
          method,
          arguments.map(eval(_, environment, context)),
          context)

      case Stmt.Return(values) =>
        values.foreach(value => escape(eval(value, environment, context)))

      case Stmt.Run(id, _, arguments, _, rest) =>
        arguments.foreach(argument => escape(eval(argument, environment, context)))
        next(rest, unknown(List(id), environment, context))

      case Stmt.If(_, thn, els) =>
        next(thn)
        next(els)

      case selection @ Stmt.Match(scrutinee, clauses, default) =>
        val value = eval(scrutinee, environment, context)
        observeMatch(selection, value, context)

        value.values.foreach {
          case constructor: Value.Constructor =>
            clauses.find(_._1 == constructor.tag) match {
              case Some((_, Clause(parameters, body))) =>
                next(body, environment ++ parameters.zip(constructor.fields))
              case None => default.foreach(next(_))
            }
          case _ => ()
        }

        if value.open then {
          clauses.foreach { case (_, Clause(parameters, body)) =>
            next(body, unknown(parameters, environment, context))
          }
          default.foreach(next(_))
        }

      case Stmt.Region(id, ks, rest) =>
        escape(eval(ks, environment, context))
        next(rest, unknown(List(id), environment, context))

      case Stmt.Alloc(id, init, _, rest) =>
        val address = Address.Cell(id, context)
        write(address, eval(init, environment, context))
        next(rest, environment + (id -> address))

      case Stmt.Var(id, init, ks, rest) =>
        escape(eval(ks, environment, context))
        val address = Address.Cell(id, context)
        write(address, eval(init, environment, context))
        next(rest, environment + (id -> address))

      case Stmt.Dealloc(_, rest) => next(rest)

      case Stmt.Get(reference, id, rest) =>
        val address = binding(id, context)
        write(address, read(environment.getOrElse(reference, Address.External)))
        next(rest, environment + (id -> address))

      case Stmt.Put(reference, value, rest) =>
        write(
          environment.getOrElse(reference, Address.External),
          eval(value, environment, context))
        next(rest)

      case Stmt.Reset(prompt, ks, k, body, ks1, k1) =>
        escape(eval(ks1, environment, context))
        escape(eval(k1, environment, context))
        next(body, unknown(List(prompt, ks, k), environment, context))

      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        escape(read(environment.getOrElse(prompt, Address.External)))
        escape(eval(ks1, environment, context))
        escape(eval(k1, environment, context))
        next(body, unknown(List(resume, ks, k), environment, context))

      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        escape(read(environment.getOrElse(resumption, Address.External)))
        escape(eval(ks1, environment, context))
        escape(eval(k1, environment, context))
        next(body, unknown(List(ks, k), environment, context))

      case Stmt.Hole(_) => ()
    }
  }

  private def nestedDefinitions: List[(Id, List[Id], Stmt)] = {
    val result = mutable.ListBuffer.empty[(Id, List[Id], Stmt)]

    def visit(statement: Stmt): Unit = statement match {
      case Stmt.Def(id, parameters, body, rest) =>
        result += ((id, parameters, body)); visit(body); visit(rest)
      case Stmt.New(_, _, operations, rest) =>
        operations.foreach(operation => visit(operation.body)); visit(rest)
      case Stmt.Let(_, _, rest) => visit(rest)
      case Stmt.Call(_, _, _, _, _, rest) => visit(rest)
      case Stmt.Run(_, _, _, _, rest) => visit(rest)
      case Stmt.If(_, thn, els) => visit(thn); visit(els)
      case Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => visit(clause.body) }
        default.foreach(visit)
      case Stmt.Region(_, _, rest) => visit(rest)
      case Stmt.Alloc(_, _, _, rest) => visit(rest)
      case Stmt.Var(_, _, _, rest) => visit(rest)
      case Stmt.Dealloc(_, rest) => visit(rest)
      case Stmt.Get(_, _, rest) => visit(rest)
      case Stmt.Put(_, _, rest) => visit(rest)
      case Stmt.Reset(_, _, _, body, _, _) => visit(body)
      case Stmt.Shift(_, _, _, _, body, _, _) => visit(body)
      case Stmt.Resume(_, _, _, body, _, _) => visit(body)
      case _: Stmt.App | _: Stmt.Invoke | _: Stmt.Return | _: Stmt.Hole => ()
    }

    module.definitions.foreach {
      case ToplevelDefinition.Def(_, _, body) => visit(body)
      case ToplevelDefinition.Val(_, _, _, binding) => visit(binding)
    }
    result.toList
  }

  private def execute(): Unit = {
    val global: Environment = module.definitions.collect {
      case ToplevelDefinition.Def(id, _, _) => id -> binding(id, initialContext)
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
        write(binding(id, initialContext), Value.closure(
          id,
          definition.parameters,
          definition.body,
          environment,
          initialContext))

      case ToplevelDefinition.Val(id, ks, k, body) =>
        val definition = define(id, List(ks, k), body)
        entries(id) = Value.Closure(
          id,
          definition.parameters,
          definition.body,
          capture(body, global))
    }

    // Block lookup is syntactic; registering a nested definition does not make
    // its closure flow anywhere. Closures enter the store only when allocated.
    val nested = if registerNestedDefinitions then nestedDefinitions else Nil
    nested.foreach { case (id, parameters, body) => define(id, parameters, body) }

    entryPoints.foreach { case (id, arguments) =>
      entries.get(id) match {
        case Some(value) => applyClosure(value, arguments, initialContext, None)
        case None =>
          for
            definitions <- blocks.get(id)
            definition <- definitions
          do {
            val self = binding(id, initialContext)
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
              environment,
              initialContext))
            applyClosure(closure, arguments, initialContext, None)
          }
      }
    }

    initialStates.foreach { initial =>
      val environment = initial.bindings.map { case (id, value) =>
        val address = binding(id, initial.context)
        write(address, value)
        id -> address
      }
      reach(State(Site(initial.statement), environment, initial.context))
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
