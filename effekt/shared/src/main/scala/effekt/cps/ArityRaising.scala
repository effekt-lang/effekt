package effekt
package cps

import core.{ Id, ValueType }
import scala.collection.mutable

/** Monovariant arity raising.
 *
 *  One flow analysis determines where a value has one known
 *  constructor representation. Matches are representation demands: merely
 *  knowing a value's constructor does not justify changing its convention.
 *  Since compositional call remainders are reified as ordinary continuations,
 *  returned products are handled by exactly the same analysis.
 *
 *  The analysis also identifies object allocations observed through their
 *  operations. One representation-directed traversal then performs argument
 *  unbundling, return unbundling, and object splitting.
 */
object ArityRaising {

  enum Shape {
    case Unknown
    case Constructor(data: ValueType.Data, tag: Id, arity: Int)
  }

  type Entry = Vector[Shape]

  case class Analysis(entries: Map[Id, Entry]) {
    def show: String = entries.toList
      .filter(_._2.exists(_ != Shape.Unknown))
      .sortBy(_._1.id)
      .map { case (id, entry) =>
        s"${id.name.name}\n  ${showEntry(entry)}"
      }.mkString("\n")
  }

  private case class Plan(
    analysis: Analysis,
    calls: Map[Site[Stmt], Entry],
    continuations: Map[Site[Stmt], Entry],
    objects: Set[Id]
  )

  private def site(statement: Stmt): Site[Stmt] = Site(statement)

  private def showShape(shape: Shape): String = shape match {
    case Shape.Unknown => "?"
    case Shape.Constructor(_, tag, arity) =>
      s"${tag.name.name}(${List.fill(arity)("_").mkString(", ")})"
  }

  private def showEntry(entry: Entry): String =
    entry.map(showShape).mkString("<", ", ", ">")

  /** Collect values and representation observations once, then solve for one
   *  representation per monovariant calling convention. */
  private def representationPlan(
    module: ModuleDecl,
    roots: Set[Id]
  ): Plan = {
    val analysis = new FlowAnalysis(module, Some(roots), reifyCalls = true)
    import analysis.{ Address, Value }
    val flows = analysis.callFlows
    val demanded = analysis.demandedAddresses
    val successors = analysis.addressFlows

    def meet(left: Entry, right: Entry): Entry =
      if left.size != right.size then Vector.fill(left.size)(Shape.Unknown)
      else left.zip(right).map { case (l, r) => if l == r then l else Shape.Unknown }

    def meetShape(left: Shape, right: Shape): Shape =
      if left == right then left else Shape.Unknown

    def address(id: Id): Address = Address.Binding(id)

    def preferred(address: Address): Shape = address match {
      case Address.Binding(_) if demanded.contains(address) =>
        val values = analysis.valueAt(address)
        if values.open || values.values.isEmpty then Shape.Unknown
        else {
          val constructors = values.values.collect {
            case value: Value.Constructor => value
          }
          val shapes = constructors.map { value =>
            Shape.Constructor(value.tpe, value.tag, value.fields.size)
          }
          if constructors.size == values.values.size && shapes.size == 1
          then shapes.head
          else Shape.Unknown
        }
      case _ => Shape.Unknown
    }

    val continuationIds = mutable.Set.empty[Id]
    val continuationEntries = mutable.Map.empty[Site[Stmt], Entry]
    val initializers = module.definitions.collect {
      case ToplevelDefinition.Val(id, _, _, _) => id
    }.toSet
    val functions = mutable.Set.from(
      analysis.calledFunctions ++ roots ++ module.exports ++ initializers)
    val calls = analysis.compositionalCalls
    calls.foreach { call =>
      analysis.reifiedContinuationOf(call).foreach { id =>
        continuationIds += id
        functions += id
      }
    }

    val definitions = functions.iterator.flatMap { id =>
      val parameters = analysis.parameterListsOf(id)
      Option.when(parameters.nonEmpty)(id -> parameters)
    }.toMap

    val addresses = mutable.Set.empty[Address]
    definitions.valuesIterator.flatten.flatten.foreach(id => addresses += address(id))
    successors.foreach { case (source, targets) =>
      addresses += source
      addresses ++= targets
    }

    // Equality of calling conventions generates an equivalence relation on
    // addresses. Solve availability on its quotient rather than repeatedly
    // propagating agreement between individual parameters.
    val parent = mutable.Map.from(addresses.iterator.map(a => a -> a))
    val rank = mutable.Map.empty[Address, Int].withDefaultValue(0)

    def find(address: Address): Address = {
      val next = parent.getOrElseUpdate(address, address)
      if next == address then address
      else {
        val root = find(next)
        parent(address) = root
        root
      }
    }

    def union(left: Address, right: Address): Unit = {
      val l = find(left)
      val r = find(right)
      if l != r then {
        if rank(l) < rank(r) then parent(l) = r
        else {
          parent(r) = l
          if rank(l) == rank(r) then rank(l) += 1
        }
      }
    }

    val forced = mutable.Set.empty[Address]
    def agree(parameterLists: Iterable[List[Id]], open: Boolean): Unit = {
      val lists = parameterLists.toList
      if lists.nonEmpty then {
        if open || lists.map(_.size).distinct.size != 1 then
          lists.flatten.foreach(id => forced += address(id))
        else lists.transpose.foreach { parameters =>
          val members = parameters.iterator.map(address).toList.distinct
          members.tail.foreach(union(members.head, _))
        }
      }
    }

    // Definitions sharing a name (notably operations of one interface) and
    // all targets of an indirect call must expose one representation.
    definitions.values.foreach(agree(_, open = false))
    flows.values.foreach { flow =>
      agree(flow.targets.iterator.flatMap(id => definitions.getOrElse(id, Nil)).toList,
        flow.open)
    }
    addresses ++= forced

    val classes = addresses.groupBy(find)
    val representations: mutable.Map[Address, Shape] =
      mutable.Map.from(classes.iterator.map { case (root, members) =>
      val shapes: Iterator[Shape] = members.iterator.map { member =>
        if forced.contains(member) then Shape.Unknown: Shape else preferred(member)
      }
      root -> shapes.reduce(meetShape)
    })
    val quotient = mutable.Map.empty[Address, mutable.Set[Address]]
    successors.foreach { case (source, targets) =>
      val from = find(source)
      targets.foreach { target =>
        val to = find(target)
        if from != to then quotient.getOrElseUpdate(from, mutable.Set.empty) += to
      }
    }

    // A split producer can be materialized for a boxed consumer. The
    // converse is unavailable without introducing a new match, hence the
    // directed source-to-target constraint.
    val pending = mutable.Queue.from(classes.keys)
    val scheduled = mutable.Set.from(classes.keys)
    while pending.nonEmpty do {
      val source = pending.dequeue()
      scheduled -= source
      quotient.get(source).foreach { targets =>
        targets.foreach { target =>
          val previous = representations(target)
          val next = meetShape(previous, representations(source))
          if next != previous then {
            representations(target) = next
            if scheduled.add(target) then pending.enqueue(target)
          }
        }
      }
    }

    def entry(parameters: List[Id]): Entry =
      parameters.map(id => representations.getOrElse(find(address(id)), Shape.Unknown)).toVector

    val entries = definitions.iterator.flatMap { case (id, parameterLists) =>
      parameterLists.map(entry).reduceOption(meet).map(id -> _)
    }.toMap

    val callEntries = flows.iterator.flatMap { case (callSite, flow) =>
      val targetEntries = flow.targets.flatMap(entries.get)
      Option.when(!flow.open && targetEntries.size == 1) {
        callSite -> targetEntries.head
      }
    }.toMap

    calls.foreach { call =>
      analysis.reifiedContinuationOf(call).flatMap(entries.get).foreach { entry =>
        continuationEntries(site(call)) = entry
      }
    }

    Plan(
      Analysis(entries.toMap -- continuationIds),
      callEntries,
      continuationEntries.toMap,
      analysis.splitObjects)
  }

  def analyze(module: ModuleDecl, entrypoints: Set[Id]): Analysis =
    representationPlan(module, entrypoints).analysis

  private enum KnownValue {
    case Whole(expression: Expr)
    case Constructor(
      data: ValueType.Data,
      tag: Id,
      fields: List[KnownValue],
      whole: Option[Expr]
    )
  }

  private type KnownValues = Map[Id, KnownValue]
  private type Objects = Map[Id, Map[Id, Id]]

  private class Rewriter(plan: Plan) {

    import KnownValue.*

    private case class Raised(
      ids: List[Id],
      values: KnownValues,
      materializations: List[(Id, KnownValue.Constructor)]
    )

    private def materialize(value: KnownValue): Expr = value match {
      case Whole(expression) => expression
      case Constructor(data, tag, fields, whole) =>
        whole.getOrElse(Expr.Make(data, tag, fields.map(materialize)))
    }

    private def value(expression: Expr, values: KnownValues): KnownValue = expression match {
      case Expr.Variable(id) => values.getOrElse(id, Whole(expression))
      case Expr.Make(data, tag, arguments) =>
        Constructor(data, tag, arguments.map(value(_, values)), None)
      case Expr.Literal(_, _) | Expr.Abort | Expr.Toplevel => Whole(expression)
    }

    private def expression(expression: Expr, values: KnownValues): Expr =
      materialize(value(expression, values))

    private def identifier(id: Id, values: KnownValues): Id =
      expression(Expr.Variable(id), values) match {
        case Expr.Variable(result) => result
        case _ => id
      }

    private def expand(values: List[KnownValue], entry: List[Shape]): Option[List[Expr]] =
      (values, entry) match {
        case (Nil, Nil) => Some(Nil)
        case (Constructor(_, tag, fields, _) :: tail,
            Shape.Constructor(_, expected, arity) :: shapes)
            if tag == expected && fields.size == arity =>
          expand(tail, shapes).map(fields.map(materialize) ++ _)
        case (head :: tail, Shape.Unknown :: shapes) =>
          expand(tail, shapes).map(materialize(head) :: _)
        case _ => None
      }

    private def arguments(
      statement: Stmt,
      arguments: List[Expr],
      values: KnownValues,
      directTarget: Option[Id]
    ): List[Expr] = {
      val known = arguments.map(value(_, values))
      val entry = plan.calls.get(site(statement)).orElse {
        directTarget.flatMap(plan.analysis.entries.get)
      }.map(_.take(arguments.size).toList)
      entry.flatMap(expand(known, _)).getOrElse(known.map(materialize))
    }

    private def raise(ids: List[Id], entry: List[Shape], values: KnownValues): Raised = {
      val raised = mutable.ListBuffer.empty[Id]
      val materializations = mutable.ListBuffer.empty[(Id, KnownValue.Constructor)]
      var known = values

      ids.zip(entry.padTo(ids.size, Shape.Unknown)).foreach {
        case (id, Shape.Unknown) => raised += id
        case (id, Shape.Constructor(data, tag, arity)) =>
          val fields = List.tabulate(arity) { index =>
            Id(id.name.rename(name => s"${name}_${index}"))
          }
          val split: KnownValue.Constructor = Constructor(
            data,
            tag,
            fields.map(field => Whole(Expr.Variable(field))),
            Some(Expr.Variable(id)))
          raised ++= fields
          known += id -> split
          materializations += id -> split
      }
      Raised(raised.toList, known, materializations.toList)
    }

    private def materializeUsed(
      materializations: List[(Id, KnownValue.Constructor)],
      statement: Stmt
    ): Stmt = materializations.foldRight(statement) {
      case ((id, split), rest) if rest.free.contains(id) =>
        Stmt.Let(id, Expr.Make(split.data, split.tag, split.fields.map(materialize)), rest)
      case (_, rest) => rest
    }

    private def raisedDefinition(
      id: Id,
      params: List[Id],
      body: Stmt,
      values: KnownValues,
      objects: Objects
    ): (List[Id], Stmt) = {
      val entry = plan.analysis.entries.getOrElse(
        id, Vector.fill(params.size)(Shape.Unknown))
      val raised = raise(params, entry.toList, values)
      val rewritten = rewrite(body, raised.values, objects)
      raised.ids -> materializeUsed(raised.materializations, rewritten)
    }

    private def operation(
      operation: Operation,
      values: KnownValues,
      objects: Objects
    ): Operation = {
      val (params, body) = raisedDefinition(
        operation.name, operation.params, operation.body, values, objects)
      Operation(operation.name, params, body)
    }

    private def clause(
      clause: Clause,
      values: KnownValues,
      objects: Objects
    ): Clause = Clause(clause.params, rewrite(clause.body, values, objects))

    private def rewrite(
      statement: Stmt,
      values: KnownValues,
      objects: Objects
    ): Stmt = statement match {
      case Stmt.Def(id, params, body, rest) =>
        val (raisedParams, raisedBody) = raisedDefinition(id, params, body, values, objects)
        Stmt.Def(id, raisedParams, raisedBody, rewrite(rest, values, objects))

      case Stmt.New(id, interface, operations, rest)
          if plan.objects.contains(id) =>
        val projections = operations.map { operation =>
          operation.name -> Id(id.name.rename(name => s"${name}_${operation.name.name}"))
        }.toMap
        val rewrittenRest = rewrite(rest, values, objects + (id -> projections))
        if !rewrittenRest.free.contains(id) && operations.forall(!_.body.free.contains(id)) then
          operations.foldRight(rewrittenRest) { (op, next) =>
            val rewritten = operation(op, values, objects)
            Stmt.Def(projections(op.name), rewritten.params, rewritten.body, next)
          }
        else
          Stmt.New(id, interface,
            operations.map(operation(_, values, objects)),
            rewrite(rest, values, objects))

      case Stmt.New(id, interface, operations, rest) =>
        Stmt.New(id, interface,
          operations.map(operation(_, values, objects)),
          rewrite(rest, values, objects))

      case Stmt.Let(id, Expr.Variable(source), rest) if objects.contains(source) =>
        val rewritten = rewrite(rest, values, objects + (id -> objects(source)))
        if rewritten.free.contains(id) then
          Stmt.Let(id, Expr.Variable(source), rewritten)
        else rewritten

      case Stmt.Let(id, binding, rest) => value(binding, values) match {
        case split: Constructor =>
          val known = split.copy(whole = Some(Expr.Variable(id)))
          val rewritten = rewrite(rest, values + (id -> known), objects)
          if rewritten.free.contains(id) then
            Stmt.Let(id,
              Expr.Make(split.data, split.tag, split.fields.map(materialize)), rewritten)
          else rewritten
        case Whole(rewrittenBinding) =>
          Stmt.Let(id, rewrittenBinding, rewrite(rest, values, objects))
      }

      case call @ Stmt.Call(ids, returnedKs, callee, args, ks, rest) =>
        val continuation = plan.continuations.get(site(call))
          .map(_.take(ids.size).toList)
          .getOrElse(List.fill(ids.size)(Shape.Unknown))
        val raised = raise(ids, continuation, values)
        val rewrittenRest = materializeUsed(
          raised.materializations,
          rewrite(rest, raised.values, objects))
        val rewrittenCallee = callee match {
          case Callee.Function(id) => Callee.Function(identifier(id, values))
          case Callee.Method(receiver, method) =>
            objects.get(receiver).flatMap(_.get(method)) match {
              case Some(target) => Callee.Function(target)
              case None => Callee.Method(identifier(receiver, values), method)
            }
        }
        Stmt.Call(
          raised.ids,
          returnedKs,
          rewrittenCallee,
          arguments(call, args, values, callee.function),
          expression(ks, values),
          rewrittenRest)

      case app @ Stmt.App(id, args) =>
        Stmt.App(identifier(id, values), arguments(app, args, values, Some(id)))

      case invocation @ Stmt.Invoke(receiver, method, args) =>
        val target = objects.get(receiver).flatMap(_.get(method))
        val rewrittenArguments = arguments(invocation, args, values, target)
        target match {
          case Some(id) => Stmt.App(id, rewrittenArguments)
          case None => Stmt.Invoke(
            identifier(receiver, values), method, rewrittenArguments)
        }

      case Stmt.Return(results) => Stmt.Return(results.map(expression(_, values)))

      case Stmt.Run(id, callee, args, purity, rest) =>
        Stmt.Run(id, identifier(callee, values), args.map(expression(_, values)), purity,
          rewrite(rest, values, objects))

      case Stmt.If(condition, thn, els) =>
        Stmt.If(expression(condition, values),
          rewrite(thn, values, objects),
          rewrite(els, values, objects))

      case Stmt.Match(scrutinee, clauses, default) => value(scrutinee, values) match {
        case Constructor(_, tag, fields, _) =>
          clauses.find(_._1 == tag) match {
            case Some((_, Clause(params, body))) if params.size == fields.size =>
              rewrite(body, values ++ params.zip(fields), objects)
            case _ => default.map(rewrite(_, values, objects)).getOrElse {
              Stmt.Match(expression(scrutinee, values),
                clauses.map { case (tag, body) =>
                  tag -> clause(body, values, objects)
                }, default.map(rewrite(_, values, objects)))
            }
          }
        case Whole(rewrittenScrutinee) =>
          Stmt.Match(rewrittenScrutinee,
            clauses.map { case (tag, body) =>
              tag -> clause(body, values, objects)
            }, default.map(rewrite(_, values, objects)))
      }

      case Stmt.Region(id, ks, rest) =>
        Stmt.Region(id, expression(ks, values), rewrite(rest, values, objects))
      case Stmt.Alloc(id, init, region, rest) =>
        Stmt.Alloc(id, expression(init, values), identifier(region, values),
          rewrite(rest, values, objects))
      case Stmt.Var(id, init, ks, rest) =>
        Stmt.Var(id, expression(init, values), expression(ks, values),
          rewrite(rest, values, objects))
      case Stmt.Dealloc(ref, rest) =>
        Stmt.Dealloc(identifier(ref, values), rewrite(rest, values, objects))
      case Stmt.Get(ref, id, rest) =>
        Stmt.Get(identifier(ref, values), id, rewrite(rest, values, objects))
      case Stmt.Put(ref, newValue, rest) =>
        Stmt.Put(identifier(ref, values), expression(newValue, values),
          rewrite(rest, values, objects))

      case Stmt.Reset(prompt, ks, k, body, ks1, k1) =>
        Stmt.Reset(prompt, ks, k, rewrite(body, values, objects),
          expression(ks1, values), expression(k1, values))
      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        Stmt.Shift(identifier(prompt, values), resume, ks, k,
          rewrite(body, values, objects),
          expression(ks1, values), expression(k1, values))
      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        Stmt.Resume(identifier(resumption, values), ks, k,
          rewrite(body, values, objects),
          expression(ks1, values), expression(k1, values))

      case hole: Stmt.Hole => hole
    }

    def module(module: ModuleDecl): ModuleDecl = module.copy(
      definitions = module.definitions.map {
        case ToplevelDefinition.Def(id, params, body) =>
          val (raisedParams, raisedBody) = raisedDefinition(
            id, params, body, Map.empty, Map.empty)
          ToplevelDefinition.Def(id, raisedParams, raisedBody)
        case ToplevelDefinition.Val(id, ks, k, binding) =>
          ToplevelDefinition.Val(id, ks, k, rewrite(binding, Map.empty, Map.empty))
      })
  }

  def transform(module: ModuleDecl, entrypoints: Set[Id]): ModuleDecl = {
    val plan = representationPlan(module, entrypoints)
    Rewriter(plan).module(module)
  }
}
