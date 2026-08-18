package effekt
package cps

import core.{ Id, ValueType }
import scala.collection.mutable

/** Relational arity raising.
 *
 *  The analysis records the constructor shape of all arguments at every
 *  reachable, syntactically known call. Keeping complete argument vectors is
 *  what preserves dependencies between parameters. Shapes expose exactly one
 *  constructor layer: fields remain ordinary values and can be considered by
 *  a later run of the pass.
 */
object ArityRaising {

  enum Shape {
    case Unknown
    case Constructor(data: ValueType.Data, tag: Id, arity: Int)
  }

  type Entry = Vector[Shape]

  case class Analysis(entries: Map[Id, Set[Entry]]) {
    def show: String = entries.toList
      .filter(_._2.exists(_.exists(_ != Shape.Unknown)))
      .sortBy(_._1.id)
      .map { case (id, entries) =>
        val rendered = entries.toList
          .filter(_.exists(_ != Shape.Unknown))
          .sortBy(showEntry).map(e => s"  ${showEntry(e)}")
        (id.name.name :: rendered).mkString("\n")
      }.mkString("\n")
  }

  private case class Definition(params: List[Id], body: Stmt)

  private def showShape(shape: Shape): String = shape match {
    case Shape.Unknown => "?"
    case Shape.Constructor(_, tag, arity) =>
      s"${tag.name.name}(${List.fill(arity)("_").mkString(", ")})"
  }

  private def showEntry(entry: Entry): String =
    entry.map(showShape).mkString("<", ", ", ">")

  /** Collect definitions independently of their lexical nesting. Identifiers
   *  in CPS are globally unique, while the transformer below restores the
   *  original nesting. */
  private def collect(module: ModuleDecl): Map[Id, Definition] = {
    val definitions = mutable.LinkedHashMap.empty[Id, Definition]

    def visit(stmt: Stmt): Unit = stmt match {
      case Stmt.Def(id, params, body, rest) =>
        definitions(id) = Definition(params, body)
        visit(body); visit(rest)
      case Stmt.New(_, _, operations, rest) =>
        operations.foreach(op => visit(op.body)); visit(rest)
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
      case ToplevelDefinition.Def(id, params, body) =>
        definitions(id) = Definition(params, body)
        visit(body)
      case ToplevelDefinition.Val(_, _, _, binding) => visit(binding)
    }
    definitions.toMap
  }

  /** Parameters whose representation is observed by pattern matching.
   *
   *  An edge x -> y records that the value of x is passed unchanged to y.
   *  Representation demand is the inverse reachability closure of variables
   *  scrutinized by Match. Thus demand propagates through aliases and known
   *  calls, but not through a newly constructed outer value.
   */
  private def representationDemand(
    module: ModuleDecl,
    definitions: Map[Id, Definition]
  ): Map[Id, Set[Int]] = {
    val predecessors = mutable.Map.empty[Id, mutable.Set[Id]]
    val demanded = mutable.Set.empty[Id]

    def flows(expr: Expr, to: Id): Unit = expr match {
      case Expr.Variable(from) =>
        predecessors.getOrElseUpdate(to, mutable.Set.empty) += from
      case _ => ()
    }

    def call(id: Id, args: List[Expr]): Unit =
      definitions.get(id).foreach { definition =>
        args.zip(definition.params).foreach { case (arg, param) => flows(arg, param) }
      }

    def visit(stmt: Stmt): Unit = stmt match {
      case Stmt.Def(_, _, body, rest) => visit(body); visit(rest)
      case Stmt.New(_, _, operations, rest) =>
        operations.foreach(op => visit(op.body)); visit(rest)
      case Stmt.Let(id, binding, rest) => flows(binding, id); visit(rest)
      case Stmt.Call(_, _, Callee.Function(id), args, _, rest) =>
        call(id, args); visit(rest)
      case Stmt.Call(_, _, _, _, _, rest) => visit(rest)
      case Stmt.App(id, args) => call(id, args)
      case Stmt.Run(_, _, _, _, rest) => visit(rest)
      case Stmt.If(_, thn, els) => visit(thn); visit(els)
      case Stmt.Match(Expr.Variable(id), clauses, default) =>
        demanded += id
        clauses.foreach { case (_, clause) => visit(clause.body) }
        default.foreach(visit)
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
      case _: Stmt.Invoke | _: Stmt.Return | _: Stmt.Hole => ()
    }

    module.definitions.foreach {
      case ToplevelDefinition.Def(_, _, body) => visit(body)
      case ToplevelDefinition.Val(_, _, _, binding) => visit(binding)
    }

    val pending = mutable.Queue.from(demanded)
    while pending.nonEmpty do {
      val variable = pending.dequeue()
      predecessors.get(variable).foreach { variables =>
        variables.foreach { predecessor =>
          if demanded.add(predecessor) then pending.enqueue(predecessor)
        }
      }
    }

    definitions.map { case (id, definition) =>
      id -> definition.params.indices.filter(i => demanded.contains(definition.params(i))).toSet
    }
  }

  def analyze(module: ModuleDecl, entrypoints: Set[Id]): Analysis = {
    val definitions = collect(module)
    val demanded = representationDemand(module, definitions)
    val entries = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Entry]]
    val pending = mutable.Queue.empty[(Id, Entry)]

    def demand(id: Id, entry: Entry): Unit = definitions.get(id).foreach { definition =>
      if definition.params.size == entry.size then {
        val observedEntry = entry.zipWithIndex.map { case (shape, index) =>
          if demanded.getOrElse(id, Set.empty).contains(index) then shape
          else Shape.Unknown
        }
        val observed = entries.getOrElseUpdate(id, mutable.LinkedHashSet.empty)
        if observed.add(observedEntry) then pending.enqueue(id -> observedEntry)
      }
    }

    def shape(expr: Expr, env: Map[Id, Shape]): Shape = expr match {
      case Expr.Variable(id) => env.getOrElse(id, Shape.Unknown)
      case Expr.Make(data, tag, args) => Shape.Constructor(data, tag, args.size)
      case _ => Shape.Unknown
    }

    def scanOperation(operation: Operation, env: Map[Id, Shape]): Unit =
      scan(operation.body, env ++ operation.params.map(_ -> Shape.Unknown))

    def scan(stmt: Stmt, env: Map[Id, Shape]): Unit = stmt match {
      case Stmt.Def(_, _, _, rest) => scan(rest, env)

      case Stmt.New(id, _, operations, rest) =>
        operations.foreach(scanOperation(_, env))
        scan(rest, env + (id -> Shape.Unknown))

      case Stmt.Let(id, binding, rest) =>
        scan(rest, env + (id -> shape(binding, env)))

      case Stmt.Call(ids, returnedKs, Callee.Function(callee), args, ks, rest) =>
        // A compositional call supplies the conventional (ks, k) pair. They
        // remain opaque; this pass changes data arguments, not continuations.
        demand(callee,
          (args.map(shape(_, env)) ++ List(Shape.Unknown, Shape.Unknown)).toVector)
        scan(rest, env ++ ids.map(_ -> Shape.Unknown) + (returnedKs -> Shape.Unknown))

      case Stmt.Call(ids, returnedKs, _, _, _, rest) =>
        scan(rest, env ++ ids.map(_ -> Shape.Unknown) + (returnedKs -> Shape.Unknown))

      case Stmt.App(id, args) =>
        demand(id, args.map(shape(_, env)).toVector)

      case Stmt.Invoke(_, _, _) => ()
      case Stmt.Return(_) => ()

      case Stmt.Run(id, _, _, _, rest) =>
        scan(rest, env + (id -> Shape.Unknown))

      case Stmt.If(_, thn, els) => scan(thn, env); scan(els, env)

      case Stmt.Match(scrutinee, clauses, default) =>
        shape(scrutinee, env) match {
          case Shape.Constructor(_, tag, _) =>
            clauses.find(_._1 == tag) match {
              case Some((_, Clause(params, body))) =>
                scan(body, env ++ params.map(_ -> Shape.Unknown))
              case None => default.foreach(scan(_, env))
            }
          case Shape.Unknown =>
            clauses.foreach { case (_, Clause(params, body)) =>
              scan(body, env ++ params.map(_ -> Shape.Unknown))
            }
            default.foreach(scan(_, env))
        }

      case Stmt.Region(id, _, rest) => scan(rest, env + (id -> Shape.Unknown))
      case Stmt.Alloc(id, _, _, rest) => scan(rest, env + (id -> Shape.Unknown))
      case Stmt.Var(id, _, _, rest) => scan(rest, env + (id -> Shape.Unknown))
      case Stmt.Dealloc(_, rest) => scan(rest, env)
      case Stmt.Get(_, id, rest) => scan(rest, env + (id -> Shape.Unknown))
      case Stmt.Put(_, _, rest) => scan(rest, env)

      case Stmt.Reset(p, ks, k, body, _, _) =>
        scan(body, env ++ List(p, ks, k).map(_ -> Shape.Unknown))
      case Stmt.Shift(_, resume, ks, k, body, _, _) =>
        scan(body, env ++ List(resume, ks, k).map(_ -> Shape.Unknown))
      case Stmt.Resume(_, ks, k, body, _, _) =>
        scan(body, env ++ List(ks, k).map(_ -> Shape.Unknown))
      case _: Stmt.Hole => ()
    }

    // ToplevelDefinition.escapes includes the definition itself. Here we need
    // only actual value uses found in bodies: a known call does not require the
    // generic calling convention.
    val escaped = module.definitions.flatMap {
      case ToplevelDefinition.Def(_, _, body) => body.escapes
      case ToplevelDefinition.Val(_, _, _, binding) => binding.escapes
    }.toSet

    (entrypoints ++ module.exports ++ escaped).foreach { id =>
      definitions.get(id).foreach { definition =>
        demand(id, Vector.fill(definition.params.size)(Shape.Unknown))
      }
    }

    module.definitions.foreach {
      case ToplevelDefinition.Def(_, _, _) => ()
      case ToplevelDefinition.Val(_, ks, k, binding) =>
        scan(binding, Map(ks -> Shape.Unknown, k -> Shape.Unknown))
    }

    while pending.nonEmpty do {
      val (id, entry) = pending.dequeue()
      val definition = definitions(id)
      scan(definition.body, definition.params.zip(entry).toMap)
    }

    Analysis(entries.view.mapValues(_.toSet).toMap)
  }

  private case class Variant(entry: Entry, id: Id)
  private case class FunctionNames(generic: Id, variants: List[Variant]) {
    def select(entry: Entry): Option[Variant] = variants.find(_.entry == entry)
  }

  private sealed trait Value {
    def shape: Shape
  }
  private case class Whole(expr: Expr) extends Value {
    val shape: Shape = Shape.Unknown
  }
  private case class Split(
    data: ValueType.Data,
    tag: Id,
    fields: List[Value],
    whole: Option[Expr]
  ) extends Value {
    val shape: Shape = Shape.Constructor(data, tag, fields.size)
  }

  private type Values = Map[Id, Value]
  private type Functions = Map[Id, FunctionNames]

  private def variantName(id: Id, entry: Entry): Id = {
    val suffix = entry.zipWithIndex.collect {
      case (Shape.Constructor(_, tag, _), index) => s"${index}_${tag.name.name}"
    }.mkString("_")
    Id(id.name.rename(name => s"${name}_${suffix}"))
  }

  /** Exact observed vectors are the polyvariance policy. Keeping this choice
   *  separate makes a later widening or clustering policy independent of the
   *  analysis and the representation-directed rewrite. */
  private def variants(id: Id, analysis: Analysis): List[Variant] =
    analysis.entries.getOrElse(id, Set.empty).toList
      .filter(_.exists(_ != Shape.Unknown))
      .sortBy(showEntry)
      .map(entry => Variant(entry, variantName(id, entry)))

  private class Rewriter(analysis: Analysis, topLevel: Functions) {

    private def fresh(id: Id): Id = Id(id)

    private def materialize(value: Value): Expr = value match {
      case Whole(expr) => expr
      case Split(data, tag, fields, whole) =>
        whole.getOrElse(Expr.Make(data, tag, fields.map(materialize)))
    }

    private def value(expr: Expr, values: Values, functions: Functions): Value = expr match {
      case Expr.Variable(id) =>
        values.getOrElse(id,
          functions.get(id).fold[Value](Whole(expr))(names => Whole(Expr.Variable(names.generic))))
      case Expr.Make(data, tag, args) =>
        Split(data, tag, args.map(value(_, values, functions)), None)
      case Expr.Literal(_, _) | Expr.Abort | Expr.Toplevel => Whole(expr)
    }

    private def rewriteExpr(expr: Expr, values: Values, functions: Functions): Expr =
      materialize(value(expr, values, functions))

    private def rewriteId(id: Id, values: Values, functions: Functions): Id =
      materialize(value(Expr.Variable(id), values, functions)) match {
        case Expr.Variable(result) => result
        case _ => id // Well-typed CPS only uses function-like values here.
      }

    private def freshParams(params: List[Id], values: Values): (List[Id], Values) = {
      val renamed = params.map(fresh)
      (renamed, values ++ params.zip(renamed.map(id => Whole(Expr.Variable(id)))))
    }

    private def functionNames(id: Id): FunctionNames = {
      val generic = fresh(id)
      FunctionNames(generic, variants(id, analysis).map(v => v.copy(id = variantName(generic, v.entry))))
    }

    private def callArguments(
      arguments: List[Value],
      entry: List[Shape]
    ): Option[List[Expr]] = (arguments, entry) match {
      case (Nil, Nil) => Some(Nil)
      case (Split(_, tag, fields, _) :: arguments,
          Shape.Constructor(_, expected, arity) :: entry)
          if tag == expected && fields.size == arity =>
        callArguments(arguments, entry).map(fields.map(materialize) ++ _)
      case (argument :: arguments, Shape.Unknown :: entry) =>
        callArguments(arguments, entry).map(materialize(argument) :: _)
      case _ => None
    }

    private def select(
      id: Id,
      arguments: List[Value],
      values: Values,
      functions: Functions
    ): (Id, List[Expr]) = functions.get(id) match {
      case Some(names) =>
        val entry = arguments.map(_.shape).toVector
        names.select(entry).flatMap { variant =>
          callArguments(arguments, variant.entry.toList).map(variant.id -> _)
        }.getOrElse(names.generic -> arguments.map(materialize))
      case None => rewriteId(id, values, functions) -> arguments.map(materialize)
    }

    private def rewriteOperation(op: Operation, values: Values, functions: Functions): Operation = {
      val (params, bodyValues) = freshParams(op.params, values)
      Operation(op.name, params, rewriteStmt(op.body, bodyValues, functions))
    }

    private def rewriteClause(clause: Clause, values: Values, functions: Functions): Clause = {
      val (params, bodyValues) = freshParams(clause.params, values)
      Clause(params, rewriteStmt(clause.body, bodyValues, functions))
    }

    private def rewriteDefinition(
      params: List[Id],
      body: Stmt,
      entry: Entry,
      values: Values,
      functions: Functions
    ): (List[Id], Stmt) = {
      val raised = mutable.ListBuffer.empty[(Id, Split)]
      val newParams = mutable.ListBuffer.empty[Id]
      var bodyValues = values

      params.zip(entry).foreach {
        case (param, Shape.Unknown) =>
          val renamed = fresh(param)
          newParams += renamed
          bodyValues += param -> Whole(Expr.Variable(renamed))

        case (param, Shape.Constructor(data, tag, arity)) =>
          val fields = List.tabulate(arity)(index => Id(param.name.rename(name => s"${name}_${index}")))
          val whole = fresh(param)
          val split = Split(data, tag, fields.map(id => Whole(Expr.Variable(id))), Some(Expr.Variable(whole)))
          newParams ++= fields
          bodyValues += param -> split
          raised += whole -> split
      }

      val rewritten = rewriteStmt(body, bodyValues, functions)
      val withMaterializations = raised.foldRight(rewritten) { case ((whole, split), rest) =>
        if rest.free.contains(whole) then
          Stmt.Let(whole, Expr.Make(split.data, split.tag, split.fields.map(materialize)), rest)
        else rest
      }
      newParams.toList -> withMaterializations
    }

    private def genericDefinition(
      params: List[Id],
      body: Stmt,
      values: Values,
      functions: Functions
    ): (List[Id], Stmt) =
      rewriteDefinition(params, body, Vector.fill(params.size)(Shape.Unknown), values, functions)

    private def rewriteStmt(stmt: Stmt, values: Values, functions: Functions): Stmt = stmt match {
      case Stmt.Def(id, params, body, rest) =>
        val names = functionNames(id)
        val nestedFunctions = functions + (id -> names)
        val rewrittenRest = rewriteStmt(rest, values, nestedFunctions)
        val withVariants = names.variants.foldRight(rewrittenRest) { case (variant, next) =>
          val variantFunctions = functions +
            (id -> names.copy(variants = List(variant)))
          val (raisedParams, raisedBody) =
            rewriteDefinition(params, body, variant.entry, values, variantFunctions)
          Stmt.Def(variant.id, raisedParams, raisedBody, next)
        }
        val genericEntry = Vector.fill(params.size)(Shape.Unknown)
        val needsGeneric = names.variants.size != 1 ||
          analysis.entries.get(id).exists(_.contains(genericEntry))
        if needsGeneric then {
          // Local definitions are individually recursive, not mutually
          // recursive. A polyvariant family therefore uses the generic
          // definition for transitions between distinct variants.
          val genericFunctions = functions + (id -> names.copy(variants = Nil))
          val (genericParams, genericBody) =
            genericDefinition(params, body, values, genericFunctions)
          Stmt.Def(names.generic, genericParams, genericBody, withVariants)
        } else withVariants

      case Stmt.New(id, interface, operations, rest) =>
        val renamed = fresh(id)
        val objectValues = values + (id -> Whole(Expr.Variable(renamed)))
        Stmt.New(renamed, interface,
          operations.map(rewriteOperation(_, objectValues, functions)),
          rewriteStmt(rest, objectValues, functions))

      case Stmt.Let(id, binding, rest) =>
        val renamed = fresh(id)
        value(binding, values, functions) match {
          case split: Split =>
            val known = split.copy(whole = Some(Expr.Variable(renamed)))
            val rewritten = rewriteStmt(rest, values + (id -> known), functions)
            if rewritten.free.contains(renamed) then
              Stmt.Let(renamed,
                Expr.Make(split.data, split.tag, split.fields.map(materialize)), rewritten)
            else rewritten
          case Whole(expr) =>
            Stmt.Let(renamed, expr,
              rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))
        }

      case Stmt.Call(ids, returnedKs, Callee.Function(callee), args, ks, rest) =>
        val arguments = args.map(value(_, values, functions))
        val callEntry = arguments.map(_.shape).toVector ++ Vector(Shape.Unknown, Shape.Unknown)
        val (target, rewrittenArgs) = functions.get(callee).flatMap { names =>
          names.variants.find(_.entry == callEntry).flatMap { variant =>
            callArguments(arguments ++ List(Whole(rewriteExpr(ks, values, functions)), Whole(Expr.Abort)), variant.entry.toList)
              .map(all => variant.id -> all.dropRight(2))
          }.orElse(Some(names.generic -> arguments.map(materialize)))
        }.getOrElse(rewriteId(callee, values, functions) -> arguments.map(materialize))
        val results = ids.map(fresh)
        val resultKs = fresh(returnedKs)
        Stmt.Call(results, resultKs, Callee.Function(target), rewrittenArgs,
          rewriteExpr(ks, values, functions),
          rewriteStmt(rest,
            values ++ ids.zip(results).map { case (i, r) => i -> Whole(Expr.Variable(r)) } +
              (returnedKs -> Whole(Expr.Variable(resultKs))), functions))

      case Stmt.Call(ids, returnedKs, Callee.Method(receiver, method), args, ks, rest) =>
        val results = ids.map(fresh)
        val resultKs = fresh(returnedKs)
        Stmt.Call(results, resultKs,
          Callee.Method(rewriteId(receiver, values, functions), method),
          args.map(rewriteExpr(_, values, functions)), rewriteExpr(ks, values, functions),
          rewriteStmt(rest,
            values ++ ids.zip(results).map { case (i, r) => i -> Whole(Expr.Variable(r)) } +
              (returnedKs -> Whole(Expr.Variable(resultKs))), functions))

      case Stmt.App(id, args) =>
        val (target, arguments) =
          select(id, args.map(value(_, values, functions)), values, functions)
        Stmt.App(target, arguments)

      case Stmt.Invoke(id, method, args) =>
        Stmt.Invoke(rewriteId(id, values, functions), method,
          args.map(rewriteExpr(_, values, functions)))

      case Stmt.Return(results) => Stmt.Return(results.map(rewriteExpr(_, values, functions)))

      case Stmt.Run(id, callee, args, purity, rest) =>
        val renamed = fresh(id)
        Stmt.Run(renamed, rewriteId(callee, values, functions),
          args.map(rewriteExpr(_, values, functions)), purity,
          rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))

      case Stmt.If(cond, thn, els) =>
        Stmt.If(rewriteExpr(cond, values, functions),
          rewriteStmt(thn, values, functions), rewriteStmt(els, values, functions))

      case Stmt.Match(scrutinee, clauses, default) =>
        value(scrutinee, values, functions) match {
          case Split(_, tag, fields, _) =>
            clauses.find(_._1 == tag) match {
              case Some((_, Clause(params, body))) if params.size == fields.size =>
                rewriteStmt(body, values ++ params.zip(fields), functions)
              case _ => default.map(rewriteStmt(_, values, functions)).getOrElse {
                Stmt.Match(rewriteExpr(scrutinee, values, functions),
                  clauses.map { case (tag, clause) => tag -> rewriteClause(clause, values, functions) },
                  default.map(rewriteStmt(_, values, functions)))
              }
            }
          case Whole(expr) =>
            Stmt.Match(expr,
              clauses.map { case (tag, clause) => tag -> rewriteClause(clause, values, functions) },
              default.map(rewriteStmt(_, values, functions)))
        }

      case Stmt.Region(id, ks, rest) =>
        val renamed = fresh(id)
        Stmt.Region(renamed, rewriteExpr(ks, values, functions),
          rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))

      case Stmt.Alloc(id, init, region, rest) =>
        val renamed = fresh(id)
        Stmt.Alloc(renamed, rewriteExpr(init, values, functions),
          rewriteId(region, values, functions),
          rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))

      case Stmt.Var(id, init, ks, rest) =>
        val renamed = fresh(id)
        Stmt.Var(renamed, rewriteExpr(init, values, functions), rewriteExpr(ks, values, functions),
          rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))

      case Stmt.Dealloc(ref, rest) =>
        Stmt.Dealloc(rewriteId(ref, values, functions), rewriteStmt(rest, values, functions))

      case Stmt.Get(ref, id, rest) =>
        val renamed = fresh(id)
        Stmt.Get(rewriteId(ref, values, functions), renamed,
          rewriteStmt(rest, values + (id -> Whole(Expr.Variable(renamed))), functions))

      case Stmt.Put(ref, newValue, rest) =>
        Stmt.Put(rewriteId(ref, values, functions), rewriteExpr(newValue, values, functions),
          rewriteStmt(rest, values, functions))

      case Stmt.Reset(p, ks, k, body, ks1, k1) =>
        val (params, bodyValues) = freshParams(List(p, ks, k), values)
        Stmt.Reset(params(0), params(1), params(2), rewriteStmt(body, bodyValues, functions),
          rewriteExpr(ks1, values, functions), rewriteExpr(k1, values, functions))

      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        val (params, bodyValues) = freshParams(List(resume, ks, k), values)
        Stmt.Shift(rewriteId(prompt, values, functions), params(0), params(1), params(2),
          rewriteStmt(body, bodyValues, functions),
          rewriteExpr(ks1, values, functions), rewriteExpr(k1, values, functions))

      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        val (params, bodyValues) = freshParams(List(ks, k), values)
        Stmt.Resume(rewriteId(resumption, values, functions), params(0), params(1),
          rewriteStmt(body, bodyValues, functions),
          rewriteExpr(ks1, values, functions), rewriteExpr(k1, values, functions))

      case hole: Stmt.Hole => hole
    }

    def rewrite(module: ModuleDecl): ModuleDecl = {
      val rewritten = module.definitions.flatMap {
        case ToplevelDefinition.Def(id, params, body) =>
          val names = topLevel(id)
          val specialized = names.variants.map { variant =>
              val (raisedParams, raisedBody) =
                rewriteDefinition(params, body, variant.entry, Map.empty, topLevel)
              ToplevelDefinition.Def(variant.id, raisedParams, raisedBody)
            }
          val genericEntry = Vector.fill(params.size)(Shape.Unknown)
          if names.variants.isEmpty || analysis.entries.get(id).exists(_.contains(genericEntry)) then {
            val (genericParams, genericBody) =
              genericDefinition(params, body, Map.empty, topLevel)
            ToplevelDefinition.Def(names.generic, genericParams, genericBody) :: specialized
          } else specialized

        case ToplevelDefinition.Val(id, ks, k, binding) =>
          val (params, values) = freshParams(List(ks, k), Map.empty)
          List(ToplevelDefinition.Val(id, params.head, params(1),
            rewriteStmt(binding, values, topLevel)))
      }
      module.copy(definitions = rewritten)
    }
  }

  def transform(module: ModuleDecl, entrypoints: Set[Id]): ModuleDecl = {
    val analysis = analyze(module, entrypoints)
    val topLevel = module.definitions.collect {
      case ToplevelDefinition.Def(id, _, _) =>
        id -> FunctionNames(id, variants(id, analysis))
    }.toMap
    Rewriter(analysis, topLevel).rewrite(module)
  }
}
