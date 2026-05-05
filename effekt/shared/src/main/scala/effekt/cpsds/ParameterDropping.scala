package effekt
package cpsds

import core.Id
import scala.collection.mutable
import cpsds.substitutions.{ Substitution, substitute }


object ParameterDropping {

  case class DropInfo(
    // which params of a function are replaced by what?
    functions: Map[Id, List[Boolean]], // could also have the case for "unused"
    // all bindings, including let and parameters
    bindings: Map[Id, Expr]
  ) {
    def show: String =
      bindings.toList.sortBy(_._1.id).map {
        case (id, expr) => s"${util.show(id)} !-> ${util.show(expr)}"
      }.mkString("\n")

    def substitute(id: Id): Expr = bindings.getOrElse(id, Expr.Variable(id))
  }

  def solve(toplevel: ToplevelDefinition, main: Id): DropInfo = toplevel match {
    case ToplevelDefinition.Def(id, params, body) =>
      val flows = toplevel.flows
      solve(flows.functions, flows.knownFlows, params.toSet, main)
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      DropInfo(Map.empty, Map.empty)
  }

  // TODO some examples would require some form of context-dependency like
  //
  //   def f(k, x) = k(x)
  //   f(inc, 1)
  //   f(dec, 0)
  //
  // here inc is ALWAYS called with 1 and dec is ALWAYS called with 0.
  // However, we already lost this information during flow collection.
  def solve(
    signatures: Vector[Signature],
    flows: Set[KnownFlow],
    toplevelParams: Set[Id],
    main: Id
  ): DropInfo = {

    import substitutions.substitute

    val knownFunctions: Map[Id, Signature] = signatures.map(s => s.id -> s).toMap

    // iff `knownFunctions(f) = def f(x)`, then `parameters(x) = (0, f)`
    val parameters: Map[Id, (Int, Id)] = knownFunctions.flatMap {
      case (id, Signature(f, params)) => params.zipWithIndex.map { case (x, index) => x -> (index, f) }
    }

    // todos(x) = {1, x}  ~~  1, z <: x
    val bounds: mutable.Map[Id, Set[Expr]] = mutable.Map.empty

    var calls: Set[KnownFlow.Call] = Set.empty

    // [x !-> 42], [y !-> x]
    var substitution: Map[Id, Expr] = Map.empty

    // bounds, calls, and substitution are pairwise disjoint!

    val used: mutable.Set[Id] = mutable.Set.empty

    def flowsInto(from: Expr, to: Id): Unit =
      val before = bounds.getOrElse(to, Set.empty)
      bounds.update(to, before + from)

    flows.foreach {
      case KnownFlow.Data(from, to) =>
        flowsInto(from, to)
      case KnownFlow.Sink(from) =>
        used.add(from)
      case KnownFlow.Call(from, to, index) =>
        calls += KnownFlow.Call(from, to, index)
    }

    def usedAsArgument(id: Id): Boolean = calls.exists {
      case KnownFlow.Call(from, callee, index) => from == Expr.Variable(id)
    }

    def usedAsFunction(id: Id): Boolean = calls.exists {
      case KnownFlow.Call(from, callee, index) => callee == id
    }

    def hasUnknownFlows(id: Id): Boolean =
      def hasUnknownFlowItself = used.contains(id)
      def isParameterWithUnknownFlow = parameters.get(id) match {
        case Some((index, f)) => bounds.isDefinedAt(f) || usedAsArgument(f) || used.contains(f)
        case None => false
      }
      hasUnknownFlowItself || isParameterWithUnknownFlow

    def lower(id: Id, seen: Set[Id]): Set[Expr] =
      if (substitution isDefinedAt id) return Set(substitution(id))
      if (hasUnknownFlows(id)) return Set(Expr.Variable(id))
      if (seen contains id) return Set.empty
      if (bounds isDefinedAt id) return bounds(id).flatMap { l => lowerBound(l, seen + id) }
      Set(Expr.Variable(id))

    def lowerBound(lbs: Expr, seen: Set[Id]): Set[Expr] = lbs match {
      case Expr.Variable(id) => lower(id, seen)
      case other => Set(other)
    }

    var modified = true

    def learn(id: Id, expr: Expr): Unit = {
      substitution = substitution.updated(id, expr)
      modified = true

      substitution = substitution.map {
        case (from, Expr.Variable(to)) if to == id => from -> expr
        case (from, other) => from -> other
      }
    }

    def updateBoundsAndCalls(): Unit = {
      bounds.mapValuesInPlace {
        case (x, lbs) =>
          lbs.map { e => substitute(e, substitution) }
      }
      calls = calls.map {
        case KnownFlow.Call(from, callee, index) =>
          val newCallee = substitution.getOrElse(callee, Expr.Variable(callee)) match {
            case Expr.Variable(id) => id
            case other =>
              sys error "Should not happen"
          }
          KnownFlow.Call(substitute(from, substitution), newCallee, index)
      }
    }

    while (modified) {

      modified = false

      var toRemove: Set[Id] = Set.empty

      bounds.mapValuesInPlace { case (id, lowerBounds) =>
        lowerBounds.flatMap { l => lowerBound(l, Set(id)) } match {
          case newBounds if !hasUnknownFlows(id) && newBounds.size == 1 =>
            toRemove += id
            learn(id, newBounds.head)
            Set.empty

          case newBounds =>
            if (newBounds != lowerBounds) { modified = true }
            newBounds
        }
      }.filterInPlace { case (id, _) => !toRemove.contains(id) }

      updateBoundsAndCalls()

      calls = calls.filter {
        case KnownFlow.Call(from, callee, index) => knownFunctions.get(callee) match {
          case Some(Signature(_, params)) =>
            flowsInto(from, params(index))
            modified = true
            false
          case None =>
            true
        }
      }
    }

    if (used.intersect(substitution.keySet).nonEmpty) {
      sys error "Should not happen"
    }

    calls.foreach {
      case KnownFlow.Call(from, callee, index) =>
        assert(!substitution.contains(callee))
    }

    val funs = knownFunctions.map {
      case (id, Signature(f, params)) =>
        id -> params.map { p => substitution.isDefinedAt(p) }
    }

    DropInfo(funs, substitution)
  }


  def transform(expr: Expr, info: DropInfo): Expr = expr match {
    case Expr.Variable(id) => info.substitute(id)
    case Expr.Literal(value, tpe) => expr
    case Expr.Make(data, tag, args) => Expr.Make(data, tag, args.map(a => transform(a, info)))
    case Expr.Abort => expr
    case Expr.Return => expr
    case Expr.Toplevel => expr
  }

  def transform(stmt: Stmt, info: DropInfo): Stmt = stmt match {
    case Stmt.Def(id, params, body, rest) =>
      // Drop parameters according to the per-function pattern in `subst.functions`.
      // Positions where the entry is `Some(_)` are dropped; `None` positions are kept.
      val keptParams = info.functions.get(id) match {
        case Some(dropinfo) =>
          params.zip(dropinfo).collect { case (p, false) => p }
        case None =>
          params
      }
      Stmt.Def(id, keptParams, transform(body, info), transform(rest, info))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(op => transform(op, info)), transform(rest, info))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, transform(binding, info), transform(rest, info))

    case Stmt.App(id, args, canBeDirect) =>

      info.substitute(id) match {
        case Expr.Variable(id) =>
          val droppedArgs = info.functions.get(id) match {
            case Some(params) => args.zip(params).collect { case (a, false) => transform(a, info) }
            case None => args.map(a => transform(a, info))
          }
          Stmt.App(id, droppedArgs, canBeDirect)

        case _ => ???
      }

    case Stmt.Invoke(id, method, args) =>
      // We never change parameters of Invoke.
      Stmt.Invoke(id, method, args.map(a => transform(a, info)))

    case Stmt.Run(id, callee, args, purity, rest) =>
      // We never change parameters of Run.
      Stmt.Run(id, callee, args.map(a => transform(a, info)), purity, transform(rest, info))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond, info), transform(thn, info), transform(els, info))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        transform(scrutinee, info),
        clauses.map { case (id, cl) => (id, transform(cl, info)) },
        default.map(d => transform(d, info)))

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

  def transform(op: Operation, subst: DropInfo): Operation = op match {
    case Operation(name, params, body) =>
      Operation(name, params, transform(body, subst))
  }

  def transform(cl: Clause, subst: DropInfo): Clause = cl match {
    case Clause(params, body) =>
      Clause(params, transform(body, subst))
  }

  def transform(top: ToplevelDefinition, main: Id): ToplevelDefinition = top match {
    case ToplevelDefinition.Def(id, params, body) =>
      val info = solve(top, main)
      ToplevelDefinition.Def(id, params, transform(body, info))

    // TODO
    case ToplevelDefinition.Val(id, ks, k, binding) => top
  }

  def transform(m: ModuleDecl, main: Id): ModuleDecl = m match {
    case ModuleDecl(includes, declarations, externs, definitions, exports) =>
      ModuleDecl(includes, declarations, externs, definitions.map(d => transform(d, main)), exports)
  }
}
