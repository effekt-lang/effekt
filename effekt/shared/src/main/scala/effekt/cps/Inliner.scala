package effekt
package cps

import core.Id
import cps.substitutions.{ Substitution, substitute }

object Inliner {

  private case class Definition(params: List[Id], body: Stmt)

  private case class Context(definitions: Map[Id, Definition]) {
    def bind(id: Id, definition: Definition): Context =
      copy(definitions = definitions.updated(id, definition))
  }

  private def references(id: Id, tree: Stmt | ModuleDecl): Int = tree match {
    case tree: Stmt => tree.refs.getOrElse(id, 0)
    case tree: ModuleDecl => tree.refs.getOrElse(id, 0)
  }

  private def isTrivial(expr: Expr): Boolean = expr match {
    case Expr.Make(data, tag, args) => false
    case Expr.Variable(id) => true
    case Expr.Literal(value, tpe) => true
    case Expr.Abort => true
    case Expr.Toplevel => true
  }

  /**
   * A forwarder performs only one jump, possibly permuting, projecting, or
   * duplicating its arguments and adding captured atoms or literals.
   */
  private def isForwarder(id: Id, body: Stmt): Boolean =
    !body.free.contains(id) && (body match {
      case Stmt.App(_, args) => args.forall(isTrivial)
      case _ => false
    })

  private def isCandidate(id: Id, body: Stmt, occurrences: Int): Boolean =
    !body.free.contains(id) && (occurrences == 1 || isForwarder(id, body))

  private def reduce(definition: Definition, args: List[Expr]): Stmt = {
    val (bindings, subst) = bindArgs(definition.params, args)
    val body = substitute(definition.body)(using Substitution(subst))
    bindings.foldRight(body) { case ((id, expr), rest) => Stmt.Let(id, expr, rest) }
  }

  /** Compose a value-returning body with its unique remainder.
   *
   * This is the direct-style commuting conversion
   *
   *   def f(xs) = body; let y = f(es); rest
   *
   * where every return from `body` binds `y` and continues with `rest`.
   * Bodies of nested definitions and operations have their own return
   * convention and are deliberately left untouched.
   */
  private def continueWith(body: Stmt, result: Id, rest: Stmt): Stmt = body match {
    case Stmt.Def(id, params, nested, remainder) =>
      Stmt.Def(id, params, nested, continueWith(remainder, result, rest))
    case Stmt.New(id, interface, operations, remainder) =>
      Stmt.New(id, interface, operations, continueWith(remainder, result, rest))
    case Stmt.Let(id, binding, remainder) =>
      Stmt.Let(id, binding, continueWith(remainder, result, rest))
    case Stmt.Call(id, callee, args, ks, remainder) =>
      Stmt.Call(id, callee, args, ks, continueWith(remainder, result, rest))
    case Stmt.Return(value) if isTrivial(value) =>
      substitute(rest)(using Substitution(Map(result -> value)))
    case Stmt.Return(value) =>
      Stmt.Let(result, value, rest)
    case Stmt.Run(id, callee, args, purity, remainder) =>
      Stmt.Run(id, callee, args, purity, continueWith(remainder, result, rest))
    case Stmt.If(condition, thn, els) =>
      Stmt.If(
        condition,
        continueWith(thn, result, rest),
        continueWith(els, result, rest))
    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        scrutinee,
        clauses.map { case (tag, clause) =>
          tag -> clause.copy(body = continueWith(clause.body, result, rest))
        },
        default.map(continueWith(_, result, rest)))
    case Stmt.Region(id, ks, remainder) =>
      Stmt.Region(id, ks, continueWith(remainder, result, rest))
    case Stmt.Alloc(id, init, region, remainder) =>
      Stmt.Alloc(id, init, region, continueWith(remainder, result, rest))
    case Stmt.Var(id, init, ks, remainder) =>
      Stmt.Var(id, init, ks, continueWith(remainder, result, rest))
    case Stmt.Dealloc(ref, remainder) =>
      Stmt.Dealloc(ref, continueWith(remainder, result, rest))
    case Stmt.Get(ref, id, remainder) =>
      Stmt.Get(ref, id, continueWith(remainder, result, rest))
    case Stmt.Put(ref, value, remainder) =>
      Stmt.Put(ref, value, continueWith(remainder, result, rest))

    // A direct body cannot contain these control boundaries. Keeping them
    // unchanged makes this operation partial only at the point where the
    // calling-convention analysis already guarantees they are absent.
    case terminal @ (Stmt.App(_, _) | Stmt.Invoke(_, _, _) |
        Stmt.Reset(_, _, _, _, _, _) | Stmt.Shift(_, _, _, _, _, _, _) |
        Stmt.Resume(_, _, _, _, _, _) | Stmt.Hole(_)) => terminal
  }

  private def reduceCall(
    definition: Definition,
    args: List[Expr],
    result: Id,
    rest: Stmt
  ): Stmt = {
    val (bindings, subst) = bindArgs(definition.params, args)
    val body = substitute(definition.body)(using Substitution(subst))
    val composed = continueWith(body, result, rest)
    bindings.foldRight(composed) { case ((id, expr), remainder) =>
      Stmt.Let(id, expr, remainder)
    }
  }

  /** Number of result exits of the current definition, saturated at two.
   *
   * Bodies of nested definitions and operations have their own result
   * convention and are deliberately not counted. Saturation is sufficient:
   * the commuting conversion below needs only distinguish a linear result
   * path from one that would duplicate its continuation.
   */
  private def resultExits(stmt: Stmt): Int = {
    def plus(left: Int, right: => Int): Int =
      if left >= 2 then 2 else math.min(2, left + right)

    stmt match {
      case Stmt.Def(_, _, _, rest) => resultExits(rest)
      case Stmt.New(_, _, _, rest) => resultExits(rest)
      case Stmt.Let(_, _, rest) => resultExits(rest)
      case Stmt.Call(_, _, _, _, rest) => resultExits(rest)
      case Stmt.Return(_) => 1
      case Stmt.Run(_, _, _, _, rest) => resultExits(rest)
      case Stmt.If(_, thn, els) => plus(resultExits(thn), resultExits(els))
      case Stmt.Match(_, clauses, default) =>
        val clauseExits = clauses.iterator.map(_._2.body).foldLeft(0) { (count, body) =>
          plus(count, resultExits(body))
        }
        plus(clauseExits, default.fold(0)(resultExits))
      case Stmt.Region(_, _, rest) => resultExits(rest)
      case Stmt.Alloc(_, _, _, rest) => resultExits(rest)
      case Stmt.Var(_, _, _, rest) => resultExits(rest)
      case Stmt.Dealloc(_, rest) => resultExits(rest)
      case Stmt.Get(_, _, rest) => resultExits(rest)
      case Stmt.Put(_, _, rest) => resultExits(rest)
      case Stmt.App(_, _) | Stmt.Invoke(_, _, _) | Stmt.Reset(_, _, _, _, _, _) |
          Stmt.Shift(_, _, _, _, _, _, _) | Stmt.Resume(_, _, _, _, _, _) |
          Stmt.Hole(_) => 0
    }
  }

  private def bindArgs(params: List[Id], args: List[Expr]): (List[(Id, Expr)], Map[Id, Expr]) = {
    val bindings = List.newBuilder[(Id, Expr)]
    val subst = Map.newBuilder[Id, Expr]

    params.zip(args).foreach { case (param, arg) =>
      if isTrivial(arg) then
        subst += (param -> arg)
      else {
        val fresh = Id(param)
        bindings += (fresh -> arg)
        subst += (param -> Expr.Variable(fresh))
      }
    }
    (bindings.result(), subst.result())
  }


  // -------------------------------------------------------------------------
  // Rewriting

  private def rewrite(
    stmt: Stmt,
    context: Context,
    expanding: Set[Id] = Set.empty
  ): Stmt = rewriting(stmt) {

    case Stmt.Def(id, params, body, rest) =>
      val body1 = rewrite(body, context, expanding)
      val candidate = isCandidate(id, body, references(id, rest))
      val restContext =
        if candidate then context.bind(id, Definition(params, body))
        else context
      val rest1 = rewrite(rest, restContext, expanding)

      // The first case retains the old dead-definition cleanup. The second
      // removes a definition exactly when its references were inlined.
      if !rest.free.contains(id) || candidate && !rest1.free.contains(id) then rest1
      else Stmt.Def(id, params, body1, rest1)

    case Stmt.New(id, interface, operations, rest)
        if !rest.free.contains(id) && operations.forall(!_.free.contains(id)) =>
      rewrite(rest, context, expanding)

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(
        id,
        interface,
        operations.map(rewrite(_, context, expanding)),
        rewrite(rest, context, expanding))

    case Stmt.Let(id, binding, rest) if !rest.free.contains(id) =>
      rewrite(rest, context, expanding)

    case Stmt.Let(id, binding, rest) if isTrivial(binding) =>
      rewrite(substitute(rest)(using Substitution(Map(id -> binding))), context, expanding)

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding, context), rewrite(rest, context, expanding))

    // Once convention lowering has erased ks/k, a uniquely used definition
    // can commute into its call site without code duplication. Before that
    // point definitions still have two additional CPS parameters, so this
    // case deliberately does not fire.
    case Stmt.Call(id, callee, args, ks, rest)
        if context.definitions.get(callee).exists(_.params.size == args.size) &&
          !expanding.contains(callee) =>
      val args1 = args.map(rewrite(_, context))
      val rest1 = rewrite(rest, context, expanding)
      rewrite(
        reduceCall(context.definitions(callee), args1, id, rest1),
        context,
        expanding + callee)

    case Stmt.Call(id, callee, args, ks, rest) =>
      Stmt.Call(
        id,
        callee,
        args.map(rewrite(_, context)),
        rewrite(ks, context),
        rewrite(rest, context, expanding))

    case app @ Stmt.App(id, args)
        if context.definitions.contains(id) && !expanding.contains(id) =>
      val args1 = args.map(rewrite(_, context))
      rewrite(reduce(context.definitions(id), args1), context, expanding + id)

    case Stmt.App(id, args) =>
      Stmt.App(id, args.map(rewrite(_, context)))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite(_, context)))

    case Stmt.Return(value) =>
      Stmt.Return(rewrite(value, context))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(
        id,
        callee,
        args.map(rewrite(_, context)),
        purity,
        rewrite(rest, context, expanding))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(
        rewrite(cond, context),
        rewrite(thn, context, expanding),
        rewrite(els, context, expanding))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        rewrite(scrutinee, context),
        clauses.map { case (id, clause) =>
          id -> rewrite(clause, context, expanding)
        },
        default.map(rewrite(_, context, expanding)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, rewrite(ks, context), rewrite(rest, context, expanding))

    case Stmt.Alloc(id, init, region, rest) if !rest.free.contains(id) =>
      rewrite(rest, context, expanding)

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, rewrite(init, context), region, rewrite(rest, context, expanding))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(
        id,
        rewrite(init, context),
        rewrite(ks, context),
        rewrite(rest, context, expanding))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, rewrite(rest, context, expanding))

    case Stmt.Get(ref, id, rest) if !rest.free.contains(id) =>
      rewrite(rest, context, expanding)

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, rewrite(rest, context, expanding))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, rewrite(value, context), rewrite(rest, context, expanding))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(
        p, ks, k,
        rewrite(body, context, expanding),
        rewrite(ks1, context),
        rewrite(k1, context))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(
        prompt, resume, ks, k,
        rewrite(body, context, expanding),
        rewrite(ks1, context),
        rewrite(k1, context))

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      Stmt.Resume(
        resumption, ks, k,
        rewrite(body, context, expanding),
        rewrite(ks1, context),
        rewrite(k1, context))

    case hole: Stmt.Hole => hole
  }

  private def rewrite(expr: Expr, context: Context): Expr = rewriting(expr) {
    case Expr.Make(data, tag, args) =>
      Expr.Make(data, tag, args.map(rewrite(_, context)))
    case _ => expr
  }

  private def rewrite(operation: Operation, context: Context, expanding: Set[Id]): Operation =
    Operation(operation.name, operation.params, rewrite(operation.body, context, expanding))

  private def rewrite(clause: Clause, context: Context, expanding: Set[Id]): Clause =
    Clause(clause.params, rewrite(clause.body, context, expanding))


  // -------------------------------------------------------------------------
  // Toplevel

  def transform(module: ModuleDecl, entrypoint: Id): ModuleDecl = {
    val roots = module.exports.toSet + entrypoint

    val candidates = module.definitions.collect {
      case ToplevelDefinition.Def(id, params, body)
          if isCandidate(id, body, references(id, module)) &&
             (isForwarder(id, body) || !roots.contains(id)) =>
        id -> Definition(params, body)
    }.toMap

    val context = Context(candidates)
    val rewrittenDefinitions = module.definitions.map {
      case ToplevelDefinition.Def(id, params, body) =>
        ToplevelDefinition.Def(id, params, rewrite(body, context))
      case ToplevelDefinition.Val(id, ks, k, binding) =>
        ToplevelDefinition.Val(id, ks, k, rewrite(binding, context))
    }

    val rewritten = module.copy(definitions = rewrittenDefinitions)

    val originalBodies = module.definitions.collect {
      case ToplevelDefinition.Def(id, _, body) => id -> body
    }.toMap

    val definitions = rewrittenDefinitions.filter {
      case ToplevelDefinition.Def(id, _, _) if roots.contains(id) => true
      case ToplevelDefinition.Def(id, _, _) =>
        val externalReferences =
          references(id, module) - originalBodies.get(id).fold(0)(references(id, _))
        val wasInlined =
          candidates.contains(id) &&
          references(id, module) > 0 &&
          references(id, rewritten) == 0
        externalReferences > 0 && !wasInlined
      case _: ToplevelDefinition.Val => true
    }

    rewritten.copy(definitions = definitions)
  }

  /** Apply only the direct-style commuting conversion to local definitions.
   * Ordinary CPS definitions retain their existing representation. */
  def transformDirectCalls(module: ModuleDecl, inlineable: Set[Id]): ModuleDecl = {
    def direct(
      stmt: Stmt,
      context: Context,
      expanding: Set[Id] = Set.empty
    ): Stmt = stmt match {
      case Stmt.Def(id, params, body, rest) =>
        val body1 = direct(body, context, expanding)
        val candidate = inlineable.contains(id) &&
          !body.free.contains(id) && references(id, rest) == 1 &&
          resultExits(body1) <= 1
        val restContext =
          if candidate then context.bind(id, Definition(params, body1))
          else context
        val rest1 = direct(rest, restContext, expanding)
        if candidate && !rest1.free.contains(id) then rest1
        else Stmt.Def(id, params, body1, rest1)

      case Stmt.New(id, interface, operations, rest) =>
        Stmt.New(
          id,
          interface,
          operations.map(op => op.copy(body = direct(op.body, context, expanding))),
          direct(rest, context, expanding))
      case Stmt.Let(id, binding, rest) =>
        Stmt.Let(id, binding, direct(rest, context, expanding))

      case Stmt.Call(result, callee, args, ks, rest)
          if context.definitions.get(callee).exists(_.params.size == args.size) &&
            !expanding.contains(callee) =>
        direct(
          reduceCall(context.definitions(callee), args, result, rest),
          context,
          expanding + callee)
      case Stmt.Call(result, callee, args, ks, rest) =>
        Stmt.Call(result, callee, args, ks, direct(rest, context, expanding))

      case terminal @ (Stmt.App(_, _) | Stmt.Invoke(_, _, _) |
          Stmt.Return(_) | Stmt.Hole(_)) => terminal
      case Stmt.Run(id, callee, args, purity, rest) =>
        Stmt.Run(id, callee, args, purity, direct(rest, context, expanding))
      case Stmt.If(condition, thn, els) =>
        Stmt.If(
          condition,
          direct(thn, context, expanding),
          direct(els, context, expanding))
      case Stmt.Match(scrutinee, clauses, default) =>
        Stmt.Match(
          scrutinee,
          clauses.map { case (tag, clause) =>
            tag -> clause.copy(body = direct(clause.body, context, expanding))
          },
          default.map(direct(_, context, expanding)))
      case Stmt.Region(id, ks, rest) =>
        Stmt.Region(id, ks, direct(rest, context, expanding))
      case Stmt.Alloc(id, init, region, rest) =>
        Stmt.Alloc(id, init, region, direct(rest, context, expanding))
      case Stmt.Var(id, init, ks, rest) =>
        Stmt.Var(id, init, ks, direct(rest, context, expanding))
      case Stmt.Dealloc(ref, rest) =>
        Stmt.Dealloc(ref, direct(rest, context, expanding))
      case Stmt.Get(ref, id, rest) =>
        Stmt.Get(ref, id, direct(rest, context, expanding))
      case Stmt.Put(ref, value, rest) =>
        Stmt.Put(ref, value, direct(rest, context, expanding))
      case Stmt.Reset(prompt, ks, k, body, ks1, k1) =>
        Stmt.Reset(prompt, ks, k, direct(body, context, expanding), ks1, k1)
      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        Stmt.Shift(prompt, resume, ks, k, direct(body, context, expanding), ks1, k1)
      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        Stmt.Resume(resumption, ks, k, direct(body, context, expanding), ks1, k1)
    }

    module.copy(definitions = module.definitions.map {
      case ToplevelDefinition.Def(id, params, body) =>
        ToplevelDefinition.Def(id, params, direct(body, Context(Map.empty)))
      case ToplevelDefinition.Val(id, ks, k, binding) =>
        ToplevelDefinition.Val(id, ks, k, direct(binding, Context(Map.empty)))
    })
  }
}
