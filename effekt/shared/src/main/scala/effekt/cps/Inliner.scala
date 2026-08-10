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
    case Expr.Return => true
    case Expr.Toplevel => true
  }

  /**
   * A forwarder performs only one jump, possibly permuting, projecting, or
   * duplicating its arguments and adding captured atoms or literals.
   */
  private def isForwarder(id: Id, body: Stmt): Boolean =
    !body.free.contains(id) && (body match {
      case Stmt.App(_, args, _) => args.forall(isTrivial)
      case _ => false
    })

  private def isCandidate(id: Id, body: Stmt, occurrences: Int): Boolean =
    !body.free.contains(id) && (occurrences == 1 || isForwarder(id, body))

  private def reduce(definition: Definition, args: List[Expr]): Stmt = {
    val (bindings, subst) = bindArgs(definition.params, args)
    val body = substitute(definition.body)(using Substitution(subst))
    bindings.foldRight(body) { case ((id, expr), rest) => Stmt.Let(id, expr, rest) }
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

    case app @ Stmt.App(id, args, direct)
        if context.definitions.contains(id) && !expanding.contains(id) =>
      val args1 = args.map(rewrite(_, context))
      rewrite(reduce(context.definitions(id), args1), context, expanding + id)

    case Stmt.App(id, args, direct) =>
      Stmt.App(id, args.map(rewrite(_, context)), direct)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite(_, context)))

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
}
