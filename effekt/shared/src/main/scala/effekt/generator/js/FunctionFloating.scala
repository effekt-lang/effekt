package effekt
package generator
package js

import scala.annotation.tailrec

/**
 * Float function declarations to their outermost valid scope.
 *
 * At this point join points have already been translated to labeled blocks,
 * so this pass only moves declarations that already require JavaScript
 * function values. A function can cross a lexical scope exactly when it does
 * not refer to a name bound by that scope. Module-closed functions cross every
 * control boundary; other functions cross a branch only when a use outside the
 * branch requires it to do so.
 */
object FunctionFloating {

  private case class Candidate(function: Stmt.Function, free: Set[JSName])
  private case class Result(stmts: List[Stmt], floating: List[Candidate])
  private case class ExprResult(expr: Expr, floating: List[Candidate])
  private type ReferenceCounts = Map[JSName, Int]
  private type ModuleClosed = Set[JSName]

  def transform(module: Module): Module = {
    given ReferenceCounts =
      merge(referenceCounts(module.stmts), referenceCountsExprs(module.exports.map(_.expr)))
    given ModuleClosed = moduleClosed(module.stmts, module.exports.map(_.expr))
    val exports = module.exports.map { declaration =>
      val result = rewrite(declaration.expr)
      declaration.copy(expr = result.expr) -> result.floating
    }
    val body = rewriteScope(module.stmts, Set.empty)
    module.copy(
      exports = exports.map(_._1),
      stmts = materialize(body.copy(
        floating = body.floating ++ exports.flatMap(_._2)))
    )
  }

  def transform(stmts: List[Stmt]): List[Stmt] = {
    given ReferenceCounts = referenceCounts(stmts)
    given ModuleClosed = moduleClosed(stmts, Nil)
    materialize(rewriteScope(stmts, Set.empty))
  }

  /** Turn candidates back into declarations at a floating boundary. */
  private def materialize(result: Result): List[Stmt] =
    result.floating.map(_.function) ++ result.stmts

  /**
   * Keep candidates at a control-flow boundary unless their name is used
   * outside it. Dependencies of escaping candidates have to escape as well.
   */
  private def restrict(result: Result, local: ReferenceCounts)(using
    global: ReferenceCounts,
    moduleClosed: ModuleClosed
  ): Result = {
    val candidateNames = result.floating.map(_.function.name).toSet
    val initiallyEscaping = candidateNames.filter { name =>
      moduleClosed(name) || global.getOrElse(name, 0) > local.getOrElse(name, 0)
    }

    @tailrec
    def close(escaping: Set[JSName]): Set[JSName] = {
      val required = result.floating.collect {
        case Candidate(function, free) if escaping.contains(function.name) => free
      }.flatten.toSet
      val next = escaping ++ (required intersect candidateNames)
      if next == escaping then escaping else close(next)
    }

    val escapingNames = close(initiallyEscaping)
    val (escaping, staying) = result.floating.partition(c => escapingNames.contains(c.function.name))
    Result(prepend(staying.map(_.function), result.stmts), escaping)
  }

  /** Add declarations to an existing anonymous block without nesting it. */
  private def prepend(declarations: List[Stmt.Function], stmts: List[Stmt]): List[Stmt] =
    if declarations.isEmpty then stmts else stmts match {
      case List(Stmt.Block(None, body)) => List(Stmt.Block(None, declarations ++ body))
      case _ => declarations ++ stmts
    }

  /** Rewrite one lexical scope and return declarations that can cross it. */
  private def rewriteScope(stmts: List[Stmt], parameters: Set[JSName])(using
    ReferenceCounts,
    ModuleClosed
  ): Result = {
    val rewritten = stmts.map(rewrite)
    val body = rewritten.flatMap(_.stmts)
    val candidates = rewritten.flatMap(_.floating)
    val candidateNames = candidates.map(_.function.name).toSet
    val bound = (parameters ++ bindings(body)) -- candidateNames

    // If a candidate has to stay, every candidate referring to it has to stay
    // as well. This lets independent groups float together.
    @tailrec
    def close(staying: Set[JSName]): Set[JSName] = {
      val unavailable = bound ++ staying
      val next = staying ++ candidates.collect {
        case Candidate(function, free) if free.exists(unavailable.contains) => function.name
      }
      if next == staying then staying else close(next)
    }

    val stayingNames = close(Set.empty)
    val (staying, floating) = candidates.partition(c => stayingNames.contains(c.function.name))

    Result(staying.map(_.function) ++ body, floating)
  }

  private def rewrite(stmt: Stmt)(using ReferenceCounts, ModuleClosed): Result = stmt match {
    case Stmt.Block(label, stmts) =>
      val result = rewriteScope(stmts, Set.empty)
      Result(List(Stmt.Block(label, result.stmts)), result.floating)

    case Stmt.Return(expr) =>
      val result = rewrite(expr)
      Result(List(Stmt.Return(result.expr)), result.floating)

    case Stmt.RawStmt(raw, args) =>
      val result = rewrite(args)
      Result(List(Stmt.RawStmt(raw, result._1)), result._2)

    case Stmt.Const(pattern, binding) =>
      val result = rewrite(binding)
      Result(List(Stmt.Const(pattern, result.expr)), result.floating)

    case Stmt.Let(pattern, binding) =>
      val result = rewrite(binding)
      Result(List(Stmt.Let(pattern, result.expr)), result.floating)

    case Stmt.Assign(target, value) =>
      val left = rewrite(target)
      val right = rewrite(value)
      Result(List(Stmt.Assign(left.expr, right.expr)), left.floating ++ right.floating)

    case Stmt.Destruct(names, binding) =>
      val result = rewrite(binding)
      Result(List(Stmt.Destruct(names, result.expr)), result.floating)

    case Stmt.Switch(scrutinee, branches, default) =>
      val rewrittenScrutinee = rewrite(scrutinee)
      // Keep declarations control-dependent on their clause unless they are
      // module-closed or also referenced elsewhere. A clause containing
      // declarations gets an explicit block because JavaScript switch clauses
      // otherwise share one lexical scope.
      val rewrittenBranches = branches.map { case (tag, stmts) =>
        val rewrittenTag = rewrite(tag)
        val result = restrict(rewriteScope(stmts, Set.empty), referenceCounts(stmts))
        (rewrittenTag.expr, clause(result.stmts), rewrittenTag.floating ++ result.floating)
      }
      val rewrittenDefault = default.map { stmts =>
        val result = restrict(rewriteScope(stmts, Set.empty), referenceCounts(stmts))
        (clause(result.stmts), result.floating)
      }
      Result(List(Stmt.Switch(
        rewrittenScrutinee.expr,
        rewrittenBranches.map { case (tag, stmts, _) => tag -> stmts },
        rewrittenDefault.map(_._1)
      )), rewrittenScrutinee.floating ++ rewrittenBranches.flatMap(_._3) ++
        rewrittenDefault.toList.flatMap(_._2))

    case Stmt.Function(name, params, stmts) =>
      // Parameters are introduced by entering the function. Its name, however,
      // is introduced by the declaration in the surrounding scope, so nested
      // declarations may float together with the function that names them.
      val result = rewriteScope(stmts, params.toSet)
      val rewritten: Stmt.Function = Stmt.Function(name, params, result.stmts)
      val captured = free(rewritten)
      Result(Nil, result.floating :+ Candidate(rewritten, captured))

    case Stmt.Class(name, methods) =>
      // Method parameters form a lexical boundary, but module-closed nested
      // declarations can leave it just like they can leave a lambda.
      val rewritten = methods.map { method =>
        val result = restrict(
          rewriteScope(method.stmts, method.params.toSet + method.name),
          referenceCounts(method.stmts))
        method.copy(stmts = result.stmts) -> result.floating
      }
      Result(List(Stmt.Class(name, rewritten.map(_._1))), rewritten.flatMap(_._2))

    case Stmt.If(cond, thn, els) =>
      val condition = rewrite(cond)
      val thnResult = restrict(rewrite(thn), referenceCounts(thn))
      val elsResult = restrict(rewrite(els), referenceCounts(els))
      Result(
        List(Stmt.If(condition.expr, asStmt(thnResult.stmts), asStmt(elsResult.stmts))),
        condition.floating ++ thnResult.floating ++ elsResult.floating
      )

    case Stmt.Try(prog, name, handler, fin) =>
      val progResult = rewriteScope(prog, Set.empty)
      val handlerResult = rewriteScope(handler, Set(name))
      val finResult = rewriteScope(fin, Set.empty)
      Result(
        List(Stmt.Try(progResult.stmts, name, handlerResult.stmts, finResult.stmts)),
        progResult.floating ++ handlerResult.floating ++ finResult.floating
      )

    case Stmt.Throw(expr) =>
      val result = rewrite(expr)
      Result(List(Stmt.Throw(result.expr)), result.floating)

    case Stmt.While(label, cond, stmts) =>
      val condition = rewrite(cond)
      val result = rewriteScope(stmts, Set.empty)
      Result(
        List(Stmt.While(label, condition.expr, result.stmts)),
        condition.floating ++ result.floating)

    case stmt @ (Stmt.Break(_) | Stmt.Continue(_)) =>
      Result(List(stmt), Nil)

    case Stmt.ExprStmt(expr) =>
      val result = rewrite(expr)
      Result(List(Stmt.ExprStmt(result.expr)), result.floating)
  }

  private def rewrite(exprs: List[Expr])(using
    ReferenceCounts,
    ModuleClosed
  ): (List[Expr], List[Candidate]) = {
    val results = exprs.map(rewrite)
    results.map(_.expr) -> results.flatMap(_.floating)
  }

  private def rewrite(expr: Expr)(using ReferenceCounts, ModuleClosed): ExprResult = expr match {
    case Expr.Call(callee, arguments) =>
      val function = rewrite(callee)
      val args = rewrite(arguments)
      ExprResult(Expr.Call(function.expr, args._1), function.floating ++ args._2)
    case Expr.New(callee, arguments) =>
      val constructor = rewrite(callee)
      val args = rewrite(arguments)
      ExprResult(Expr.New(constructor.expr, args._1), constructor.floating ++ args._2)
    case Expr.RawExpr(raw, args) =>
      val result = rewrite(args)
      ExprResult(Expr.RawExpr(raw, result._1), result._2)
    case literal: Expr.RawLiteral => ExprResult(literal, Nil)
    case Expr.IfExpr(cond, thn, els) =>
      val condition = rewrite(cond)
      val left = rewrite(thn)
      val right = rewrite(els)
      ExprResult(
        Expr.IfExpr(condition.expr, left.expr, right.expr),
        condition.floating ++ left.floating ++ right.floating)
    case Expr.Lambda(params, body) =>
      val result = restrict(
        rewriteScope(List(body), params.toSet),
        referenceCounts(body))
      ExprResult(Expr.Lambda(params, asStmt(result.stmts)), result.floating)
    case Expr.Object(properties) =>
      val results = properties.map { case (name, value) => name -> rewrite(value) }
      ExprResult(
        Expr.Object(results.map { case (name, result) => name -> result.expr }),
        results.flatMap(_._2.floating))
    case Expr.Member(callee, selection) =>
      val result = rewrite(callee)
      ExprResult(Expr.Member(result.expr, selection), result.floating)
    case Expr.ArrayLiteral(elements) =>
      val result = rewrite(elements)
      ExprResult(Expr.ArrayLiteral(result._1), result._2)
    case variable: Expr.Variable => ExprResult(variable, Nil)
  }

  private def asStmt(stmts: List[Stmt]): Stmt = stmts match {
    case stmt :: Nil => stmt
    case stmts => Stmt.Block(None, stmts)
  }

  /** Isolate declarations from the shared lexical scope of a switch. */
  private def clause(stmts: List[Stmt]): List[Stmt] =
    if stmts.exists { case _: Stmt.Function => true; case _ => false }
    then List(Stmt.Block(None, stmts))
    else stmts

  private def merge(left: ReferenceCounts, right: ReferenceCounts): ReferenceCounts =
    right.foldLeft(left) { case (result, (name, count)) =>
      result.updated(name, result.getOrElse(name, 0) + count)
    }

  private def referenceCounts(stmts: Iterable[Stmt]): ReferenceCounts =
    countReferences(stmts.flatMap(references))

  private def referenceCounts(stmt: Stmt): ReferenceCounts =
    countReferences(references(stmt))

  private def referenceCountsExprs(exprs: Iterable[Expr]): ReferenceCounts =
    countReferences(exprs.flatMap(references))

  private def countReferences(names: Iterable[JSName]): ReferenceCounts =
    names.foldLeft(Map.empty[JSName, Int]) { (counts, name) =>
      counts.updated(name, counts.getOrElse(name, 0) + 1)
    }

  /**
   * Greatest set of declarations whose free names are available at module
   * scope, possibly through other declarations in the same set.
   */
  private def moduleClosed(stmts: List[Stmt], exprs: List[Expr]): ModuleClosed = {
    val definitions =
      (stmts.flatMap(functions) ++ exprs.flatMap(functions))
        .map(function => function.name -> free(function)).toMap
    val names = definitions.keySet
    val moduleNames = bindings(stmts) ++ free(stmts) ++ exprs.flatMap(free)
    val available = moduleNames ++ names

    // The complement of the greatest closed set is the least set containing
    // every definition with an unavailable dependency and all its dependents.
    val dependents = definitions.foldLeft(Map.empty[JSName, Set[JSName]]) {
      case (index, (name, dependencies)) =>
        (dependencies intersect names).foldLeft(index) { (index, dependency) =>
          index.updated(dependency, index.getOrElse(dependency, Set.empty) + name)
        }
    }
    val open = definitions.collect {
      case (name, dependencies) if !dependencies.subsetOf(available) => name
    }.toSet

    @tailrec
    def propagate(open: Set[JSName], pending: List[JSName]): Set[JSName] = pending match {
      case Nil => names -- open
      case dependency :: pending =>
        val discovered = dependents.getOrElse(dependency, Set.empty) -- open
        propagate(open ++ discovered, discovered.toList ::: pending)
    }

    propagate(open, open.toList)
  }

  private def functions(stmt: Stmt): List[Stmt.Function] = stmt match {
    case Stmt.Block(_, stmts) => stmts.flatMap(functions)
    case Stmt.Return(expr) => functions(expr)
    case Stmt.RawStmt(_, args) => args.flatMap(functions)
    case Stmt.Const(_, binding) => functions(binding)
    case Stmt.Let(_, binding) => functions(binding)
    case Stmt.Assign(target, value) => functions(target) ++ functions(value)
    case Stmt.Destruct(_, binding) => functions(binding)
    case Stmt.Switch(scrutinee, branches, default) =>
      functions(scrutinee) ++ branches.flatMap((tag, stmts) =>
        functions(tag) ++ stmts.flatMap(functions)) ++
        default.toList.flatten.flatMap(functions)
    case function @ Stmt.Function(_, _, stmts) =>
      function :: stmts.flatMap(functions)
    case Stmt.Class(_, methods) =>
      methods.flatMap(_.stmts.flatMap(functions))
    case Stmt.If(cond, thn, els) => functions(cond) ++ functions(thn) ++ functions(els)
    case Stmt.Try(prog, _, handler, fin) =>
      prog.flatMap(functions) ++ handler.flatMap(functions) ++ fin.flatMap(functions)
    case Stmt.Throw(expr) => functions(expr)
    case Stmt.While(_, cond, stmts) => functions(cond) ++ stmts.flatMap(functions)
    case Stmt.Break(_) | Stmt.Continue(_) => Nil
    case Stmt.ExprStmt(expr) => functions(expr)
  }

  private def functions(expr: Expr): List[Stmt.Function] = expr match {
    case Expr.Call(callee, arguments) => functions(callee) ++ arguments.flatMap(functions)
    case Expr.New(callee, arguments) => functions(callee) ++ arguments.flatMap(functions)
    case Expr.RawExpr(_, args) => args.flatMap(functions)
    case Expr.RawLiteral(_) => Nil
    case Expr.IfExpr(cond, thn, els) => functions(cond) ++ functions(thn) ++ functions(els)
    case Expr.Lambda(_, body) => functions(body)
    case Expr.Object(properties) => properties.flatMap((_, value) => functions(value))
    case Expr.Member(callee, _) => functions(callee)
    case Expr.ArrayLiteral(elements) => elements.flatMap(functions)
    case Expr.Variable(_) => Nil
  }

  /** All variable occurrences, including occurrences bound in this subtree. */
  private def references(stmt: Stmt): List[JSName] = stmt match {
    case Stmt.Block(_, stmts) => stmts.flatMap(references)
    case Stmt.Return(expr) => references(expr)
    case Stmt.RawStmt(_, args) => args.flatMap(references)
    case Stmt.Const(_, binding) => references(binding)
    case Stmt.Let(_, binding) => references(binding)
    case Stmt.Assign(target, value) => references(target) ++ references(value)
    case Stmt.Destruct(_, binding) => references(binding)
    case Stmt.Switch(scrutinee, branches, default) =>
      references(scrutinee) ++ branches.flatMap((tag, stmts) => references(tag) ++ stmts.flatMap(references)) ++
        default.toList.flatten.flatMap(references)
    case Stmt.Function(_, _, stmts) => stmts.flatMap(references)
    case Stmt.Class(_, methods) => methods.flatMap(method => method.stmts.flatMap(references))
    case Stmt.If(cond, thn, els) => references(cond) ++ references(thn) ++ references(els)
    case Stmt.Try(prog, _, handler, fin) =>
      prog.flatMap(references) ++ handler.flatMap(references) ++ fin.flatMap(references)
    case Stmt.Throw(expr) => references(expr)
    case Stmt.While(_, cond, stmts) => references(cond) ++ stmts.flatMap(references)
    case Stmt.Break(_) | Stmt.Continue(_) => Nil
    case Stmt.ExprStmt(expr) => references(expr)
  }

  private def references(expr: Expr): List[JSName] = expr match {
    case Expr.Call(callee, arguments) => references(callee) ++ arguments.flatMap(references)
    case Expr.New(callee, arguments) => references(callee) ++ arguments.flatMap(references)
    case Expr.RawExpr(_, args) => args.flatMap(references)
    case Expr.RawLiteral(_) => Nil
    case Expr.IfExpr(cond, thn, els) => references(cond) ++ references(thn) ++ references(els)
    case Expr.Lambda(_, body) => references(body)
    case Expr.Object(properties) => properties.flatMap((_, value) => references(value))
    case Expr.Member(callee, _) => references(callee)
    case Expr.ArrayLiteral(elements) => elements.flatMap(references)
    case Expr.Variable(name) => List(name)
  }

  private def patternBindings(pattern: Pattern): Set[JSName] = pattern match {
    case Pattern.Variable(name) => Set(name)
    case Pattern.Array(patterns) => patterns.flatMap(patternBindings).toSet
  }

  /** Names introduced directly by one statement list. */
  private def bindings(stmts: Iterable[Stmt]): Set[JSName] =
    stmts.flatMap {
      case Stmt.Const(pattern, _) => patternBindings(pattern)
      case Stmt.Let(pattern, _) => patternBindings(pattern)
      case Stmt.Destruct(names, _) => names
      case Stmt.Function(name, _, _) => List(name)
      case Stmt.Class(name, _) => List(name)
      case _ => Nil
    }.toSet

  private def free(function: Stmt.Function): Set[JSName] =
    free(function.stmts) -- function.params - function.name

  private def free(stmts: List[Stmt]): Set[JSName] =
    stmts.flatMap(free).toSet -- bindings(stmts)

  private def free(stmt: Stmt): Set[JSName] = stmt match {
    case Stmt.Block(_, stmts) => free(stmts)
    case Stmt.Return(expr) => free(expr)
    case Stmt.RawStmt(_, args) => args.flatMap(free).toSet
    case Stmt.Const(_, binding) => free(binding)
    case Stmt.Let(_, binding) => free(binding)
    case Stmt.Assign(target, value) => free(target) ++ free(value)
    case Stmt.Destruct(_, binding) => free(binding)
    case Stmt.Switch(scrutinee, branches, default) =>
      free(scrutinee) ++ branches.flatMap((_, stmts) => free(stmts)) ++ default.toList.flatMap(free)
    case function: Stmt.Function => free(function)
    case Stmt.Class(name, methods) => methods.flatMap(free).toSet - name
    case Stmt.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Stmt.Try(prog, name, handler, fin) =>
      free(prog) ++ (free(handler) - name) ++ free(fin)
    case Stmt.Throw(expr) => free(expr)
    case Stmt.While(_, cond, stmts) => free(cond) ++ free(stmts)
    case Stmt.Break(_) | Stmt.Continue(_) => Set.empty
    case Stmt.ExprStmt(expr) => free(expr)
  }

  private def free(expr: Expr): Set[JSName] = expr match {
    case Expr.Call(callee, arguments) => free(callee) ++ arguments.flatMap(free)
    case Expr.New(callee, arguments) => free(callee) ++ arguments.flatMap(free)
    case Expr.RawExpr(_, args) => args.flatMap(free).toSet
    case Expr.RawLiteral(_) => Set.empty
    case Expr.IfExpr(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Expr.Lambda(params, body) => free(body) -- params
    case Expr.Object(properties) => properties.flatMap((_, value) => free(value)).toSet
    case Expr.Member(callee, _) => free(callee)
    case Expr.ArrayLiteral(elements) => elements.flatMap(free).toSet
    case Expr.Variable(name) => Set(name)
  }
}
