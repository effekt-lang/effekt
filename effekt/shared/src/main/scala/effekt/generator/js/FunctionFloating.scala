package effekt
package generator
package js

import scala.annotation.tailrec

/**
 * Float function declarations out of repeated scopes while preserving both
 * lexical captures and control dependence.
 *
 * At this point join points have already been translated to labeled blocks,
 * so this pass only moves declarations that already require JavaScript
 * function values. A function can cross a lexical scope exactly when it does
 * not refer to a name bound by that scope. It crosses a branch only when a use
 * outside the branch requires it to do so.
 */
object FunctionFloating {

  private case class Candidate(function: Stmt.Function, free: Set[JSName])
  private case class Result(stmts: List[Stmt], floating: List[Candidate])
  private type ReferenceCounts = Map[JSName, Int]

  def transform(module: Module): Module = {
    given ReferenceCounts =
      merge(referenceCounts(module.stmts), referenceCountsExprs(module.exports.map(_.expr)))
    module.copy(
      exports = module.exports.map(e => e.copy(expr = rewrite(e.expr))),
      stmts = materialize(rewriteScope(module.stmts, Set.empty))
    )
  }

  def transform(stmts: List[Stmt]): List[Stmt] = {
    given ReferenceCounts = referenceCounts(stmts)
    materialize(rewriteScope(stmts, Set.empty))
  }

  /** Turn candidates back into declarations at a floating boundary. */
  private def materialize(result: Result): List[Stmt] =
    result.floating.map(_.function) ++ result.stmts

  /**
   * Keep candidates at a control-flow boundary unless their name is used
   * outside it. Dependencies of escaping candidates have to escape as well.
   */
  private def restrict(result: Result, local: ReferenceCounts)(using global: ReferenceCounts): Result = {
    val candidateNames = result.floating.map(_.function.name).toSet
    val initiallyEscaping = candidateNames.filter { name =>
      global.getOrElse(name, 0) > local.getOrElse(name, 0)
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
  private def rewriteScope(stmts: List[Stmt], parameters: Set[JSName])(using ReferenceCounts): Result = {
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

  private def rewrite(stmt: Stmt)(using ReferenceCounts): Result = stmt match {
    case Stmt.Block(label, stmts) =>
      val result = rewriteScope(stmts, Set.empty)
      Result(List(Stmt.Block(label, result.stmts)), result.floating)

    case Stmt.Return(expr) =>
      Result(List(Stmt.Return(rewrite(expr))), Nil)

    case Stmt.RawStmt(raw, args) =>
      Result(List(Stmt.RawStmt(raw, args.map(rewrite))), Nil)

    case Stmt.Const(pattern, binding) =>
      Result(List(Stmt.Const(pattern, rewrite(binding))), Nil)

    case Stmt.Let(pattern, binding) =>
      Result(List(Stmt.Let(pattern, rewrite(binding))), Nil)

    case Stmt.Assign(target, value) =>
      Result(List(Stmt.Assign(rewrite(target), rewrite(value))), Nil)

    case Stmt.Destruct(names, binding) =>
      Result(List(Stmt.Destruct(names, rewrite(binding))), Nil)

    case Stmt.Switch(scrutinee, branches, default) =>
      // Keep declarations control-dependent on their clause unless they are
      // also referenced elsewhere. A clause containing declarations gets an
      // explicit block because JavaScript switch clauses otherwise share one
      // lexical scope.
      val rewrittenBranches = branches.map { case (tag, stmts) =>
        val result = restrict(rewriteScope(stmts, Set.empty), referenceCounts(stmts))
        (rewrite(tag), clause(result.stmts), result.floating)
      }
      val rewrittenDefault = default.map { stmts =>
        val result = restrict(rewriteScope(stmts, Set.empty), referenceCounts(stmts))
        (clause(result.stmts), result.floating)
      }
      Result(List(Stmt.Switch(
        rewrite(scrutinee),
        rewrittenBranches.map { case (tag, stmts, _) => tag -> stmts },
        rewrittenDefault.map(_._1)
      )), rewrittenBranches.flatMap(_._3) ++ rewrittenDefault.toList.flatMap(_._2))

    case Stmt.Function(name, params, stmts) =>
      // Parameters are introduced by entering the function. Its name, however,
      // is introduced by the declaration in the surrounding scope, so nested
      // declarations may float together with the function that names them.
      val result = rewriteScope(stmts, params.toSet)
      val rewritten: Stmt.Function = Stmt.Function(name, params, result.stmts)
      val captured = free(rewritten)
      Result(Nil, result.floating :+ Candidate(rewritten, captured))

    case Stmt.Class(name, methods) =>
      // Methods are not ordinary nested declarations. Optimize their bodies,
      // but keep all declarations within the method boundary.
      val rewritten = methods.map { method =>
        val result = rewriteScope(method.stmts, method.params.toSet + method.name)
        method.copy(stmts = materialize(result))
      }
      Result(List(Stmt.Class(name, rewritten)), Nil)

    case Stmt.If(cond, thn, els) =>
      val thnResult = restrict(rewrite(thn), referenceCounts(thn))
      val elsResult = restrict(rewrite(els), referenceCounts(els))
      Result(
        List(Stmt.If(rewrite(cond), asStmt(thnResult.stmts), asStmt(elsResult.stmts))),
        thnResult.floating ++ elsResult.floating
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
      Result(List(Stmt.Throw(rewrite(expr))), Nil)

    case Stmt.While(label, cond, stmts) =>
      val result = rewriteScope(stmts, Set.empty)
      Result(List(Stmt.While(label, rewrite(cond), result.stmts)), result.floating)

    case stmt @ (Stmt.Break(_) | Stmt.Continue(_)) =>
      Result(List(stmt), Nil)

    case Stmt.ExprStmt(expr) =>
      Result(List(Stmt.ExprStmt(rewrite(expr))), Nil)
  }

  /** Lambdas are function boundaries, so declarations cannot leave them. */
  private def rewrite(expr: Expr)(using ReferenceCounts): Expr = expr match {
    case Expr.Call(callee, arguments) =>
      Expr.Call(rewrite(callee), arguments.map(rewrite))
    case Expr.New(callee, arguments) =>
      Expr.New(rewrite(callee), arguments.map(rewrite))
    case Expr.RawExpr(raw, args) =>
      Expr.RawExpr(raw, args.map(rewrite))
    case literal: Expr.RawLiteral => literal
    case Expr.IfExpr(cond, thn, els) =>
      Expr.IfExpr(rewrite(cond), rewrite(thn), rewrite(els))
    case Expr.Lambda(params, body) =>
      val result = rewrite(body)
      Expr.Lambda(params, asStmt(materialize(result)))
    case Expr.Object(properties) =>
      Expr.Object(properties.map { case (name, value) => name -> rewrite(value) })
    case Expr.Member(callee, selection) =>
      Expr.Member(rewrite(callee), selection)
    case Expr.ArrayLiteral(elements) =>
      Expr.ArrayLiteral(elements.map(rewrite))
    case variable: Expr.Variable => variable
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
