package effekt
package generator
package js

import scala.annotation.tailrec

/**
 * Float function declarations out of repeatedly executed JavaScript scopes.
 *
 * At this point join points have already been translated to labeled blocks,
 * so this pass only moves declarations that already require JavaScript
 * function values. A function can cross a lexical scope exactly when it does
 * not refer to a name bound by that scope.
 */
object FunctionFloating {

  private case class Candidate(function: Stmt.Function, free: Set[JSName])
  private case class Result(stmts: List[Stmt], floating: List[Candidate])

  def transform(module: Module): Module =
    module.copy(
      exports = module.exports.map(e => e.copy(expr = rewrite(e.expr))),
      stmts = transform(module.stmts)
    )

  def transform(stmts: List[Stmt]): List[Stmt] = {
    val result = rewriteScope(stmts, Set.empty, repeated = false)
    result.floating.map(_.function) ++ result.stmts
  }

  /** Rewrite one lexical scope and return declarations that can cross it. */
  private def rewriteScope(stmts: List[Stmt], parameters: Set[JSName], repeated: Boolean): Result = {
    val rewritten = stmts.map(rewrite(_, repeated))
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

  private def rewrite(stmt: Stmt, repeated: Boolean): Result = stmt match {
    case Stmt.Block(label, stmts) =>
      val result = rewriteScope(stmts, Set.empty, repeated)
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
      // JavaScript switch clauses share one lexical scope.
      val switchBindings = branches.iterator.flatMap((_, stmts) => bindings(stmts)) ++
        default.iterator.flatMap(bindings)
      val bound = switchBindings.toSet
      val rewrittenBranches = branches.map { case (tag, stmts) =>
        val result = rewriteScope(stmts, bound, repeated)
        (rewrite(tag), result)
      }
      val rewrittenDefault = default.map(rewriteScope(_, bound, repeated))
      val floating = rewrittenBranches.flatMap(_._2.floating) ++ rewrittenDefault.toList.flatMap(_.floating)
      Result(List(Stmt.Switch(
        rewrite(scrutinee),
        rewrittenBranches.map { case (tag, result) => tag -> result.stmts },
        rewrittenDefault.map(_.stmts)
      )), floating)

    case Stmt.Function(name, params, stmts) =>
      // Parameters are introduced by entering the function. Its name, however,
      // is introduced by the declaration in the surrounding scope, so nested
      // declarations may float together with the function that names them.
      val result = rewriteScope(stmts, params.toSet, repeated = false)
      val rewritten: Stmt.Function = Stmt.Function(name, params, result.stmts)
      val captured = free(rewritten)
      val floating = result.floating ++ Option.when(repeated)(Candidate(rewritten, captured))
      Result(if repeated then Nil else List(rewritten), floating)

    case Stmt.Class(name, methods) =>
      // Methods are not ordinary nested declarations. Optimize their bodies,
      // but keep all declarations within the method boundary.
      val rewritten = methods.map { method =>
        val result = rewriteScope(method.stmts, method.params.toSet + method.name, repeated = false)
        method.copy(stmts = result.floating.map(_.function) ++ result.stmts)
      }
      Result(List(Stmt.Class(name, rewritten)), Nil)

    case Stmt.If(cond, thn, els) =>
      val thnResult = rewrite(thn, repeated)
      val elsResult = rewrite(els, repeated)
      Result(
        List(Stmt.If(rewrite(cond), asStmt(thnResult.stmts), asStmt(elsResult.stmts))),
        thnResult.floating ++ elsResult.floating
      )

    case Stmt.Try(prog, name, handler, fin) =>
      val progResult = rewriteScope(prog, Set.empty, repeated)
      val handlerResult = rewriteScope(handler, Set(name), repeated)
      val finResult = rewriteScope(fin, Set.empty, repeated)
      Result(
        List(Stmt.Try(progResult.stmts, name, handlerResult.stmts, finResult.stmts)),
        progResult.floating ++ handlerResult.floating ++ finResult.floating
      )

    case Stmt.Throw(expr) =>
      Result(List(Stmt.Throw(rewrite(expr))), Nil)

    case Stmt.While(label, cond, stmts) =>
      val result = rewriteScope(stmts, Set.empty, repeated = true)
      Result(List(Stmt.While(label, rewrite(cond), result.stmts)), result.floating)

    case stmt @ (Stmt.Break(_) | Stmt.Continue(_)) =>
      Result(List(stmt), Nil)

    case Stmt.ExprStmt(expr) =>
      Result(List(Stmt.ExprStmt(rewrite(expr))), Nil)
  }

  /** Lambdas are function boundaries, so declarations cannot leave them. */
  private def rewrite(expr: Expr): Expr = expr match {
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
      val result = rewrite(body, repeated = false)
      Expr.Lambda(params, asStmt(result.floating.map(_.function) ++ result.stmts))
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
