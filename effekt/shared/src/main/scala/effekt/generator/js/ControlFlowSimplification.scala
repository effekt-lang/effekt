package effekt
package generator
package js

/** Local, scope-preserving simplifications of the generated JavaScript
 *  control flow. */
object ControlFlowSimplification {

  def transform(module: Module): Module =
    module.copy(
      exports = module.exports.map(binding => binding.copy(expr = transform(binding.expr))),
      stmts = transform(module.stmts))

  def transform(stmts: List[Stmt]): List[Stmt] =
    stmts.map(transform)

  /** A path ending in `break label` already ends at the end of the block
   *  labeled `label`, so the break is redundant. */
  private def removeRedundantBreaks(
    label: JSName,
    stmts: List[Stmt]
  ): (List[Stmt], Int) = stmts.lastOption match {
    case Some(Stmt.Break(Some(target))) if target == label =>
      stmts.init -> 1
    case Some(last) =>
      val (simplified, count) = removeRedundantBreaks(label, last)
      (stmts.init :+ simplified) -> count
    case None => stmts -> 0
  }

  private def removeRedundantBreaks(
    label: JSName,
    stmt: Stmt
  ): (Stmt, Int) = stmt match {
    case Stmt.Block(inner, stmts) =>
      val (body, count) = removeRedundantBreaks(label, stmts)
      Stmt.Block(inner, body) -> count

    case Stmt.If(cond, thn, els) =>
      val (newThn, thnCount) = removeRedundantBreaks(label, thn)
      val (newEls, elsCount) = removeRedundantBreaks(label, els)
      Stmt.If(cond, newThn, newEls) -> (thnCount + elsCount)

    // Removing the final statement of a switch arm would introduce
    // JavaScript fallthrough into the next arm.
    case _ => stmt -> 0
  }

  /** Whether a break that still requires the label remains. Function bodies
   *  are separate JavaScript control-flow domains and cannot target it. */
  private def breaksTo(label: JSName, stmt: Stmt): Boolean = stmt match {
    case Stmt.Break(Some(target)) => target == label
    case Stmt.Block(_, stmts) => stmts.exists(breaksTo(label, _))
    case Stmt.Switch(_, branches, default) =>
      branches.exists(_._2.exists(breaksTo(label, _))) ||
        default.exists(_.exists(breaksTo(label, _)))
    case Stmt.If(_, thn, els) => breaksTo(label, thn) || breaksTo(label, els)
    case Stmt.Try(prog, _, handler, fin) =>
      prog.exists(breaksTo(label, _)) ||
        handler.exists(breaksTo(label, _)) ||
        fin.exists(breaksTo(label, _))
    case Stmt.While(_, _, stmts) => stmts.exists(breaksTo(label, _))
    case _: (Stmt.Function | Stmt.Class) => false
    case _ => false
  }

  private def transform(stmt: Stmt): Stmt = stmt match {
    case Stmt.Block(label, stmts) =>
      val body = transform(stmts)
      label match {
        case Some(current) =>
          val (simplified, count) = removeRedundantBreaks(current, body)
          if count > 1 && !simplified.exists(breaksTo(current, _)) then {
            // The label encoded only a multi-way join. Preserve an
            // anonymous block when it carries a lexical scope.
            simplified match {
              case single :: Nil => single
              case _ => Stmt.Block(None, simplified)
            }
          } else if count > 0 then Stmt.Block(label, simplified)
          else Stmt.Block(label, body)
        case None => Stmt.Block(None, body)
      }

    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.RawStmt(raw, args) =>
      Stmt.RawStmt(raw, args.map(transform))
    case Stmt.Const(pattern, binding) =>
      Stmt.Const(pattern, transform(binding))
    case Stmt.Let(pattern, binding) =>
      Stmt.Let(pattern, transform(binding))
    case Stmt.Assign(target, value) =>
      Stmt.Assign(transform(target), transform(value))
    case Stmt.Destruct(names, binding) =>
      Stmt.Destruct(names, transform(binding))
    case Stmt.Switch(scrutinee, branches, default) =>
      Stmt.Switch(
        transform(scrutinee),
        branches.map { case (tag, body) => transform(tag) -> transform(body) },
        default.map(transform))
    case Stmt.Function(name, params, stmts) =>
      Stmt.Function(name, params, transform(stmts))
    case Stmt.Class(name, methods) =>
      Stmt.Class(name, methods.map(method => method.copy(stmts = transform(method.stmts))))
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Try(prog, name, handler, fin) =>
      Stmt.Try(transform(prog), name, transform(handler), transform(fin))
    case Stmt.Throw(expr) =>
      Stmt.Throw(transform(expr))
    case Stmt.While(label, cond, stmts) =>
      Stmt.While(label, transform(cond), transform(stmts))
    case break: Stmt.Break => break
    case continue: Stmt.Continue => continue
    case Stmt.ExprStmt(expr) =>
      Stmt.ExprStmt(transform(expr))
  }

  private def transform(expr: Expr): Expr = expr match {
    case Expr.Call(callee, arguments) =>
      Expr.Call(transform(callee), arguments.map(transform))
    case Expr.New(callee, arguments) =>
      Expr.New(transform(callee), arguments.map(transform))
    case Expr.RawExpr(raw, args) =>
      Expr.RawExpr(raw, args.map(transform))
    case literal: Expr.RawLiteral => literal
    case Expr.IfExpr(cond, thn, els) =>
      Expr.IfExpr(transform(cond), transform(thn), transform(els))
    case Expr.Lambda(params, body) =>
      Expr.Lambda(params, transform(body))
    case Expr.Object(properties) =>
      Expr.Object(properties.map { case (name, value) => name -> transform(value) })
    case Expr.Member(callee, selection) =>
      Expr.Member(transform(callee), selection)
    case Expr.ArrayLiteral(elements) =>
      Expr.ArrayLiteral(elements.map(transform))
    case variable: Expr.Variable => variable
  }
}
