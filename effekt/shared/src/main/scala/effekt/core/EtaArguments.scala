package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context

/**
 * Passes a block on instead of a block literal that only forwards (eta for a block variable):
 *
 *   f(...){ [T](x){g} => h[T](x){g} }   ~>   f(...){h}     if `h` is free in the literal
 *
 * Only in argument position!
 */
object EtaArguments extends Phase[CoreTransformed, CoreTransformed] {
  override val phaseName: String = "eta-arguments"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case input @ CoreTransformed(_, _, _, core) =>
      if (Context.config.optimize()) {
        Some(input.copy(core = reduceArguments.rewrite(core)))
      } else {
        Some(input)
      }
  }

  object reduceArguments extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = super.rewrite(s) match {
      case Stmt.App(callee, targs, vargs, bargs) => Stmt.App(callee, targs, vargs, bargs.map(reduced))
      case Stmt.Invoke(callee, method, tpe, targs, vargs, bargs) => Stmt.Invoke(callee, method, tpe, targs, vargs, bargs.map(reduced))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Stmt.ImpureApp(id, callee, targs, vargs, bargs.map(reduced), body)
      case other => other
    }
  }

  def reduced(b: Block): Block = b match {
    case literal @ BlockLit(tparams, cparams, vparams, bparams, Stmt.App(h: BlockVar, targs, vargs, bargs))
        if targs == tparams.map(ValueType.Var(_))
        && vargs.map { case ValueVar(x, _) => Some(x); case _ => None } == vparams.map { p => Some(p.id) }
        && bargs.map { case BlockVar(g, _, _) => Some(g); case _ => None } == bparams.map { p => Some(p.id) }
        && literal.free.contains(h.id) => h

    case other => other
  }
}
