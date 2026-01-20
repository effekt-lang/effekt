package effekt.core.optimizer

import effekt.{Phase, PhaseResult}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.Stmt.App
import effekt.core.{Block, Expr, ModuleDecl, Stmt, Tree, ValueType}

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    val CoreTransformed(source, tree, mod, modDec) = input
    
    object transform extends Tree.Rewrite{
      override def stmt: PartialFunction[Stmt, Stmt] = {
        case Stmt.App(callee, targs, vargs, bargs) if tailRecModCons(callee, targs, vargs, bargs) => 
          trmc(callee, targs, vargs, bargs)
      }
    }

    val transformed = Context.timed(phaseName, source.name) {
      transform.rewrite(modDec)
    }

    Some(CoreTransformed(source, tree, mod, transformed))
    
  def tailRecModCons(callee: Block, targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Boolean =
    false
  
  def trmc(callee: Block, targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Stmt =
    App(callee: Block, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])
  
}
