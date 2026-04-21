package effekt.core.optimizer

import effekt.{Phase, PhaseResult, core, symbols}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.Stmt.{App, Def}
import effekt.core.{Block, Expr, Id, ModuleDecl, Stmt, Toplevel, Tree, ValueType}
import effekt.core.Block.*
import effekt.symbols.{TermSymbol, TypeSymbol}

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    val CoreTransformed(source, tree, mod, modDec) = input
    //println(effekt.util.PrettyPrinter.format(modDec).layout)

    object transform extends Tree.Rewrite {
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) =>
          println("in Toplevel")
          println(id.name.name)
          if (id.name.name == "simpleTRMC") {
            trmc(id, block)
          } else {
            Toplevel.Def(id, block)
          }

      }

      override def stmt: PartialFunction[Stmt, Stmt] = {
        case Def(id, block, body) if id.name.name == "simpleTRMC" =>
          trmc(id, block, body)
      }
    }

    val transformed = Context.timed(phaseName, source.name) {
      transform.rewrite(modDec)
    }

    Some(CoreTransformed(source, tree, mod, transformed))
  }
    

  def freeInStmt(id:Id, stmt: Stmt): Boolean = stmt.free.freeIds.contains(id)
  def freeInExpr(id:Id, expr: Expr): Boolean = expr.free.freeIds.contains(id)
  def freeInBlock(id:Id, block: Block): Boolean = block.free.freeIds.contains(id)
  

  def trmc(id: Id, block: Block): Toplevel = ???
  
  def trmc(id: Id, block: Block, body: Stmt): Stmt = ???
  
  def trmc(id: Id, stmt: Stmt, cont: Stmt): Stmt = ???

}
