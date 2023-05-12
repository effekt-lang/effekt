package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, module) =>
        Some(CoreTransformed(source, tree, mod, optimize(module)))
    }

  def optimize(m: ModuleDecl)(using Context): ModuleDecl = {
    val ModuleDecl(path, imports, declarations, externs, definitions, exports) = m

    ModuleDecl(path, imports, declarations, externs, definitions map optimize, exports)
  }

  def optimize(s: Definition)(using Context): Definition = {
    val opt = eliminateReturnRun.rewrite(s)
    directStyleVal.rewrite(opt)
  }


  // a very small and easy post processing step...
  // reduces run-return pairs
  object eliminateReturnRun extends core.Tree.Rewrite {
    override def expr = {
      case core.Run(core.Return(p)) => rewrite(p)
    }
  }

  // rewrite (Val (Return e) s) to (Let e s)
  object directStyleVal extends core.Tree.Rewrite {
    override def stmt = {
      case core.Val(id, core.Return(expr), body) =>
        Let(id, rewrite(expr), rewrite(body))
    }
  }
}
