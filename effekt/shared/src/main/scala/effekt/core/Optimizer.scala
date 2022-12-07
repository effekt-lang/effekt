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
      case CoreTransformed(source, tree, mod, ModuleDecl(path, imports, decls, externs, defs, exports)) =>
        val optimized = defs map optimize
        Some(CoreTransformed(source, tree, mod, ModuleDecl(path, imports, decls, externs, optimized, exports)))
    }

  def optimize(d: Definition)(using Context): Definition = {

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
          core.Let(id, rewrite(expr), rewrite(body))
      }
    }

    val opt = eliminateReturnRun.rewrite(d)
    val res = directStyleVal.rewrite(opt)

    // More optimizations can go here.
    res
  }
}
