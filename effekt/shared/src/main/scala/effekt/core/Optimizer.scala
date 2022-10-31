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
      case CoreTransformed(source, tree, mod, ModuleDecl(path, imports, defs, exports)) =>
        val optimized = optimize(defs)
        Some(CoreTransformed(source, tree, mod, ModuleDecl(path, imports, optimized, exports)))
    }

  def optimize(s: Stmt)(using Context): Stmt = {

    // a very small and easy post processing step...
    // reduces run-return pairs
    object eliminateReturnRun extends core.Tree.Rewrite {
      override def expr = {
        case core.Run(core.Return(p), _) => rewrite(p)
      }
    }

    // rewrite (Val (Return e) s) to (Let e s)
    object directStyleVal extends core.Tree.Rewrite {
      override def stmt = {
        case core.Val(id, tpe, core.Return(expr), body) =>
          core.Let(id, tpe, rewrite(expr), rewrite(body))
      }
    }

    val opt = eliminateReturnRun.rewrite(s)
    val res = directStyleVal.rewrite(opt)

    // More optimizations can go here.
    res
  }
}
