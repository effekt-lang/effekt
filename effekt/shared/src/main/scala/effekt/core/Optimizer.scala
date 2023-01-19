package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*

def rmIdKey[T](input: Map[Id, T], rm: Set[String]): Map[Id, T] =
  input.filter((x, _) => !rm.contains(x.name.name))

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, module) =>
        Some(CoreTransformed(source, tree, mod, optimize(module)))
    }

  def optimize(m: ModuleDecl)(using Context): ModuleDecl = {

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

    m match
      case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
        val opt = definitions.map(eliminateReturnRun.rewrite(_))
        val start = ModuleDecl(path, imports, declarations, externs, opt.map(directStyleVal.rewrite(_)), exports)

        val aliases = rmIdKey[Id](collectAliases(start), Set("main"))
        val dealiased = dealiasing(start)(using aliases)

        val calls = rmIdKey[Int](countFunctionCalls(dealiased), Set("main"))
        val unused_removed = removeUnusedFunctions(dealiased, calls).asInstanceOf[ModuleDecl]

        unused_removed
  }
}
