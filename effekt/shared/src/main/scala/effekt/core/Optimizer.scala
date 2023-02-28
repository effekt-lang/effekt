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
        val opt = definitions.map(eliminateReturnRun.rewrite)
        val start = ModuleDecl(path, imports, declarations, externs, opt.map(directStyleVal.rewrite), exports)

        val aliases = rmIdKey[Id](collectAliases(start), Set("main"))
        var optimized = dealiasing(start)(using aliases) //Dealiasing

        optimized = constantPropagation(optimized) // Constant Propagation

        val recursiveFunctions = findRecursiveFunctions(optimized)
        optimized = staticArgumentTransformation(optimized, recursiveFunctions) //Static Argument Transformation

        var occurences = rmIdKey[Int](countFunctionOccurences(optimized), Set("main"))
        var bodies = rmIdKey[Block](collectFunctionDefinitions(optimized), Set("main"))
        optimized = inlineUnique(optimized, bodies, occurences) //Inline Unique Functions

        bodies = rmIdKey[Block](collectFunctionDefinitions(optimized), Set("main"))
        optimized = inlineGeneral(optimized, bodies, 30) // Inline General

        occurences = rmIdKey[Int](countFunctionOccurences(optimized), Set("main"))
        optimized = removeUnusedFunctions(optimized, occurences, recursiveFunctions, exports) //Remove Unused Functions

        optimized = betaReduction(optimized) // Beta Reduction
        optimized = betaReduction(optimized) // Beta Reduction

        optimized
  }
}
