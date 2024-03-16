package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val term = Context.checkMain(mod)
        val optimized = Context.timed(phaseName, source.name) { optimize(term, core) }
        Some(CoreTransformed(source, tree, mod, optimized))
    }

  def optimize(mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl =
     // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
    val tree = Deadcode.remove(mainSymbol, core)

    // (2) inline unique block definitions
    //Inline.full(Set(mainSymbol), tree)
    tree
}
