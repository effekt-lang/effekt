package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context

import kiama.util.Source

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val term = Context.checkMain(mod)
        val optimized = optimize(source, term, core)
        Some(CoreTransformed(source, tree, mod, optimized))
    }

  def optimize(source: Source, mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl =
     // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
    val tree = Context.timed("deadcode-elimination", source.name) { Deadcode.remove(mainSymbol, core) }

    if !Context.config.optimize() then return tree;

    // (2) inline unique block definitions
    Context.timed("inliner", source.name) {
      Inline.full(Set(mainSymbol), tree, Context.config.maxInlineSize().toInt)
    }
}
