package effekt
package core
package optimizer

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context

import kiama.util.Source

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val term = Context.checkMain(mod)
        val optimized = Context.timed("optimize", source.name) { optimize(source, term, core) }
        Some(CoreTransformed(source, tree, mod, optimized))
    }

  def optimize(source: Source, mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl =

    var tree = core

     // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
    tree = Context.timed("deadcode-elimination", source.name) {
      Deadcode.remove(mainSymbol, tree)
    }

    if !Context.config.optimize() then return tree;

    // (2) lift static arguments
    tree = Context.timed("static-argument-transformation", source.name) {
      StaticArguments.transform(mainSymbol, tree)
    }

    def normalize(m: ModuleDecl) = {
      val anfed = BindSubexpressions.transform(m)
      val normalized = Normalizer.normalize(Set(mainSymbol), anfed, Context.config.maxInlineSize().toInt)
      Deadcode.remove(mainSymbol, normalized)
    }

    // (3) normalize once and remove beta redexes
    tree = Context.timed("normalize-1", source.name) { normalize(tree) }

    // (4) optimize continuation capture in the tail-resumptive case
    tree = Context.timed("tail-resumptions", source.name) { RemoveTailResumptions(tree) }

    // (5) normalize again to clean up and remove new redexes
    tree = Context.timed("normalize-2", source.name) { normalize(tree) }

    tree
}
