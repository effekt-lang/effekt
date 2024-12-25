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
        val optimized = Context.timed("optimize", source.name) { optimize(source, term, core) }
        Some(CoreTransformed(source, tree, mod, optimized))
    }

  def optimize(source: Source, mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl =
     // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
    val tree = Context.timed("deadcode-elimination", source.name) { Deadcode.remove(mainSymbol, core) }

    if !Context.config.optimize() then return tree;

    // (2) lift static arguments (worker/wrapper)
    val lifted = StaticArguments.transform(mainSymbol, tree)

    val inlineSize = Context.config.maxInlineSize().toInt

    def normalizeOnce(m: ModuleDecl) = {
      val anfed = BindSubexpressions.transform(m)
      val normalized = Normalizer.normalize(Set(mainSymbol), anfed, inlineSize)
      Deadcode.remove(mainSymbol, normalized)
    }

    val normalized1 = Context.timed("normalize-1", source.name) { normalizeOnce(lifted) }
    val withoutTail = Context.timed("tail-resumptions", source.name) { RemoveTailResumptions(normalized1) }
    val normalized2 = Context.timed("normalize-2", source.name) { normalizeOnce(withoutTail) }

    normalized2
}
