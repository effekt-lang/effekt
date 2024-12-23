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
    val tree = Deadcode.remove(mainSymbol, core)

    if !Context.config.optimize() then return tree;

    // (2) lift static arguments (worker/wrapper)
    val lifted = StaticArguments.transform(mainSymbol, tree)

    val inlineSize = Context.config.maxInlineSize().toInt

    def normalizeOnce(m: ModuleDecl) = {
      val anfed = BindSubexpressions.transform(m)
      val normalized = Normalizer.normalize(Set(mainSymbol), anfed, inlineSize)
      Deadcode.remove(mainSymbol, normalized)
    }

    // we normalize twice in order to deadcode -> remove tailresumption
    val normalized1 = normalizeOnce(lifted)

    // println(util.show(normalized1))
    val normalized2 = normalizeOnce(normalized1)
    normalized2

}
