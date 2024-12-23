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

    val anfed = BindSubexpressions.transform(lifted)

    // println(s"BEFORE\n\n ${util.show(anfed)}")

    val normalized = Normalizer.normalize(Set(mainSymbol), anfed)
    //
    val gced = Deadcode.remove(mainSymbol, normalized)

    val anfed2 = BindSubexpressions.transform(gced)

    val normalized2 = Normalizer.normalize(Set(mainSymbol), anfed2)
    //
    val gced2 = Deadcode.remove(mainSymbol, normalized2)

    //println(s"AFTER\n\n ${util.show(gced)}")
    gced2
}
