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
        val term = Context.ensureMainExists(mod)
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

    tree = Context.timed("new-normalizer-1", source.name) { NewNormalizer.run(tree) }
    Normalizer.assertNormal(tree)
    // println(util.show(tree))
    tree = Context.timed("new-normalizer-2", source.name) { NewNormalizer.run(tree) }
    Normalizer.assertNormal(tree)
    //tree = Normalizer.normalize(Set(mainSymbol), tree, Context.config.maxInlineSize().toInt)

    //    tree = Context.timed("old-normalizer-1", source.name) { Normalizer.normalize(Set(mainSymbol), tree, 0) }
    //    tree = Context.timed("old-normalizer-2", source.name) { Normalizer.normalize(Set(mainSymbol), tree, 0) }
    //
    //    tree = Context.timed("new-normalizer-3", source.name) { NewNormalizer.run(tree) }
    //    Normalizer.assertNormal(tree)
    //
    //    // (2) lift static arguments
    //    tree = Context.timed("static-argument-transformation", source.name) {
    //      StaticArguments.transform(mainSymbol, tree)
    //    }
    //
    //    def normalize(m: ModuleDecl) = {
    //      val anfed = BindSubexpressions.transform(m)
    //      val normalized = Normalizer.normalize(Set(mainSymbol), anfed, Context.config.maxInlineSize().toInt)
    //      Normalizer.assertNormal(normalized)
    //      val live = Deadcode.remove(mainSymbol, normalized)
    //      val tailRemoved = RemoveTailResumptions(live)
    //      val contified = DirectStyle.rewrite(tailRemoved)
    //      contified
    //    }
    //
    //    // (3) normalize a few times (since tail resumptions might only surface after normalization and leave dead Resets)
    //    tree = Context.timed("normalize-1", source.name) { normalize(tree) }
    //    tree = Context.timed("normalize-2", source.name) { normalize(tree) }
    //    tree = Context.timed("normalize-3", source.name) { normalize(tree) }

    tree
}
