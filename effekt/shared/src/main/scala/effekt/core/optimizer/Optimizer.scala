package effekt
package core
package optimizer

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.optimizer.Usage.{ Once, Recursive }
import effekt.core.optimizer.normalizer.NewNormalizer
import kiama.util.Source

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val term = Context.ensureMainExists(mod)
        val optimizer = if (Context.config.newNormalizer()) {
          optimizeWithNewNormalizer
        } else {
          optimize
        }
        val optimized = Context.timed("optimize", source.name) { optimizer(source, term, core) }
        Some(CoreTransformed(source, tree, mod, optimized))
    }

  def optimize(source: Source, mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl = {
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
        val live = Deadcode.remove(mainSymbol, normalized)
        val tailRemoved = RemoveTailResumptions(live)
        val contified = DirectStyle.rewrite(tailRemoved)
        contified
      }

      // (3) normalize a few times (since tail resumptions might only surface after normalization and leave dead Resets)
      tree = Context.timed("normalize-1", source.name) {
        normalize(tree)
      }
      tree = Context.timed("normalize-2", source.name) {
        normalize(tree)
      }
      tree = Context.timed("normalize-3", source.name) {
        normalize(tree)
      }

      tree
  }

  def optimizeWithNewNormalizer(source: Source, mainSymbol: symbols.Symbol, core: ModuleDecl)(using Context): ModuleDecl = {
    var tree = core

    // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
    tree = Context.timed("deadcode-elimination", source.name) {
      Deadcode.remove(mainSymbol, tree)
    }

    if !Context.config.optimize() then return tree;

    val inliningPolicy = UniqueJumpSimple(
      maxInlineSize = 15
    )

    def normalize(m: ModuleDecl) = Context.timed("new-normalizer", source.name) {
      val staticArgs = StaticArguments.transform(mainSymbol, m)
      val normalized = NewNormalizer().run(staticArgs)
      val reachability = Reachable(Set(mainSymbol), normalized)
      val inlined = Inliner(inliningPolicy, reachability).run(normalized)
      val live = Deadcode.remove(mainSymbol, inlined)
      val tailRemoved = RemoveTailResumptions(live)
      //val contified = DirectStyle.rewrite(tailRemoved)
      tailRemoved
    }

    tree = normalize(tree)
    tree = normalize(tree)
    tree = normalize(tree)
    tree = normalize(tree)
    //util.trace(tree)
    tree
  }
}
