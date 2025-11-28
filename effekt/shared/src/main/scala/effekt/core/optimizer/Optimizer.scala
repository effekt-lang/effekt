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
