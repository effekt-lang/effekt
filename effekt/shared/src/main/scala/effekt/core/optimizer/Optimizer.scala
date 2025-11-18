package effekt
package core
package optimizer

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.optimizer.Usage.{ Once, Recursive }
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

    // (2) lift static arguments
    tree = StaticArguments.transform(mainSymbol, tree)

    val inliningPolicy = UniqueJumpSimple(
      maxInlineSize = 150
    )
    def normalize(m: ModuleDecl) = {
      val anfed = BindSubexpressions.transform(m)
      val normalized = NewNormalizer().run(anfed)
      val inlined = Inliner(inliningPolicy, Reachable(Set(mainSymbol), normalized)).run(normalized)
      val live = Deadcode.remove(mainSymbol, inlined)
      val tailRemoved = RemoveTailResumptions(live)
      val contified = DirectStyle.rewrite(tailRemoved)
      contified
    }

    tree = Context.timed("new-normalizer-1", source.name) { normalize(tree) }
    tree = Context.timed("new-normalizer-2", source.name) { normalize(tree) }
    tree
}
