package effekt.core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.optimizer.Deadcode
import effekt.Phase

object DeadCodeElimination extends Phase[CoreTransformed, CoreTransformed] {
    val phaseName: String = "deadcode-elimination"

    def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
        input match {
            case CoreTransformed(source, tree, mod, core) => 
                val term = Context.ensureMainExists(mod)
                val dce = Context.timed("deadcode-elimination", source.name) {
                    Deadcode.remove(term, core)
                }
                Some(CoreTransformed(source, tree, mod, dce))
        }
}