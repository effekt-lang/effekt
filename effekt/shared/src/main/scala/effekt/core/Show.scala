package effekt.core

import effekt.Phase
import effekt.PhaseResult.CoreTransformed
import effekt.context.Context

object Show extends Phase[CoreTransformed, CoreTransformed] {

    override val phaseName: String = "show instances"

    override def run(input: CoreTransformed)(using ctx: Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      
      core.declarations.foreach(println)
      println()
      core.definitions.foreach(println)
      println()
      core.includes.foreach(println)

      Some(CoreTransformed(source, tree, mod, core))
    }
  }

}
