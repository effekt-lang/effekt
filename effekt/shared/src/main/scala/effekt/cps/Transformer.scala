package effekt
package cps

import effekt.PhaseResult
import effekt.Phase
import effekt.context.Context
import effekt.lifted
import effekt.symbols.{ Symbol, builtins }
import effekt.context.assertions.*
import effekt.util.messages.ErrorReporter

object Transformer extends Phase[PhaseResult.CoreLifted, PhaseResult.CpsTransformed] {

  val phaseName = "cps-transform"


  def run(input: CoreLifted)(using Context): Option[CpsTransformed] =
    val transformed = cps.transform(input.core)
    Some(CpsTransformed(input.source, input.tree, input.mod, input.core, transformed))
}

def transform(core: lifted.ModuleDecl): ModuleDecl = ???

