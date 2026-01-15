package effekt.core.optimizer

import effekt.{Phase, PhaseResult}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.{ModuleDecl, Tree}

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    val CoreTransformed(source, tree, mod, modDec) = input

    val transformed = Context.timed(phaseName, source.name) {
      trmc(modDec)
    }

    Some(CoreTransformed(source, tree, mod, transformed))
  def trmc(tree: ModuleDecl): ModuleDecl =
    tree
  
}
