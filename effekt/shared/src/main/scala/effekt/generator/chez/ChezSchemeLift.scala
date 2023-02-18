package effekt
package generator
package chez

import effekt.context.Context
import effekt.lifted.LiftInference

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source


class ChezSchemeLift extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".ss"

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => Separate(source).map { expr => pretty(expr) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => Separate(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Lifted => Chez
  lazy val Compile =
    allToCore(Core) andThen Aggregate andThen LiftInference andThen Chez map { case (main, expr) =>
      (Map(main -> pretty(expr)), main)
    }

  lazy val Core = Phase.cached("core") { Frontend andThen Middleend }

  lazy val Chez = Phase("chez") {
    case CoreLifted(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      mainFile -> chez.Let(Nil, TransformerLift.compilationUnit(mainSymbol, mod, core))
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  lazy val Separate =
    allToCore(Core) map { all => all.main } andThen LiftInference andThen Chez map { case (_, expr) => expr }


  // Helpers
  // -------
  private def pretty(expr: chez.Expr): Document =
    chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(expr), 100)
}