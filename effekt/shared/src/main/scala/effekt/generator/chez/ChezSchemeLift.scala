package effekt
package generator
package chez

import effekt.context.Context
import effekt.lifted.{ LiftInference, Monomorphize }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

class ChezSchemeLift extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".ss"

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => steps.afterLift(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => steps.afterChez(source).map { expr => pretty(expr) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => res.core }
    case Stage.Lifted => steps.afterLift(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => steps.afterChez(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Lifted => Chez
  lazy val Compile =
    allToCore(Core) andThen Aggregate andThen LiftInference andThen Monomorphize andThen ToChez map { case (main, expr) =>
      (Map(main -> pretty(expr)), main)
    }

  lazy val Core = Phase.cached("core") { Frontend andThen Middleend }

  lazy val ToChez = Phase("chez") {
    case CoreLifted(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      mainFile -> chez.Let(Nil, TransformerLift.compilationUnit(mainSymbol, mod, core))
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) map { c => c.main }
    val afterLift = afterCore andThen LiftInference andThen Monomorphize
    val afterChez = afterLift andThen ToChez map { case (f, prog) => prog }
  }

  // Helpers
  // -------
  private def pretty(expr: chez.Expr): Document =
    chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(expr), 100)
}
