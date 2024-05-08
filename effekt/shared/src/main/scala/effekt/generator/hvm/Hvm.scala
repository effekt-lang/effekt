package effekt
package generator
package hvm

import effekt.context.Context
import effekt.symbols.{ Module, Symbol }
import effekt.lifted.{ LiftInference, Monomorphize }
import effekt.cps

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.core.PolymorphismBoxing.transform



class Hvm extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".hvm"

  override def supportedFeatureFlags: List[String] = List("hvm")

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => steps.afterLift(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => steps.afterHvm(source).map { expr => pretty(expr) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => res.core }
    case Stage.Lifted => steps.afterLift(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => steps.afterHvm(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Lifted => Chez
  lazy val Compile =
    allToCore(Core) andThen Aggregate andThen core.Optimizer andThen LiftInference andThen cps.Transformer andThen ToHvm map { case (main, expr) =>
      (Map(main -> pretty(expr).layout), main)
    }

  lazy val Core = Phase.cached("core") { Frontend andThen Middleend }

  lazy val ToHvm = Phase("hvm") {
    case CpsTransformed(source, tree, mod, core, cpsed) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      val realResult = hvm.transform(cpsed) //result map transform
      mainFile -> realResult //------------------------------------------------------------------------------------------
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) map { c => c.main }
    val afterLift = afterCore andThen LiftInference
    val afterHvm = afterLift andThen ToHvm map { case (f, prog) => prog }
  }

  // Helpers
  // -------
  private def pretty(expr: hvm.Term): Document =
    hvm.PrettyPrinter.pretty(hvm.PrettyPrinter.toDoc(expr), 100)

  private def pretty(expr: hvm.Book): Document =
    hvm.PrettyPrinter.pretty(hvm.PrettyPrinter.toDoc(expr), 100)
}