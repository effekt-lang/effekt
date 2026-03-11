package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.optimizer.{DropBindings, Optimizer}
import kiama.util.Source
import kiama.output.PrettyPrinterTypes.Document

class ChezSchemeCPS extends Compiler[String] {

  override def extension: String = ".ss"

  override def supportedFeatureFlags: List[String] = List("chez", "chezCPS")

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = stage match {
    case Stage.Core if C.config.optimize() => Optimized(source).map { (_, _, res) => core.PrettyPrinter(Context.config.debug()).format(res) }
    case Stage.Core => Core(source).map { res => core.PrettyPrinter(Context.config.debug()).format(res.core) }
    case Stage.CPS => CPSTransformed(source).map { (_, _, _, res) => cps.PrettyPrinter.format(res) }
    case Stage.Machine => None
    case Stage.Target => LSP(source)
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.CPS => CPSTransformed(source).map { (_, _, _, res) => res }
    case Stage.Machine => None
    case Stage.Target => LSP(source)
  }

  override def compile(source: Source)(using C: Context) = Chez(source)

  // The Compilation Pipeline
  // ------------------------
  // Source => Core => CPS => Chez
  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate andThen Optimizer andThen core.Show map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.ensureMainExists(mod)
      val mainFile = path(mod)
      (mainSymbol, mainFile, core)
  }

  lazy val CPSTransformed = Optimized map {
    case (mainSymbol, mainFile, core) =>
      val cpsTransformed = effekt.cps.Transformer.transform(core)
      (mainSymbol, mainFile, core, cpsTransformed)
  }

  lazy val Chez = CPSTransformed map {
    case (mainSymbol, mainFile, core, cps) =>
      val compiled = TransformerCPS.compile(cps, mainSymbol)
      val doc = pretty(chez.Let(Nil, compiled))
      (Map(mainFile -> doc.layout), mainFile)
  }

  // TODO: Only show generated code
  lazy val LSP = CPSTransformed map {
    case (mainSymbol, mainFile, core, cps) =>
      val compiled = TransformerCPS.compileLSP(cps, mainSymbol)
      pretty(chez.Let(Nil, compiled))
  }

  def pretty(expr: chez.Expr): Document =
    chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(expr), 100)
}