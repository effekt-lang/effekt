package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.optimizer.{DropBindings, Optimizer}
import kiama.util.Source
import kiama.output.PrettyPrinterTypes.Document

class ChezSchemeCPS extends Compiler[String] {

    override def extension: String = ".ss"

    override def supportedFeatureFlags: List[String] = List("chez", "chez-cps")

    override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = ???

    override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = ???

    override def compile(source: Source)(using C: Context) = Compile(source)

    // The Compilation Pipeline
    // ------------------------
    // Source => Core => CPS => ChezCPS
    lazy val Core = Phase.cached("core") {
        Frontend andThen Middleend
    }

    lazy val Optimized = allToCore(Core) andThen Aggregate andThen Optimizer andThen DropBindings map {
        case input @ CoreTransformed(source, tree, mod, core) =>
        val mainSymbol = Context.ensureMainExists(mod)
        val mainFile = path(mod)
        (mainSymbol, mainFile, core)
    }

    lazy val CPSTransformed = Optimized map {
        case (mainSymbol, mainFile, core) =>
        val cpsTransformed = effekt.cps.Transformer.transform(core)
        val contified = cps.Contify.rewrite(cpsTransformed)
        (mainSymbol, mainFile, core, contified)
    }

    lazy val Compile = CPSTransformed map {
        case (mainSymbol, mainFile, core, cps) =>
        val doc = pretty(TransformerCPS.compile(cps, core, mainSymbol))
        (Map(mainFile -> doc.layout), mainFile)
    }

    private def pretty(block: chez.Block): Document =
        chez.PrettyPrinterCPS.format(block)
}