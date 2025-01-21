package effekt
package generator
package vm

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.{ ModuleDecl, Id }

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

/**
 * A "backend" that simply outputs the aggregated core module.
 * This is called IR and note Core to avoid name clashes with package `effekt.core`
 *
 * This is, for example, used by the interpreter.
 */
class VM extends Compiler[(Id, symbols.Module, ModuleDecl)] {

  def extension = ".effekt-core.ir"

  override def supportedFeatureFlags: List[String] = List("vm")

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = None

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = None

  override def compile(source: Source)(using C: Context): Option[(Map[String, String], (Id, symbols.Module, ModuleDecl))] =
    Optimized.run(source).map { res => (Map.empty, res) }


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => CPS => JS
  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate andThen core.optimizer.Optimizer map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      (mainSymbol, mod, core)
  }
}
