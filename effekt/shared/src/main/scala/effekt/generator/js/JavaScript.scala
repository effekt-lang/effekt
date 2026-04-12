package effekt
package generator
package js

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.Renamer
import effekt.core.optimizer.{ Deadcode, DropBindings, Optimizer }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source


class JavaScript(additionalFeatureFlags: List[String] = Nil) extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".js"

  override def supportedFeatureFlags: List[String] = additionalFeatureFlags ++ TransformerCps.jsFeatureFlags

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = stage match {
    case Stage.Core if C.config.optimize() => Optimized(source).map { (_, _, res) => core.PrettyPrinter(Context.config.debug()).format(res) }
    case Stage.CPS => CPSTransformed(source).map { (_, _, _, res) => cpsds.PrettyPrinter.format(res) }
    case Stage.Core => Core(source).map { res => core.PrettyPrinter(Context.config.debug()).format(res.core) }
    case Stage.Machine => None
    case Stage.Target => CompileLSP(source).map { pretty }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.CPS => None
    case Stage.Machine => None
    case Stage.Target => CompileLSP(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)

  def compileWeb(source: Source)(using C: Context) = CompileWeb(source)


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => CPS => JS
  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate andThen Deadcode andThen core.Show andThen Optimizer andThen DropBindings map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.ensureMainExists(mod)
      val mainFile = path(mod)
      (mainSymbol, mainFile, core)
  }

  lazy val CPSTransformed = Optimized map {
    case (mainSymbol, mainFile, core) =>
      // establish unique names
      val renamed = new Renamer().apply(core)

      var tree = effekt.cpsds.transform(renamed)

      def optimize(tree: effekt.cpsds.ModuleDecl) =
        val static = effekt.cpsds.StaticArguments.transform(tree)
        effekt.cpsds.Inliner.transform(mainSymbol, static)

      tree = optimize(tree)
      tree = optimize(tree)
      tree = optimize(tree)

      (mainSymbol, mainFile, core, tree)
  }

  lazy val Compile = CPSTransformed map {
    case (mainSymbol, mainFile, core, cps) =>
      val doc = pretty(TransformerCpsDs.compile(cps, core, mainSymbol).commonjs)
      (Map(mainFile -> doc.layout), mainFile)
  }

  /**
   * Like [[Compile]], but uses module layout [[js.Module.virtual]]
   */
  lazy val CompileWeb = CPSTransformed map {
    case (mainSymbol, mainFile, core, cps) =>
      val doc = pretty(TransformerCpsDs.compile(cps, core, mainSymbol).virtual)
      (Map(mainFile -> doc.layout), mainFile)
  }

  /**
   * Like [[Compile]], but shows only the generated javascript functions (no externs, no declarations)
   */
  lazy val CompileLSP = CPSTransformed map {
    case (mainSymbol, mainFile, core, cps) =>
      TransformerCpsDs.compileLSP(cps, core)
  }

  private def pretty(stmts: List[js.Stmt]): Document =
    js.PrettyPrinter.format(stmts)
}
class JavaScriptWeb extends JavaScript(List("jsWeb")) {}
class JavaScriptNode extends JavaScript(List("jsNode")) {}
