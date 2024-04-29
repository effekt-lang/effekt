package effekt
package generator
package chez

import effekt.context.Context
import effekt.symbols.{Module, Symbol}
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

class ChezSchemeMonadic extends ChezScheme {
  def compilationUnit(mainSymbol: Symbol, mod: Module, decl: core.ModuleDecl): chez.Block =
    chez.TransformerMonadic.compilationUnit(mainSymbol, mod, decl)

  override def supportedFeatureFlags: List[String] = List("chezMonadic", "chez")
}


class ChezSchemeCallCC extends ChezScheme {
  def compilationUnit(mainSymbol: Symbol, mod: Module, decl: core.ModuleDecl): chez.Block =
    chez.TransformerCallCC.compilationUnit(mainSymbol, mod, decl)

  override def supportedFeatureFlags: List[String] =  List("chezCallCC", "chez")
}


trait ChezScheme extends Compiler[String] {

  def compilationUnit(mainSymbol: Symbol, mod: Module, decl: core.ModuleDecl): chez.Block

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".ss"

  override def supportedFeatureFlags: List[String] = List("chez")

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => Separate(source).map { res => pretty(res) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => Separate(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)

  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Chez
  lazy val Compile =
    allToCore(Core) andThen Aggregate andThen core.Optimizer andThen Chez map { case (main, expr) =>
      (Map(main -> pretty(expr).layout), main)
    }

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Chez = Phase("chez") {
    case CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      mainFile -> chez.Let(Nil, compilationUnit(mainSymbol, mod, core))
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  lazy val Separate =
    allToCore(Core) map { all => all.main } andThen Chez map { case (_, expr) => expr }

  // Helpers
  // -------
  def pretty(expr: chez.Expr): Document =
    chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(expr), 100)

  def pretty(defs: List[chez.Def]): Document =
    chez.PrettyPrinter.format(defs)
}
