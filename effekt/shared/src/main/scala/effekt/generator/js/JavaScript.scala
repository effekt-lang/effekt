package effekt
package generator
package js

import effekt.context.Context
import effekt.core.DirectStyleMutableState

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source


class JavaScript extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".js"

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => Separate(source) map { case (core, prog) => pretty(prog.virtual) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => Separate(source) map { case (core, prog) => prog }
  }

  override def compile(source: Source)(using C: Context) = Compile(source)

  override def compileSeparate(source: Source)(using Context): Option[(CoreTransformed, String)] =
    Separate(source).map { (core, prog) => (core, pretty(prog.virtual).layout) }


  // The Compilation Pipeline
  // ------------------------
  // Source => Core [=> DirectStyleState] => JS
  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend andThen DirectStyleMutableState
  }

  lazy val Compile = allToCore(Core) andThen Aggregate andThen core.Optimizer map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      val doc = pretty(TransformerDS.compile(input, mainSymbol).commonjs)
      (Map(mainFile -> doc.layout), mainFile)
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  lazy val Separate = allToCore(Core) map { in => (in.main, Transformer.compileSeparate(in)) }

  private def pretty(stmts: List[js.Stmt]): Document =
    js.PrettyPrinter.format(stmts)
}
