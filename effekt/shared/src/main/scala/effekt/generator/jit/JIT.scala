package effekt
package generator
package jit

import effekt.context.Context
import effekt.lifted.LiftInference
import effekt.symbols.{Module, TermSymbol}
import effekt.util.paths.*

import scala.language.implicitConversions
import kiama.output.PrettyPrinterTypes.{Document, emptyLinks}
import kiama.util.Source

import scala.sys.process.Process

class JIT() extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".rpyeffect"

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted | Stage.Machine => None
    case Stage.Target => steps.afterJIT(source).map { res => pretty(res) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => res.core }
    case Stage.Lifted | Stage.Machine => None
    case Stage.Target => steps.afterJIT(source)
  }

  override def compile(source: Source)(using C: Context) =
    Compile(source) map { case (mod, defs) =>
      val mainFile = path(mod)
      (Map(mainFile -> pretty(defs).layout), mainFile)
    }

  // The Compilation Pipeline
  // ------------------------
  // Source => Core => JIT
  lazy val Compile = allToCore(Core) andThen Aggregate map {
    case CoreTransformed(source, tree, mod, core) => ???
  }

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }


  // The Compilation Pipeline for VSCode
  // -----------------------------------
  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) map { c => c.main }
    val afterJIT = afterCore map {
      case prog =>
        // we don't print declarations here.
        ???
    }
  }

  // Helpers
  // -------
  private def pretty(defs: jit.Program)(using Context): Document =
    Document(jit.PrettyPrinter.format(defs).layout, emptyLinks)
}
