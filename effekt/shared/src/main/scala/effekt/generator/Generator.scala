package effekt
package generator

import effekt.context.Context
import effekt.symbols.Module

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

trait Generator extends Phase[Source, Document] {

  val phaseName = "generator"

  /**
   * A Unix path that is *not* platform dependent.
   */
  def path(m: Module)(implicit C: Context): String

  /**
   * Backends should use Context.saveOutput to write files to also work with virtual file systems
   */
  def compileWhole: Phase[CompilationUnit, Unit] = ???

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate: Phase[CoreTransformed, Document] = ???
}
