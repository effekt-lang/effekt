package effekt
package generator

import effekt.context.Context
import effekt.symbols.Module

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

trait BackendPhase {

  /**
   * A Unix path that is *not* platform dependent.
   */
  def path(m: Module)(implicit C: Context): String

  /**
   * Backends should use Context.saveOutput to write files to also work with virtual file systems
   */
  def whole: Phase[CompilationUnit, Unit]

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def separate: Phase[CoreTransformed, Document]
}

/**
 * As long as phases are not used *within* a backend, it is easier to
 * implement abstract methods.
 */
trait Backend extends BackendPhase {

  /**
   * Backends should use Context.saveOutput to write files to also work with virtual file systems
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context): Unit

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(implicit C: Context): Option[Document]

  // Using the two methods above, we can implement the required phases.
  val whole = Phase("compile-whole") { input => Some(compileWhole(input.main, input.dependencies)) }
  val separate = Phase("compile-separate") { compileSeparate }
}
