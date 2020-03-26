package effekt
package context

import effekt.symbols.Module

import org.bitbucket.inkytonik.kiama.util.Source

trait ModuleDB { self: Context =>

  /**
   * Tries to find a file in the workspace, that matches the import path
   *
   * Used by Namer to resolve FFI includes
   */
  def contentsOf(moduleSource: Source, path: String): String


  /**
   * Tries to find a module for the given path.
   *
   * Used by Namer and Evaluator to resolve imports
   */
  def moduleOf(path: String): Module
}