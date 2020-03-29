package effekt
package context

import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama.util.Source

import scala.collection.mutable

/**
 * The ModuleDB depends on three things:
 * - method `contentsOf` to resolve FFI includes (js files)
 * - method `findSource` to resolve module sources (effekt files)
 * - field `compiler` to run the compiler on sources, on demand
 */
trait ModuleDB { self: Context =>

  // Cache containing processed units -- compilationUnits are cached by source
  private val units: mutable.Map[Source, Module] = mutable.Map.empty

  /**
   * Tries to find a file in the workspace, that matches the import path
   *
   * Used by Namer to resolve FFI includes
   */
  def contentsOf(moduleSource: Source, path: String): String

  /**
   * Find the source for a given module name / path
   */
  private[context] def findSource(path: String): Option[Source]

  /**
   * Tries to find a module for the given path, will run compiler on demand
   *
   * Used by Namer and Evaluator to resolve imports
   */
  def moduleOf(path: String): Module =
    moduleOf(findSource(path).getOrElse { abort(s"Cannot find source for $path") })

  /**
   * Tries to find a module for the given source, will run compiler on demand
   */
  def moduleOf(source: Source): Module = {
    tryModuleOf(source).getOrElse {
      abort(s"Cannot compile dependency: ${source.name}")
    }
  }

  /**
   * Tries to find a module for the given source, will run compiler on demand
   */
  def tryModuleOf(source: Source): Option[Module] = {
    units.get(source).orElse {
      val mod = compiler.compile(source)(this)
      mod.foreach { units.put(source, _) }
      mod
    }
  }

}
