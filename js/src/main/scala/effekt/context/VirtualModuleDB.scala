package effekt
package context

import effekt.symbols.Module

import org.bitbucket.inkytonik.kiama.util.{ FileSource, Filenames, IO, Source, StringSource }

import scala.collection.mutable

trait VirtualModuleDB extends ModuleDB { self: Context =>

  // Cache containing processed units -- compilationUnits are cached by source
  private val units: mutable.Map[Source, Module] = mutable.Map.empty

  /**
   * tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  def contentsOf(moduleSource: Source, path: String): String = ""

  def moduleOf(source: Source): Module =
    units.getOrElseUpdate(source, process(source))

  def moduleOf(path: String): Module =
    moduleOf(findSource(path).getOrElse { abort(s"Cannot find source for $path") })


  // first try to find it in the includes paths, then in the bundled resources
  private def findSource(path: String): Option[Source] = None
}