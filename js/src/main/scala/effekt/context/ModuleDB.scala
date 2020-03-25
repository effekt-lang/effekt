package effekt
package context

import effekt.symbols.Module

import org.bitbucket.inkytonik.kiama.util.{ FileSource, Filenames, IO, Source, StringSource }

import scala.collection.mutable

trait ModuleDB { self: Context =>

  // Cache containing processed units -- compilationUnits are cached by source
  private val units: mutable.Map[Source, Module] = mutable.Map.empty

  /**
   * Just runs the frontend (no code generation)
   * When there are errors in processing `source` it returns None
   */
  def frontend(source: Source): Option[Module]

  /**
   * Runs both frontend and backend.
   * In case of an error, this variant will abort and report any errors
   */
  def process(source: Source): Module


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