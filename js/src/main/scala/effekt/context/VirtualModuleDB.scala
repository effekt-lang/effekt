package effekt
package context

import effekt.symbols.Module
import effekt.util.{ Resources, VirtualFS }
import org.bitbucket.inkytonik.kiama.util.{ Filenames, Source, StringSource }

import scala.collection.mutable

trait VirtualModuleDB extends ModuleDB { self: Context =>

  // initialize resources, which are copied by sbt
  Resources.load()

  /**
   * tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  override def contentsOf(moduleSource: Source, path: String): String = {
    val fullPath = Filenames.directory(moduleSource.name) + path
    VirtualFS.tryRead(fullPath).getOrElse {
      error(s"Cannot find include: ${path}")
      "" // continue nonetheless
    }
  }

  override def findSource(path: String): Option[Source] = {
    val filename = path + ".effekt"
    VirtualFS.tryRead(filename).map { content => StringSource(content, filename) }
  }
}