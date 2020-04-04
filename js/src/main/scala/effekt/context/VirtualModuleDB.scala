package effekt
package context

import effekt.util.Resources
import effekt.util.JSPathUtils._

import org.bitbucket.inkytonik.kiama.util.{ Source, StringSource }

trait VirtualModuleDB extends ModuleDB { self: Context =>

  // initialize resources, which are copied by sbt
  Resources.load()

  /**
   * tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  override def contentsOf(path: String): String = {
    val f = file(module.source.name).parent / path
    if (!f.exists) {
      error(s"Cannot find include: ${path}");
      "" // continue nonetheless
    } else {
      f.read
    }
  }

  override def findSource(path: String): Option[Source] = {
    val f = file(path + ".effekt")
    f.tryRead.map { content => StringSource(content, f.canonicalPath) }
  }
}
