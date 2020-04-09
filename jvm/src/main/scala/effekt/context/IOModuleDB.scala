package effekt
package context

import java.io.{ File, Reader }

import effekt.util.JavaPathUtils._

import scala.io.{ Source => ScalaSource }
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Filenames, IO, Source }

trait IOModuleDB extends ModuleDB { self: Context =>

  /**
   * Tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  override def contentsOf(path: String): String = {
    val includeFile = file(module.source.name).parent / path

    if (!includeFile.exists) {
      abort(s"Missing include: ${includeFile}")
    } else {
      FileSource(includeFile.canonicalPath).content
    }
  }

  /**
   * First try to find it in the includes paths, then in the bundled resources
   */
  override def findSource(modulePath: String): Option[Source] = {
    // ATTENTION modulePath can contain non-platform dependent path separators
    val filename = modulePath + ".effekt"

    config.includes().map { p => p / filename } collectFirst {
      case f if f.exists => FileSource(f.canonicalPath)
    }
  }
}
