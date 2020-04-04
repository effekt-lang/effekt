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

    module.source match {
      case JarSource(name) =>
        if (!includeFile.existsInJar) {
          abort(s"Missing include ${path}")
        } else {
          JarSource(includeFile.unixPath).content
        }
      case _ =>
        if (!includeFile.exists) {
          abort(s"Missing include: ${includeFile}")
        } else {
          FileSource(includeFile.canonicalPath).content
        }
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
    } orElse {
      val f = "lib" / filename
      if (f.existsInJar) {
        Some(JarSource(f.unixPath))
      } else {
        None
      }
    }
  }
}

/**
 * A source that is a string.
 *
 * TODO open a PR and make `content`` a def in Kiama
 */
case class JarSource(name: String) extends Source {

  private val resource = ScalaSource.fromResource(name)

  def reader: Reader = resource.bufferedReader

  val content: String = resource.mkString

  def useAsFile[T](fn: String => T): T = {
    val filename = Filenames.makeTempFilename(name)
    try {
      IO.createFile(filename, content)
      fn(filename)
    } finally {
      IO.deleteFile(filename)
    }
  }
}
