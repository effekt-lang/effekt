package effekt
package context

import java.io.{ File, Reader }

import scala.io.{ BufferedSource, Source => ScalaSource }
import effekt.context.CompilerContext
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Filenames, IO, Source, StringSource }

import scala.collection.mutable
import java.net.URL

trait ModuleDB { self: CompilerContext =>

  // Cache containing processed units -- compilationUnits are cached by source
  private val units: mutable.Map[Source, CompilationUnit] = mutable.Map.empty

  def process(source: Source): Either[Messages, CompilationUnit]

  // - tries to find a file in the workspace, that matches the import path
  def resolveInclude(moduleSource: Source, path: String): String = moduleSource match {
    case JarSource(name) =>
      val p = new File(name).toPath.getParent.resolve(path).toString
      if (!JarSource.exists(p)) {
        abort(s"Missing include in: ${p}")
      } else {
        JarSource(p).content
      }
    case _ =>
      val modulePath = moduleSource.name
      val p = new File(modulePath).toPath.getParent.resolve(path).toFile
      if (!p.exists()) {
        abort(s"Missing include: ${p}")
      } else {
        FileSource(p.getCanonicalPath).content
      }
  }

  def resolve(source: Source): Either[Messages, CompilationUnit] =
    units.get(source).map(cu => Right(cu)).getOrElse {
      process(source).map { cu => units.update(source, cu); cu }
    }

  def resolve(path: String): Either[Messages, CompilationUnit] =
    resolve(findSource(path).getOrElse { abort(s"Cannot find source for $path") })

  // first try to find it in the includes paths, then in the bundled resources
  def findSource(path: String): Option[Source] = {
    val filename = path + ".effekt"

    config.includes().map { p => p.toPath.resolve(filename).toFile } collectFirst {
      case file if file.exists => FileSource(file.getCanonicalPath)
    } orElse {
      val jarPath = "lib/" + filename
      if (JarSource.exists(jarPath)) {
        Some(JarSource(jarPath))
      } else {
        None
      }
    }
  }
}

/**
 * A source that is a string.
 *
 * TODO open a PR and make content a def in Kiama
 */
case class JarSource(name: String) extends Source {

  val resource = ScalaSource.fromResource(name)

  def reader: Reader = resource.bufferedReader

  val content: String = resource.mkString

  def useAsFile[T](fn : String => T) : T = {
    val filename = Filenames.makeTempFilename(name)
    try {
      IO.createFile(filename, content)
      fn(filename)
    } finally {
      IO.deleteFile(filename)
    }
  }
}

object JarSource {
  def exists(name: String): Boolean = {
    val cl = Thread.currentThread().getContextClassLoader()
    val stream = cl.getResourceAsStream(name)
    stream != null
  }
}
