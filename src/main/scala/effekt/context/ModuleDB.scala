package effekt
package context

import java.io.File

import effekt.context.CompilerContext
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Source }

import scala.collection.mutable

trait ModuleDB { self: CompilerContext =>

  // Cache containing processed units -- compilationUnits are cached by source
  private val units: mutable.Map[Source, CompilationUnit] = mutable.Map.empty

  def process(source: Source): Either[Messages, CompilationUnit]

  // - tries to find a file in the workspace, that matches the import path
  def resolveInclude(modulePath: String, path: String): String = {
    val p = new File(modulePath).toPath.getParent.resolve(path).toFile

    if (!p.exists()) { sys error s"Missing include: ${p}" }
    FileSource(p.getCanonicalPath).content
  }

  def resolve(source: Source): Either[Messages, CompilationUnit] =
    units.get(source).map(cu => Right(cu)).getOrElse {
      process(source).map { cu => units.update(source, cu); cu }
    }

  def resolve(path: String): Either[Messages, CompilationUnit] =
    resolve(findSource(path).getOrElse { abort(s"Cannot find source for $path") })

  def findSource(path: String): Option[Source] = {
    val filename = path + ".effekt"
    config.includes().map { p => p.toPath.resolve(filename).toFile }.collectFirst {
      case file if file.exists => FileSource(file.getCanonicalPath)
    }
  }
}
