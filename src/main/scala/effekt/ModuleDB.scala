package effekt

import java.io.File

import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Source }

import scala.collection.mutable

trait ModuleDB { self: CompilerContext =>

  // Cache containing processed units -- compilationUnits are cached by path
  val units = mutable.Map.empty[String, CompilationUnit]

  val process: Source => Either[CompilationUnit, Messages]

  // - tries to find a file in the workspace, that matches the import path
  def resolveInclude(modulePath: String, path: String): String = {
    val p = new File(modulePath).toPath.getParent.resolve(path).toFile

    if (!p.exists()) { sys error s"Missing include: ${p}" }
    FileSource(p.getCanonicalPath).content
  }

  def resolve(path: String): Either[CompilationUnit, Messages] =
    units.get(path).map(cu => Left(cu)).getOrElse {
      val source = findSource(path).getOrElse { sys error s"Cannot find source for $path" }
      process(source)  match {
        case Left(cu) =>
          units.update(path, cu)
          Left(cu)
        case Right(msgs) => Right(msgs)
      }
    }

  def findSource(path: String): Option[Source] = {
    val filename = path + ".effekt"
    config.includes().map { p => p.toPath.resolve(filename).toFile }.collectFirst {
      case file if file.exists => FileSource(file.getCanonicalPath)
    }
  }
}