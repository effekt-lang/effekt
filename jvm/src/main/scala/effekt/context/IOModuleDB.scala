package effekt
package context

import effekt.util.paths._
import effekt.util.MarkdownSource
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Filenames, IO, Source }

trait IOModuleDB extends ModuleDB { self: Context =>

  /**
   * Tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  override def contentsOf(path: String): Option[String] = {
    val includeFile = file(sourceModule.source.name).parent / path
    if (!includeFile.exists) {
      None
    } else {
      Some(FileSource(includeFile.toString).content)
    }
  }

  /**
   * First try to find it in the includes paths, then in the bundled resources
   */
  override def findSource(modulePath: String): Option[Source] = {
    // ATTENTION modulePath can contain non-platform dependent path separators

    def effektFile(include: File) = include / (modulePath + ".effekt")
    def mdFile(include: File) = include / (modulePath + ".md")

    config.includes().collectFirst {
      case p if effektFile(p).exists => FileSource(effektFile(p).toString)
      case p if mdFile(p).exists     => MarkdownSource(FileSource(mdFile(p).toString))
    }
  }
}
