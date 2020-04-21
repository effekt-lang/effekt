package effekt
package context

import effekt.util.JavaPathUtils._
import effekt.util.MarkdownSource
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

    def effektFile(include: File) = include / (modulePath + ".effekt")
    def mdFile(include: File) = include / (modulePath + ".md")

    config.includes().collectFirst {
      case p if effektFile(p).exists => FileSource(effektFile(p).canonicalPath)
      case p if mdFile(p).exists     => MarkdownSource(FileSource(mdFile(p).canonicalPath))
    }
  }
}
