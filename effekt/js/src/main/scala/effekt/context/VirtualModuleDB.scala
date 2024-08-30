package effekt
package context

import effekt.util.Resources
import effekt.util.paths._

import kiama.util.Source

trait VirtualModuleDB extends ModuleDB { self: Context =>

  // initialize resources, which are copied by sbt
  Resources.load()

  /**
   * tries to find a file in the workspace, that matches the import path
   *
   * used by Namer to resolve FFI includes
   */
  override def contentsOf(path: String): Option[String] = {
    val f = file(module.source.name).parent / path
    if (!f.exists) {
      None
    } else {
      Some(f.read)
    }
  }

  override def findSource(path: String): Option[Source] = {
    val filename = path + ".effekt"
    val f = file(filename)

    def effektFile(include: File) = include / (path + ".effekt")

    config.includes().collectFirst {
      case p if effektFile(p).exists =>
        VirtualFileSource(effektFile(p).toString)
    }
  }
}

case class VirtualFileSource(name: String) extends Source {
  lazy val content = file(name).read
}
