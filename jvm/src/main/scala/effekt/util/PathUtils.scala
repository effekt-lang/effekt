package effekt
package util

import java.io.{ File => JFile }
import java.nio.file.{ Path => JPath }

object JavaPathUtils extends PathUtils {

  val separatorChar = JFile.separatorChar

  val unixSeparator = '/'

  class File(underlying: JFile) extends FileOps {
    def /(other: String): File =
      file(new JFile(underlying, other))

    def parent: File =
      file(underlying.getParentFile)

    def unixPath: String =
      underlying.getPath.replace(separatorChar, unixSeparator)

    def canonicalPath: String =
      underlying.getCanonicalPath

    def exists: Boolean =
      underlying.exists()

    def existsInJar: Boolean = {
      val cl = Thread.currentThread().getContextClassLoader
      val stream = cl.getResourceAsStream(unixPath)
      stream != null
    }

    override def toString = underlying.toString
  }

  implicit def file(filepath: String) = new File(new JFile(filepath))
  implicit def file(underlying: JFile): File = new File(underlying)
}
