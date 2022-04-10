package effekt
package util

import java.io.{ File => JFile }
import java.net.URI

object paths extends PathUtils {

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

    def lastModified: Long = underlying.lastModified

    def existsInJar: Boolean = {
      val cl = Thread.currentThread().getContextClassLoader
      val stream = cl.getResourceAsStream(unixPath)

      stream != null
    }

    def toFile: JFile = underlying

    override def toString = underlying.toString
  }

  implicit def file(underlying: JFile): File = new File(underlying)
  implicit def file(uri: URI): File = file(uri.getPath)
  implicit def file(filepath: String) =
    if (filepath.startsWith("file:"))
      file(new URI(filepath).getPath)
    else
      file(new JFile(filepath))
}
