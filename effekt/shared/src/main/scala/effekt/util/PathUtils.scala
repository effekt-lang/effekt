package effekt
package util

import kiama.util.{ FileSource, Source }

/**
 * PathUtils independent of Java as a host platform.
 * Can be implemented to also be compatible with JS
 */
trait PathUtils {

  val separatorChar: Char
  def separator: String = separatorChar.toString

  def file(filepath: String): File

  type File <: FileOps
  trait FileOps {
    def /(other: String): File

    def parent: File

    /**
     * Prints the path as a unix path as used by Node and Jar files.
     */
    def unixPath: String

    def exists: Boolean

    def lastModified: Long

    /**
     * Prints the path as platform dependent path
     */
    def canonicalPath: String
  }

  /**
   * Given an Effekt module path (like `data/option`) computes the
   * filename of the module (like `data_option.js`).
   */
  def moduleFileName(modulePath: String) =
    modulePath.replace('/', '_') + ".js"

  def lastModified(src: Source): Long = src match {
    case FileSource(name, encoding) => file(name).lastModified
    case MarkdownSource(src) => lastModified(src)

    // it is always 0 for string sources and virtual sources since they are compared by content
    case _ => 0L
  }
}



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
