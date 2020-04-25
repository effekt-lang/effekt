package effekt.util

import scala.collection.mutable

class FileNotFound(filename: String) extends Exception(s"File not found: ${filename}")

/**
 * A very primitive virtual in-memory file system to be used from JavaScript
 *
 * TODO Extend and implement util.PathUtil
 */
object paths extends PathUtils {

  private type Path = String

  private lazy val files: mutable.Map[Path, String] = mutable.Map.empty

  val separatorChar = '/'

  class File(path: Path) extends FileOps {

    def /(other: String): File =
      if (path.isEmpty)
        file(other)
      else if (path.endsWith(separator))
        file(path + other)
      else
        file(path + separator + other)

    def unixPath = path
    def canonicalPath = unixPath

    def parent = file(path.split(separatorChar).init.mkString(separator))

    // TODO implement by also keeping a timestamp in the map
    def lastModified: Long = 0

    def read: String = files.getOrElse(path, throw new FileNotFound(path))
    def tryRead: Option[String] = files.get(path)
    def write(content: String) = files.put(path, content)
    def exists: Boolean = files.isDefinedAt(path)

    override def toString = path
  }

  implicit def file(path: String): File = new File(path)
}
