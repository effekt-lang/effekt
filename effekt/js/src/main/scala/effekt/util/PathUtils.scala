package effekt.util

import effekt.context.VirtualFileSource

import scala.collection.mutable
import kiama.util.Source

class FileNotFound(filename: String) extends Exception(s"File not found: ${filename}")

/**
 * A very primitive virtual in-memory file system to be used from JavaScript
 *
 * TODO Extend and implement util.PathUtil
 */
object paths extends PathUtils {

  private type Path = String

  case class VirtualFile(timestamp: Long, contents: String)

  private lazy val files: mutable.Map[Path, VirtualFile] = mutable.Map.empty

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

    def lastModified: Long = files.getOrElse(path, throw new FileNotFound(path)).timestamp

    def read: String = files.getOrElse(path, throw new FileNotFound(path)).contents
    def tryRead: Option[String] = files.get(path).map(_.contents)
    def write(content: String) = files.put(path, VirtualFile(System.currentTimeMillis, content))
    def exists: Boolean = files.isDefinedAt(path)

    override def toString = path
  }

  override def lastModified(src: Source): Long = src match {
    case VirtualFileSource(name) => file(name).lastModified
    case _ => super.lastModified(src)
  }

  implicit def file(path: String): File = new File(path)
}
