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

  private type Path = Seq[String]

  case class VirtualFile(timestamp: Long, contents: String)

  private lazy val files: mutable.Map[Path, VirtualFile] = mutable.Map.empty

  val separatorChar = '/'

  class File(path: Path) extends FileOps {

    val normalized: Path = {
      var res: Path = Seq.empty
      path.foreach {
        case "."  => ()
        case ".." if res.nonEmpty => res = res.init
        case seg  => res = res :+ seg.trim.stripSuffix("/")
      }
      res
    }

    def /(other: String): File = new File(path ++ other.split(separatorChar).toSeq)

    def unixPath = normalized.mkString(separatorChar.toString)
    def canonicalPath = unixPath

    def parent = file(normalized.init)

    def lastModified: Long = files.getOrElse(normalized, throw new FileNotFound(unixPath)).timestamp

    def read: String = files.getOrElse(normalized, throw new FileNotFound(unixPath)).contents
    def tryRead: Option[String] = files.get(normalized).map(_.contents)
    def write(content: String) = files.put(normalized, VirtualFile(System.currentTimeMillis, content))
    def exists: Boolean = files.isDefinedAt(normalized)

    override def toString = unixPath
  }

  override def lastModified(src: Source): Long = src match {
    case VirtualFileSource(name) => file(name).lastModified
    case _ => super.lastModified(src)
  }

  implicit def file(path: String): File = file(path.split(separatorChar).toSeq)
  def file(path: Seq[String]): File = new File(path)
}
