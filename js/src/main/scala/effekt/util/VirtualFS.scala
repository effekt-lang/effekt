package effekt.util

import scala.collection.mutable

class FileNotFound(filename: String) extends Exception(s"File not found: ${filename}")

/**
 * A very primitive virtual in-memory file system to be used from JavaScript
 */
object VirtualFS {

  type Path = String

  private lazy val files: mutable.Map[Path, String] = mutable.Map.empty

  def write(path: Path, content: String): Unit =
    files.put(path, content)

  def exists(path: Path): Boolean =
    files.isDefinedAt(path)

  def read(path: Path): String =
    files.getOrElse(path, throw new FileNotFound(path))

  def tryRead(path: Path): Option[String] =
    files.get(path)
}