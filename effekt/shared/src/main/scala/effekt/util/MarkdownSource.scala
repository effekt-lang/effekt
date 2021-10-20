package effekt
package util

import kiama.util.Source

case class MarkdownSource(source: Source) extends Source {

  val fenceLine = """^```[^`\n]*$""".r

  def name = source.name

  lazy val content = {
    var inCode = false
    // we map non-code lines to empty lines to preserve line numbers
    val lines = source.content.linesIterator.map {
      case line if fenceLine.matches(line) =>
        inCode = !inCode
        ""
      case line if inCode =>
        line
      case _ =>
        ""
    }
    lines.mkString("\n")
  }

  // Are these every used?
  def reader: java.io.Reader = ???
  def useAsFile[T](fn: String => T): T = ???
}
