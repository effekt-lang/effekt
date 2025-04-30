package effekt
package util

import kiama.util.Source

/**
 * A decorator for markdown sources -- useful for literate programming
 */
case class MarkdownSource(source: Source) extends Source {

  val fenceLine = """^```\s*(effekt)?\s*((\s|:)[^`]*)?$""".r
  val otherFenceLine = """^```[^`\n]*$""".r

  def name = source.name

  lazy val content = {
    var inCode = false
    var inOtherFence = false
    // we map non-code lines to empty lines to preserve line numbers
    val lines = source.content.linesIterator.map {
      case line if fenceLine.matches(line) && !inOtherFence =>
        inCode = !inCode
        ""
      case line if otherFenceLine.matches(line) =>
        inOtherFence = !inOtherFence
        ""
      case line if inCode =>
        line
      case _ =>
        ""
    }
    lines.mkString("\n")
  }
}
