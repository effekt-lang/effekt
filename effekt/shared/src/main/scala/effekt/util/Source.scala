package effekt
package util

//TODO-LLVM import kiama.util.Source
import effekt.source.ModuleDecl

import kiama.util.Source

/**
 * A decorator for markdown sources -- useful for literate programming
 */
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
}

/**
 * Use by the REPL with synthesized modules. "input" contains the last REPL entry,
 * not the whole source.
 */
case class VirtualSource(virtualModule: ModuleDecl, input: Source) extends Source {
  val name = input.name
  val content = input.content
}
