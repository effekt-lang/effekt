package effekt
package util

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


  // For purposes of caching, we have to use object identity to compare the virtual modules.
  // Later compiler phases also use object identity to attach symbols etc. Not using
  // object identity, will result in problems as observed in #77
  override def equals(obj: Any): Boolean = obj match {
    case VirtualSource(otherModule, otherInput) => (otherModule eq virtualModule) && (otherInput == input)
    case _ => false
  }

  override def hashCode(): Int = super.hashCode() + System.identityHashCode(virtualModule)
}
