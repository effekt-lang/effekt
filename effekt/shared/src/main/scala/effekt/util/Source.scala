package effekt
package util

import scala.collection.mutable.ListBuffer

import effekt.source.ModuleDecl

import kiama.util.Source

/**
 * A decorator for markdown sources -- useful for literate programming
 */
case class MarkdownSource(source: Source) extends Source {

  val fenceLine = """^```\s*(\w+)?\s*((?::[^:\n]*)*)$""".r
  val emptyFence = """^```\s*$""".r

  enum FenceType {
    case Repl
    case Ignore
    case Code
    case Other
  }

  def name = source.name

  lazy val content = {
    import FenceType.*
    var code = ListBuffer.empty[String]
    var replCounter = 0
    val lines = ListBuffer.empty[String]
    var fenceType: Option[FenceType] = None
    // we map non-code lines to empty lines to preserve line numbers
    source.content.linesIterator.foreach { 
      case fenceLine(lang, ots) =>
        val opts = if (ots != null) { ots.tail.split(":").toList } else { Nil }
        if (opts.contains("ignore")) {
          fenceType = Some(Ignore)
          lines += ""
        } else if (opts.contains("repl")) {
          fenceType = Some(Repl)
          // opening fences are replaced by a wrapper function
          lines += s"def repl${replCounter}() = {"
          replCounter += 1
        } else {
          fenceType match {
            // opening fences
            case None =>
              // ```effekt
              if (lang != null && lang == "effekt") {
                fenceType = Some(Code)
              // ```scala
              } else if (lang != null) {
                fenceType = Some(Other)
              // ```
              } else {
                fenceType = Some(Code)
              }
              lines += ""
            // closing fences
            case Some(fence) =>
              fence match {
                // closing fence of ```effekt:repl
                case Repl =>
                  // take care when wrapping to preserve line numbers
                  lines ++= code
                  // remember to close wrapper function
                  lines += "}"
                // closing fence of ```effekt or ```
                case Code =>
                  if (!code.isEmpty) lines ++= code
                  lines += ""
                // closing fence of ```effekt:ignore
                // closing fence of ```scala or other languages
                case _ =>
                  lines ++= code.map { _ => "" }
                  lines += ""
              }
              fenceType = None
              code = ListBuffer.empty
          }
        }
      // collect code if in code fence
      case line if fenceType.isDefined =>
        code += line
      // add empty lines to preserve positional information outside of code fences
      case line =>
        lines += ""
    }
    val prog = lines.mkString("\n")
    val repls = 0.until(replCounter).map { i => s"inspect(repl${i}())"}
    // We either allow to have one manually defined main function XOR one or more REPLs
    val main = if (replCounter > 0) {
      s"""
      |def main() = {
      |${repls.mkString("\n") }
      |()
      |}
      """.stripMargin
    } else {
      ""
    }
    prog ++ main
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
