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
    for (line <- source.content.linesIterator) {
      line match {
        case fenceLine(lang, ots) =>
          val opts = if (ots != null) { ots.tail.split(":").toList } else { Nil }
          if (opts.contains("ignore")) {
            fenceType = Some(Ignore)
          } else if (opts.contains("repl")) {
            fenceType = Some(Repl)
          } else {
            fenceType match {
              // ```effekt
              case None if lang != null && lang == "effekt" =>
                fenceType = Some(Code)
              // ```scala
              case None if lang != null =>
                fenceType = Some(Other)
              // ```
              case None =>
                fenceType = Some(Code)
              // closing fence of ```effekt:ignore
              case Some(fence) =>
                fence match {
                  // closing fence of ```effekt:repl
                  case Repl =>
                    // take care when wrapping to preserve line numbers
                    val wrapped = code.toList match {
                      case Nil => ""
                      case List(l) =>
                        val c = s"def repl$replCounter() = ${code.mkString}"
                        replCounter += 1
                        c
                      case hd :: rest =>
                        val c = s"def repl$replCounter() = $hd\n  ${rest.mkString("\n  ")}"
                        replCounter += 1
                        c
                    }
                    lines += wrapped
                  // closing fence of ```effekt or ```
                  case Code =>
                    lines ++= code
                  // closing fence of ```effekt:ignore
                  case Ignore =>
                    ()
                  // closing fence of ```scala or other languages
                  // same semantic as effekt:ignore
                  case Other =>
                    ()
                }
                fenceType = None
                code = ListBuffer.empty
                // make sure to add empty line for closing fence
                lines += ""
            }
          }
        // collect code if in code fence
        case _ if fenceType.isDefined =>
          code += line
        // do nothing if outside of code fence
        case _ =>
          ()
      }
    }
    val prog = lines.mkString("\n")
    val repls = 0.until(replCounter).map { i => s"repl${i}()"}
    val main = if (replCounter > 0) {
      s"""
      |def main() = {
      |${repls.map { r => s"  inspect($r)" }.mkString("\n") }
      |  ()
      |}
      """.stripMargin
    } else {
      "def main() = ()"
    }
    val res = prog ++ main
    println(res)
    res
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
