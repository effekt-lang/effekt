package effekt
package util

import kiama.util.Positions

import scala.util.matching._

object AnsiHighlight {

  object parser extends EffektLexers(new Positions)

  case class Token(r: Regex, highlight: String => String = identity) {
    def apply(in: String): Option[(String, String)] =
      r.findPrefixMatchOf(in) map {
        res => (highlight(res.matched), in.substring(res.end))
      }
  }

  val keyword = Token(
    ("(" + parser.keywordStrings.mkString("|") + ")\\b").r,
    Console.MAGENTA + _ + Console.RESET
  )

  val ident = Token(parser.name)

  val comment = Token(
    parser.singleline,
    Console.WHITE + _ + Console.RESET
  )

  // TODO reuse number regex from parser here
  val number = Token(
    """[+-]?[0-9]+([.][0-9]+)?""".r,
    Console.BLUE + _ + Console.RESET
  )

  val string = Token(
    """["][^"]*["]""".r,
    Console.GREEN + _ + Console.RESET
  )

  val whitespace = Token("""[\s]+""".r)

  val any = Token(""".""".r)

  val tokens = List(whitespace, comment, keyword, ident, number, string, any)

  final def apply(input: String): String = {
    val out = new StringBuffer
    var in = input

    while (in.nonEmpty) {
      val (hl, rest) =
        whitespace(in) orElse
          comment(in) orElse
          keyword(in) orElse
          ident(in) orElse
          number(in) orElse
          string(in) orElse
          any(in) getOrElse { sys error s"Cannot lex input ${in}" }

      out.append(hl)
      in = rest
    }
    out.toString
  }
}
