package effekt
package util

/**
 * Highlights Effekt code using Ansi colors.
 *
 * The generated output should be exactly the same as the input
 * (including whitespaces), modulo the added colors.
 *
 * Use like:
 *
 *   AnsiHighlight("val x = 3; return x + 1")
 */
object AnsiHighlight {

  def escape(highlight: Highlight): String = highlight match {
    case Highlight.Number  => Console.BLUE
    case Highlight.Str     => Console.GREEN
    case Highlight.Comment => Console.WHITE
    case Highlight.Keyword => Console.MAGENTA
    case Highlight.Error   => Console.RED + Console.UNDERLINED
    case Highlight.Ident   => ""
  }

  def style(highlight: Highlight, text: String): String = escape(highlight) match {
    case ""    => text
    case start => start + text + Console.RESET
  }

  final def apply(input: String): String =
    Highlight.tokens(input).map { (text, highlight) => highlight.map { h => style(h, text) }.getOrElse(text) }.mkString
}
