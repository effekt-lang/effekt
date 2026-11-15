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
object AnsiHighlight extends messages.Style {
  val escape = messages.Style.ansi

  final def apply(input: String): String =
    Highlight.tokens(input).map { (text, highlight) =>
      highlight.map { h => styled(messages.Role.Token(h), text) }.getOrElse(text)
    }.mkString
}
