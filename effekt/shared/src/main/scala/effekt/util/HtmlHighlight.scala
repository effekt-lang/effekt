package effekt.util


/**
 * Highlights Effekt code using HTML <span> tags.
 *
 * The generated output preserves the original input (including whitespace)
 * and escapes HTML special characters.
 *
 * Usage:
 *   HtmlHighlight("val x = 3; return x + 1")
 */
object HtmlHighlight {
  private def htmlSpan(cls: String, text: String): String =
    s"<span class=\"$cls\">$text</span>"

  private def cls(highlight: Highlight, text: String): String = highlight match {
    case Highlight.Number  => "effekt-number"
    case Highlight.Str     => "effekt-string"
    case Highlight.Comment => "effekt-comment"
    case Highlight.Keyword => "effekt-keyword"
    case Highlight.Error   => "effekt-error"
    case Highlight.Ident   => if (text.headOption.exists(_.isUpper)) "effekt-ident pascal-case" else "effekt-ident camel-case"
  }

  final def apply(input: String): String = Highlight.tokens(input).map {
    case (text, Some(highlight)) => htmlSpan(cls(highlight, text), scala.xml.Utility.escape(text))
    case (text, None)            => scala.xml.Utility.escape(text)
  }.mkString
}
