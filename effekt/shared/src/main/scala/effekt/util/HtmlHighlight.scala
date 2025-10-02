package effekt.util

import effekt.lexer.TokenKind.*
import effekt.lexer.{Lexer, Token, TokenKind}
import kiama.util.{Source, StringSource}

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

  // Highlighting
  // ------------

  private def highlight(kind: TokenKind, raw: String): String = {
    val text = scala.xml.Utility.escape(raw)
    kind match {
      case EOF               => ""
      case Error(_)          => htmlSpan("effekt-error", text)

      case Integer(_)        => htmlSpan("effekt-number", text)
      case Float(_)          => htmlSpan("effekt-number", text)

      case Str(_, _)         => htmlSpan("effekt-string", text)
      case HoleStr(_)        => htmlSpan("effekt-string", text)
      case Chr(_)            => htmlSpan("effekt-string", text)

      case Ident(id)         =>
        if (id.headOption.exists(_.isUpper))
          htmlSpan("effekt-ident pascal-case", text)
        else
          htmlSpan("effekt-ident camel-case", text)

      case Comment(_)        => htmlSpan("effekt-comment", text)
      case DocComment(_)     => htmlSpan("effekt-comment", text)
      case Shebang(_)        => htmlSpan("effekt-comment", text)

      // keywords
      case
        `let`  | `val` | `var` | `if` | `else` | `while` |
        `type` | `effect` | `interface` | `do` | `case` | `with` | `try` |
        `true` | `false` |
        `match` | `def` | `module`| `import`| `export`| `extern`| `include`|
        `record` | `box` | `return` | `region` |
        `resource`| `new` | `and` | `is` | `namespace`
      => htmlSpan("effekt-keyword", text)

      case _                 => text
    }
  }

  extension (token: Token) {
    def content(using src: Source): String = token.kind match {
      case EOF => ""
      case _ => src.content.substring(token.start, token.end + 1)
    }
  }

  final def apply(input: String): String = {

    given source: Source = StringSource(input)

    val in = Lexer(source)

    // to track whitespace (and other input skipped by the lexer)
    //
    //   def foo() = 1 + 2
    //     ^ ^
    //     2 4
    //
    // so we need to emit from offset 3 to 4 (non-inclusive).
    var previousOffset = -1

    val out = new StringBuffer

    while (in.hasNext) {
      val token = in.next()

      if (token.start > previousOffset) {
        out.append(input.substring(previousOffset + 1, token.start))
      }
      previousOffset = token.end

      out.append(highlight(token.kind, token.content))
    }
    out.toString
  }
}
