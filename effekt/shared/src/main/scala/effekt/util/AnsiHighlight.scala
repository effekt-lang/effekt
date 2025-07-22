package effekt
package util

import effekt.lexer.TokenKind.*
import kiama.util.{ StringSource, Source }
import effekt.lexer.{ Lexer, Token, TokenKind }

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

  // Styles
  // ------

  private def number(text: String): String =
    Console.BLUE + text + Console.RESET

  private def string(text: String): String =
    Console.GREEN + text + Console.RESET

  private def comment(text: String): String =
    Console.WHITE + text + Console.RESET

  private def keyword(text: String): String =
    Console.MAGENTA + text + Console.RESET

  private def error(text: String): String =
    Console.RED + Console.UNDERLINED + text + Console.RESET


  // Highlighting
  // ------------

  private def highlight(kind: TokenKind, text: String): String = kind match {
    case EOF               => ""
    case Error(message)    => error(text)

    case Integer(n)        => number(text)
    case Float(d)          => number(text)

    case Str(s, multiline) => string(text)
    case HoleStr(s)        => string(text)
    case Chr(c)            => string(text)

    case Ident(id)         => id

    case Comment(msg)      => comment(text)
    case DocComment(msg)   => comment(text)
    case Shebang(cmd)      => comment(text)

    // keywords
    case
      `let`  | `val` | `var` | `if` | `else` | `while` |
      `type` | `effect` | `interface` | `do` | `case` | `with` | `try` |
      `true` | `false` |
      `match` | `def` | `module`| `import`| `export`| `extern`| `include`|
      `record`| `box`| `return`| `region`|
      `resource`| `new`| `and`| `is`| `namespace`| `pure` => keyword(text)

    case _ => text
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
