package effekt
package util

import effekt.lexer.TokenKind.*
import kiama.util.{ Positions, StringSource, Source }
import effekt.lexer.{ Lexer, Token, TokenKind }

object AnsiHighlight {

  private def highlight(token: Token)(using Source): String = token.kind match {
    case Error(error) => Console.RED + Console.UNDERLINED + token.content + Console.RESET

    // numbers
    case Integer(n) => Console.BLUE + n.toString + Console.RESET
    case Float(d) => Console.BLUE + d.toString + Console.RESET

    // strings
    case Str(s, multiline) => Console.GREEN + token.content + Console.RESET
    case HoleStr(s) => Console.GREEN + token.content + Console.RESET
    case Chr(c) => Console.GREEN + token.content + Console.RESET

    case Ident(id) => id

    case Comment(msg) => Console.WHITE + token.content + Console.RESET
    case DocComment(msg) => Console.WHITE + token.content + Console.RESET
    case Shebang(cmd) => Console.WHITE + token.content + Console.RESET

    // keywords
    case
      `let`  | `val` | `var` | `if` | `else` | `while` |
      `type` | `effect` | `interface` | `fun` | `do` | `case` | `with` | `try` |
      `true` | `false` |
      `match` | `def` | `module`| `import`| `export`| `extern`| `include`|
      `record`| `box`| `unbox`| `return`| `region`|
      `resource`| `new`| `and`| `is`| `namespace`| `pure` =>
        Console.MAGENTA + token.content + Console.RESET

    case EOF => ""

    case _ => token.content
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

      out.append(highlight(token))
    }
    out.toString
  }
}
