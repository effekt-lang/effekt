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

  enum Highlight { case Number, Str, Comment, Keyword, Error }

  def escape(highlight: Highlight): String = highlight match {
    case Highlight.Number  => Console.BLUE
    case Highlight.Str     => Console.GREEN
    case Highlight.Comment => Console.WHITE
    case Highlight.Keyword => Console.MAGENTA
    case Highlight.Error   => Console.RED + Console.UNDERLINED
  }

  def style(highlight: Highlight, text: String): String = escape(highlight) + text + Console.RESET

  def classify(kind: TokenKind): Option[Highlight] = kind match {
    case Error(message)    => Some(Highlight.Error)
    case Integer(n)        => Some(Highlight.Number)
    case Float(d)          => Some(Highlight.Number)
    case Str(s, multiline) => Some(Highlight.Str)
    case HoleStr(s)        => Some(Highlight.Str)
    case Chr(c)            => Some(Highlight.Str)
    case Comment(msg)      => Some(Highlight.Comment)
    case DocComment(msg)   => Some(Highlight.Comment)
    case Shebang(cmd)      => Some(Highlight.Comment)
    // keywords
    case
      `let`  | `val` | `var` | `if` | `else` | `while` |
      `type` | `effect` | `interface` | `do` | `case` | `with` | `try` |
      `true` | `false` |
      `match` | `def` | `module`| `import`| `export`| `extern`| `include`|
      `record` | `box` | `unbox` | `return` | `region` |
      `resource` | `new` | `and` | `is` | `namespace` => Some(Highlight.Keyword)
    case _ => None
  }

  extension (token: Token) {
    def content(using src: Source): String = token.kind match {
      case EOF => ""
      case _ => src.content.substring(token.start, token.end + 1)
    }
  }

  def tokens(input: String): List[(String, Option[Highlight])] = {

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

    val out = scala.collection.mutable.ListBuffer.empty[(String, Option[Highlight])]

    while (in.hasNext) {
      val token = in.next()

      if (token.start > previousOffset) {
        out += ((input.substring(previousOffset + 1, token.start), None))
      }
      previousOffset = token.end

      token.kind match {
        case EOF       => ()
        case Ident(id) => out += ((id, None))
        case kind      => out += ((token.content, classify(kind)))
      }
    }
    out.toList
  }

  final def apply(input: String): String =
    tokens(input).map { (text, highlight) => highlight.map { h => style(h, text) }.getOrElse(text) }.mkString
}
