package effekt
package util

import effekt.lexer.TokenKind.*
import effekt.lexer.{ Lexer, Token, TokenKind }
import kiama.util.{ Source, StringSource }

enum Highlight { case Number, Str, Comment, Keyword, Error, Ident }

object Highlight {

  def classify(kind: TokenKind): Option[Highlight] = kind match {
    case TokenKind.Error(message)    => Some(Highlight.Error)
    case Integer(n)                  => Some(Highlight.Number)
    case Float(d)                    => Some(Highlight.Number)
    case TokenKind.Str(s, multiline) => Some(Highlight.Str)
    case HoleStr(s)                  => Some(Highlight.Str)
    case Chr(c)                      => Some(Highlight.Str)
    case TokenKind.Comment(msg)      => Some(Highlight.Comment)
    case DocComment(msg)             => Some(Highlight.Comment)
    case Shebang(cmd)                => Some(Highlight.Comment)
    // keywords
    case
      `let`  | `val` | `var` | `if` | `else` | `while` |
      `type` | `effect` | `interface` | `do` | `case` | `with` | `try` |
      `true` | `false` |
      `match` | `def` | `module`| `import`| `export`| `extern`| `include`|
      `record` | `box` | `unbox` | `return` | `region` |
      `resource` | `new` | `and` | `is` | `namespace` => Some(Highlight.Keyword)
    case TokenKind.Ident(_)          => Some(Highlight.Ident)
    case _                           => None
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
        case EOF  => ()
        case kind => out += ((token.content, classify(kind)))
      }
    }
    out.toList
  }
}
