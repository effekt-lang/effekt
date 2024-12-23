package effekt.lexer

import java.lang.Integer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.BufferedIterator
import scala.util.matching.Regex

import effekt.context.Context

import kiama.util.{ Source, Range }

/** An error encountered during lexing a source string. */
case class LexerError(msg: String, start: Int, end: Int) extends Throwable(msg)


/**
 * A token consist of the absolute starting position, the absolute end position in the source file
 * and the kind of token. Both position are to be understood as inclusive.
 */
case class Token(start: Int, end: Int, kind: TokenKind)

enum TokenKind {
  // literals
  case Integer(n: Long)
  case Float(d: Double)
  case Str(s: String, multiline: Boolean)
  case Chr(c: Int)
  // identifiers
  case Ident(id: String)
  // misc
  case Comment(msg: String)
  case Newline
  case Space
  case EOF
  case Error(err: LexerError)
  // symbols
  case `=`
  case `===`
  case `!==`
  case `:`
  case `;`
  case `::`
  case `@`
  case `${`
  case `{`
  case `}`
  case `}$`
  case `(`
  case `)`
  case `[`
  case `]`
  case `,`
  case `'`
  case `.`
  case `/`
  case `=>`
  case `<>`
  case `<`
  case `<=`
  case `>`
  case `>=`
  case `<{`
  case `}>`
  case `!`
  case `|`
  case `||`
  case `&`
  case `&&`
  case `+`
  case `++`
  case `-`
  case `*`
  // keywords
  case `let`
  case `true`
  case `false`
  case `val`
  case `var`
  case `if`
  case `else`
  case `while`
  case `type`
  case `effect`
  case `interface`
  case `try`
  case `with`
  case `case`
  case `do`
  case `fun`
  case `match`
  case `def`
  case `module`
  case `import`
  case `export`
  case `extern`
  case `include`
  case `record`
  case `box`
  case `unbox`
  case `return`
  case `region`
  case `resource`
  case `new`
  case `and`
  case `is`
  case `namespace`
  case `pure`
}
object TokenKind {
  // "Soft" keywords
  val `resume` = TokenKind.Ident("resume")
  val `in` = TokenKind.Ident("in")
  val `at` = TokenKind.Ident("at")
  val `__` = Ident("_")

  def explain(kind: TokenKind): String = kind match {
    case t if keywords.contains(t) => s"keyword ${kind}"
    case Ident(n)          => s"identifier ${n}"
    case Integer(n)        => s"integer ${n}"
    case Float(d)          => s"float ${d}"
    case Str(s, true)      => s"multi-line string"
    case Str(s, false)     => s"string \"${s}\""
    case Chr(c)            => s"character '${c}'"
    case Comment(contents) => "comment"
    case Newline           => "newline"
    case Space             => "space"
    case EOF               => "end of file"
    case other             => other.toString
  }

  val keywords = Vector(
    `let`, `true`, `false`, `val`, `var`, `if`, `else`, `while`, `type`, `effect`, `interface`,
    `try`, `with`, `case`, `do`, `fun`, `match`, `def`, `module`, `import`, `export`, `extern`,
    `include`, `record`, `box`, `unbox`, `return`, `region`, `resource`, `new`, `and`, `is`,
    `namespace`, `pure`)

}




object Lexer {
  import TokenKind.*

  val keywordMap: immutable.HashMap[String, TokenKind] =
    immutable.HashMap.from(TokenKind.keywords.map { case t => t.toString -> t })
}

/**
 *
 * @param source A string of a Effekt program to be lexed.
 */
class Lexer(source: Source) {
  import Lexer.*

  /** The absolute starting index in the source string of the currently scanned token */
  var start: Int = 0
  /** The absolute index of the lexer's reading 'head' in the source string. Example
   * "hello world"
   *   ^
   *  current = 1
   *  chars.next() = 'e'
   */
  var current: Int = 0
  /** The sequence of tokens the source strings contains. Returned as the result by [[Lexer.run()]] */
  val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  // For future improvement, this is probably more performant than using substring on the source
  // val currLexeme: mutable.StringBuilder = mutable.StringBuilder()
  /** A peekable iterator of the source string. This is used instead of directly indexing the source string.
  * This may only be advanced by using the `consume` function, otherwise positions cannot be keept track of.
  */
  val chars: BufferedIterator[Char] = source.content.iterator.buffered

  val str: String = source.content

  lazy val whitespace: Regex = """([ \t\r\n])""".r // single whitespace characters
  val whitespaces = Set(' ', '\t', '\r', '\n')
  val nameFirst = ('a'.to('z').iterator ++ 'A'.to('Z').iterator ++ List('_').iterator).toSet
  val nameRest = ('a'.to('z').iterator ++ 'A'.to('Z').iterator ++ '0'.to('9').iterator ++ List('_', '!', '?', '$').iterator).toSet

  def isEOF: Boolean =
    current >= str.length

  /** Advance the [[Lexer.chars]] iterator and [[Lexer.current]] index. Returns [[None]] if EOF is reached. */
  def consume(): Option[Char] = {
    if (!chars.hasNext) {
      None
    } else {
      current += 1
      Some(chars.next())
    }
  }

  def expect(c: Char): Unit = {
    val copt = consume()
    copt match {
      case Some(c1) =>
        if (c == c1) ()
        else err(s"Expected $c but found $c1 instead.")
      case None => err(s"Expected $c but reached end of file.")
    }

  }

  /** Advance the [[Lexer.chars]] iterator while the next character matches the given predicate. */
  @tailrec
  final def consumeWhile(pred: Char => Boolean): Unit =
    peek() match {
      case Some(c) if pred(c) => consume(); consumeWhile(pred)
      case _ => ()
    }

  /** Peek at the next character. Returns [[None]] if EOF is reached. */
  def peek(): Option[Char] = chars.headOption

  def peekMatches(pred: Char => Boolean): Boolean = peek().exists(pred)

  def peekN(lookahead: Int): Option[Char] = {
    if (current - 1 + lookahead < str.length) Some(str.charAt(current - 1 + lookahead))
    else None
  }

  /** Convenience function for creating a token.
   * Assumed Invariant: When making a token, the lexer's head [[current]] is pointing not at the token's
   * last position but at the next one.
   */
  def makeToken(kind: TokenKind): Token =
    Token(start, current - 1, kind)

  /** Used for reporting erros while lexing. These are only caught by the `lex` function. */
  def err(msg: String, start: Int = start, end: Int = current): Nothing =
    throw LexerError(msg, start, end)

  /** Checks if the characters starting at [[start]] match the expected string and only then
   * consumes all corresponding characters. That is, if there's no match, no characters are consumed.
   **/
  def matches(expected: String, start: Int = start): Boolean = {
    val len = expected.length
    val end = start + len - 1
    if (end >= str.length) return false
    val slice = str.substring(start, end + 1)
    if (slice == expected) {
      (start to end).foreach(_ => consume())
      true
    } else {
      false
    }
  }

  /** Extract a slice of the source string delimited by the starting and (exclusive) current index */
  def slice(start: Int = start, end: Int = current): String =
    str.substring(start, end)

  /** Like [[Lexer.matches]] but starts matching at the lexer's current head [[current]]. */
  def nextMatches(expected: String): Boolean = matches(expected, current)

  def nextMatches(expected: Char): Boolean =
    peek().exists { c =>
      val eq = c == expected
      if (eq) consume()
      eq
    }

  def matchesRegex(r: Regex): Boolean = {
    val rest = str.substring(start)
    val candidate = rest.takeWhile { c => !whitespace.matches(c.toString) }
    r.matches(candidate)
  }

  def lex()(using ctx: Context): Vector[Token] =
    try {
      run()
    } catch {
      case LexerError(msg, start, end) => {
        val relativeStart = source.offsetToPosition(start)
        val relativeEnd = source.offsetToPosition(end)
        val range = Range(relativeStart, relativeEnd)
        ctx.abort(effekt.util.messages.ParseError(msg, Some(range)))
      }
    }

  /** Main entry-point of the lexer. Whitespace is ignored and comments are collected.
   * If an error is encountered, all successfully scanned tokens this far will returned,
   * including the error.
   */
  def run(): Vector[Token] = {
    var eof = false
    while (!eof) {
      val kind =
        // If the last token was `}` and we are currently inside unquotes, remember to directly continue
        // lexing a string
        if (tokens.lastOption.map { _.kind }.contains(TokenKind.`}`) && !delimiters.isEmpty) {
          delimiters.pop() match {
            case `${{` =>
              // there has to be a string delimiter on the stack since `${ }` may only appear in strings
              // and cannot be directly nested within other `${}`
              val errMsg = "string interpolation ${ ... } may only appear inside strings"
              if (delimiters.isEmpty) err(errMsg)
              delimiters.pop() match {
                case `${{` | `{{` =>  err(errMsg)
                case strDelim: StrDelim => matchString(strDelim, true)
              }
            // Last `}` is not be considered as part of string interpolation. Let the parser fail if braces don't match
            // and just continue lexing
            case `{{` | `"` | `"""` | `'` => nextToken()
          }
        } else nextToken()
      kind match {
        case TokenKind.EOF =>
          eof = true
          // EOF has the position of one after the last character
          // this may be useful for a streaming lexer
          tokens += Token(current + 1, current + 1, TokenKind.EOF)
        case k =>
          tokens += makeToken(k)
      }
      start = current
    }
    tokens.toVector
  }

  // --- Literal and comment matchers ---
  // It is assumed that the prefix character which was used for deciding on the kind of the token is already consumed.

  /** Matches a number literal -- both floats and integers. However, signs are not lexed but instead
   * are to be handled by the parser later on.
   */
  def matchNumber(): TokenKind = {
    consumeWhile(_.isDigit)
    peekN(1) match {
      case Some('.') => {
        peekN(2) match {
          case Some(c) if c.isDigit =>
            consume()
            consumeWhile(_.isDigit)
            slice().toDoubleOption match {
              case None => err("Not a 64bit floating point literal.")
              case Some(n) => TokenKind.Float(n)
            }
          case _ => TokenKind.Integer(slice().toInt)
        }
      }
      case _ =>
        slice().toLongOption match {
          case None => err("Not a 64bit integer literal.")
          case Some(n) => TokenKind.Integer(n)
        }
    }
  }

  /** This is for remembering delimiters of strings and unquotes for string interpolation (`${`, `}`).
  * If we encounter a `"` or `"""`, we push it onto the stack and pop from the stack if we see such
  * delimiters again while lexing a string.
  * Likewise, if we are at a `${`, push it onto the stack and pop from the stack upon seeing a `}`.
  */
  val delimiters = mutable.Stack[Delimiter]()
  sealed trait Delimiter
  sealed trait InterpolateDelim extends Delimiter
  case object `${{` extends InterpolateDelim
  case object `{{` extends InterpolateDelim
  sealed trait StrDelim extends Delimiter {
    def offset: Int =
      this match {
        case `"` => 1
        case `"""` => 3
        case `'` => 1
      }

    def first: Char =
      this match {
        case `"` | `"""` => '"'
        case `'` => '\''
      }

    override def toString: String =
      this match {
        case `"` => "\""
        case `"""` => "\"\"\""
        case `'` => "'"
      }

    def isMultiline: Boolean =
      this match {
        case `"` => false
        case `"""` => true
        case `'` => false
      }
  }
  case object `"` extends StrDelim
  case object `"""` extends StrDelim
  /** Delimiter for characters */
  case object `'` extends StrDelim

  /** Matches a string literal -- both single- and multi-line strings.
   * Strings may contain space characters (e.g. \n, \t, \r), which are stored unescaped.
   * Strings may also contain unquotes, i.e., "f = ${x + 1}" that include arbitrary expressions.
   */
  def matchString(delim: StrDelim, continued: Boolean = false): TokenKind = {
    if (nextMatches(delim.toString)) return TokenKind.Str("", delim.isMultiline)
    val offset = delim.offset
    val st = if (continued) start else start + offset
    var endString = false
    val stringContent = StringBuilder()

    delimiters.push(delim)
    while (!endString) {
      peek() match {
        case Some(c) if c == delim.first => {
          consume()
          delim match {
            case `"""`  if (peekN(1).contains('\"') && peekN(2).contains('\"')) => {
              consume(); consume()
              delimiters.pop()
              endString = true
            }
            case `"` => {
              delimiters.pop()
              endString = true
            }
            case `'` => {
              delimiters.pop()
              endString = true
            }
            case _ => stringContent.addOne(c)
          }
        }
        case Some('\n') if !delim.isMultiline =>
          return err("Linebreaks are not allowed in single-line strings.")
        // handle escape sequences
        case Some('\\') if !delim.isMultiline => {
          expect('\\')
          peek() match {
            case Some('"') => consume(); stringContent.addOne('"')
            case Some('\'') => consume(); stringContent.addOne('\'')
            case Some('n') => consume(); stringContent.addOne('\n')
            case Some('t') => consume(); stringContent.addOne('\t')
            case Some('r') => consume(); stringContent.addOne('\r')
            case Some('\\') => consume(); stringContent.addOne('\\')
            case Some('$') => consume(); stringContent.addOne('$')
            case Some('u') => {
              consume()

              def isHexDigit(c: Char) = c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

              val hexCodePoint = peek() match {
                case Some(c) if isHexDigit(c) =>
                  val escapeStart = current
                  consumeWhile(isHexDigit)
                  slice(escapeStart, current)
                case Some('{') =>
                  consume()
                  val escapeStart = current
                  consumeWhile(c => c != '}')
                  expect('}')
                  slice(escapeStart, current - 1)
                case _ => err("Invalid escape sequence.", current - 1, current)
              }

              try {
                val codePoint = Integer.parseInt(hexCodePoint, 16)
                stringContent.append(String.valueOf(Character.toChars(codePoint)))
              } catch {
                case e: NumberFormatException => err(e.toString)
              }
            }
            case _ => err("Invalid escape sequence.", current - 1, current)
          }
        }
        case Some('$') if peekN(2).contains('{') => {
          return TokenKind.Str(stringContent.mkString, delim.isMultiline)
        }
        case None =>
          return err("Unterminated string.", start, start + offset)
        case Some(c) =>
          consume()
          stringContent.addOne(c)
      }
    }

    TokenKind.Str(stringContent.mkString, delim.isMultiline)
  }

  /** Matches a character: '.+' */
  def matchChar(): TokenKind = {
    matchString(`'`) match {
      case TokenKind.Str("", _) =>
        TokenKind.Error(LexerError("Empty character literal", start, current))
      case TokenKind.Str(cs, _) if (cs.codePointCount(0, cs.length) > 1) =>
        TokenKind.Error(LexerError("Character literal consists " +
            "of multiple code points.", start, current))
      case TokenKind.Str(cs, _) =>
        TokenKind.Chr(cs.codePointAt(0))
      case err => err
    }
  }

  lazy val hexadecimal = ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')

  /** \u<HEXADECIMAL>+ */
  def matchUnicodeLiteral(): TokenKind = {
    consumeWhile(hexadecimal.contains(_))
    val n = slice(start + 2, current)
    try {
      TokenKind.Chr(Integer.parseInt(n, 16))
    } catch {
      case e: Throwable => err("Invalid unicode literal.")
    }
  }

  /** Matches a mult-line comment delimited by /* and */. */
  def matchMultilineComment(): TokenKind = {
    var closed = false
    while (!closed) {
      consume() match {
        // end comment on */
        case Some('*') if nextMatches('/') => closed = true
        case Some(_) => ()
        // reached EOF without encountering */
        case None => return err("Unterminated multi-line comment; expected closing `*/`.")
      }
    }
    // exclude /* and */ from the comment's content
    val comment = slice(start + 2, current - 2)
    TokenKind.Comment(comment)
  }

  /** Matches a single-line comment delimited by // and a newline. */
  def matchComment(): TokenKind = {
    var reachedNewline = false
    while (!reachedNewline) {
      // peek to ensure the newline is not part of the comment
      peek() match {
        // comment is terminated when encountering a new line
        case Some('\n') => reachedNewline = true
        case None => reachedNewline = true
        case _ => consume()
      }
    }
    // exclude //
    val comment = slice(start + 2, current)
    TokenKind.Comment(comment)
  }

  @tailrec
  final def matchWhitespaces(): Unit = {
      peek() match {
        case Some(' ') | Some('\t') => {
          consume()
          // currently, we ignore any whitespace other than newlines
          // tokens += makeToken(TokenKind.Space)
          matchWhitespaces()
        }
        case Some('\n') => {
          consume()
          tokens += makeToken(TokenKind.Newline)
          matchWhitespaces()
        }
        // windows, of course, has to do things differently. A newline there is represented by \r\n
        case Some('\r') => {
          consume()
          nextMatches("\n")
          tokens += makeToken(TokenKind.Newline)
          matchWhitespaces()
        }
        case _ => start = current
      }
  }

  def nextToken(): TokenKind = {
    import TokenKind.*
    matchWhitespaces()
    val maybeChar = consume()
    if (maybeChar.isEmpty) return EOF
    val c = maybeChar.get
    c match {
      // --- symbols & pre- and infix operations ---
      case '=' if nextMatches('>') => `=>`
      case '=' if nextMatches('=') => `===`
      case '=' => `=`
      case ':' if nextMatches(':') => `::`
      case ':' => `:`
      case ';' => `;`
      case '@' => `@`
      case '<' if nextMatches('{') => `<{`
      case '<' if nextMatches('>') => `<>`
      case '<' if nextMatches('=') => `<=`
      case '<' => `<`
      case '>' if nextMatches('=') => `>=`
      case '>' => `>`
      case '{' => {
        delimiters.push(`{{`)
        `{`
      }
      case '}' if nextMatches('>') => `}>`
      case '}' => `}`
      case '(' => `(`
      case ')' => `)`
      case '[' => `[`
      case ']' => `]`
      case ',' => `,`
      case '.' => `.`
      case '/' if nextMatches('*') => matchMultilineComment()
      case '/' if nextMatches('/') => matchComment()
      case '/' => `/`
      case '!' if nextMatches('=') => `!==`
      case '!' => `!`
      case '|' if nextMatches('|') => `||`
      case '|' => `|`
      case '&' if nextMatches('&') => `&&`
      case '&' => `&`
      case '*' => `*`
      case '+' if nextMatches('+') => `++`
      case '+' => `+`
      case '-' if peek().exists(_.isDigit) => matchNumber()
      case '-' => `-`
      case '$' if nextMatches('{') => {
        delimiters.push(`${{`)
        TokenKind.`${`
      }
      case '$' => TokenKind.`${`
      // --- literals ---
      case '\'' => matchChar()
      case '\\' if nextMatches("u") => matchUnicodeLiteral()
      case '\"' if nextMatches("\"\"") => matchString(`"""`)
      case '\"' => matchString(`"`)
      case c if c.isDigit => matchNumber()
      // --- keywords & identifiers ---
      case c if nameFirst.contains(c) => {
        // since keywords are a subclass of identifiers and we want to match either keywords or identifiers,
        // we look for valid names
        consumeWhile { c => nameRest.contains(c) }
        val s = slice()
        // check if the slice matches any know keyword, otherwise it is necessarily an identifier
        keywordMap.getOrElse(s, TokenKind.Ident(s))
      }
      case c => err(s"Invalid keyword/identifier: $c.")
    }
  }
}
