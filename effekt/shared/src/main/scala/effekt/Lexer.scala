package effekt.lexer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.BufferedIterator
import scala.util.matching.Regex

/** An error encountered during lexing a source string. */
enum LexerError {
  case MalformedFloat
  case MalformedStringInterpolation
  case InvalidKeywordIdent(s: String)
  case UnterminatedString
  case UnterminatedComment
  case UnterminatedQuote
  case Eof
  case Expected(expected: String, got: String)
  case Custom(msg: String)
}

/**
 * A position in the source string.
 * @param line depending on line breaks.
 * @param column in a line.
 * @param offset is the absolute offset in the source string without considering
 *               line breaks.
 */
case class Position(line: Int, column: Int, offset: Int)

/**
 * A token consist of the absolute starting position, the absolute end position in the source file
 * and the kind of token. Both position are to be understood as inclusive.
 */
case class Token(start: Int, end: Int, kind: TokenKind)

enum TokenKind {
  // literals
  case Integer(n: Int)
  case Float(d: Double)
  case Str(s: String, multiline: Boolean)
  case Char(c: Char)
  // identifiers
  case Ident(id: String)
  // misc
  case Comment(msg: String)
  case Newline
  case Space
  case EOF
  case Error(err: LexerError)
  // symbols
  case `__`
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
  case `resume`
  case `match`
  case `def`
  case `module`
  case `import`
  case `export`
  case `extern`
  case `include`
  case `record`
  case `at`
  case `in`
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

object Lexer {
  import TokenKind.*

  val keywords: immutable.HashMap[String, TokenKind] = immutable.HashMap(
    "let" -> `let`,
    "true" -> `true`,
    "false" -> `false`,
    "val" -> `val`,
    "var" -> `var`,
    "if" -> `if`,
    "else" -> `else`,
    "while" -> `while`,
    "type" -> `type`,
    "effect" -> `effect`,
    "interface" -> `interface`,
    "try" -> `try`,
    "with" -> `with`,
    "case" -> `case`,
    "do" -> `do`,
    "fun" -> `fun`,
    "resume" -> `resume`,
    "match" -> `match`,
    "def" -> `def`,
    "module" -> `module`,
    "import" -> `import`,
    "export" -> `export`,
    "extern" -> `extern`,
    "include" -> `include`,
    "record" -> `record`,
    "at" -> `at`,
    "in" -> `in`,
    "box" -> `box`,
    "unbox" -> `unbox`,
    "return" -> `return`,
    "region" -> `region`,
    "resource" -> `resource`,
    "new" -> `new`,
    "and" -> `and`,
    "is" -> `is`,
    "namespace" -> `namespace`,
    "pure" -> `pure`
  )

  enum Mode {
    case Normal
    case Interpolated
  }
}

/**
 *
 * @param source A string of a Effekt program to be lexed.
 */
class Lexer(source: String) {
  import Lexer.*

  /** The absolute starting index in the source string of the currently scanned token */
  var start: Int = 0
  /** The absolute index of the lexer's reading 'head' in the source string. Example
   * "hello world"
   *     ^
   *  chars.next() => current = 3
   */
  var current: Int = 0
  /** The sequence of tokens the source strings contains. Returned as the result by [[Lexer.run()]] */
  val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  // For future improvement, this is probably more performant than using substring on the source
  val currLexeme: mutable.StringBuilder = mutable.StringBuilder()
  /** A peekable iterator of the source string. This is used instead of directly indexing the source string. */
  val chars: BufferedIterator[Char] = source.iterator.buffered

  lazy val whitespace: Regex = """([ \t\r\n])""".r // single whitespace characters
  //lazy val nameFirst: Regex = """[a-zA-Z_]""".r
  //lazy val nameRest: Regex = """[a-zA-Z0-9_!?$]""".r
  //lazy val nameBoundary: Regex = """(?!%s)""".format(nameRest).r
  //lazy val name: Regex = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r
  val whitespaces = Set(' ', '\t', '\r', '\n')
  val nameFirst = ('a'.to('z').iterator ++ 'A'.to('Z').iterator ++ List('_').iterator).toSet
  val nameRest = ('a'.to('z').iterator ++ 'A'.to('Z').iterator ++ '0'.to('9').iterator ++ List('_', '!', '?', '$').iterator).toSet

  def isEOF: Boolean =
    current >= source.length

  /** Advance the [[Lexer.chars]] iterator and [[Lexer.current]] index. Returns [[None]] if EOF is reached. */
  def consume(): Option[Char] = {
    if (!chars.hasNext) {
      None
    } else {
      current += 1
      Some(chars.next())
    }
  }

  def expect(c: Char, err: LexerError): Either[LexerError, Unit] = {
    val copt = consume()
    copt match {
      case Some(c1) if c == c1 => Right(())
      case None => Left(LexerError.Eof)
      case _ => Left(err)
    }

  }

  /** Advance the [[Lexer.chars]] iterator while the next character matches the given predicate. */
  @tailrec
  final def consumeWhile(pred: Char => Boolean): Either[LexerError, Unit] =
    peek() match {
      case Some(c) if pred(c) => consume(); consumeWhile(pred)
      case Some(_) => Right(())
      case _ => Left(LexerError.Eof)
    }

  /** Peek at the next character. Returns [[None]] if EOF is reached. */
  def peek(): Option[Char] = chars.headOption

  def peekMatches(pred: Char => Boolean): Boolean = peek().exists(pred)

  def peekN(lookahead: Int): Option[Char] = {
    if (current - 1 + lookahead < source.length) Some(source.charAt(current - 1 + lookahead))
    else None
  }

  /** Convenience function for creating a token.
   * Assumed Invariant: When making a token, the lexer's head [[current]] is pointing not at the token's
   * last position but at the next one.
   */
  def makeToken(kind: TokenKind): Token =
    Token(start, current - 1, kind)

  def err(err: LexerError): TokenKind =
    TokenKind.Error(err)

  /** Checks if the characters starting at [[start]] match the expected string and only then
   * consumes all corresponding characters. That is, if there's no match, no characters are consumed.
   **/
  def matches(expected: String, start: Int = start): Boolean = {
    val len = expected.length
    val end = start + len - 1
    if (end >= source.length) return false
    val slice = source.substring(start, end + 1)
    if (slice == expected) {
      (start to end).foreach(_ => consume())
      true
    } else {
      false
    }
  }

  /** Extract a slice of the source string delimited by the starting and (exclusive) current index */
  def slice(start: Int = start, end: Int = current): String =
    source.substring(start, end)

  /** Like [[Lexer.matches]] but starts matching at the lexer's current head [[current]]. */
  def nextMatches(expected: String): Boolean = matches(expected, current)

  def nextMatches(expected: Char): Boolean =
    peek().exists { c =>
      val eq = c == expected
      if (eq) consume()
      eq
    }

  def matchesRegex(r: Regex): Boolean = {
    val rest = source.substring(start)
    val candidate = rest.takeWhile { c => !whitespace.matches(c.toString) }
    r.matches(candidate)
  }

  /** Main entry-point of the lexer. Whitespace is ignored and comments are collected.
   * If an error is encountered, all successfully scanned tokens this far will returned,
   * including the error.
   */
  def run(): (List[Token], Option[LexerError]) = {
    var err: Option[LexerError] = None
    var eof = false
    while (!eof && err.isEmpty) {
      val kind =
        if (tokens.lastOption.map { _.kind }.contains(TokenKind.`}`) && delimiters.pop() == `${{`) {
          // TODO: catch error
          val delim = delimiters.pop().asInstanceOf[StrDelim]
          matchString(delim, true)
        } else nextToken()
      kind match {
        case TokenKind.EOF =>
          eof = true
          // EOF has the position of one after the last character
          // this may be useful for a streaming lexer
          tokens += Token(current + 1, current + 1, TokenKind.EOF)
        case TokenKind.Error(e) =>
          err = Some(e)
        case k =>
          tokens += makeToken(k)
      }
      start = current
    }
    (tokens.toList, err)
  }

  // --- Literal and comment matchers ---
  // It is assumed that the prefix character which was used for deciding on the kind of the token is already consumed.

  /** Matches a number literal -- both floats and integers. However, signs are not lexed but instead
   * are to be handled by the parser later on.
   */
  def matchNumber(): TokenKind = {
    consumeWhile(_.isDigit)
    peek() match {
      case Some('.') => {
        consume()
        consumeWhile(_.isDigit)
        TokenKind.Float(slice().toDouble)
      }
      case _ => TokenKind.Integer(slice().toInt)
    }
  }

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
      }

    override def toString: String =
      this match {
        case `"` => "\""
        case `"""` => "\"\"\""
      }

    def isMultiline: Boolean =
      this match {
        case `"` => false
        case `"""` => true
      }
  }
  case object `"` extends StrDelim
  case object `"""` extends StrDelim

  /** Matches a string literal -- both single- and multi-line strings. Strings may contain arbitrary escaped
   * characters, these are not validated. Strings may also contain quotes, i.e., "f = ${x + 1}" that include arbitrary
   * expressions.
   */
  def matchString(delim: StrDelim, continued: Boolean = false): TokenKind = {
    if (nextMatches(delim.toString)) return TokenKind.Str("", delim.isMultiline)
    val offset = delim.offset
    val st = if (continued) start else start + offset
    var endString = false
    delimiters.push(delim)
    while (!endString) {
      peek() match {
        case Some('\"') => {
          consume()
          delim match {
            case `"""` => {
              if (peekN(1).contains('\"') && peekN(2).contains('\"')) {
                consume(); consume()
                delimiters.pop()
                endString = true
              }
            }
            case `"` => {
              delimiters.pop()
              endString = true
            }
          }
        }
        case Some('\n') if delim == `"` =>
          return err(LexerError.Custom("linebreaks are not allowed in single-line strings"))
        case Some('\\') => {
          consume()
          consume()
        }
        case Some('$') if peekN(2).contains('{') => {
          val s = slice(st, current)
          return TokenKind.Str(s, delim.isMultiline)
        }
        case None =>
          return err(LexerError.UnterminatedString)
        case _ =>
          consume()
      }
    }
    val s = slice(st, current - offset)
    TokenKind.Str(s, delim.isMultiline)
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
        case None => return err(LexerError.UnterminatedComment)
      }
    }
    // exclude /* and */ from the comment's content
    val comment = slice(start + 2, current - 2)
    TokenKind.Comment(comment)
  }

  /** Matches a single-line comment. */
  def matchComment(): TokenKind = {
    var newline = false
    while (!newline) {
      consume() match {
        // comment is terminated when encountering a new line
        case Some('\n') => newline = true
        case _ => ()
      }
    }
    // exclude // and \n
    val comment = slice(start + 2, current - 1)
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
      case '_' if peek().exists(!nameRest.contains(_)) => `__`
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
        keywords.getOrElse(s, TokenKind.Ident(s))
      }
      case c => TokenKind.Error(LexerError.InvalidKeywordIdent(c.toString))
    }
  }
}
