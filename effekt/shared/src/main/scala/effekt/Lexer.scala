package effekt.lexer

import scala.collection.mutable
import scala.collection.immutable
import effekt.source.Span
import kiama.util.Source

/** Lexing errors that can occur during tokenization */
enum LexerError {
  case InvalidEscapeSequence(escapeString: String)
  case Expected(char: Char)
  case UnknownChar(char: Char)
  case UnterminatedStringLike(kind: TokenKind)
  case UnterminatedComment
  case EmptyCharLiteral
  case MultipleCodePointsInChar
  case InvalidIntegerFormat
  case InvalidDoubleFormat
  case UnterminatedInterpolation(depth: Int)

  def message: String = this match {
    case InvalidEscapeSequence(str) =>
      s"Invalid character in escape sequence: `\\${str}`"
    case Expected(char) =>
      s"Expected '$char' (U+${char.toInt.toHexString}) while lexing"
    case UnknownChar(char) =>
      s"Unknown character '$char' (U+${char.toInt.toHexString}) in file"
    case UnterminatedStringLike(TokenKind.Str(_, false)) =>
      s"Unterminated single-line string; expected closing `\"` on the end of the line"
    case UnterminatedStringLike(kind) =>
      s"Unterminated ${TokenKind.explain(kind)}"
    case UnterminatedComment => "Unterminated multi-line comment; expected closing `*/`"
    case EmptyCharLiteral => "Empty character literal"
    case MultipleCodePointsInChar => "Character literal consists of multiple code points"
    case InvalidIntegerFormat => "Invalid integer format, not a 64bit integer literal"
    case InvalidDoubleFormat => "Invalid float format, not a double literal"
    case UnterminatedInterpolation(depth) =>
      s"Unterminated string interpolation ($depth unclosed splices)"
  }
}

/**
 * A token consist of the absolute starting position, the absolute end position in the source file
 * and the kind of token. Both positions are to be understood as inclusive.
 */
case class Token(start: Int, end: Int, kind: TokenKind) {
  def span(source: Source): Span = Span(source, start, end)
  def isError: Boolean = this.kind match {
    case TokenKind.Error(_) => true
    case _ => false
  }
}

enum TokenKind {
  // control tokens
  case EOF // end of input
  case Error(error: LexerError) // invalid token with a reason
  case Newline // newline

  // literals
  case Integer(n: Long)
  case Float(d: Double)
  case Str(s: String, multiline: Boolean)
  case HoleStr(s: String)
  case Chr(c: Int)

  // identifiers
  case Ident(id: String)

  // misc
  case Comment(msg: String)
  case DocComment(msg: String)
  case Shebang(cmd: String)
  case Space // Note: unused now

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
    case Ident(n)             => s"identifier ${n}"
    case Integer(n)           => s"integer ${n}"
    case Float(d)             => s"float ${d}"
    case Str(_, true)         => s"multi-line string literal"
    case Str(s, false)        => s"string literal \"${s}\""
    case Chr(c)               => s"character literal '${c.toChar}'"
    case HoleStr(_)           => "natural language hole `<\" ... \">`"
    case Comment(_)           => "comment"
    case DocComment(_)        => "doc comment"
    case Newline              => "newline"
    case Space                => "space"
    case EOF                  => "end of file"
    case `}$`                 => "a splice ending brace `}`"
    case Error(error)         => s"invalid token: ${error.message}"
    case other                => other.toString
  }

  val keywords = Vector(
    `let`, `true`, `false`, `val`, `var`, `if`, `else`, `while`, `type`, `effect`, `interface`,
    `try`, `with`, `case`, `do`, `fun`, `match`, `def`, `module`, `import`, `export`, `extern`,
    `include`, `record`, `box`, `unbox`, `return`, `region`, `resource`, `new`, `and`, `is`,
    `namespace`, `pure`
  )

  val keywordMap: immutable.HashMap[String, TokenKind] =
    immutable.HashMap.from(keywords.map(k => k.toString -> k))
}

/**
 * We use `Position` to track the byte offset (0-indexed), line (1-indexed) and column (1-indexed).
 */
case class Position(offset: Int, line: Int, column: Int) {
  def advance(isNewline: Boolean): Position =
    if isNewline then
      Position(offset + 1, line + 1, 1)
    else
      Position(offset + 1, line, column + 1)
}

object Position {
  def begin: Position = Position(0, 1, 1)
}

/**
 * Tracks brace/paren/bracket depth for string interpolation and error recovery.
 *
 * This is specifically for tracking brace depth to determine
 * interpolation boundaries. When we see ${, we record the current brace depth
 * and know the interpolation ends when we return to that depth.
 */
case class DepthTracker(var parens: Int, var braces: Int, var brackets: Int)

/**
 * Never throws exceptions - always returns Error tokens for errors.
 *
 * @param source The source file to be lexed
 */
class Lexer(source: Source) extends Iterator[Token] {
  import TokenKind.*

  // Lexer state with current/next character lookahead
  private var currentChar: Char = if source.content.nonEmpty then source.content(0) else '\u0000'
  private var nextChar: Char = if source.content.length > 1 then source.content(1) else '\u0000'
  private var position: Position = Position.begin
  private var tokenStartPosition: Position = Position.begin
  private val charIterator = source.content.iterator.drop(2) // we already consumed first two chars

  // String interpolation state
  private val delimiters = mutable.Stack[Delimiter]()
  private val depthTracker = DepthTracker(0, 0, 0)
  private val interpolationDepths = mutable.Stack[Int]()

  /**
   * String interpolation requires a two-step dance.
   *
   * When we see a `}` ending interpolation, we
   * 1. MUST return the `}$` token first,
   * 2. and ensure that the NEXT call to next() must resume lexing a string.
   *
   * In order to do step 2, we have this [[resumeStringNext]] flag.
   */
  private var resumeStringNext = false

  // Character classification functions
  // NOTE: These were measured to be slightly faster than `('a' to 'z').iterator.toSet`, esp. on larger files.
  inline private def isNameFirst(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  inline private def isNameRest(c: Char): Boolean =
    isNameFirst(c) || (c >= '0' && c <= '9') || c == '!' || c == '?' || c == '$'

  private def isHexDigit(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  // Various kinds of string delimiters supported by Effekt
  enum Delimiter {
    //          "...",   """...""",      '...',    <"...">
    case SingleString, MultiString, CharString, HoleString

    def allowsInterpolation: Boolean = this match {
      case SingleString => true
      case MultiString => true
      case CharString => false
      case HoleString => true
    }

    def isMultiline: Boolean = this match {
      case SingleString => false
      case MultiString => true
      case CharString => false
      case HoleString => true
    }

    def allowsEscapes: Boolean = !this.isMultiline

    def toTokenKind(cs: String): TokenKind = this match {
      case SingleString => TokenKind.Str(cs, multiline = false)
      case MultiString => TokenKind.Str(cs, multiline = true)
      case HoleString => TokenKind.HoleStr(cs)
      case CharString if cs.isEmpty => TokenKind.Error(LexerError.EmptyCharLiteral)
      case CharString if cs.codePointCount(0, cs.length) > 1 => TokenKind.Error(LexerError.MultipleCodePointsInChar)
      case CharString /* otherwise */ => TokenKind.Chr(cs.codePointAt(0))
    }
  }
  export Delimiter.*

  private var eof: Boolean = false

  override def hasNext: Boolean = !eof

  override def next(): Token =
    if !resumeStringNext || delimiters.isEmpty then
      skipWhitespace()

    tokenStartPosition = position
    val kind = nextToken()

    if kind == TokenKind.EOF then
      eof = true
      Token(tokenStartPosition.offset, position.offset, kind)
    else
      Token(tokenStartPosition.offset, position.offset - 1, kind)

  private def skipWhitespace(): Unit =
    while !atEndOfInput do
      currentChar match {
        case ' ' | '\t' => advance()
        case '\n' => return // Stop here, let newline be handled as a token
        case '\r' =>
          if nextChar == '\n' then return // Stop here for \r\n
          else advance() // Treat standalone \r as whitespace
        case _ => return
      }

  private def atEndOfInput: Boolean =
    currentChar == '\u0000'

  /**
   * Advances the lexer by one, consuming (and returning) the current character.
   *
   * Consider using the [[advanceWith]], [[advance2With]], [[advance3With]] versions
   * for when you need to [[advance]] and then return the given token kind.
   */
  private def advance(): Char =
    val ret = currentChar
    currentChar = nextChar
    nextChar = if charIterator.hasNext then charIterator.next() else '\u0000'
    position = position.advance(ret == '\n')
    ret

  inline def advance(inline k: Int): Unit =
    if k > 0 then { advance(); advance(k - 1) }

  inline def advanceWith(inline k: Int, inline kind: => TokenKind): TokenKind =
    advance(k); kind

  inline def advanceWith(inline kind: => TokenKind): TokenKind = advanceWith(1, kind)
  inline def advance2With(inline kind: => TokenKind): TokenKind = advanceWith(2, kind)
  inline def advance3With(inline kind: => TokenKind): TokenKind = advanceWith(3, kind)

  private def expect(expected: Char, kind: TokenKind): TokenKind =
    if currentChar == expected then
      advanceWith(kind)
    else
      advanceWith(TokenKind.Error(LexerError.Expected(expected)))

  private def getCurrentSlice(skipAfterStart: Int = 0, skipBeforeEnd: Int = 0): String =
    source.content.substring(tokenStartPosition.offset + skipAfterStart, position.offset - skipBeforeEnd)

  private def advanceWhile(predicate: (Char, Char) => Boolean): Unit =
    while (predicate(currentChar, nextChar) && !atEndOfInput) {
      advance()
    }

  private def peekAhead(offset: Int): Char =
    val targetIndex = position.offset + offset
    if targetIndex < source.content.length then
      source.content(targetIndex)
    else
      '\u0000'

  /**
   * Check if we're at an interpolation boundary.
   * This happens when the current brace depth matches the depth when interpolation started.
   */
  private def isAtInterpolationBoundary: Boolean =
    interpolationDepths.nonEmpty && interpolationDepths.top == depthTracker.braces

  /**
   * "Main" function for getting the next token kind.
   * Wrapped on the outside by [[Lexer.next]] which handles whitespace.
   */
  private def nextToken(): TokenKind =
    // First handle pending string continuation / return from a splice.
    if (resumeStringNext) {
      resumeStringNext = false
      if (delimiters.nonEmpty) {
        return stringLike(delimiters.top, continued = true)
      }
      // otherwise fallthrough
    }

    (currentChar, nextChar) match {
      case ('\n',    _) => advanceWith(TokenKind.Newline)
      case ('\r', '\n') => advance2With(TokenKind.Newline)

      // Numbers
      case (c, _) if c.isDigit => number()

      // Identifiers and keywords
      case (c, _) if isNameFirst(c) => identifier()

      // String literals, character literals, hole string literals
      case ('"', '"') if peekAhead(2) == '"' => advance3With(stringLike(MultiString)) // """ ... """
      case ('"',   _)                        => advanceWith(stringLike(SingleString)) // " ... """
      case ('\'',  _)                        => advanceWith(stringLike(CharString))   // ' ... '
      case ('<', '"')                        => advance2With(stringLike(HoleString))  // <" ... ">

      // Comments
      case ('/', '*') => advance2With(multilineComment())
      case ('/', '/') => advance2With(singlelineComment())
      case ('/',   _) => advanceWith(TokenKind.`/`)

      // Shebang
      case ('#', '!') => advance2With(shebang())

      // Two-character operators
      case ('=', '=') => advance2With(TokenKind.`===`)
      case ('=', '>') => advance2With(TokenKind.`=>`)
      case ('=',   _) => advanceWith(TokenKind.`=`)

      case ('!', '=') => advance2With(TokenKind.`!==`)
      case ('!',   _) => advanceWith(TokenKind.`!`)

      case ('<', '=') => advance2With(TokenKind.`<=`)
      case ('<', '>') => advance2With(TokenKind.`<>`)
      case ('<', '{') => advance2With(TokenKind.`<{`)
      case ('<',   _) => advanceWith(TokenKind.`<`)

      case ('>', '=') => advance2With(TokenKind.`>=`)
      case ('>',   _) => advanceWith(TokenKind.`>`)

      case (':', ':') => advance2With(TokenKind.`::`)
      case (':',   _) => advanceWith(TokenKind.`:`)

      case ('|', '|') => advance2With(TokenKind.`||`)
      case ('|',   _) => advanceWith(TokenKind.`|`)

      case ('&', '&') => advance2With(TokenKind.`&&`)
      case ('&',   _) => advanceWith(TokenKind.`&`)

      case ('+', '+') => advance2With(TokenKind.`++`)
      case ('+',   _) => advanceWith(TokenKind.`+`)

      case ('-', c) if c.isDigit => advanceWith(number(negative = true))
      case ('-', _)              => advanceWith(TokenKind.`-`)

      case ('$', '{') =>
        interpolationDepths.push(depthTracker.braces + 1)
        depthTracker.braces += 1
        advance2With(TokenKind.`${`)
      case ('$', _) =>
        advanceWith(TokenKind.Error(LexerError.UnknownChar('$')))

      case ('}', _) if isAtInterpolationBoundary =>
        interpolationDepths.pop()
        depthTracker.braces -= 1
        resumeStringNext = true // remember to resume with a string next!
        advanceWith(TokenKind.`}$`)
      case ('}', '>') => advance2With(TokenKind.`}>`)
      case ('}', _) =>
        depthTracker.braces -= 1
        advanceWith(TokenKind.`}`)

      // Single-character tokens
      case (';', _) => advanceWith(TokenKind.`;`)
      case ('@', _) => advanceWith(TokenKind.`@`)
      case ('{', _) =>
        depthTracker.braces += 1
        advanceWith(TokenKind.`{`)
      case ('(', _) =>
        depthTracker.parens += 1
        advanceWith(TokenKind.`(`)
      case (')', _) =>
        depthTracker.parens -= 1
        advanceWith(TokenKind.`)`)
      case ('[', _) =>
        depthTracker.brackets += 1
        advanceWith(TokenKind.`[`)
      case (']', _) =>
        depthTracker.brackets -= 1
        advanceWith(TokenKind.`]`)
      case (',', _) => advanceWith(TokenKind.`,`)
      case ('.', _) => advanceWith(TokenKind.`.`)
      case ('*', _) => advanceWith(TokenKind.`*`)

      case ('\u0000', _) =>
        // EOF reached - provide context about unclosed constructs
        if interpolationDepths.nonEmpty && delimiters.nonEmpty then
          val depth = interpolationDepths.length
          interpolationDepths.clear()
          delimiters.clear()
          TokenKind.Error(LexerError.UnterminatedInterpolation(depth))
        else if delimiters.nonEmpty then
          val latestDelimiter = delimiters.top
          delimiters.clear()
          // NOTE: We only report the most recent one here...
          TokenKind.Error(LexerError.UnterminatedStringLike(latestDelimiter.toTokenKind("")))
        else
          TokenKind.EOF
      case (c,        _) => advanceWith(TokenKind.Error(LexerError.UnknownChar(c)))
    }

  private def number(negative: Boolean = false): TokenKind =
    // Consume the integer part
    advanceWhile { (curr, _) => curr.isDigit }

    if currentChar == '.' && nextChar.isDigit then
      advance() // consume '.'
      // Consume fractional part
      advanceWhile { (curr, _) => curr.isDigit }

      // Get the entire float string
      val floatString = getCurrentSlice()

      floatString.toDoubleOption match {
        case Some(float) => TokenKind.Float(float)
        case None => TokenKind.Error(LexerError.InvalidDoubleFormat)
      }
    else
      // Get the integer string
      val integerString = getCurrentSlice()

      integerString.toLongOption match {
        case Some(integer) => TokenKind.Integer(integer)
        case None => TokenKind.Error(LexerError.InvalidIntegerFormat)
      }

  private def identifier(): TokenKind =
    advanceWhile { (curr, _) => isNameRest(curr) }
    val word = getCurrentSlice()

    TokenKind.keywordMap.getOrElse(word, TokenKind.Ident(word))

  /**
   * Lexes any kind of string given by [[Delimiter]].
   * [[continued]] is set when we return to lexing a string after a stint in a splice.
   *
   * Contract: Expects its caller to already consume the delimiter itself!
   */
  private def stringLike(delimiter: Delimiter, continued: Boolean = false): TokenKind = {
    if !continued then delimiters.push(delimiter)

    val contents = StringBuilder()

    /**
     * Creates the correct token to be returned, takes care of the [[delimiters]] stack.
     */
    def close(shouldPop: Boolean = true, unterminated: Boolean = false) = {
      if shouldPop then delimiters.pop()

      val kind = delimiter.toTokenKind(contents.toString)

      if (unterminated) {
        TokenKind.Error(LexerError.UnterminatedStringLike(kind))
      } else {
        kind
      }
    }

    while (!atEndOfInput) {
      (currentChar, nextChar) match {
        // closing characters
        case ('"', '"') if delimiter == MultiString && peekAhead(2) == '"' =>
          return advance3With(close())
        case ('"', _) if delimiter == SingleString =>
          return advanceWith(close())
        case ('"', '>') if delimiter == HoleString =>
          return advance2With(close())
        case ('\'', _) if delimiter == CharString =>
          return advanceWith(close())

        // escapes
        case ('\\', _) if delimiter.allowsEscapes =>
          advance()
          currentChar match {
            case '\\' | '"' | '\'' | '$' => contents.addOne(advance())
            case 'n' => advance(); contents.addOne('\n')
            case 'r' => advance(); contents.addOne('\r')
            case 't' => advance(); contents.addOne('\t')
            case 'u' =>
              val start = position.offset
              advance()
              lexUnicodeEscape() match {
                case -1 => return TokenKind.Error(LexerError.InvalidEscapeSequence(source.content.substring(start, position.offset - 1)))
                case codePoint => contents.append(String.valueOf(Character.toChars(codePoint)))
              }
            case c =>
              return TokenKind.Error(LexerError.InvalidEscapeSequence(c.toString))
          }

        // interpolation
        case ('$', '{') if delimiter.allowsInterpolation =>
          // We *do not* pop in this case, we need to keep the delimiter on the stack
          // in order to know into what kind of string we should resume!
          return close(shouldPop = false)

        // newlines
        case ('\n', _) if !delimiter.isMultiline =>
          return close(unterminated = true)

        // "normal" characters of a string
        case (_, _) => contents.addOne(advance())
      }
    }

    // End of input reached
    close(unterminated = true)
  }

  // Returns a Char represented as a 32bit integer or -1 on failure
  private def lexUnicodeEscape(): Int =
    currentChar match {
      case '{' =>
        advance()
        val start = position.offset
        advanceWhile { (curr, _) => curr != '}' }
        if currentChar != '}' then return -1

        advance() // consume '}'
        val hexStr = source.content.substring(start, position.offset - 1)
        try java.lang.Integer.parseInt(hexStr, 16)
        catch case _: NumberFormatException => -1

      // TODO(jiribenes): revamp this syntax, count to 4 at most?
      case c if isHexDigit(c) =>
        val start = position.offset
        advanceWhile { (curr, _) => isHexDigit(curr) }
        val hexStr = source.content.substring(start, position.offset)
        try java.lang.Integer.parseInt(hexStr, 16)
        catch case _: NumberFormatException => -1

      case _ => -1
    }

  private def singlelineComment(): TokenKind =
    val isDocComment = currentChar == '/'
    advanceWhile { (curr, _) => curr != '\n' }

    if isDocComment then
      TokenKind.DocComment(getCurrentSlice(skipAfterStart = 3)) // Remove '///'
    else
      TokenKind.Comment(getCurrentSlice(skipAfterStart = 2)) // Remove '//'

  private def multilineComment(): TokenKind =
    var done = false
    while (!atEndOfInput && !done) {
      (currentChar, nextChar) match {
        case ('/', '*') => advance(); advance() // ignored!
        case ('*', '/') => advance(); advance(); done = true
        case _ => advance()
      }
    }

    if !done then
      TokenKind.Error(LexerError.UnterminatedComment)
    else
      val comment = getCurrentSlice(skipAfterStart = 2, skipBeforeEnd = 2) // Remove `/*` and `*/`
      TokenKind.Comment(comment)

  private def shebang(): TokenKind =
    advanceWhile { (curr, _) => curr != '\n' }
    val command = getCurrentSlice(skipAfterStart = 2) // Remove `#!`
    TokenKind.Shebang(command)
}

object Lexer {
  def lex(source: Source): Vector[Token] =
    val lexer = Lexer(source)
    lexer.toVector
}
