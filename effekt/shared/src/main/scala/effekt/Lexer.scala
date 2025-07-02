package effekt.lexer

import scala.collection.mutable
import scala.collection.immutable
import effekt.source.Span
import kiama.util.Source

/** Lexing errors that can occur during tokenization */
enum LexerError:
  case InvalidEscapeSequence(char: Char)
  case Expected(char: Char)
  case UnknownChar(char: Char)
  case LinebreakInSinglelineString
  case UnterminatedString
  case UnterminatedComment
  case EmptyCharLiteral
  case MultipleCodePointsInChar
  case InvalidUnicodeLiteral
  case InvalidIntegerFormat
  case InvalidDoubleFormat

  def message: String = this match
    case InvalidEscapeSequence(char) =>
      s"Invalid character in escape sequence: '$char' (U+${char.toInt.toHexString})"
    case Expected(char) =>
      s"Expected '$char' (U+${char.toInt.toHexString}) while lexing"
    case UnknownChar(char) =>
      s"Unknown character '$char' (U+${char.toInt.toHexString}) in file"
    case LinebreakInSinglelineString => "Unexpected line break in a single-line string"
    case UnterminatedString => "Unterminated string literal"
    case UnterminatedComment => "Unterminated multi-line comment; expected closing `*/`"
    case EmptyCharLiteral => "Empty character literal"
    case MultipleCodePointsInChar => "Character literal consists of multiple code points"
    case InvalidUnicodeLiteral => "Invalid unicode literal"
    case InvalidIntegerFormat => "Invalid integer format, not a 64bit integer literal"
    case InvalidDoubleFormat => "Invalid float format, not a double literal"

/**
 * A token consist of the absolute starting position, the absolute end position in the source file
 * and the kind of token. Both positions are to be understood as inclusive.
 */
case class Token(start: Int, end: Int, kind: TokenKind):
  def span(source: Source): Span = Span(source, start, end)
  def isError: Boolean = this.kind match {
    case TokenKind.Error(_) => true
    case _ => false
  }

enum TokenKind:
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

object TokenKind:
  // "Soft" keywords
  val `resume` = TokenKind.Ident("resume")
  val `in` = TokenKind.Ident("in")
  val `at` = TokenKind.Ident("at")
  val `__` = Ident("_")

  def explain(kind: TokenKind): String = kind match
    case t if keywords.contains(t) => s"keyword ${kind}"
    case Ident(n)             => s"identifier ${n}"
    case Integer(n)           => s"integer ${n}"
    case Float(d)             => s"float ${d}"
    case Str(_, true)         => s"multi-line string"
    case Str(s, false)        => s"string \"${s}\""
    case Chr(c)               => s"character '${c.toChar}'"
    case Comment(_)           => "comment"
    case DocComment(_)        => "doc comment"
    case Newline              => "newline"
    case Space                => "space"
    case EOF                  => "end of file"
    case Error(error)         => s"invalid token: ${error.message}"
    case other                => other.toString

  val keywords = Vector(
    `let`, `true`, `false`, `val`, `var`, `if`, `else`, `while`, `type`, `effect`, `interface`,
    `try`, `with`, `case`, `do`, `fun`, `match`, `def`, `module`, `import`, `export`, `extern`,
    `include`, `record`, `box`, `unbox`, `return`, `region`, `resource`, `new`, `and`, `is`,
    `namespace`, `pure`
  )

  val keywordMap: immutable.HashMap[String, TokenKind] =
    immutable.HashMap.from(keywords.map(k => k.toString -> k))

// Full position tracking
case class Position(index: Int, byteOffset: Int, line: Int, column: Int):
  def advance(charWidth: Int, isNewline: Boolean): Position =
    if isNewline then
      Position(index + charWidth, byteOffset + 1, line + 1, 1)
    else
      Position(index + charWidth, byteOffset + 1, line, column + charWidth)

object Position:
  def begin: Position = Position(0, 0, 1, 1)

/**
 * Tracks brace/paren/bracket depth for string interpolation.
 *
 * DESIGN NOTE: This is specifically for tracking brace depth to determine
 * interpolation boundaries. When we see ${, we record the current brace depth
 * and know the interpolation ends when we return to that depth.
 */
case class BraceTracker(var parens: Int, var braces: Int, var brackets: Int):
  def incrementParens = { this.parens += 1 }
  def decrementParens = { this.parens -= 1 }
  def incrementBraces = { this.braces += 1 }
  def decrementBraces = { this.braces -= 1 }
  def incrementBrackets = { this.brackets += 1 }
  def decrementBrackets = { this.brackets -= 1 }

/**
 * Never throws exceptions - always returns Error tokens for errors.
 *
 * @param source The source file to be lexed
 */
class Lexer(source: Source) extends Iterator[Token]:
  import TokenKind.*

  // Lexer state with current/next character lookahead
  private var currentChar: Char = if source.content.nonEmpty then source.content(0) else '\u0000'
  private var nextChar: Char = if source.content.length > 1 then source.content(1) else '\u0000'
  private var position: Position = Position.begin
  private var tokenStartPosition: Position = Position.begin
  private val charIterator = source.content.iterator.drop(2) // we already consumed first two chars

  // String interpolation state
  private val delimiters = mutable.Stack[Delimiter]()
  private val braceTracker = BraceTracker(0, 0, 0)
  private val interpolationDepths = mutable.Stack[Int]()

  /**
   * DESIGN DECISION: String interpolation requires a two-step dance.
   *
   * When we see `}` ending interpolation:
   * 1. MUST return the `}$` token first (Iterator contract)
   * 2. NEXT call to next() must resume string lexing
   *
   * Any alternative (token buffering, state machines, etc.) just moves this
   * complexity elsewhere without eliminating it.
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

  sealed trait Delimiter
  case object StringDelim extends Delimiter
  case object MultiStringDelim extends Delimiter
  case object CharDelim extends Delimiter
  case object HoleDelim extends Delimiter
  case object InterpolationDelim extends Delimiter

  private var done: Boolean = false

  override def hasNext: Boolean = !done

  override def next(): Token =
    if !resumeStringNext then
      skipWhitespaceAndNewlines()

    tokenStartPosition = position
    val kind = nextToken()

    makeToken(kind)

  private def makeToken(kind: TokenKind): Token =
    if kind == TokenKind.EOF then
      done = true
      Token(tokenStartPosition.byteOffset, position.byteOffset, kind)
    else
      Token(tokenStartPosition.byteOffset, position.byteOffset - 1, kind)

  private def skipWhitespaceAndNewlines(): Unit =
    while !atEndOfInput do
      currentChar match
        case ' ' | '\t' => advance()
        case '\n' => return // Stop here, let newline be handled as a token
        case '\r' =>
          if nextChar == '\n' then return // Stop here for \r\n
          else advance() // Treat standalone \r as whitespace
        case _ => return

  private def atEndOfInput: Boolean =
    position.byteOffset >= source.content.length

  private def advance(): Char =
    val ret = currentChar
    currentChar = nextChar
    nextChar = if charIterator.hasNext then charIterator.next() else '\u0000'
    position = position.advance(1 /* ret.toString.getBytes("UTF-8").length */, ret == '\n')
    ret

  private def advanceWith(token: TokenKind): TokenKind =
    advance()
    token

  private def advance2With(token: TokenKind): TokenKind =
    advance()
    advance()
    token

  private def peek(): Char = currentChar
  private def peekNext(): Char = nextChar

  private def expect(expected: Char, token: TokenKind): TokenKind =
    if currentChar == expected then
      advanceWith(token)
    else
      advanceWith(TokenKind.Error(LexerError.Expected(expected)))

  private def getCurrentSlice: String =
    source.content.substring(tokenStartPosition.byteOffset, position.byteOffset)

  private def advanceWhile(predicate: (Char, Char) => Boolean): String =
    while predicate(currentChar, nextChar) && !atEndOfInput do
      advance()
    getCurrentSlice

  private def peekAhead(offset: Int): Char =
    val targetIndex = position.byteOffset + offset
    if targetIndex < source.content.length then
      source.content(targetIndex)
    else
      '\u0000'

  /**
   * Check if we're at an interpolation boundary.
   * This happens when the current brace depth matches the depth when interpolation started.
   */
  private def isAtInterpolationBoundary: Boolean =
    interpolationDepths.nonEmpty && interpolationDepths.top == braceTracker.braces

  /**
   * Handle the end of an interpolation expression.
   * This pops the interpolation state and sets up string resumption.
   */
  private def handleInterpolationEnd(): Unit =
    interpolationDepths.pop()
    braceTracker.decrementBraces
    advance() // consume '}'

    if delimiters.nonEmpty && delimiters.top == InterpolationDelim then
      delimiters.pop()
      resumeStringNext = true

  // String interpolation resumption
  private def resumeStringAfterInterpolation(): TokenKind =
    if delimiters.nonEmpty then
      delimiters.top match
        case StringDelim => lexString(continued = true)
        case MultiStringDelim => lexString(multiline = true, continued = true)
        case HoleDelim =>
          lexString(delimiter = '>', multiline = true, continued = true, allowInterpolation = true) match
            case TokenKind.Str(content, _) => TokenKind.HoleStr(content)
            case other => other
        case InterpolationDelim | CharDelim =>
          skipWhitespaceAndNewlines()
          nextToken() // recovery
    else
      nextToken() // recovery

  /**
   * "Main" function for getting the next token kind.
   * Wrapped on the outside by [[Lexer.next]] which handles whitespace.
   */
  private def nextToken(): TokenKind =
    if resumeStringNext then
      resumeStringNext = false
      return resumeStringAfterInterpolation()

    currentChar match
      case '\n' => advanceWith(TokenKind.Newline)
      case '\r' if nextChar == '\n' => advance2With(TokenKind.Newline)

      // Numbers
      case c if c.isDigit => lexNumber()

      // Identifiers and keywords
      case c if isNameFirst(c) => lexAlphanumeric()

      // String literals
      case '"' =>
        if nextChar == '"' && peekAhead(2) == '"' then
          advance(); advance(); advance()
          lexString(multiline = true)
        else
          advance()
          lexString(multiline = false)

      // Character literals
      case '\'' => advance(); lexCharLiteral()

      // Hole literals - let lexHole handle consuming the opening delimiter
      case '<' if nextChar == '"' =>
        advance(); advance()
        lexString(delimiter = '>', multiline = true, allowInterpolation = true) match
          case TokenKind.Str(content, _) => TokenKind.HoleStr(content)
          case other => other

      // Unicode literals
      // TODO(jiribenes, 2025-07-01): Do we even want to keep supporting these?
      case '\\' if nextChar == 'u' =>
        advance(); advance()
        lexUnicodeLiteral()

      // Comments
      case '/' =>
        nextChar match
          case '*' => advance(); advance(); lexMultilineComment()
          case '/' => advance(); advance(); lexSinglelineComment()
          case _ => advanceWith(TokenKind.`/`)

      // Shebang
      case '#' if nextChar == '!' =>
        advance(); advance()
        lexShebang()

      // Two-character operators
      case '=' =>
        nextChar match
          case '=' => advance2With(TokenKind.`===`)
          case '>' => advance2With(TokenKind.`=>`)
          case _ => advanceWith(TokenKind.`=`)

      case '!' =>
        nextChar match
          case '=' => advance2With(TokenKind.`!==`)
          case _ => advanceWith(TokenKind.`!`)

      case '<' =>
        nextChar match
          case '=' => advance2With(TokenKind.`<=`)
          case '>' => advance2With(TokenKind.`<>`)
          case '{' => advance2With(TokenKind.`<{`)
          case _ => advanceWith(TokenKind.`<`)

      case '>' =>
        nextChar match
          case '=' => advance2With(TokenKind.`>=`)
          case _ => advanceWith(TokenKind.`>`)

      case ':' =>
        nextChar match
          case ':' => advance2With(TokenKind.`::`)
          case _ => advanceWith(TokenKind.`:`)

      case '|' =>
        nextChar match
          case '|' => advance2With(TokenKind.`||`)
          case _ => advanceWith(TokenKind.`|`)

      case '&' =>
        nextChar match
          case '&' => advance2With(TokenKind.`&&`)
          case _ => advanceWith(TokenKind.`&`)

      case '+' =>
        nextChar match
          case '+' => advance2With(TokenKind.`++`)
          case _ => advanceWith(TokenKind.`+`)

      case '-' =>
        if nextChar.isDigit then
          advance()
          lexNumber(negative = true)
        else
          advanceWith(TokenKind.`-`)

      case '$' =>
        if nextChar == '{' then
          // Record the current brace depth for interpolation tracking
          interpolationDepths.push(braceTracker.braces + 1)
          braceTracker.incrementBraces
          delimiters.push(InterpolationDelim)
          advance2With(TokenKind.`${`)
        else
          advanceWith(TokenKind.Error(LexerError.UnknownChar('$')))

      case '}' =>
        if isAtInterpolationBoundary then
          handleInterpolationEnd()
          TokenKind.`}$`
        else
          braceTracker.decrementBraces
          nextChar match
            case '>' => advance2With(TokenKind.`}>`)
            case _ => advanceWith(TokenKind.`}`)

      // Single-character tokens
      case ';' => advanceWith(TokenKind.`;`)
      case '@' => advanceWith(TokenKind.`@`)
      case '{' =>
        braceTracker.incrementBraces
        advanceWith(TokenKind.`{`)
      case '(' =>
        braceTracker.incrementParens
        advanceWith(TokenKind.`(`)
      case ')' =>
        braceTracker.decrementParens
        advanceWith(TokenKind.`)`)
      case '[' =>
        braceTracker.incrementBrackets
        advanceWith(TokenKind.`[`)
      case ']' =>
        braceTracker.decrementBrackets
        advanceWith(TokenKind.`]`)
      case ',' => advanceWith(TokenKind.`,`)
      case '.' => advanceWith(TokenKind.`.`)
      case '*' => advanceWith(TokenKind.`*`)

      case '\u0000' =>
        TokenKind.EOF

      case c =>
        advance()
        TokenKind.Error(LexerError.UnknownChar(c))

  private def lexNumber(negative: Boolean = false): TokenKind =
    // Consume the integer part
    advanceWhile { (curr, _) => curr.isDigit }

    if currentChar == '.' && nextChar.isDigit then
      advance() // consume '.'
      // Consume fractional part
      advanceWhile { (curr, _) => curr.isDigit }

      // Get the entire float string
      val floatString = getCurrentSlice

      floatString.toDoubleOption match
        case Some(float) => TokenKind.Float(float)
        case None => TokenKind.Error(LexerError.InvalidDoubleFormat)
    else
      // Get the integer string
      val integerString = getCurrentSlice

      integerString.toLongOption match
        case Some(integer) => TokenKind.Integer(integer)
        case None => TokenKind.Error(LexerError.InvalidIntegerFormat)

  private def lexAlphanumeric(): TokenKind =
    advanceWhile { (curr, _) => isNameRest(curr) }
    val word = getCurrentSlice

    TokenKind.keywordMap.getOrElse(word, TokenKind.Ident(word))

  private def lexString(
                         delimiter: Char = '"',
                         multiline: Boolean = false,
                         continued: Boolean = false,
                         allowInterpolation: Boolean = true
                       ): TokenKind =

    val delimiterType: Delimiter = delimiter match
      case '"' => if multiline then MultiStringDelim else StringDelim
      case '\'' => CharDelim
      case '>' => HoleDelim // for <"...">
      case _ => ??? // TODO(jiribenes, 2025-07-02): take the delimiter explicitly...

    if !continued then delimiters.push(delimiterType)

    val contents = StringBuilder()

    while !atEndOfInput do
      currentChar match
        case '"' =>
          if delimiter == '"' then
            if multiline then
              if nextChar == '"' && peekAhead(2) == '"' then
                advance();
                advance();
                advance()
                delimiters.pop()
                return TokenKind.Str(contents.toString, multiline)
              else
                contents.addOne(advance())
            else
              advance()
              delimiters.pop()
              return TokenKind.Str(contents.toString, multiline)
          else if delimiter == '>' && nextChar == '>' then
            advance();
            advance() // consume ">
            delimiters.pop()
            return TokenKind.Str(contents.toString, multiline = false)
          else
            contents.addOne(advance())

        case c if c == delimiter && delimiter != '"' =>
          advance()
          delimiters.pop()
          return TokenKind.Str(contents.toString, multiline)

        case '\\' if delimiter != '\'' || !multiline =>
          advance()
          currentChar match
            case '\\' | '"' | '\'' | '$' => contents.addOne(advance())
            case 'n' => advance(); contents.addOne('\n')
            case 'r' => advance(); contents.addOne('\r')
            case 't' => advance(); contents.addOne('\t')
            case 'u' =>
              advance()
              lexUnicodeEscape() match
                case -1 => return TokenKind.Error(LexerError.InvalidUnicodeLiteral)
                case codePoint => contents.append(String.valueOf(Character.toChars(codePoint)))
            case c =>
              return TokenKind.Error(LexerError.InvalidEscapeSequence(c))

        case '$' if allowInterpolation && nextChar == '{' =>
          // Don't consume the '$', just return what we have so far
          if delimiter == '>'
          then return TokenKind.HoleStr(contents.toString)
          else return TokenKind.Str(contents.toString, multiline)

        case '\n' if !multiline =>
          return TokenKind.Error(LexerError.LinebreakInSinglelineString)

        case c =>
          contents.addOne(advance())

    // End of input reached
    delimiters.pop()
    TokenKind.Error(LexerError.UnterminatedString)

  private def lexCharLiteral(): TokenKind =
    lexString(delimiter = '\'', allowInterpolation = false) match
      case TokenKind.Str("", _) =>
        TokenKind.Error(LexerError.EmptyCharLiteral)
      case TokenKind.Str(cs, _) if cs.codePointCount(0, cs.length) > 1 =>
        TokenKind.Error(LexerError.MultipleCodePointsInChar)
      case TokenKind.Str(cs, _) =>
        TokenKind.Chr(cs.codePointAt(0))
      case err => err

  private def lexUnicodeLiteral(): TokenKind =
    advanceWhile { (curr, _) => isHexDigit(curr) }
    val hexStr = getCurrentSlice.substring(2) // Remove '\u'

    if hexStr.isEmpty then
      return TokenKind.Error(LexerError.InvalidUnicodeLiteral)

    try
      val codePoint = java.lang.Integer.parseInt(hexStr, 16)
      TokenKind.Chr(codePoint)
    catch
      case _: NumberFormatException =>
        TokenKind.Error(LexerError.InvalidUnicodeLiteral)

  // Returns a Char represented as a 32bit integer or -1 on failure
  private def lexUnicodeEscape(): Int =
    currentChar match
      case '{' =>
        advance()
        val start = position.byteOffset
        advanceWhile { (curr, _) => curr != '}' }
        if currentChar != '}' then return -1
        advance() // consume '}'
        val hexStr = source.content.substring(start, position.byteOffset - 1)
        try java.lang.Integer.parseInt(hexStr, 16)
        catch case _: NumberFormatException => -1

      case c if isHexDigit(c) =>
        val start = position.byteOffset
        advanceWhile { (curr, _) => isHexDigit(curr) }
        val hexStr = source.content.substring(start, position.byteOffset)
        try java.lang.Integer.parseInt(hexStr, 16)
        catch case _: NumberFormatException => -1

      case _ => -1

  private def lexSinglelineComment(): TokenKind =
    advanceWhile { (curr, _) => curr != '\n' }
    val comment = getCurrentSlice.substring(2) // Remove '//'

    if comment.startsWith("/") then
      TokenKind.DocComment(comment.substring(1)) // Remove '///'
    else
      TokenKind.Comment(comment)

  private def lexMultilineComment(): TokenKind =
    var done = false
    while !atEndOfInput && !done do
      (currentChar, nextChar) match {
        case ('/', '*') => advance(); advance() // ignored!
        case ('*', '/') => advance(); advance(); done = true
        case _ => advance()
      }

    if !done then
      TokenKind.Error(LexerError.UnterminatedComment)
    else
      val comment = getCurrentSlice.substring(2, getCurrentSlice.length - 2) // Remove /* and */
      TokenKind.Comment(comment)

  private def lexMultilineCommentNested(): TokenKind =
    var depth = 1

    while depth > 0 && !atEndOfInput do
      currentChar match
        case '/' if nextChar == '*' =>
          advance(); advance()
          depth += 1
        case '*' if nextChar == '/' =>
          advance(); advance()
          depth -= 1
        case _ =>
          advance()

    if depth > 0 then
      TokenKind.Error(LexerError.UnterminatedComment)
    else
      val comment = getCurrentSlice.substring(2, getCurrentSlice.length - 2) // Remove /* and */
      TokenKind.Comment(comment)

  private def lexShebang(): TokenKind =
    advanceWhile { (curr, _) => curr != '\n' }
    val command = getCurrentSlice.substring(2) // Remove '#!'
    TokenKind.Shebang(command)

object Lexer:
  def lex(source: Source): Vector[Token] =
    val lexer = Lexer(source)
    lexer.toVector