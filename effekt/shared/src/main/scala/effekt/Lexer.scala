package effekt

import effekt.LexerError.{InvalidKeywordIdent, MalformedFloat}
import effekt.TokenKind.EOF

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.BufferedIterator
import scala.util.matching.Regex

/** An error encountered during lexing a source string. */
enum LexerError {
  case MalformedFloat
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
  case Str(s: String)
  case QuotedStr(ts: List[Token])
  case Char(c: Char)
  // identifiers
  case Ident(id: String)
  case IdentQualified(id: String)
  // misc
  case Comment(msg: String)
  case EOF
  case Error(err: LexerError)
  // symbols
  case `=`
  case `==`
  case `!=`
  case `:`
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
  case Let
  case True
  case False
  case Val
  case Var
  case If
  case Else
  case While
  case Type
  case Effect
  case Interface
  case Try
  case With
  case Case
  case Do
  case Fun
  case Resume
  case Match
  case Def
  case Module
  case Import
  case Export
  case Extern
  case Include
  case Record
  case At
  case In
  case Box
  case Unbox
  case Return
  case Region
  case Resource
  case New
  case And
  case Is
  case Namespace
}

object Lexer {
  import TokenKind.*

  val keywords: immutable.HashMap[String, TokenKind] = immutable.HashMap(
    "let" -> Let,
    "true" -> True,
    "false" -> False,
    "val" -> Val,
    "false" -> False,
    "val" -> Val,
    "var" -> Var,
    "if" -> If,
    "else" -> Else,
    "while" -> While,
    "type" -> Type,
    "effect" -> Effect,
    "interface" -> Interface,
    "try" -> Try,
    "with" -> With,
    "case" -> Case,
    "do" -> Do,
    "fun" -> Fun,
    "resume" -> Resume,
    "match" -> Match,
    "def" -> Def,
    "module" -> Module,
    "import" -> Import,
    "export" -> Export,
    "extern" -> Extern,
    "include" -> Include,
    "record" -> Record,
    "at" -> At,
    "in" -> In,
    "box" -> Box,
    "unbox" -> Unbox,
    "return" -> Return,
    "region" -> Region,
    "resource" -> Resource,
    "new" -> New,
    "and" -> And,
    "is" -> Is,
    "namespace" -> Namespace,
  )
}

/**
 * 
 * @param source A string of a Effekt program to be lexed.
 */
class Lexer(source: String) {
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
  /** A peekable iterator of the source string. This is used instead of directly indexing the source string. */
  val chars: BufferedIterator[Char] = source.iterator.buffered
  
  lazy val whitespace: Regex = """([ \t\r\n])""".r // single whitespace characters
  lazy val nameFirst: Regex = """[a-zA-Z_]""".r
  lazy val nameRest: Regex = """[a-zA-Z0-9_!?$]""".r
  lazy val nameBoundary: Regex = """(?!%s)""".format(nameRest).r
  lazy val name: Regex = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r
  
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
   * */  
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
      val kind = nextToken()
      kind match {
        case TokenKind.EOF =>
          eof = true
          // EOF has the position of one after the last character
          tokens += Token(current + 1, current + 1, EOF)
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

  /** Matches a string literal -- both single- and multi-line strings. Strings may contain arbitrary escaped
   * characters, these are not validated. Strings may also contain quotes, i.e., "f = ${x + 1}" that include arbitrary
   * expressions.
   */
  def matchString(): TokenKind = {
    val sliceStr = (start: Int, end: Int) => TokenKind.Str(slice(start, end))
    var endString = false
    val stringStart = start
    var multiline = false
    var quoted = false
    val stringTokens = mutable.ListBuffer[Token]()
    // each opening brace gets pushed and each closing brace pops the matching brace from the stack
    // to determine whether the quote has ended, while allowing for nested braces.
    val delimiters = mutable.Stack[Delimiter]()
    enum Delimiter {
      case `${`
      case `{{`
    }
    import Delimiter.*

    if (nextMatches("\"\"")) multiline = true
    if (multiline && nextMatches("\"\"\"") || !multiline && nextMatches("\"")) return TokenKind.Str("")

    val delimOffset =
      if (multiline) 3
      else 1
    start += delimOffset

    while (!endString) {
      consume() match {
        // escaped character, allow arbitrary next character
        case Some('\\') => consume()
        // check for termination
        case Some('"') if !multiline => endString = true
        case Some('"') if multiline && nextMatches("\"\"") => endString = true
        // quoted string
        case Some('$') if nextMatches("{") => {
          quoted = true
          // string before the quote
          if (current - 2 > start)
            stringTokens += Token(start, current - 3, sliceStr(start, current - 2))
          // set start to $
          start = current - 2
          stringTokens += makeToken(TokenKind.`${`)

          delimiters.push(`${`)
          var endQuote = false
          while (!endQuote) {
            val token = makeToken(nextToken())
            token.kind match {
              case TokenKind.`}` => {
                delimiters.pop() match {
                  // check if quote has been terminated
                  case `${` => {
                    endQuote = true
                    stringTokens += Token(token.start, token.end, TokenKind.`}$`)
                  }
                  case _ =>
                    stringTokens += token
                }
              }
              // additional nested level of braces
              case TokenKind.`{` => {
                delimiters.push(`{{`)
                stringTokens += token
              }
              // reached EOF without closing quote
              case TokenKind.EOF => return err(LexerError.UnterminatedQuote)
              case _ => stringTokens += token
            }
          }
          start = current
        }
        // anything that is not escaped or terminates the string is allowed
        case Some(_) => ()
        // reached EOF without encountering "
        case None => return err(LexerError.UnterminatedString)
      }
    }
    // add remaining string after potential quote. Check if non-empty first.
    if (current - delimOffset > start) {
      stringTokens += Token(
        start,
        current - delimOffset - 1,
        // exclude " symbols
        sliceStr(start, current - delimOffset)
      )
    }
    // reset starting location to original position of opening "
    start = stringStart
    if (quoted) TokenKind.QuotedStr(stringTokens.toList)
    else stringTokens.head.kind
  }

  /** Matches a mult-line comment delimited by /* and */. */
  def matchMultilineComment(): TokenKind = {
    var closed = false
    while (!closed) {
      consume() match {
        // end comment on */
        case Some('*') if nextMatches("/") => closed = true
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
  final def skipWhitespaces(): Unit = {
    peek() match {
      case Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
        consume()
        skipWhitespaces()
      }
      case _ => start = current
    }
  }

  def nextToken(): TokenKind = {
    import TokenKind.*
    skipWhitespaces()
    val maybeChar = consume()
    if (maybeChar.isEmpty) return EOF
    val c = maybeChar.get
    c match {
      // --- symbols & pre- and infix operations ---
      case '=' if nextMatches(">") => `=>`
      case '=' if nextMatches("=") => TokenKind.`==`
      case '=' => `=`
      case ':' => `:`
      case '@' => `@`
      case '<' if nextMatches("{") => `<{`
      case '{' => `{`
      case '}' if nextMatches(">") => `}>`
      case '}' => `}`
      case '(' => `(`
      case ')' => `)`
      case '[' => `[`
      case ']' => `]`
      case '<' if nextMatches(">") => `<>`
      case '<' if nextMatches("=") => `<=`
      case '<' => `<`
      case '>' if nextMatches("=") => `>=`
      case '>' => `>`
      case ',' => `,`
      case '.' => `.`
      case '/' if nextMatches("*") => matchMultilineComment()
      case '/' if nextMatches("/")  => matchComment()
      case '/' => `/`
      case '!' if nextMatches("=") => TokenKind.`!=`
      case '!' => `!`
      case '|' if nextMatches("|") => `||`
      case '|' => `|`
      case '&' if nextMatches("&") => `&&`
      case '&' => `&`
      case '*' => `*`
      case '+' if nextMatches("+") => `++`
      case '+' => `+`
      case '-' => `-`
      // --- literals ---
      case '\"' => matchString()
      case c if c.isDigit => matchNumber()
      // --- keywords & identifiers ---
      case c if c.isLetter => {
        // since keywords are a subclass of identifiers and we want to match either keywords or identifiers,
        // we look for valid names
        consumeWhile { c => name.matches(c.toString) }
        val s = slice()
        // check if the slice matches any know keyword, otherwise it is necessarily an identifier
        Lexer.keywords.get(s) match {
          case Some(tok) => tok
          // identifier
          case _ =>
            if (name.matches(s)) TokenKind.Ident(s)
            // cannot occur
            else err(LexerError.InvalidKeywordIdent(s))
        }
      }
      case _ => err(LexerError.InvalidKeywordIdent(c.toString))
    }
  }
}
