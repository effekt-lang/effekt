package effekt

import effekt.LexerError.{InvalidKeywordIdent, MalformedFloat}
import effekt.TokenKind.EOF

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.BufferedIterator
import scala.util.matching.Regex

enum LexerError {
  case MalformedFloat
  case InvalidKeywordIdent(s: String)
  case UnterminatedString
  case UnterminatedComment
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

case class Token(start: Int, end: Int, kind: TokenKind)

enum TokenKind {
  // literals
  case Integer(n: Int)
  case Float(d: Double)
  case Str(s: String)
  case Char(c: Char)
  // identifiers
  case Ident(id: String)
  case IdentQualified(id: String)
  // misc
  case Comment(msg: String)
  case EOF
  case Whitespace
  case Error(err: LexerError)
  // symbols
  case `=`
  case `==`
  case `!=`
  case `:`
  case `@`
  case `{`
  case `}`
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

class Lexer(source: String) {
  var start: Int = 0
  var current: Int = 0
  val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  val chars: BufferedIterator[Char] = source.iterator.buffered
  
  lazy val whitespace: Regex = """([ \t\r\n])""".r // single whitespace characters
  lazy val nameFirst: Regex = """[a-zA-Z_]""".r
  lazy val nameRest: Regex = """[a-zA-Z0-9_!?$]""".r
  lazy val nameBoundary: Regex = """(?!%s)""".format(nameRest).r
  lazy val name: Regex = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r
  
  def slice(): String =
    source.substring(start, current)
  
  def isEOF: Boolean =
    current >= source.length
    
  def consume(): Option[Char] = {
    if (!chars.hasNext) {
      None
    } else {
      current += 1
      Some(chars.next())
    }
  }
  
  def expect(c: Char, msg: String): Either[LexerError, Unit] = {
    val copt = consume()
    copt match {
      case Some(c1) if c == c1 => Right(())
      case None => Left(LexerError.Eof)
      case _ => Left(LexerError.Custom(msg))
    }
    
  }
  
  @tailrec
  final def consumeWhile(pred: Char => Boolean): Either[LexerError, Unit] =
    peek() match {
      case Some(c) if pred(c) => consume(); consumeWhile(pred)
      case Some(_) => Right(())
      case _ => Left(LexerError.Eof)
    }

  def peek(): Option[Char] = chars.headOption
  
  def peekMatches(pred: Char => Boolean): Boolean = peek().exists(pred)
  
  def makeToken(kind: TokenKind): Token =
    Token(start, current - 1, kind)
    
  def err(err: LexerError): TokenKind =
    TokenKind.Error(err)
    
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
  
  def slice(start: Int = start, end: Int = current): String =
    source.substring(start, end)
  
  def nextMatches(expected: String): Boolean = matches(expected, current)
  
  def matchesRegex(r: Regex): Boolean = {
    val rest = source.substring(start)
    val candidate = rest.takeWhile { c => !whitespace.matches(c.toString) }
    r.matches(candidate)
  }
  
  def run(): mutable.ListBuffer[Token] = {
    var err = false
    var eof = false
    while (!eof && !err) {
      val kind = nextToken()
      kind match {
        case TokenKind.Whitespace =>
          ()
        case TokenKind.EOF =>
          eof = true
          tokens += makeToken(EOF)
        case TokenKind.Error(e) => 
          err = true
          println(e) 
        case k => 
          tokens += makeToken(k)
      }
      start = current
    }
    tokens
  }
  
  def matchIdentifier(): TokenKind = ???
  
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
  
  def matchString(): TokenKind = {
    var closed = false
    var multiline = false
    
    if (nextMatches("\"\"")) multiline = true

    if (!multiline) {
      if (nextMatches("\"")) return TokenKind.Str("")
      while (!closed) {
        consume() match {
          case Some('\\') => consume()
          case Some('"') => closed = true
          case Some(c) => ()
          case None => return err(LexerError.UnterminatedString)
        }
      }
      TokenKind.Str(slice(start + 1, current - 1))
    } else {
      if (nextMatches("\"\"")) return TokenKind.Str("")
      while (!closed) {
        consume() match {
          case Some('"') if nextMatches("\"\"") => closed = true
          case Some(_) => ()
          case None => return err(LexerError.UnterminatedString)
        }
      }
      TokenKind.Str(slice(start + 3, current - 3))
    }
  }
  
  def matchMultilineComment(): TokenKind = {
    var closed = false
    while (!closed) {
      consume() match {
        case Some('*') if nextMatches("/") => closed = true
        case Some(_) => ()
        case None => return err(LexerError.UnterminatedComment)
      }
    }
    val comment = slice(start + 2, current - 2)
    TokenKind.Comment(comment)
  }
  
  def matchComment(): TokenKind = {
    var newline = false
    while (!newline) {
      consume() match {
        case Some('\n') => newline = true
        case _ => ()
      }
    }
    val comment = slice(start + 2, current - 1)
    TokenKind.Comment(comment)
  }
  
  def nextToken(): TokenKind = {
    import TokenKind.*
    val maybeChar = consume()
    if (maybeChar.isEmpty) return EOF
    val c = maybeChar.get
    c match {
      case c if whitespace.matches(c.toString) => Whitespace
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
      
      // literals
      case '\"' => matchString()
      case c if c.isDigit => matchNumber()
      // keywords
      case c if c.isLetter => {
        consumeWhile { c => name.matches(c.toString) }
        val s = slice()
        Lexer.keywords.get(s) match {
          case Some(tok) => tok
          // identifier
          case _ => 
            if (name.matches(s)) TokenKind.Ident(s)
            else err(LexerError.InvalidKeywordIdent(""))
        }
      }
      case _ => err(LexerError.InvalidKeywordIdent(c.toString))
    }
  }
}
