package effekt

import effekt.context.Context
import effekt.source.*
import effekt.util.{ VirtualSource }
import effekt.util.messages.ParseError
import kiama.parsing.{ Failure, Input, NoSuccess, ParseResult, Parsers, Success }
import kiama.util.{ Position, Positions, Range, Source, StringSource }

import scala.language.implicitConversions

/**
 * String templates containing unquotes `${... : T}`
 */
case class Template[+T](strings: List[String], args: List[T]) {
  def map[R](f: T => R): Template[R] = Template(strings, args.map(f))
}

object Parser extends Phase[Source, Parsed] {

  val phaseName = "parser"

  def run(source: Source)(implicit C: Context): Option[PhaseResult.Parsed] = source match {
    case VirtualSource(decl, _) => Some(decl)
    case source =>
      //println(s"parsing ${source.name}")
      Context.timed(phaseName, source.name) {
        val lexer = effekt.lexer.Lexer(source)
        val tokens = lexer.lex()
        val parser = RecursiveDescent(C.positions, tokens, source)
        parser.parse(Input(source, 0))
      }
  } map { tree =>
    Parsed(source, tree)
  }
}


class EffektLexers(positions: Positions) extends Parsers(positions) {

  type P[T] = PackratParser[T]

  // === Lexing ===

  lazy val nameFirst = """[a-zA-Z_]""".r
  lazy val nameRest = """[a-zA-Z0-9_!?$]""".r
  lazy val nameBoundary = """(?!%s)""".format(nameRest).r
  lazy val name = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r
  lazy val moduleName = "%s([/]%s)*%s".format(name, name, nameBoundary).r
  lazy val qualifiedName = "%s(::%s)*%s".format(name, name, nameBoundary).r

  lazy val ident =
    (not(anyKeyword) ~> name
    | failure("Expected an identifier")
    )

  lazy val identRef =
    (not(anyKeyword) ~> qualifiedName
    | failure("Expected an identifier")
    )

  lazy val `=` = literal("=")
  lazy val `:` = literal(":")
  lazy val `@` = literal("@")
  lazy val `{` = literal("{")
  lazy val `}` = literal("}")
  lazy val `(` = literal("(")
  lazy val `)` = literal(")")
  lazy val `[` = literal("[")
  lazy val `]` = literal("]")
  lazy val `,` = literal(",")
  lazy val `'` = literal("'")
  lazy val `.` = literal(".")
  lazy val `/` = literal("/")
  lazy val `=>` = literal("=>")
  lazy val `<>` = literal("<>")
  lazy val `<{` = literal("<{")
  lazy val `}>` = literal("}>")
  lazy val `!` = literal("!")
  lazy val `|` = literal("|")

  lazy val `let` = keyword("let")
  lazy val `true` = keyword("true")
  lazy val `false` = keyword("false")
  lazy val `val` = keyword("val")
  lazy val `var` = keyword("var")
  lazy val `if` = keyword("if")
  lazy val `else` = keyword("else")
  lazy val `while` = keyword("while")
  lazy val `type` = keyword("type")
  lazy val `effect` = keyword("effect")
  lazy val `interface` = keyword("interface")
  lazy val `try` = keyword("try")
  lazy val `with` = keyword("with")
  lazy val `case` = keyword("case")
  lazy val `do` = keyword("do")
  lazy val `fun` = keyword("fun")
  lazy val `resume` = keyword("resume")
  lazy val `match` = keyword("match")
  lazy val `def` = keyword("def")
  lazy val `module` = keyword("module")
  lazy val `import` = keyword("import")
  lazy val `export` = keyword("export")
  lazy val `extern` = keyword("extern")
  lazy val `include` = keyword("include")
  lazy val `record` = keyword("record")
  lazy val `at` = keyword("at")
  lazy val `in` = keyword("in")
  lazy val `box` = keyword("box")
  lazy val `unbox` = keyword("unbox")
  lazy val `return` = keyword("return")
  lazy val `region` = keyword("region")
  lazy val `resource` = keyword("resource")
  lazy val `new` = keyword("new")
  lazy val `and` = keyword("and")
  lazy val `is` = keyword("is")
  lazy val `namespace` = keyword("namespace")

  def keywordStrings: List[String] = List(
    "def", "let", "val", "var", "true", "false", "else", "type",
    "effect", "interface", "try", "with", "case", "do", "if", "while",
    "match", "module", "import", "extern", "fun",
    "at", "box", "unbox", "return", "region", "new", "resource", "and", "is", "namespace"
  )

  def keyword(kw: String): Parser[String] =
    regex((s"$kw(?!$nameRest)").r, kw)

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9]".r, keywordStrings)

  /**
   * Whitespace Handling
   */
  lazy val linebreak      = """(\r\n|\n)""".r
  lazy val singleline     = """//[^\n]*(\n|\z)""".r
  lazy val multiline      = """/\*[^*]*\*+(?:[^/*][^*]*\*+)*/""".r
  lazy val simplespace    = """\s+""".r

  override val whitespace = rep(simplespace | singleline | multiline | failure("Expected whitespace"))

  /**
   * Literals
   */
  lazy val integerLiteral  = regex("([-+])?(0|[1-9][0-9]*)".r, s"Integer literal")
  lazy val doubleLiteral   = regex("([-+])?(0|[1-9][0-9]*)[.]([0-9]+)".r, "Double literal")
  lazy val stringLiteral   = regex("""\"(\\.|\\[\r?\n]|[^\r\n\"])*+\"""".r, "String literal")
  lazy val charLiteral   = regex("""'.'""".r, "Character literal") ^^ { s => s.codePointAt(1) }
  lazy val unicodeChar   = regex("""\\u\{[0-9A-Fa-f]{1,6}\}""".r, "Unicode character literal") ^^ {
    case contents =>  Integer.parseInt(contents.stripPrefix("\\u{").stripSuffix("}"), 16)
  }

  // Delimiter for multiline strings
  val multi = "\"\"\""

  // multi-line strings `(?s)` sets multi-line mode.
  lazy val multilineString: P[String] = regex(s"(?s)${ multi }[\t\n\r ]*(.*?)[\t\n\r ]*${ multi }".r) ^^ {
    contents => contents.strip().stripPrefix(multi).stripSuffix(multi)
  }

  // === Utils ===
  def many[T](p: => Parser[T]): Parser[List[T]] =
    rep(p) ^^ { _.toList }

  def some[T](p: => Parser[T]): Parser[List[T]] =
    rep1(p) ^^ { _.toList }

  def manySep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    repsep(p, sep) ^^ { _.toList }

  def someSep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    rep1sep(p, sep) ^^ { _.toList }

  extension [T] (p: Parser[T]) {
    def !!(errorMessage: T => String): Parser[Nothing] =
      p.flatMap(t => error(errorMessage(t)))

    def !!!(errorMessage: String): Parser[Nothing] =
      p.flatMap(_ => error(errorMessage))
  }

  implicit class PositionOps[T](val self: T) {
    def withPositionOf(other: Any): self.type = { dupAll(other, self); self }

    private def dupIfEmpty(from: Any, to: Any): Unit =
      if (positions.getStart(to).isEmpty) { positions.dupPos(from, to) }

    private def dupAll(from: Any, to: Any): Unit = to match {
      case t: Tree =>
        dupIfEmpty(from, t)
        t.productIterator.foreach { dupAll(from, _) }
      case t: Iterable[t] => t.foreach { dupAll(from, _) }
      case _ => ()
    }

    def range: Option[Range] = for {
      from <- positions.getStart(self)
      to <- positions.getFinish(self)
    } yield SourceRange(from, to)
  }

  trait Range {
    def ++(other: Range): Range
  }

  case object EmptyRange extends Range {
    def ++(other: Range) = other
  }

  case class SourceRange(val from: Position, val to: Position) extends Range {
    // computes the envelope containing both ranges
    def ++(other: Range) = other match {
      case EmptyRange => this
      case SourceRange(from2, to2) =>
        SourceRange(if (from2 < from) from2 else from, if (to < to2) to2 else to)
    }
  }

  /**
   * Check positions of all subtrees, stopping at trees that already have positions
   */
  def checkPosition(t: Tree): Range = t.range.getOrElse {
    t.productIterator.map(checkPositions).fold(EmptyRange)(_ ++ _) match {
      case EmptyRange => sys error s"Missing position for ${ t }. Cannot guess the source position from its children."
      case rng @ SourceRange(from, to) =>
        positions.setStart(t, from)
        positions.setFinish(t, to)
        rng
    }
  }

  def checkPositions(t: Any): Range = t match {
    case t: Tree => checkPosition(t)
    case t: Iterable[t] => t.map(checkPositions).fold(EmptyRange)(_ ++ _)
    case _ => EmptyRange
  }

  override implicit def memo[T](parser: => Parser[T]): PackratParser[T] =
    new PackratParser[T](parser.map { t =>
      checkPositions(t)
      t
    })

  def parseAll[T](p: Parser[T], input: String): ParseResult[T] =
    parseAll(p, StringSource(input, "input-string"))

  lazy val path = someSep(ident, `/`)

  def oneof(strings: String*): Parser[String] =
    strings.map(literal).reduce(_ | _)
}
