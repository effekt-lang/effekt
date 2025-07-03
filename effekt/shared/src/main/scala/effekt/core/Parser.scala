package effekt
package core

import effekt.context.Context
import effekt.core.ValueParam
import effekt.util.VirtualSource
import effekt.source.{FeatureFlag, Span}
import effekt.util.messages.{DebugMessaging, ErrorReporter, ParseError}
import kiama.parsing.{Failure, Input, NoSuccess, ParseResult, Success, Parsers}

import kiama.util.{Position, Positions, Range, Source, StringSource}
import effekt.util.messages.ParseError

import scala.util.matching.Regex

class Names(var knownNames: Map[String, Id]) {
  def idFor(name: String): Id = knownNames.getOrElse(name, {
    val id = Id(name)
    knownNames = knownNames.updated(name, id)
    id
  })
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


class CoreParsers(positions: Positions, names: Names) extends EffektLexers(positions) {

  def parse(source: Source)(using C: ErrorReporter): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) =>
        Some(ast)
      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range)))
        None
    }

  lazy val `run`  = keyword("run")
  lazy val `;`    = super.literal(";")
  lazy val `make` = keyword("make")

  /**
   * Literals
   */
  lazy val int    = integerLiteral ^^ { n => Literal(n.toInt, Type.TInt) }
  lazy val bool   = `true` ^^^ Literal(true, Type.TBoolean) | `false` ^^^ Literal(false, Type.TBoolean)
  lazy val unit   = literal("()") ^^^ Literal((), Type.TUnit)
  lazy val double = doubleLiteral ^^ { n => Literal(n.toDouble, Type.TDouble) }
  lazy val string = stringLiteral ^^ { s => Literal(s.substring(1, s.size - 1), Type.TString) }

  /**
   * Names
   */
  lazy val id = ident ^^ { name => names.idFor(name) }
  lazy val wildcard = success(names.idFor("_"))

  /**
   * Main Entry
   */
  lazy val program: P[ModuleDecl] =
    `module` ~/> moduleName ~
      many(includeDecl) ~
      many(declaration) ~
      many(externDecl) ~
      many(toplevel) ~
      many(exportDecl) ^^ ModuleDecl.apply

  lazy val includeDecl: P[String] =
    `import` ~/> moduleName


  // Externs
  // -------
  lazy val externDecl: P[Extern] =
    ( `extern` ~> featureFlag ~ externBody ^^ Extern.Include.apply
    | `extern` ~> (captures <~ `def`) ~ signature ~ (`=` ~> (featureFlag ~ externBody)) ^^ {
      case captures ~ (id, tparams, cparams, vparams, bparams, result) ~ body =>
        Extern.Def(id, tparams, cparams, vparams, bparams, result, captures, body match {
          case ff ~ (body: String) =>
            ExternBody.StringExternBody(ff, Template(List(body), Nil))
        })
    })

  lazy val featureFlag: P[FeatureFlag] =
    ("else" ^^ { _ => FeatureFlag.Default(Span.missing) }
    | ident ^^ (id => FeatureFlag.NamedFeatureFlag(id, Span.missing))
    )


  lazy val externBody = stringLiteral | multilineString


  // Declarations
  // ------------
  lazy val declaration: P[Declaration] = dataDecl | interfaceDecl

  lazy val dataDecl: P[Declaration] =
    `type` ~> id ~ maybeTypeParams ~ (`{` ~/> many(constructor) <~ `}`) ^^ Declaration.Data.apply

  lazy val interfaceDecl: P[Declaration] =
    `interface` ~> id ~ maybeTypeParams ~ (`{` ~/> many(property) <~ `}`) ^^ Declaration.Interface.apply

  lazy val constructor: P[Constructor] =
    id ~ valueParams ^^ { case id ~ params => Constructor(id, params.map(p => Field(p.id, p.tpe))) }

  lazy val property: P[Property] =
    id ~ (`:` ~> blockType) ^^ Property.apply

  // Definitions
  // -----------
  lazy val toplevel: P[Toplevel] =
  ( `val` ~> id ~ maybeTypeAnnotation ~ (`=` ~/> stmt) ^^ {
    case (name ~ tpe ~ binding) => Toplevel.Val(name, tpe.getOrElse(binding.tpe), binding)
  }
  | `def` ~> id ~ (`=` ~/> block) ^^ Toplevel.Def.apply
  | `def` ~> id ~ parameters ~ (`=` ~> stmt) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ body =>
        Toplevel.Def(name, BlockLit(tparams, cparams, vparams, bparams, body))
    }
  | failure("Expected a definition.")
  )


  // Statements
  // ----------
  lazy val stmt: P[Stmt] =
    ( `{` ~/> stmts <~ `}`
    | `return` ~> pure ^^ Stmt.Return.apply
    | block ~ (`.` ~> id ~ (`:` ~> blockType)).? ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ {
        case (recv ~ Some(method ~ tpe) ~ targs ~ vargs ~ bargs) => Invoke(recv, method, tpe, targs, vargs, bargs)
        case (recv ~ None ~ targs ~ vargs ~ bargs) => App(recv, targs, vargs, bargs)
      }
    | (`if` ~> `(` ~/> pure <~ `)`) ~ stmt ~ (`else` ~> stmt) ^^ Stmt.If.apply
    | `region` ~> blockLit ^^ Stmt.Region.apply
    | `<>` ^^^ Hole()
    | (pure <~ `match`) ~/ (`{` ~> many(clause) <~ `}`) ~ (`else` ~> stmt).? ^^ Stmt.Match.apply
    )

  lazy val stmts: P[Stmt] =
    ( `let` ~/> id ~ maybeTypeAnnotation ~ (`=` ~/> expr) ~ stmts ^^ {
        case (name ~ tpe ~ binding ~ body) => Let(name, tpe.getOrElse(binding.tpe), binding, body)
      }
    | `def` ~> id ~ (`=` ~/> block) ~ stmts ^^ Stmt.Def.apply
    | `def` ~> id ~ parameters ~ (`=` ~/> stmt) ~ stmts ^^ {
        case name ~ (tparams, cparams, vparams, bparams) ~ body ~ rest =>
          Stmt.Def(name, BlockLit(tparams, cparams, vparams, bparams, body), rest)
      }
    | `val` ~> id ~ maybeTypeAnnotation ~ (`=` ~> stmt) ~ (`;` ~> stmts) ^^ {
        case id ~ tpe ~ binding ~ body => Val(id, tpe.getOrElse(binding.tpe), binding, body)
      }
    | `var` ~> id ~ (`in` ~> id) ~ (`=` ~> pure) ~ (`;` ~> stmts) ^^ { case id ~ region ~ init ~ body => Alloc(id, init, region, body) }
    | `var` ~> id ~ (`@` ~> id) ~ (`=` ~> pure) ~ (`;` ~> stmts) ^^ { case id ~ cap ~ init ~ body => Var(id, init, cap, body) }
    | stmt
    )

  lazy val clause: P[(Id, BlockLit)] = (id <~ `:`) ~ blockLit ^^ { case id ~ cl => id -> cl }

  // Implementations
  // ---------------
  lazy val implementation: P[Implementation] =
    interfaceType ~ (`{` ~/> many(operation) <~ `}`) ^^ Implementation.apply

  lazy val operation: P[Operation] =
    `def` ~/> id ~ parameters ~ (`=` ~/> stmt) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ body =>
        Operation(name, tparams, cparams, vparams, bparams, body)
    }


  // Pure Expressions
  // ----------------
  lazy val pure: P[Pure] =
    ( literal
    | id ~ (`:` ~> valueType) ^^ Pure.ValueVar.apply
    | `box` ~> captures ~ block ^^ { case capt ~ block => Pure.Box(block, capt) }
    | `make` ~> dataType ~ id ~ maybeTypeArgs ~ valueArgs ^^ Pure.Make.apply
    | maybeParens(blockVar) ~ maybeTypeArgs ~ valueArgs ^^ Pure.PureApp.apply
    | failure("Expected a pure expression.")
    )

  lazy val literal: P[Pure] = int | bool | string | unit | double


  // Calls
  // -----
  lazy val valueArgs: P[List[Pure]] =
    `(` ~> manySep(pure, `,`) <~ `)`

  lazy val blockArgs: P[List[Block]] =
    many(blockLit | `{` ~> block <~ `}`)


  // Expressions
  // -----------
  lazy val expr: P[Expr] =
    ( pure
    | (`!` ~/> maybeParens(blockVar)) ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ DirectApp.apply
    )

  def maybeParens[T](p: P[T]): P[T] = (p | `(` ~> p <~ `)`)


  // Blocks
  // ------
  lazy val block: P[Block] =
    ( blockVar
    | `unbox` ~> pure ^^ Block.Unbox.apply
    | `new` ~> implementation ^^ Block.New.apply
    | blockLit
    // TODO check left associative nesting (also for select)
    | `(` ~> block <~ `)`
    )

  lazy val blockVar: P[Block.BlockVar] =
    id ~ (`:` ~> blockType) ~ (`@` ~> captures) ^^ {
      case id ~ tpe ~ capt => Block.BlockVar(id, tpe, capt) : Block.BlockVar
    }

  lazy val blockLit: P[Block.BlockLit] =
    `{` ~> parameters ~ (`=>` ~/> stmts) <~ `}` ^^ {
      case (tparams, cparams, vparams, bparams) ~ body =>
        Block.BlockLit(tparams, cparams, vparams, bparams, body) : Block.BlockLit
      }

  // Exports
  // -------
  lazy val exportDecl: P[Id] = `export` ~> id


  // Signatures
  // -------
  // foo[Int, String](x: Int) { f@f2: Exc }: Int
  lazy val signature: P[(Id, List[Id], List[Id], List[ValueParam], List[BlockParam], ValueType)] =
    id ~ parameters ~ (`:` ~> valueType) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ result =>
        (name, tparams, cparams, vparams, bparams, result)
    }

  lazy val parameters: P[(List[Id], List[Id], List[ValueParam], List[BlockParam])] =
    maybeTypeParams ~ valueParams ~ many(trackedBlockParam) ^^ {
      case tparams ~ vparams ~ bcparams =>
        val (cparams, bparams) = bcparams.unzip
        (tparams, cparams, vparams, bparams)
    }

  lazy val typeParams: P[List[Id]] = `[` ~> someSep(typeParam, `,`) <~ `]`
  lazy val maybeTypeParams: P[List[Id]] = typeParams | success(Nil)
  lazy val typeParam: P[Id] = `'` ~> id


  lazy val valueParam: P[ValueParam] =
    id ~ (`:` ~> valueType) ^^ { case id ~ tpe => ValueParam(id, tpe) }

  lazy val valueParams: P[List[ValueParam]] =
    `(` ~> manySep(valueParam, `,`) <~ `)`

  // f@f2 : Exc
  lazy val trackedBlockParam: P[(Id, BlockParam)] =
    ( `{` ~> id ~ (`@` ~> id)  ~ (`:` ~> blockType) <~ `}` ^^ {
        case id ~ capt ~ tpe => capt -> BlockParam(id, tpe, Set(capt))
      }
    // abbreviation: f : Exc .= f@f : Exc
    | blockParam ^^ { p => p.id -> p }
    )

  lazy val blockParam: P[BlockParam] =
    `{` ~> id ~ (`:` ~> blockType) <~ `}` ^^ { case id ~ tpe => BlockParam(id, tpe, Set(id)) }


  // Types
  // -----
  lazy val captures: P[Captures] = `{` ~> manySep(id, `,`) <~ `}` ^^ { ids => ids.toSet }

  lazy val valueType: P[ValueType] =
    ( nocut(blockType) ~ (`at` ~/> captures) ^^ ValueType.Boxed.apply
    | primValueType
    )

  lazy val primValueType: P[ValueType] =
    ( typeParam ^^ ValueType.Var.apply
    | dataType
    | `(` ~> valueType <~ `)`
    | failure("Expected a value type")
    )

  lazy val dataType: P[ValueType.Data] =
    id ~ maybeTypeArgs ^^ { case id ~ targs => ValueType.Data(id, targs) : ValueType.Data }

  lazy val blockType: P[BlockType] =
    ( maybeTypeParams ~ maybeValueTypes ~ many(blockTypeParam) ~ (`=>` ~/> primValueType) ^^ {
      case tparams ~ vparams ~ bcparams ~ result =>
        val (cparams, bparams) = bcparams.unzip
        BlockType.Function(tparams, cparams, vparams, bparams, result)
      }
    | interfaceType
    )


  lazy val maybeValueTypes: P[List[ValueType]] =
    ( `(` ~> manySep(valueType, `,`) <~ `)`
    | success(Nil)
    )

  lazy val maybeTypeAnnotation: P[Option[ValueType]] =
    (`:` ~> valueType).?

  // { f : S }
  // abbreviation { S } .= { _: S }
  lazy val blockTypeParam: P[(Id, BlockType)] =
    `{` ~> (id <~ `:` | wildcard) ~ blockType <~ `}` ^^ { case id ~ tpe => id -> tpe }

  lazy val interfaceType: P[BlockType.Interface] =
    ( id ~ maybeTypeArgs ^^ { case id ~ tpe => BlockType.Interface(id, tpe) : BlockType.Interface }
    | failure("Expected an interface")
    )

  lazy val typeArgs: P[List[ValueType]] =
    `[` ~/> manySep(valueType, `,`) <~ `]`

  lazy val maybeTypeArgs: P[List[ValueType]] =
    typeArgs.? ^^ { o => o.getOrElse(Nil) }
}

object CoreParsers {

  def apply(names: Map[String, Id]): CoreParsers = apply(Names(names))

  def apply(names: Names): CoreParsers =
    object pos extends Positions
    object parsers extends CoreParsers(pos, names)
    parsers

  // Some alternative main entry points for most common usages
  def module(input: String, names: Map[String, Id] = Map.empty): ParseResult[ModuleDecl] =
    val in = StringSource(input, "input-string")
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.program, in)

  def module(input: String, names: Names): ParseResult[ModuleDecl] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.program, input)

  def statement(input: String, names: Names): ParseResult[Stmt] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.stmt, input)

  def definition(input: String, names: Names): ParseResult[Toplevel] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.toplevel, input)
}
