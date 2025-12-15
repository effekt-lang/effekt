package effekt
package core

import effekt.core.Type.{PromptSymbol, ResumeSymbol}
import effekt.source.{FeatureFlag, Span}
import effekt.symbols.builtins
import effekt.util.messages.{ErrorReporter, ParseError}
import kiama.parsing.{NoSuccess, ParseResult, Parsers, Success}
import kiama.util.{Position, Range, Severities, Source, StringSource}

class Names(private var knownNames: Map[String, Id]) {
  private val Suffix = """^(.*)\$(\d+)$""".r
  private var renamed = knownNames

  def isKnown(id: Id): Boolean =
    builtins.coreBuiltinSymbolFromString(id.name.name).isDefined || knownNames.contains(id.name.name)

  def getKnown(id: Id): Option[Id] =
    builtins.coreBuiltinSymbolFromString(id.name.name).orElse(knownNames.get(id.name.name))

  def idFor(name: String): Id = {
    builtins.coreBuiltinSymbolFromString(name).getOrElse(
      renamed.getOrElse(name, {
        val id = Id(name)
        renamed = renamed.updated(name, id)
        id
      })
    )
  }
}


class EffektLexers extends Parsers {

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
  lazy val `${` = literal("${")
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
  lazy val `++` = literal("++")

  lazy val `get` = keyword("get")
  lazy val `put` = keyword("put")
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
  lazy val `resume` = keyword("resume")
  lazy val `reset` = keyword("reset")
  lazy val `shift` = keyword("shift")
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
    "at", "box", "unbox", "return", "region", "new", "resource", "and", "is", "namespace",
    "reset", "shift", "make"
  )

  def keyword(kw: String): Parser[String] =
    regex((s"$kw(?!$nameRest)").r, kw)

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9_!?$]".r, keywordStrings)

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
  lazy val integerLiteral = regex("([-+])?(0|[1-9][0-9]*)".r, s"Integer literal")
  lazy val doubleLiteral =
    regex("([-+])?(0|[1-9][0-9]*)[.]([0-9]+)([eE][+-]?[0-9]+)?".r, "Double literal")
  lazy val stringLiteral =
    regex("""\"(\\.|\\[\r?\n]|[^\r\n\"])*+\"""".r, "String literal") ^^ { s =>
      val contents = s.substring(1, s.length - 1)
      unescapeString(contents)
    }
  lazy val charLiteral = regex("""'\\\d+'""".r, "Character literal") ^^ {
    case s => Integer.parseUnsignedInt(s.stripPrefix("'\\").stripSuffix("'"))
  }
  lazy val unicodeChar = regex("""\\u\{[0-9A-Fa-f]{1,6}\}""".r, "Unicode character literal") ^^ {
    case contents =>  Integer.parseInt(contents.stripPrefix("\\u{").stripSuffix("}"), 16)
  }

  /** Inverse of PrettyPrinter.escapeString */
  private def unescapeString(s: String): String = {
    val sb = new StringBuilder
    var i  = 0

    while (i < s.length) {
      val c = s.charAt(i)
      if (c == '\\' && i + 1 < s.length) {
        s.charAt(i + 1) match {
          case '\\' => sb.append('\\'); i += 2
          case '"'  => sb.append('"');  i += 2
          case 'r'  => sb.append('\r'); i += 2
          case 't'  => sb.append('\t'); i += 2
          case 'n'  => sb.append('\n'); i += 2
          case other =>
            sb.append('\\')
            sb.append(other)
            i += 2
        }
      } else {
        sb.append(c)
        i += 1
      }
    }

    sb.toString
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

  override implicit def memo[T](parser: => Parser[T]): PackratParser[T] =
    new PackratParser[T](parser)

  def parseAll[T](p: Parser[T], input: String): ParseResult[T] =
    parseAll(p, StringSource(input, "input-string"))

  lazy val path = someSep(ident, `/`)

  def oneof(strings: String*): Parser[String] =
    strings.map(literal).reduce(_ | _)
}


class CoreParsers(names: Names) extends EffektLexers {

  def parse(source: Source)(using C: ErrorReporter): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) =>
        Some(ast)
      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range), Severities.Error))
        None
    }

  lazy val `run`  = keyword("run")
  lazy val `;`    = super.literal(";")
  lazy val `make` = keyword("make")

  /**
   * Literals
   */
  lazy val int    = integerLiteral ^^ { n => Literal(n.toLong, Type.TInt) }
  lazy val char   = charLiteral ^^ { n => Literal(n.toLong, Type.TChar) }
  lazy val bool   = `true` ^^^ Literal(true, Type.TBoolean) | `false` ^^^ Literal(false, Type.TBoolean)
  lazy val unit   = literal("()") ^^^ Literal((), Type.TUnit)
  lazy val double = doubleLiteral ^^ { n => Literal(n.toDouble, Type.TDouble) }
  lazy val string: P[Expr] = (multilineString | stringLiteral) ^^ { s => Literal(s, Type.TString) }

  /**
   * Names
   */
  lazy val id = ident ^^ { name => names.idFor(name) }

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
    | `extern` ~> (captures <~ `def`) ~ signature ~ (`=` ~> (featureFlag ~ externBodyTemplate)) ^^ {
      case captures ~ (id, tparams, cparams, vparams, bparams, result) ~ (ff ~ templ) =>
        Extern.Def(
          id, tparams, cparams, vparams, bparams, result, captures,
          ExternBody.StringExternBody(ff, templ)
        )
    })

  lazy val externBodyTemplate: P[Template[Expr]] =
    (
      (multilineString | stringLiteral) ~
        rep((`++` ~> expr) ~ (`++` ~> (multilineString | stringLiteral)))
      ) ^^ {
      case firstStr ~ pairs =>
        val strings = List.newBuilder[String]
        val args = List.newBuilder[Expr]
        strings += firstStr
        pairs.foreach { case e ~ s =>
          args += e
          strings += s
        }
        Template(strings.result(), args.result())
    }

  lazy val featureFlag: P[FeatureFlag] =
    ("else" ^^ { _ => FeatureFlag.Default(Span.missing) }
    | ident ^^ (id => FeatureFlag.NamedFeatureFlag(id, Span.missing))
    )


  lazy val externBody = multilineString | stringLiteral


  // Declarations
  // ------------
  lazy val declaration: P[Declaration] = dataDecl | interfaceDecl

  lazy val dataDecl: P[Declaration] =
    `type` ~> id ~ maybeTypeParams ~ (`{` ~/> many(constructor) <~ `}`) ^^ Declaration.Data.apply

  lazy val interfaceDecl: P[Declaration] =
    `interface` ~> id ~ maybeTypeParams ~ (`{` ~/> many(property) <~ `}`) ^^ Declaration.Interface.apply

  lazy val constructor: P[Constructor] =
    id ~ maybeTypeParams ~ valueParams ^^ { case id ~ tparams ~ params => Constructor(id, tparams, params.map(p => Field(p.id, p.tpe))) }

  lazy val property: P[Property] =
    id ~ (`:` ~> blockType) ^^ Property.apply

  // Definitions
  // -----------
  lazy val toplevel: P[Toplevel] =
  ( `val` ~> id ~ maybeTypeAnnotation ~ (`=` ~/> stmt) ^^ {
    case (name ~ tpe ~ binding) => Toplevel.Val(name, binding)
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
    | `return` ~> expr ^^ Stmt.Return.apply
    | `reset` ~> blockLit ^^ Stmt.Reset.apply
    | `shift` ~> maybeParens(blockVar) ~ (`{` ~> blockParam) ~ (`=>` ~/> stmts <~ `}`) ^^ {
        case p ~ k ~ body => Stmt.Shift(p, k, body)
      }
    | `resume` ~> maybeParens(blockVar) ~ stmt ^^ Stmt.Resume.apply
    | block ~ (`.` ~> id ~ (`:` ~> blockType)).? ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ {
      case recv ~ Some(method ~ tpe) ~ targs ~ vargs ~ bargs => Invoke(recv, method, tpe, targs, vargs, bargs)
      case recv ~ None ~ targs ~ vargs ~ bargs => App(recv, targs, vargs, bargs)
    }
    | (`if` ~> `(` ~/> expr <~ `)`) ~ stmt ~ (`else` ~> stmt) ^^ Stmt.If.apply
    | `region` ~> blockLit ^^ Stmt.Region.apply
    | `<>` ~> (`:` ~> valueType) ~ (`@` ~> (stringLiteral <~ `:`) ~ (integerLiteral <~ `:`) ~ integerLiteral) ^^ {
      case (tpe ~ (name ~ from ~ to)) =>
        val source = if (name.startsWith("file://")) {
          kiama.util.FileSource(name.stripPrefix("file://"))
        } else if (name.startsWith("string://")) {
          kiama.util.StringSource("", name.stripPrefix("string://"))
        } else {
          sys error s"Unsupported source scheme in hole source name: $name"
        }
        Hole(tpe, effekt.source.Span(source, from.toInt, to.toInt))
      }
    | `<>` ~> (`:` ~> valueType) ^^ { tpe => Hole(tpe, effekt.source.Span.missing) }
    | (expr <~ `match`) ~/ (`[` ~> valueType <~ `]`) ~ (`{` ~> many(clause) <~ `}`) ~ (`else` ~> stmt).? ^^ Stmt.Match.apply
    )

  lazy val stmts: P[Stmt] =
    ( (`let` ~ `!` ~/> id) ~ (`=` ~/> maybeParens(blockVar)) ~ maybeTypeArgs ~ valueArgs ~ blockArgs ~ stmts ^^ {
        case (name ~ callee ~ targs ~ vargs ~ bargs ~ body) =>
          ImpureApp(name, callee, targs, vargs, bargs, body)
      }
    | `let` ~/> id ~ (`=` ~/> expr) ~ stmts ^^ {
      case (name ~ binding ~ body) =>
        Let(name, binding, body)
    }
      | `get` ~> id ~ (`:` ~> valueType) ~ (`=` ~> `!` ~> id) ~ (`@` ~> id) ~ (`;` ~> stmts) ^^ {
      case name ~ tpe ~ ref ~ cap ~ body => Get(name, tpe, ref, Set(cap), body)
    }
      | `put` ~> id ~ (`@` ~> id) ~ (`=` ~> expr) ~ (`;` ~> stmts) ^^ {
      case ref ~ capt ~ value ~ body => Put(ref, Set(capt), value, body)
    }
      | `def` ~> id ~ (`=` ~/> block) ~ stmts ^^ Stmt.Def.apply
      | `def` ~> id ~ parameters ~ (`=` ~/> stmt) ~ stmts ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ body ~ rest =>
        Stmt.Def(name, BlockLit(tparams, cparams, vparams, bparams, body), rest)
    }
      | `val` ~> id ~ maybeTypeAnnotation ~ (`=` ~> stmt) ~ (`;` ~> stmts) ^^ {
      case id ~ tpe ~ binding ~ body => Val(id, binding, body)
    }
      | `var` ~> id ~ (`in` ~> id) ~ (`=` ~> expr) ~ (`;` ~> stmts) ^^ { case id ~ region ~ init ~ body => Alloc(id, init, region, body) }
      | `var` ~> id ~ (`@` ~> id) ~ (`=` ~> expr) ~ (`;` ~> stmts) ^^ { case id ~ cap ~ init ~ body => Var(id, init, cap, body) }
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
  lazy val expr: P[Expr] =
    ( literal
    | `box` ~> captures ~ block ^^ { case capt ~ block => Expr.Box(block, capt) }
    | `make` ~> dataType ~ id ~ maybeTypeArgs ~ valueArgs ^^ Expr.Make.apply
    | id ~ (`:` ~> valueType) ^^ Expr.ValueVar.apply
    | maybeParens(blockVar) ~ maybeTypeArgs ~ valueArgs ^^ Expr.PureApp.apply
    | failure("Expected a pure expression.")
    )

  lazy val literal: P[Expr] = double | int | char | bool | string | unit


  // Calls
  // -----
  lazy val valueArgs: P[List[Expr]] =
    `(` ~> manySep(expr, `,`) <~ `)`

  lazy val blockArgs: P[List[Block]] =
    many(blockLit | `{` ~> block <~ `}`)

  def maybeParens[T](p: P[T]): P[T] = (p | `(` ~> p <~ `)`)


  // Blocks
  // ------
  lazy val block: P[Block] =
    ( blockVar
    | `unbox` ~> expr ^^ Block.Unbox.apply
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
    `{` ~> (id <~ `:`) ~ blockType <~ `}` ^^ { case id ~ tpe => id -> tpe }

  lazy val interfaceType: P[BlockType.Interface] =
    (
      id ~ maybeTypeArgs ^^ {
        case id ~ targs => BlockType.Interface(id, targs): BlockType.Interface
      }
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
    object parsers extends CoreParsers(names)
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
