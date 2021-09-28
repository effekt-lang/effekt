package effekt

import effekt.context.Context
import effekt.source._
import org.bitbucket.inkytonik.kiama.parsing.{ Failure, Input, NoSuccess, ParseResult, Parsers, Success }
import org.bitbucket.inkytonik.kiama.util.{ Position, Positions, Source }

import scala.language.implicitConversions

/**
 * TODO at the moment parsing is still very slow. I tried to address this prematurily
 * by adding cuts and using PackratParser for nonterminals. Maybe moving to a separate lexer phase
 * could help remove more backtracking?
 */
class Parser(positions: Positions) extends Parsers(positions) with Phase[Source, ModuleDecl] {

  val phaseName = "parser"

  def run(source: Source)(implicit C: Context): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) =>
        Some(ast)

      case res: NoSuccess =>
        val input = res.next
        positions.setStart(res, input.position)
        positions.setFinish(res, input.nextPosition)
        C.error(res, res.message)
        None
    }

  type P[T] = PackratParser[T]

  // === Lexing ===

  lazy val nameFirst = """[a-zA-Z$_]""".r
  lazy val nameRest = """[a-zA-Z0-9$_]""".r
  lazy val name = "%s(%s)*\\b".format(nameFirst, nameRest).r
  lazy val moduleName = "%s([/]%s)*\\b".format(name, name).r

  lazy val `=` = literal("=")
  lazy val `:` = literal(":")
  lazy val `{` = literal("{")
  lazy val `}` = literal("}")
  lazy val `(` = literal("(")
  lazy val `)` = literal(")")
  lazy val `[` = literal("[")
  lazy val `]` = literal("]")
  lazy val `,` = literal(",")
  lazy val `.` = literal(".")
  lazy val `/` = literal("/")
  lazy val `=>` = literal("=>")
  lazy val `<>` = literal("<>")
  lazy val `<{` = literal("<{")
  lazy val `}>` = literal("}>")

  lazy val `handle` = keyword("handle")
  lazy val `true` = keyword("true")
  lazy val `false` = keyword("false")
  lazy val `val` = keyword("val")
  lazy val `var` = keyword("var")
  lazy val `for` = keyword("for")
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
  lazy val `extern` = keyword("extern")
  lazy val `include` = keyword("include")
  lazy val `pure` = keyword("pure")
  lazy val `record` = keyword("record")
  lazy val `at` = keyword("at")
  lazy val `box` = keyword("box")
  lazy val `unbox` = keyword("unbox")

  def keywordStrings: List[String] = List(
    "def", "val", "var", "handle", "true", "false", "else", "type",
    "effect", "try", "with", "case", "do", "if", "while",
    "match", "module", "import", "extern", "fun", "for", "interface", "at", "box", "unbox"
  )

  // we escape names that would conflict with JS early on to simplify the pipeline
  def additionalKeywords: List[String] = List(
    "delete", "new", "catch", "in", "finally", "switch", "case", "this", "yield"
  )

  def keyword(s: String): Parser[String] =
    s // todo check suffix

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9]".r, keywordStrings)

  lazy val ident =
    (not(anyKeyword) ~> name ^^ { n =>
      if (additionalKeywords.contains(n)) { "_" + n } else { n }
    }
      | failure("Expected an identifier"))

  lazy val idDef: P[IdDef] = ident ^^ IdDef
  lazy val idRef: P[IdRef] = ident ^^ IdRef

  lazy val path = someSep(ident, `/`)

  def oneof(strings: String*): Parser[String] =
    strings.map(literal).reduce(_ | _)

  /**
   * Whitespace Handling
   */
  lazy val linebreak = """(\r\n|\n)""".r
  lazy val singleline = """//[^\n]*(\n|\z)""".r
  override val whitespace = rep("""\s+""".r | singleline)

  /**
   * Automatic Semicolon Insertion
   *
   * Since Kiama already consumes whitespace:
   * the idea is to scroll back whitespaces until we find a newline
   */
  lazy val `;` = new Parser[Unit] {

    def apply(in: Input): ParseResult[Unit] = {
      val content = in.source.content
      var pos = in.offset
      val str = content.substring(pos)

      // \n   ; while
      //      ^
      if (str.startsWith(";")) {
        return Success((), Input(in.source, in.offset + 1))

        // foo }
        //     ^
      } else if (str.startsWith("}") || str.startsWith("case")) {
        return Success((), in)
      } else {
        // \n   while
        //      ^
        pos = pos - 1
        while (pos > 0 && (content.charAt(pos) == ' ' || content.charAt(pos) == '\t')) {
          pos = pos - 1
        }

        // \n   while
        //  ^
        if (content.charAt(pos) == '\n') {
          Success((), in)
        } else {
          Failure(s"Expected ;", in)
        }
      }
    }
  }

  object defaultModulePath extends Parser[String] {
    // we are purposefully not using File here since the parser needs to work both
    // on the JVM and in JavaScript
    def apply(in: Input): ParseResult[String] = {
      val filename = in.source.name
      val baseWithExt = filename.split("[\\\\/]").last
      val base = baseWithExt.split('.').head
      Success(base, in)
    }
  }

  /**
   * Numbers
   */
  lazy val digit = regex("[0-9]".r)
  lazy val decimalInt = regex("([-+])?(0|[1-9][0-9]*)".r)

  lazy val int = decimalInt ^^ { n => IntLit(n.toInt) }
  lazy val bool = `true` ^^^ BooleanLit(true) | `false` ^^^ BooleanLit(false)
  lazy val unit = literal("()") ^^^ UnitLit()
  lazy val double = regex("([-+])?(0|[1-9][0-9]*)[.]([0-9]+)".r) ^^ { n => DoubleLit(n.toDouble) }
  lazy val string = """\"([^\"]*)\"""".r ^^ {
    s => StringLit(s.substring(1, s.size - 1))
  }


  // === Lexing ===


  // === Parsing ===

  // turn scalariform formatting off!
  // format: OFF

  lazy val program: P[ModuleDecl] =
    ( moduleDecl ~ many(importDecl) ~ many(definition) ^^ {
      case name ~ imports ~ defs if name != "effekt" => ModuleDecl(name, Import("effekt") :: imports, defs)
      case name ~ imports ~ defs => ModuleDecl(name, imports, defs)
    }
    | failure("Required at least one top-level function or effect definition")
    )

  lazy val moduleDecl: P[String] =
    ( `module` ~/> moduleName
    | defaultModulePath
    )

  lazy val importDecl: P[Import] =
    `import` ~/> moduleName ^^ Import


  /**
   * For the REPL
   */
  lazy val repl: P[Tree] = definition | expr | importDecl

  /**
   * Definitions
   */
  lazy val definition: P[Def] =
    ( valDef
    | funDef
    | interfaceDef
    | dataDef
    | externType
    // | externEffect
    | externFun
    | externInclude
    | failure("Expected a definition")
    )

  lazy val funDef: P[Def] =
    `def` ~/> idDef ~ maybeTypeParams ~ maybeValueParams ~ many(blockParam) ~ (`:` ~> valueType).? ~ ( `=` ~/> stmt) ^^ FunDef

  lazy val maybePure: P[Boolean] =
    `pure`.? ^^ { _.isDefined }

  lazy val interfaceDef: P[Def] =
    `interface` ~> idDef ~ maybeTypeParams ~ (`{` ~/> many(`def` ~> operation)  <~ `}`) ^^ InterfaceDef

  lazy val operation: P[Operation] =
    idDef ~ maybeTypeParams ~ valueParams ~/ (`:` ~/> valueType) ^^ Operation

  lazy val externType: P[Def] =
    `extern` ~> `type` ~/> idDef ~ maybeTypeParams ^^ ExternType

  //  lazy val externEffect: P[Def] =
  //    `extern` ~> `effect` ~/> idDef ~ maybeTypeParams ^^ ExternEffect

  lazy val externFun: P[Def] =
    `extern` ~> (maybePure <~ `def`) ~/ idDef ~ maybeTypeParams ~ valueParams ~ (`:` ~> valueType) ~ ( `=` ~/> """\"([^\"]*)\"""".r) ^^ {
      case pure ~ id ~ tparams ~ params ~ tpe ~ body => ExternFun(pure, id, tparams, params, tpe, body.stripPrefix("\"").stripSuffix("\""))
    }

  lazy val externInclude: P[Def] =
    `extern` ~> `include` ~/> """\"([^\"]*)\"""".r ^^ { s => ExternInclude(s.stripPrefix("\"").stripSuffix("\"")) }


  /**
   * Parameters
   */
  lazy val valueParams: P[List[ValueParam]] =
    `(` ~/> manySep(valueParam, `,`) <~ `)`

  lazy val maybeValueParams: P[List[ValueParam]] =
    valueParams | success(Nil)

  lazy val blockParam: P[BlockParam] = `{` ~/> blockParamSig <~ `}`

  lazy val valueParam: P[ValueParam] =
    idDef ~ (`:` ~> valueType) ^^ { case id ~ tpe => ValueParam(id, tpe) }

  lazy val blockParamSig: P[BlockParam] =
    idDef ~ (`:` ~> blockType) ^^ BlockParam

  lazy val typeParams: P[List[Id]] =
    `[` ~/> manySep(idDef, `,`) <~ `]`

  lazy val maybeTypeParams: P[List[Id]] =
    typeParams.? ^^ { o => o getOrElse Nil }

  /**
   * Arguments
   */
  lazy val blockArg: P[BlockArg] =
    ( `{` ~> maybeTypeParams ~ valueParams ~ many(blockParam) ~  (`=>` ~/> stmts <~ `}`) ^^ FunctionArg
    | (`{` ~> idRef <~ `}`) ^^ InterfaceArg
    | (`{` ~> `unbox` ~> expr <~ `}`) ^^ UnboxArg
    )

  lazy val valueArgs: P[List[Term]] =
    `(` ~/> manySep(expr, `,`) <~ `)` | failure("Expected a value argument list")

  lazy val maybeValueArgs: P[List[Term]] =
    `(` ~/> manySep(expr, `,`) <~ `)` | success(Nil)

  lazy val typeArgs: P[List[ValueType]] =
    `[` ~/> manySep(valueType, `,`) <~ `]`

  lazy val maybeTypeArgs: P[List[ValueType]] =
    typeArgs.? ^^ { o => o.getOrElse(Nil) }

  lazy val stmt: P[Stmt] =
    ( expr ^^ Return
    | `{` ~/> stmts <~ `}` ^^ BlockStmt
    | failure("Expected a statement")
    )

  /**
   * Statements
   */
  lazy val stmts: P[Stmt] =
    ( (expr <~ `;`) ~ stmts ^^ ExprStmt
    | (definition <~ `;`) ~ stmts ^^ DefStmt
    | (varDef  <~ `;`) ~ stmts ^^ DefStmt
    | (expr <~ `;`) ^^ Return
    | matchDef
    )

  lazy val valDef: P[ValDef] =
     `val` ~> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ ValDef

  lazy val varDef: P[VarDef] =
     `var` ~/> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ VarDef

  // TODO make the scrutinee a statement
  lazy val matchDef: P[Stmt] =
     `val` ~> pattern ~ (`=` ~/> expr) ~ (`;` ~> stmts) ^^ {
       case p ~ sc ~ body =>
        Return(Match(sc, List(MatchClause(p, body)))) withPositionOf p
     }

  lazy val dataDef: P[DataDef] =
    `type` ~> idDef ~ maybeTypeParams ~ (`{` ~/> manySep(constructor, `;`) <~ `}`) ^^ DataDef

  lazy val constructor: P[Constructor] =
    idDef ~ valueParams ^^ Constructor

  /**
   * Expressions
   */
  lazy val expr:    P[Term] = matchExpr | assignExpr | orExpr | failure("Expected an expression")
  lazy val orExpr:  P[Term] = orExpr  ~ "||" ~/ andExpr ^^ binaryOp | andExpr
  lazy val andExpr: P[Term] = andExpr ~ "&&" ~/ eqExpr ^^ binaryOp | eqExpr
  lazy val eqExpr:  P[Term] = eqExpr  ~ oneof("==", "!=") ~/ relExpr ^^ binaryOp | relExpr
  lazy val relExpr: P[Term] = relExpr ~ oneof("<=", ">=", "<", ">") ~/ addExpr ^^ binaryOp | addExpr
  lazy val addExpr: P[Term] = addExpr ~ oneof("++", "+", "-") ~/ mulExpr ^^ binaryOp | mulExpr
  lazy val mulExpr: P[Term] = mulExpr ~ oneof("*", "/") ~/ callExpr ^^ binaryOp | accessExpr

  lazy val arguments = valueArgs ~ many(blockArg) | maybeValueArgs ~ some(blockArg)

  lazy val accessExpr: P[Term] =
    ( callExpr ~ some(`.` ~> idRef) ~ maybeTypeArgs ~ arguments ^^ {
        case firstTarget ~ accesses ~ targs ~ (vargs ~ bargs) =>
          val selection = accesses.foldLeft(firstTarget) {
            case (firstArg, id) => Select(firstArg, id).withPositionOf(id)
          }
          Call(selection, targs, vargs, bargs)
      }
    | callExpr
    )

  lazy val callExpr: P[Term] =
    ( ifExpr
    | whileExpr
    | funCall
    | handleExpr
    | boxExpr
    | unboxExpr
    | primExpr
    )

  lazy val boxExpr: P[Term] = `box` ~> captureSet.? ~ blockArg ^^ Box
  lazy val unboxExpr: P[Term] = `unbox` ~> expr ^^ Unbox

  lazy val funCall: P[Term] =
    funCall ~ maybeTypeArgs ~ arguments ^^ { case fun ~ targs ~ (vargs ~ bargs) => Call(fun, targs, vargs, bargs) } | primExpr

  lazy val matchExpr: P[Term] =
    (accessExpr <~ `match` ~/ `{`) ~/ (some(clause) <~ `}`) ^^ Match

  lazy val handleExpr: P[Term] =
    `try` ~/> stmt ~ some(handler) ^^ TryHandle

  lazy val handler: P[Handler] =
    ( `with` ~> blockParamSig ~ (`{` ~> some(defClause) <~ `}`) ^^ {
      case cap ~ clauses =>
        Handler(cap, clauses)
      }
    )

  lazy val defClause: P[OpClause] =
    (`def` ~/> idRef) ~ maybeTypeParams ~ valueParams ~ implicitResume ~ (`=` ~/> stmt) ^^ {
      case id ~ tparams ~ vparams ~ resume ~ body => OpClause(id, tparams, vparams, body, resume)
    }

  lazy val clause: P[MatchClause] =
    `case` ~/> pattern ~ (`=>` ~/> stmts) ^^ MatchClause

  lazy val pattern: P[MatchPattern] =
    ( "_" ^^^ IgnorePattern()
    | literals ^^ { l => LiteralPattern(l) }
    | idRef ~ (`(` ~> manySep(pattern, `,`)  <~ `)`) ^^ TagPattern
    | idDef ^^ AnyPattern
    | `(` ~> pattern ~ (`,` ~> some(pattern) <~ `)`) ^^ { case f ~ r =>
        TagPattern(IdRef(s"Tuple${r.size + 1}") withPositionOf f, f :: r)
      }
    )

  lazy val implicitResume: P[IdDef] = success(IdDef("resume"))


  lazy val assignExpr: P[Term] =
    idRef ~ (`=` ~> expr) ^^ Assign

  lazy val ifExpr: P[Term] =
    `if` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ~ (`else` ~/> stmt) ^^ If

  lazy val whileExpr: P[Term] =
    `while` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ^^ While

  lazy val primExpr: P[Term] =
    variable | literals | tupleLiteral | listLiteral | hole | `(` ~/> expr <~ `)`

  lazy val variable: P[Term] =
    idRef ^^ Var

  lazy val hole: P[Term] =
    ( `<>` ^^^ Hole(Return(UnitLit()))
    | `<{` ~> stmts <~ `}>` ^^ Hole
    )

  lazy val literals: P[Literal[_]] =
    double | int | bool | unit | string

  lazy val listLiteral: P[Term] =
    `[` ~> manySep(expr, `,`) <~ `]` ^^ { exprs => exprs.foldRight(NilTree) { ConsTree } withPositionOf exprs }

  lazy val tupleLiteral: P[Term] =
    `(` ~> expr ~ (`,` ~/> someSep(expr, `,`) <~ `)`) ^^ { case tup @ (first ~ rest) => TupleTree(first :: rest) withPositionOf tup }

  private def NilTree: Term =
    Call(Var(IdRef("Nil")), Nil, Nil, Nil)

  private def ConsTree(el: Term, rest: Term): Term =
    Call(Var(IdRef("Cons")), Nil, List(el, rest), Nil)

  private def TupleTree(args: List[Term]): Term =
    Call(Var(IdRef(s"Tuple${args.size}")), Nil, args, Nil)

  /**
   * Types and Effects
   */

  lazy val valueType: P[ValueType] =
    ( boxedType
    | `(` ~> valueType <~ `)`
    | `(` ~> valueType ~ (`,` ~/> some(valueType) <~ `)`) ^^ { case f ~ r => TupleTypeTree(f :: r) }
    | idRef ~ typeArgs ^^ ValueTypeApp
    | idRef ^^ TypeVar
    | failure("Expected a type")
    )

  // for now function types need to be parenthesized
  lazy val boxedType: P[BoxedType] = functionType ~ (`at` ~> captureSet) ^^ BoxedType

  lazy val captureSet: P[CaptureSet] = `{` ~> manySep(idRef, `,`) <~ `}` ^^ CaptureSet

  lazy val blockType: P[BlockType] =
    ( idRef ~ typeArgs ^^ BlockTypeApp
    | functionType
    | interfaceType
    )

  lazy val interfaceType: P[InterfaceType] =
    idRef ^^ InterfaceType

  lazy val valueTypeParams: P[List[ValueType]] = (`(` ~> manySep(valueType, `,`) <~ `)`)
  lazy val maybeValueTypeParams = (valueTypeParams | success(Nil))
  lazy val namedBlockType: P[(Option[IdDef], BlockType)] = `{` ~> (idDef <~ `:`).? ~ blockType <~ `}` ^^ { case name ~ tpe => (name, tpe) }

  lazy val functionType: P[FunctionType] =
    ( maybeTypeParams ~ maybeValueTypeParams ~ many(namedBlockType) ~ (`=>` ~/> valueType) ^^ FunctionType
    | valueType ~ (`=>` ~/> valueType) ^^ { case t ~ e => FunctionType(Nil, List(t), Nil, e) }
    )

  // === AST Helpers ===

  private def binaryOp(lhs: Term, op: String, rhs: Term): Term =
     Call(Var(IdRef(opName(op))), Nil, List(lhs, rhs), Nil)

  private def opName(op: String): String = op match {
    case "||" => "infixOr"
    case "&&" => "infixAnd"
    case "==" => "infixEq"
    case "!=" => "infixNeq"
    case "<"  => "infixLt"
    case ">"  => "infixGt"
    case "<=" => "infixLte"
    case ">=" => "infixGte"
    case "+"  => "infixAdd"
    case "-"  => "infixSub"
    case "*"  => "infixMul"
    case "/"  => "infixDiv"
    case "++" => "infixConcat"
  }

  private def TupleTypeTree(tps: List[ValueType]): ValueType =
    ValueTypeApp(IdRef(s"Tuple${tps.size}"), tps)

  // === Utils ===
  def many[T](p: => Parser[T]): Parser[List[T]] =
    rep(p) ^^ { _.toList }

  def some[T](p: => Parser[T]): Parser[List[T]] =
    rep1(p) ^^ { _.toList }

  def manySep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    repsep(p, sep) ^^ { _.toList }

  def someSep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    rep1sep(p, sep) ^^ { _.toList }

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
      to   <- positions.getFinish(self)
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
      case EmptyRange => sys error s"Missing position for ${t}. Cannot guess the source position from its children."
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

  override implicit def memo[T](parser : => Parser[T]) : PackratParser[T] =
    new PackratParser[T](parser.map { t =>
      checkPositions(t)
      t
    })
}
