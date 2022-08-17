package effekt

import effekt.context.Context
import effekt.source.*
import effekt.util.{ SourceTask, VirtualSource }
import effekt.util.messages.ParseError
import kiama.parsing.{ Failure, Input, NoSuccess, ParseResult, Parsers, Success }
import kiama.util.{ Position, Positions, Range, Source }

import scala.language.implicitConversions

object Parser extends Phase[Source, Parsed] {

  val phaseName = "parser"

  /**
   * Note: The parser needs to be created freshly since otherwise the memo tables will maintain wrong results for
   * new input sources. Even though the contents differ, two sources are considered equal since only the paths are
   * compared.
   */
  def parser(implicit C: Context) = new EffektParsers(C.positions)

  def run(source: Source)(implicit C: Context) = source match {
    case VirtualSource(decl, _) => Some(decl)
    case source =>
      //println(s"parsing ${source.name}")
      parser.parse(source)
  } map { tree => Parsed(source, tree) }
}

/**
 * TODO at the moment parsing is still very slow. I tried to address this prematurily
 * by adding cuts and using PackratParser for nonterminals. Maybe moving to a separate lexer phase
 * could help remove more backtracking?
 */
class EffektParsers(positions: Positions) extends Parsers(positions) {

  def parse(source: Source)(implicit C: Context): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) =>
        Some(ast)

      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range)))
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
  lazy val `control` = keyword("control")
  lazy val `io` = keyword("io")
  lazy val `record` = keyword("record")
  lazy val `at` = keyword("at")
  lazy val `in` = keyword("in")
  lazy val `box` = keyword("box")
  lazy val `unbox` = keyword("unbox")
  lazy val `return` = keyword("return")
  lazy val `region` = keyword("region")
  lazy val `new` = keyword("new")

  def keywordStrings: List[String] = List(
    "def", "val", "var", "handle", "true", "false", "else", "type",
    "effect", "interface", "try", "with", "case", "do", "if", "while",
    "match", "module", "import", "extern", "fun", "for",
    "at", "box", "unbox", "return", "region", "new"
  )

  def keyword(s: String): Parser[String] =
    s // todo check suffix

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9]".r, keywordStrings)

  lazy val ident =
    ( not(anyKeyword) ~> name
    | failure("Expected an identifier")
    )

  lazy val idDef: P[IdDef] = ident ^^ IdDef.apply
  lazy val idRef: P[IdRef] = ident ^^ IdRef.apply

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
    `import` ~/> moduleName ^^ Import.apply


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
    | defDef
    | effectDef
    | typeDef
    | effectAliasDef
    | dataDef
    | recordDef
    | externType
    | externEffect
    | externFun
    | externInclude
    | failure("Expected a definition")
    )

  lazy val funDef: P[Def] =
    `def` ~/> idDef ~ maybeTypeParams ~ params ~ (`:` ~> effectful).? ~ (`=` ~/> functionBody) ^^ {
      case id ~ tparams ~ (vparams ~ bparams) ~ eff ~ body => FunDef(id, tparams, vparams, bparams, eff, body)
    }

  lazy val functionBody: P[Stmt] =
    ( stmt
    | failure("Expected the body of a function definition, starting with =")
    )

  lazy val effectDef: P[Def] =
    ( `effect` ~> effectOp ^^ {
        case op =>
          InterfaceDef(IdDef(op.id.name) withPositionOf op.id, Nil, List(op), true)
      }
    | (`effect` | `interface`) ~> idDef ~ maybeTypeParams ~ (`{` ~/> many(`def` ~> effectOp)  <~ `}`) ^^ {
        case id ~ tps ~ ops => InterfaceDef(id, tps, ops, true)
      }
    )

  lazy val effectOp: P[Operation] =
    idDef ~ maybeTypeParams ~ valueParams ~/ (`:` ~/> effectful) ^^ Operation.apply

  lazy val externType: P[Def] =
    `extern` ~> `type` ~/> idDef ~ maybeTypeParams ^^ ExternType.apply

  lazy val externEffect: P[Def] =
    `extern` ~> `effect` ~/> idDef ~ maybeTypeParams ^^ ExternEffect.apply

  lazy val externFun: P[Def] =
    `extern` ~> ((externFlag | success(ExternFlag.IO)) <~ `def`) ~/ idDef ~ maybeTypeParams ~ params ~ (`:` ~> effectful) ~ ( `=` ~/> """\"([^\"]*)\"""".r) ^^ {
      case pure ~ id ~ tparams ~ (vparams ~ bparams) ~ tpe ~ body =>
        ExternFun(pure, id, tparams, vparams, bparams, tpe, body.stripPrefix("\"").stripSuffix("\""))
    }

  lazy val externFlag: P[ExternFlag.Purity] =
    ( `pure` ^^^ ExternFlag.Pure
    | `io` ^^^ ExternFlag.IO
    | `control` ^^^ ExternFlag.Control
    )

  lazy val externInclude: P[Def] =
    `extern` ~> `include` ~/> """\"([^\"]*)\"""".r ^^ { s => ExternInclude(s.stripPrefix("\"").stripSuffix("\"")) }


  /**
   * Parameters
   */
  lazy val params: P[List[ValueParam] ~ List[BlockParam]] =
    ( valueParamsOpt ~ blockParams
    | valueParams ~ success(List.empty[BlockParam])
    | success(List.empty[ValueParam]) ~ blockParams
    | failure("Expected a parameter list (multiple value parameters or one block parameter)")
    )

  lazy val blockParams: P[List[BlockParam]] =
    some(`{` ~/> blockParam <~ `}`)

  lazy val valueParams: P[List[ValueParam]] =
    some(valueParamSection) ^^ { _.flatten }

  lazy val valueParamsOpt: P[List[ValueParam]] =
    some(valueParamsOptSection) ^^ { _.flatten }

  lazy val valueParamSection: P[List[ValueParam]] =
    `(` ~/> manySep(valueParam, `,`) <~ `)`

  lazy val valueParamsOptSection: P[List[ValueParam]] =
    `(` ~/> manySep(valueParamOpt, `,`) <~ `)`

  lazy val valueParam: P[ValueParam] =
    idDef ~ (`:` ~> valueType) ^^ { case id ~ tpe => ValueParam(id, Some(tpe)) }

  lazy val valueParamOpt: P[ValueParam] =
    idDef ~ (`:` ~> valueType).? ^^ ValueParam.apply

  lazy val blockParam: P[BlockParam] =
    idDef ~ (`:` ~> blockType) ^^ BlockParam.apply

  lazy val typeParams: P[List[Id]] =
    `[` ~/> manySep(idDef, `,`) <~ `]`

  lazy val maybeTypeParams: P[List[Id]] =
    typeParams.? ^^ { o => o getOrElse Nil }

  /**
   * Arguments
   */

  lazy val maybeBlockArgs: P[List[BlockArg]] =
    many(blockArg)

  lazy val blockArgs: P[List[BlockArg]] =
    some(blockArg)

  lazy val blockArg: P[BlockArg] =
    ( `{` ~> idRef <~ `}` ^^ { case id => source.InterfaceArg(id) }
    | functionArg
    )

  lazy val blockDefinition: P[BlockArg] =
    ( idRef ^^ { case id => source.InterfaceArg(id) }
    | functionArg
    )

  lazy val functionArg: P[FunctionArg] =
    ( `{` ~> lambdaArgs ~ (`=>` ~/> stmts <~ `}`) ^^ { case ps ~ body => FunctionArg(Nil, ps, Nil, body) }
    | `{` ~> some(clause) <~ `}` ^^ { cs =>
      // TODO positions should be improved here and fresh names should be generated for the scrutinee
      // also mark the temp name as synthesized to prevent it from being listed in VSCode
      val name = "__tmpRes"
      FunctionArg(
        Nil,
        List(ValueParam(IdDef(name), None)),
        Nil,
        Return(Match(Var(IdRef(name)), cs))) withPositionOf cs
    }
    | `{` ~> stmts <~ `}` ^^ { s => FunctionArg(Nil, Nil, Nil, s) }
    | failure("Expected a block argument")
    )

  lazy val lambdaArgs: P[List[ValueParam]] =
    valueParamsOpt | (idDef ^^ { id => List(ValueParam(id, None)) })

  lazy val maybeValueArgs: P[List[Term]] =
    many(valueArgSection) ^^ { _.flatten }

  lazy val valueArgs: P[List[Term]] =
    some(valueArgSection) ^^ { _.flatten }

  lazy val valueArgSection: P[List[Term]] =
    `(` ~/> manySep(expr, `,`) <~ `)` | failure("Expected a value argument list")

  lazy val typeArgs: P[List[ValueType]] =
    `[` ~/> manySep(valueType, `,`) <~ `]`

  lazy val maybeTypeArgs: P[List[ValueType]] =
    typeArgs.? ^^ { o => o.getOrElse(Nil) }

  lazy val stmt: P[Stmt] =
    ( expr ^^ Return.apply
    | `{` ~/> stmts <~ `}` ^^ BlockStmt.apply
    | failure("Expected a statement")
    )

  /**
   * Statements
   */
  lazy val stmts: P[Stmt] =
    ( withStmt
    | (expr <~ `;`) ~ stmts ^^ ExprStmt.apply
    | (definition <~ `;`) ~ stmts ^^ DefStmt.apply
    | (varDef  <~ `;`) ~ stmts ^^ DefStmt.apply
    | (`return`.? ~> expr <~ `;`.?) ^^ Return.apply
    | matchDef
    | failure("Expected a statement")
    )

  lazy val withStmt: P[Stmt] =
    ( `with` ~> (valueParamsOpt | valueParamOpt ^^ { p => List(p) withPositionOf p }) ~
          (`=` ~/> idRef) ~ maybeTypeArgs ~ maybeValueArgs ~ (`;`  ~> stmts) ^^ {
        case params ~ id ~ tps ~ vargs ~ body =>
          val tgt = IdTarget(id) withPositionOf(id)
          Return(Call(tgt, tps, vargs, List(FunctionArg(Nil, params, Nil, body)) withPositionOf params))
       }
    | `with` ~> idRef ~ maybeTypeArgs ~ maybeValueArgs ~ (`;` ~> stmts) ^^ {
        case id ~ tps ~ vargs ~ body =>
          val tgt = IdTarget(id) withPositionOf(id)
          Return(Call(tgt, tps, vargs, List(FunctionArg(Nil, Nil, Nil, body)) withPositionOf id))
       }
    )

  lazy val valDef: P[ValDef] =
     `val` ~> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ ValDef.apply

  lazy val varDef: P[VarDef] =
    `var` ~/> idDef ~ (`:` ~/> valueType).? ~ (`in` ~/> idRef).? ~ (`=` ~/> stmt) ^^ {
      case id ~ tpe ~ reg ~ expr => VarDef(id, tpe, reg, expr)
    }

  lazy val defDef: P[DefDef] =
    `def` ~/> idDef ~ (`:` ~/> blockType).? ~ (`=` ~/> expr) ^^ {
      case id ~ tpe ~ block => DefDef(id, tpe, block)
    }

  // TODO make the scrutinee a statement
  lazy val matchDef: P[Stmt] =
     `val` ~> pattern ~ (`=` ~/> expr) ~ (`;` ~> stmts) ^^ {
       case p ~ sc ~ body =>
        Return(Match(sc, List(MatchClause(p, body)))) withPositionOf p
     }

  lazy val typeDef: P[TypeDef] =
    `type` ~> idDef ~ maybeTypeParams ~ (`=` ~/> valueType) ^^ TypeDef.apply

  lazy val effectAliasDef: P[EffectDef] =
    `effect` ~> idDef ~ (`=` ~/> effects) ^^ EffectDef.apply

  lazy val dataDef: P[DataDef] =
    `type` ~> idDef ~ maybeTypeParams ~ (`{` ~/> manySep(constructor, `;`) <~ `}`) ^^ DataDef.apply

  lazy val recordDef: P[RecordDef] =
    `record` ~/> idDef ~ maybeTypeParams ~ valueParams ^^ RecordDef.apply

  lazy val constructor: P[Constructor] =
    idDef ~ valueParams ^^ Constructor.apply

  /**
   * Expressions
   */
  lazy val expr:    P[Term] = matchExpr | assignExpr | orExpr | failure("Expected an expression")
  lazy val orExpr:  P[Term] = orExpr  ~ "||" ~/ andExpr ^^ binaryOp | andExpr
  lazy val andExpr: P[Term] = andExpr ~ "&&" ~/ eqExpr ^^ binaryOp | eqExpr
  lazy val eqExpr:  P[Term] = eqExpr  ~ oneof("==", "!=") ~/ relExpr ^^ binaryOp | relExpr
  lazy val relExpr: P[Term] = relExpr ~ oneof("<=", ">=", "<", ">") ~/ addExpr ^^ binaryOp | addExpr
  lazy val addExpr: P[Term] = addExpr ~ oneof("++", "+", "-") ~/ mulExpr ^^ binaryOp | mulExpr
  lazy val mulExpr: P[Term] = mulExpr ~ oneof("*", "/") ~/ accessExpr ^^ binaryOp | accessExpr

  lazy val accessExpr: P[Term] =
    callExpr ~ many(`.` ~>
      ( idRef ~ maybeTypeArgs ~ maybeValueArgs ~ maybeBlockArgs ^^ Left.apply
      | idRef ^^ Right.apply
      )
    ) ^^ {
      case firstTarget ~ accesses => accesses.foldLeft(firstTarget) {
        case (receiver, Left(id ~ targs ~ vargs ~ bargs)) =>
          MethodCall(receiver, id, targs, vargs, bargs)
        case (receiver, Right(id)) =>
          Select(receiver, id)
      }
    }

  lazy val callExpr: P[Term] =
    ( ifExpr
    | whileExpr
    | funCall
    | doExpr
    | handleExpr
    | regionExpr
    | lambdaExpr
    | unboxExpr
    | primExpr
    )

  lazy val callTarget: P[CallTarget] =
    ( idTarget
    | `(` ~> expr <~ `)` ^^ ExprTarget.apply
    )

  lazy val idTarget: P[IdTarget] =
    idRef ^^ IdTarget.apply

  lazy val unboxExpr: P[Term] = `unbox` ~/> expr ^^ Unbox.apply

  lazy val funCall: P[Term] =
    ( callTarget ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ Call.apply
    | callTarget ~ maybeTypeArgs ~ valueArgs ~ success(List.empty[BlockArg]) ^^ Call.apply
    | callTarget ~ maybeTypeArgs ~ success(List.empty[Term]) ~ blockArgs ^^ Call.apply
    )

  lazy val matchExpr: P[Term] =
    (accessExpr <~ `match` ~/ `{`) ~/ (some(clause) <~ `}`) ^^ Match.apply

  lazy val doExpr: P[Term] =
    `do` ~/> idRef ~ maybeTypeArgs ~ valueArgs ^^ {
      case op ~ targs ~ vargs => Do(None, op, targs, vargs)
    }

  lazy val handleExpr: P[Term] =
    `try` ~/> stmt ~ some(handler) ^^ TryHandle.apply

  lazy val regionExpr: P[Term] =
    `region` ~/> idDef ~ stmt ^^ Region.apply

  lazy val handler: P[Handler] =
    ( `with` ~> (idDef <~ `:`).? ~ interfaceType ~ (`{` ~> many(defClause) <~ `}`) ^^ {
      case capabilityName ~ effect ~ clauses =>
        val capability = capabilityName map { name => BlockParam(name, effect) }
        Handler(effect, capability, clauses)
      }
    | `with` ~> (idDef <~ `:`).? ~ (idRef ^^ InterfaceVar.apply) ~ maybeTypeParams ~ implicitResume ~ functionArg ^^ {
      case capabilityName ~ effect ~ tparams ~ resume ~ FunctionArg(_, vparams, _, body) =>
        val synthesizedId = IdRef(effect.id.name)
        val capability = capabilityName map { name => BlockParam(name, effect) }
        Handler(effect, capability, List(OpClause(synthesizedId, tparams, vparams, body, resume) withPositionOf effect))
      }
    )

  lazy val defClause: P[OpClause] =
    (`def` ~/> idRef) ~ maybeTypeParams ~ valueParamsOpt ~ implicitResume ~ (`=` ~/> functionBody)  ^^ {
      case id ~ tparams ~ vparams ~ resume ~ body => OpClause(id, tparams, vparams, body, resume)
    }

  lazy val clause: P[MatchClause] =
    `case` ~/> pattern ~ (`=>` ~/> stmts) ^^ MatchClause.apply

  lazy val pattern: P[MatchPattern] =
    ( "_" ^^^ IgnorePattern()
    | literals ^^ { l => LiteralPattern(l) }
    | idRef ~ (`(` ~> manySep(pattern, `,`)  <~ `)`) ^^ TagPattern.apply
    | idDef ^^ AnyPattern.apply
    | `(` ~> pattern ~ (`,` ~> some(pattern) <~ `)`) ^^ { case f ~ r =>
        TagPattern(IdRef(s"Tuple${r.size + 1}") withPositionOf f, f :: r)
      }
    )

  lazy val implicitResume: P[IdDef] = success(IdDef("resume"))


  lazy val assignExpr: P[Term] =
    idRef ~ (`=` ~> expr) ^^ Assign.apply

  lazy val ifExpr: P[Term] =
    `if` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ~ (`else` ~/> stmt | success(Return(UnitLit()))) ^^ If.apply

  lazy val whileExpr: P[Term] =
    `while` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ^^ While.apply

  lazy val primExpr: P[Term] =
    variable | literals | tupleLiteral | listLiteral | hole | `(` ~/> expr <~ `)`

  lazy val variable: P[Term] =
    idRef ^^ Var.apply

  lazy val hole: P[Term] =
    ( `<>` ^^^ Hole(Return(UnitLit()))
    | `<{` ~> stmts <~ `}>` ^^ Hole.apply
    )

  lazy val literals: P[Literal[_]] =
    double | int | bool | unit | string

  lazy val lambdaExpr: P[Box] =
    `fun` ~> valueParams ~ (`{` ~/> stmts <~ `}`)  ^^ { case ps ~ body => Box(None, FunctionArg(Nil, ps, Nil, body)) }

  lazy val listLiteral: P[Term] =
    `[` ~> manySep(expr, `,`) <~ `]` ^^ { exprs => exprs.foldRight(NilTree) { ConsTree } withPositionOf exprs }

  lazy val tupleLiteral: P[Term] =
    `(` ~> expr ~ (`,` ~/> someSep(expr, `,`) <~ `)`) ^^ { case tup @ (first ~ rest) => TupleTree(first :: rest) withPositionOf tup }

  private def NilTree: Term =
    Call(IdTarget(IdRef("Nil")), Nil, Nil, Nil)

  private def ConsTree(el: Term, rest: Term): Term =
    Call(IdTarget(IdRef("Cons")), Nil, List(el, rest), Nil)

  private def TupleTree(args: List[Term]): Term =
    Call(IdTarget(IdRef(s"Tuple${args.size}")), Nil, args, Nil)

  /**
   * Types and Effects
   */

  lazy val valueType: P[ValueType] =
    ( nocut(blockType) ~ (`at` ~/> captureSet) ^^ BoxedType.apply
    | primValueType
    )

  lazy val primValueType: P[ValueType] =
    ( idRef ~ typeArgs ^^ ValueTypeApp.apply
    | idRef ^^ TypeVar.apply
    | `(` ~> valueType <~ `)`
    | `(` ~> valueType ~ (`,` ~/> some(valueType) <~ `)`) ^^ { case f ~ r => TupleTypeTree(f :: r) }
    | failure("Expected a type")
    )

  lazy val captureSet: P[CaptureSet] = `{` ~> manySep(idRef, `,`) <~ `}` ^^ CaptureSet.apply

  lazy val blockType: P[BlockType] =
    ( (`(` ~> manySep(valueType, `,`) <~ `)`) ~ (`=>` ~/> primValueType) ~ maybeEffects ^^ FunctionType.apply
    | primValueType ~ (`=>` ~/> primValueType) ~ maybeEffects ^^ { case t ~ ret ~ eff => FunctionType(List(t), ret, eff) }
    // TODO only allow this on parameters, not elsewhere...
    | interfaceType
    | `=>` ~/> primValueType ~ maybeEffects ^^ { case ret ~ eff => FunctionType(Nil, ret, eff) }
    )

  lazy val interfaceType: P[InterfaceType] =
    ( idRef ~ typeArgs ^^ BlockTypeApp.apply
    | idRef ^^ InterfaceVar.apply
    | failure("Expected an interface / effect")
    )

  lazy val maybeEffects: P[Effects] =
    (`/` ~/> effects).? ^^ {
      case Some(es) => es
      case None => Effects.Pure
    }

  lazy val effectful: P[Effectful] =
    valueType ~ maybeEffects ^^ Effectful.apply

  lazy val effects: P[Effects] =
    ( interfaceType ^^ { e => Effects(e) }
    | `{` ~/> manySep(interfaceType, `,`) <~  `}` ^^ Effects.apply
    | failure("Expected an effect set")
    )


  // === AST Helpers ===

  private def binaryOp(lhs: Term, op: String, rhs: Term): Term =
     Call(IdTarget(IdRef(opName(op))), Nil, List(lhs, rhs), Nil)

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
