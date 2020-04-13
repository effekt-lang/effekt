package effekt

import effekt.context.Context
import effekt.source._
import org.bitbucket.inkytonik.kiama.parsing.{ NoSuccess, Parsers, Success }
import org.bitbucket.inkytonik.kiama.util.{ Positions, Source }

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
  lazy val name = "%s(%s)*".format(nameFirst, nameRest).r
  lazy val moduleName = "%s([/]%s)*".format(name, name).r

  lazy val `=` = literal("=")
  lazy val `:` = literal(":")
  lazy val `;` = literal(";")
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

  lazy val `handle` = keyword("handle")
  lazy val `true` = keyword("true")
  lazy val `false` = keyword("false")
  lazy val `lazy val` = keyword("lazy val")
  lazy val `val` = keyword("val")
  lazy val `var` = keyword("var")
  lazy val `if` = keyword("if")
  lazy val `else` = keyword("else")
  lazy val `while` = keyword("while")
  lazy val `type` = keyword("type")
  lazy val `effect` = keyword("effect")
  lazy val `try` = keyword("try")
  lazy val `with` = keyword("with")
  lazy val `case` = keyword("case")
  lazy val `do` = keyword("do")
  lazy val `yield` = keyword("yield")
  lazy val `resume` = keyword("resume")
  lazy val `match` = keyword("match")
  lazy val `def` = keyword("def")
  lazy val `module` = keyword("module")
  lazy val `import` = keyword("import")
  lazy val `extern` = keyword("extern")
  lazy val `include` = keyword("include")
  lazy val `pure` = keyword("pure")
  lazy val `record` = keyword("record")

  def keywordStrings: List[String] = List(
    "def", "val", "var", "handle", "true", "false", "else", "type",
    "effect", "try", "with", "case", "do", "yield", "if", "while", "match", "module", "import", "extern"
  )

  // we escape names that would conflict with JS early on to simplify the pipeline
  def additionalKeywords: List[String] = List(
    "catch", "in", "finally", "switch", "case", "this"
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
   * Numbers
   */
  lazy val digit = regex("[0-9]".r)
  lazy val decimalInt = regex("(0|[1-9][0-9]*)".r)

  lazy val int = decimalInt ^^ { n => IntLit(n.toInt) }
  lazy val bool = `true` ^^^ BooleanLit(true) | `false` ^^^ BooleanLit(false)
  lazy val unit = literal("()") ^^^ UnitLit()
  lazy val double = regex("(0|[1-9][0-9]*)[.]([0-9]+)".r) ^^ { n => DoubleLit(n.toDouble) }
  lazy val string = """\"([^\"]*)\"""".r ^^ {
    s => StringLit(s.substring(1, s.size - 1))
  }


  // === Lexing ===


  // === Parsing ===
  
  // turn scalariform formatting off!
  // format: OFF

  lazy val program: P[ModuleDecl] =
    ( `module` ~/> moduleName ~ many(importDecl) ~ many(definition) ^^ {
      case name ~ imports ~ defs if name != "effekt" => ModuleDecl(name, Import("effekt") :: imports, defs)
      case name ~ imports ~ defs => ModuleDecl(name, imports, defs)
    }
    | failure("Required at least one top-level function or effect definition")
    )

  lazy val importDecl: P[Import] =
    `import` ~/> moduleName ^^ Import


  /**
   * For the REPL
   */
  lazy val repl: P[Tree] = definition | valDef  | expr | importDecl

  /**
   * Definitions
   */
  lazy val definition: P[Def] =
    funDef | effectDef | typeDef | effectAliasDef | dataDef | recordDef | externType | externEffect | externFun | externInclude | failure("Expected a definition")

  lazy val funDef: P[Def] =
    `def` ~/> idDef ~ maybeTypeParams ~ some(params) ~ (`:` ~> effectful).? ~ ( `=` ~/> stmt) ^^ FunDef

  lazy val maybePure: P[Boolean] =
    `pure`.? ^^ { _.isDefined }

  lazy val effectDef: P[Def] =
    ( `effect` ~> effectOp ^^ {
        case op =>
          val effectId = IdRef(op.id.name)
          positions.dupPos(op.id, effectId)
          EffDef(effectId, List(op))
      }
    | `effect` ~> idDef ~ (`{` ~/> some(`def` ~> effectOp)  <~ `}`) ^^ EffDef
    )

  lazy val effectOp: P[Operation] =
    idDef ~ maybeTypeParams ~ some(valueParams) ~/ (`:` ~/> valueType) ^^ Operation

  lazy val externType: P[Def] =
    `extern` ~> `type` ~/> idDef ~ maybeTypeParams ^^ ExternType

  lazy val externEffect: P[Def] =
    `extern` ~> `effect` ~/> idDef ~ maybeTypeParams ^^ ExternEffect

  lazy val externFun: P[Def] =
    `extern` ~> (maybePure <~ `def`) ~/ idDef ~ maybeTypeParams ~ some(params) ~ (`:` ~> effectful) ~ ( `=` ~/> """\"([^\"]*)\"""".r) ^^ {
      case pure ~ id ~ tparams ~ params ~ tpe ~ body => ExternFun(pure, id, tparams, params, tpe, body.stripPrefix("\"").stripSuffix("\""))
    }

  lazy val externInclude: P[Def] =
    `extern` ~> `include` ~/> """\"([^\"]*)\"""".r ^^ { s => ExternInclude(s.stripPrefix("\"").stripSuffix("\"")) }


  /**
   * Parameters
   */
  lazy val params: P[ParamSection] =
    ( valueParams
    | `{` ~/> blockParam <~ `}`
    | failure("Expected a parameter list (multiple value parameters or one block parameter)")
    )

  lazy val valueParams: P[ValueParams] =
    `(` ~/> manySep(valueParam, `,`) <~ `)` ^^ ValueParams

  lazy val valueParamsOpt: P[ValueParams] =
    `(` ~/> manySep(valueParamOpt, `,`) <~ `)` ^^ ValueParams

  lazy val valueParam: P[ValueParam] =
    idDef ~ (`:` ~> valueType) ^^ { case id ~ tpe => ValueParam(id, Some(tpe)) }

  lazy val valueParamOpt: P[ValueParam] =
    idDef ~ (`:` ~> valueType).? ^^ ValueParam

  lazy val blockParam: P[BlockParam] =
    idDef ~ (`:` ~> blockType) ^^ BlockParam

  lazy val typeParams: P[List[Id]] =
    `[` ~/> manySep(idDef, `,`) <~ `]`

  lazy val maybeTypeParams: P[List[Id]] =
    typeParams.? ^^ { o => o.getOrElse(Nil) }

  /**
   * Arguments
   */
  lazy val args: P[ArgSection] =
    ( valueArgs
    | `{` ~/> ( (lambdaArgs <~ `=>`) ~ stmts <~ `}` ^^ BlockArg
              | stmts <~ `}` ^^ { s => BlockArg(ValueParams(Nil), s) })
    | failure("Expected at an argument list")
    )

  lazy val lambdaArgs: P[ValueParams] =
    valueParamsOpt | (idDef ^^ { id => ValueParams(List(ValueParam(id, None))) })

  lazy val valueArgs: P[ValueArgs] =
    `(` ~/> manySep(expr, `,`) <~ `)` ^^ ValueArgs | failure("Expected a value argument list")

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
    ( withStmt
    | exprStmt
    | defStmt
    | valDef  ~ (`;` ~/> stmts) ^^ DefStmt
    | varDef  ~ (`;` ~/> stmts) ^^ DefStmt
    | dataDef ~ (`;` ~/> stmts) ^^ DefStmt
    | recordDef ~ (`;` ~/> stmts) ^^ DefStmt
    | matchDef
    | expr ^^ Return
    )

  lazy val exprStmt: P[Stmt] =
    expr ~ (`;` ~/> stmts) ^^ ExprStmt

  lazy val defStmt: P[Stmt] =
    definition ~/ stmts ^^ DefStmt

  lazy val handleStmt: P[Stmt] =
    handleExpr ~/ stmts ^^ { case h ~ s => ExprStmt(h, s) }

  lazy val withStmt: P[Stmt] =
    ( `with` ~> (valueParamsOpt | valueParamOpt ^^ { p => ValueParams(List(p))}) ~ (`=` ~/> idRef) ~ maybeTypeArgs ~ many(args) ~ (`;`  ~/> stmts) ^^ {
        case params ~ id ~ tps ~ args ~ body => Return(Call(id, tps, args :+ BlockArg(params, body) ))
       }
    | `with` ~> idRef ~ maybeTypeArgs ~ many(args) ~ (`;`  ~/> stmts) ^^ {
        case id ~ tps ~ args ~ body => Return(Call(id, tps, args :+ BlockArg(ValueParams(Nil), body) ))
       }
    )

  lazy val valDef: P[ValDef] =
     `val` ~> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ ValDef

  lazy val varDef: P[VarDef] =
     `var` ~/> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ VarDef

  // TODO make the scrutinee a statement
  lazy val matchDef: P[Stmt] =
     `val` ~> pattern ~ (`=` ~/> expr) ~ (`;` ~/> stmts) ^^ {
       case p ~ sc ~ body => Return(MatchExpr(sc, List(MatchClause(p, body))))
     }

  lazy val typeDef: P[TypeDef] =
    `type` ~> idDef ~ maybeTypeParams ~ (`=` ~/> valueType) ^^ TypeDef

  lazy val effectAliasDef: P[EffectDef] =
    `effect` ~> idDef ~ (`=` ~/> effects) ^^ EffectDef

  lazy val dataDef: P[DataDef] =
    `type` ~> idDef ~ maybeTypeParams ~ (`{` ~/> manySep(constructor, `;`) <~ `}`) ^^ DataDef

  lazy val recordDef: P[RecordDef] =
    `record` ~/> idDef ~ maybeTypeParams ~ valueParams ^^ RecordDef

  lazy val constructor: P[Constructor] =
    idDef ~ valueParams ^^ Constructor

  /**
   * Expressions
   */
  lazy val expr:    P[Expr] = matchExpr | assignExpr | orExpr | failure("Expected an expression")
  lazy val orExpr:  P[Expr] = orExpr  ~ "||" ~/ andExpr ^^ binaryOp | andExpr
  lazy val andExpr: P[Expr] = andExpr ~ "&&" ~/ eqExpr ^^ binaryOp | eqExpr
  lazy val eqExpr:  P[Expr] = eqExpr  ~ oneof("==", "!=") ~/ relExpr ^^ binaryOp | relExpr
  lazy val relExpr: P[Expr] = relExpr ~ oneof("<=", ">=", "<", ">") ~/ addExpr ^^ binaryOp | addExpr
  lazy val addExpr: P[Expr] = addExpr ~ oneof("++", "+", "-") ~/ mulExpr ^^ binaryOp | mulExpr
  lazy val mulExpr: P[Expr] = mulExpr ~ oneof("*", "/") ~/ accessExpr ^^ binaryOp | accessExpr

  lazy val accessExpr: P[Expr] =
    callExpr ~ many(`.` ~> idRef ~ maybeTypeArgs ~ many(args)) ^^ {
      case firstTarget ~ accesses => accesses.foldLeft(firstTarget) {
        case (firstArg, name ~ targs ~ otherArgs) =>
          Call(name, targs, ValueArgs(List(firstArg)) :: otherArgs)
      }
    }

  lazy val callExpr: P[Expr] =
    ( ifExpr
    | whileExpr
    | resumeExpr
    | funCall
    | doExpr
    | yieldExpr
    | handleExpr
    | primExpr
    )

  lazy val funCall: P[Expr] =
    idRef ~ maybeTypeArgs ~ some(args) ^^ Call

  lazy val matchExpr: P[Expr] =
    (callExpr <~ `match` ~/ `{`) ~/ (some(clause) <~ `}`) ^^ MatchExpr

  lazy val doExpr: P[Expr] =
    `do` ~/> idRef ~ maybeTypeArgs ~ some(valueArgs) ^^ Call

  lazy val yieldExpr: P[Expr] =
    idRef ~ maybeTypeArgs ~ some(args) ^^ Call

  lazy val resumeExpr: P[Expr] =
    (`resume` ^^^ IdRef("resume")) ~ valueArgs ^^ { case r ~ args => Call(r, Nil, List(args)) }

  lazy val handleExpr: P[Expr] =
    `try` ~/> stmt ~ some(handler) ^^ TryHandle

  lazy val handler: P[Handler] =
    ( `with` ~> idRef ~ (`{` ~> some(defClause) <~ `}`) ^^ Handler
    | `with` ~> idRef ~ (`{` ~>lambdaArgs) ~ implicitResume ~ (`=>` ~/> stmts) <~ `}` ^^ {
      case effectId ~ params ~ resume ~ body =>
        val opId = IdRef(effectId.name)
        positions.dupPos(effectId, opId)
        Handler(effectId, List(OpClause(opId, List(params), body, resume)))
      }
    )

  lazy val defClause: P[OpClause] =
    (`def` ~/> idRef) ~ some(valueParamsOpt) ~ implicitResume ~ (`=` ~/> stmt) ^^ {
      case id ~ params ~ resume ~ body => OpClause(id, params, body, resume)
    }

  lazy val clause: P[MatchClause] =
    `case` ~/> pattern ~ (`=>` ~/> stmts) ^^ MatchClause

  lazy val pattern: P[MatchPattern] =
    ( "_" ^^^ IgnorePattern()
    | idRef ~ (`(` ~> manySep(pattern, `,`)  <~ `)`) ^^ TagPattern
    | idDef ^^ AnyPattern
    | `(` ~> pattern ~ (`,` ~> pattern <~ `)`) ^^ { case f ~ s => TagPattern(IdRef("Pair"), List(f, s)) }
    )

  lazy val implicitResume: P[IdDef] = success(IdDef("resume"))


  lazy val assignExpr: P[Expr] =
    idRef ~ (`=` ~> expr) ^^ Assign

  lazy val ifExpr: P[Expr] =
    `if` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ~ (`else` ~/> stmt) ^^ If

  lazy val whileExpr: P[Expr] =
    `while` ~/> (`(` ~/> expr <~ `)`) ~/ stmt ^^ While

  lazy val primExpr: P[Expr] =
    literals | pairLiteral | `(` ~/> expr <~ `)`

  lazy val variable: P[Expr] =
    idRef ^^ Var

  lazy val literals: P[Expr] =
    double | int | bool | unit | variable | string | listLiteral

  lazy val listLiteral: P[Expr] =
    `[` ~> manySep(expr, `,`) <~ `]` ^^ { exprs => exprs.foldRight(NilTree) { ConsTree } }

  lazy val pairLiteral: P[Expr] =
    `(` ~> expr ~ (`,` ~/> expr <~ `)`) ^^ PairTree

  private def NilTree: Expr =
    Call(IdRef("Nil"), Nil, List(ValueArgs(Nil)))

  private def ConsTree(el: Expr, rest: Expr): Expr =
    Call(IdRef("Cons"), Nil, List(ValueArgs(List(el, rest))))

  private def PairTree(fst: Expr, snd: Expr): Expr =
    Call(IdRef("Pair"), Nil, List(ValueArgs(List(fst, snd))))

  /**
   * Types and Effects
   */

  lazy val valueType: P[ValueType] =
    ( idRef ~ typeArgs ^^ TypeApp
    | idRef ^^ TypeVar
    | `(` ~> valueType ~ (`,` ~/> valueType <~ `)`) ^^ PairTypeTree
    | failure("Expected a type")
    )

  lazy val blockType: P[BlockType] =
    ( (`(` ~/> manySep(valueType, `,`) <~ `)`) ~ (`=>` ~> effectful) ^^ BlockType
    | valueType ~ (`=>` ~> effectful) ^^ { case t ~ e => BlockType(List(t), e) }
    | effectful ^^ { e => BlockType(Nil, e) }
    )

  lazy val effectful: P[Effectful] =
    valueType ~ (`/` ~> effects).? ^^ {
      case t ~ Some(es) => Effectful(t, es)
      case t ~ None => Effectful(t, Effects.Pure)
    }

  lazy val effects: P[Effects] =
    ( effectType ^^ { e => Effects(e) }
    | `{` ~/> manySep(effectType, `,`) <~  `}` ^^ Effects.apply
    | failure("Expected an effect set")
    )

  lazy val effectType: P[Effect] =
    idRef ^^ Effect | failure("Expected a single effect type")


  // === AST Helpers ===

  private def binaryOp(lhs: Expr, op: String, rhs: Expr): Expr =
     Call(IdRef(opName(op)), Nil, List(ValueArgs(List(lhs, rhs))))

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

  private def PairTypeTree(fst: ValueType, snd: ValueType): ValueType =
    TypeApp(IdRef("Pair"), (List(fst, snd)))

  // === Utils ===
  def many[T](p: => Parser[T]): Parser[List[T]] =
    rep(p) ^^ { _.toList }

  def some[T](p: => Parser[T]): Parser[List[T]] =
    rep1(p) ^^ { _.toList }

  def manySep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    repsep(p, sep) ^^ { _.toList }

  def someSep[T](p: => Parser[T], sep: => Parser[_]): Parser[List[T]] =
    rep1sep(p, sep) ^^ { _.toList }

}
