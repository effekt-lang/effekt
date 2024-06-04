package effekt

import effekt.context.Context
import effekt.source.*
import effekt.util.{SourceTask, VirtualSource}
import effekt.util.messages.ParseError
import kiama.parsing.{Failure, Input, NoSuccess, ParseResult, Parsers, Success}
import kiama.util.{Position, Positions, Range, Source, StringSource}

import scala.util.matching.Regex
import scala.language.implicitConversions

/**
 * String templates containing unquotes `${... : T}`
 */
case class Template[+T](strings: List[String], args: List[T])

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
      Context.timed(phaseName, source.name) { parser.parse(source) }
  } map { tree => Parsed(source, tree) }
}

/**
 * TODO at the moment parsing is still very slow. I tried to address this prematurily
 * by adding cuts and using PackratParser for nonterminals. Maybe moving to a separate lexer phase
 * could help remove more backtracking?
 */
class EffektParsers(positions: Positions) extends EffektLexers(positions) {

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

  /**
   * Names
   */
  lazy val idDef: P[IdDef] = ident ^^ IdDef.apply
  lazy val idRef: P[IdRef] = identRef ^^ { path =>
    val ids = path.split("::").toList
    IdRef(ids.init, ids.last)
  }


  /**
   * Main entry point
   */
  lazy val program: P[ModuleDecl] =
    ( moduleDecl ~ many(includeDecl) ~ toplevelDefs ^^ ModuleDecl.apply
    | failure("Required at least one top-level function or effect definition")
    )

  lazy val moduleDecl: P[String] =
    ( `module` ~/> moduleName
    | defaultModulePath
    )

  lazy val includeDecl: P[Include] =
    `import` ~/> moduleName ^^ Include.apply


  /**
   * For the REPL
   */
  lazy val repl: P[Tree] = toplevel | expr | includeDecl

  lazy val toplevel: P[Def] =
    ( valDef
    | funDef
    | defDef
    | interfaceDef
    | typeAliasDef
    | effectAliasDef
    | dataDef
    | recordDef
    | externDef
    | `var` ~/> failure("Mutable variable declarations are currently not supported on the toplevel.")
    | failure("Expected a top-level definition")
    )

  lazy val toplevelDefs: P[List[Def]] =
    ( `namespace` ~> idDef ~ (`{` ~/> toplevelDefs <~ `}`) ~ toplevelDefs  ^^ { case (id ~ defs ~ rest) => NamespaceDef(id, defs) :: rest }
    | `namespace` ~> idDef ~/ toplevelDefs ^^ { case (id ~ defs) => List(NamespaceDef(id, defs)) }
    | toplevel ~ toplevelDefs ^^ { case defn ~ defs => defn :: defs }
    | success(Nil)
    )

  /**
   * Definitions
   */
  lazy val definition: P[Def] =
    ( valDef
    | funDef
    | defDef
    // aliases are allowed, since they are fully resolved during name checking
    | typeAliasDef
    | effectAliasDef
    | namespaceDef
    | (`extern` | `effect` | `interface` | `type` | `record`).into { (kw: String) =>
        failure(s"Only supported on the toplevel: ${kw} declaration.")
      }
    | failure("Expected a definition")
    )

  lazy val funDef: P[Def] =
    `def` ~/> idDef ~ params ~ (`:` ~> effectful).? ~ (`=` ~/> functionBody) ^^ {
      case id ~ (tparams ~ vparams ~ bparams) ~ eff ~ body => FunDef(id, tparams, vparams, bparams, eff, body)
    }

  lazy val functionBody: P[Stmt] =
    ( stmt
    | failure("Expected the body of a function definition, starting with =")
    )

  lazy val interfaceDef: P[Def] =
    ( `effect` ~> effectOp ^^ {
        case op =>
          InterfaceDef(IdDef(op.id.name) withPositionOf op.id, Nil, List(op), true)
      }
    | `interface` ~> idDef ~ maybeTypeParams ~ (`{` ~/> many(`def` ~/> effectOp)  <~ `}`) ^^ {
        case id ~ tps ~ ops => InterfaceDef(id, tps, ops, true)
      }
    )

  lazy val effectOp: P[Operation] =
    idDef ~ params ~ (`:` ~> effectful) ^^ {
      case id ~ (tps ~ vps ~ bps) ~ ret => Operation(id, tps, vps, bps, ret)
    }

  lazy val featureFlag: P[FeatureFlag] =
    ( "default" ^^ { _ => FeatureFlag.Default }
    | ident ^^ { flag => FeatureFlag.NamedFeatureFlag(flag) }
    )

  lazy val maybeFeatureFlag: P[FeatureFlag] = opt(featureFlag) ^^ {
    case Some(flag) => flag
    case None => FeatureFlag.Default
  }

  lazy val externDef: P[Def] =
    ( externType
    | externInterface
    | externFun
    | externResource
    | externInclude
    )

  lazy val externType: P[Def] =
    `extern` ~> `type` ~/> idDef ~ maybeTypeParams ^^ ExternType.apply

  lazy val externInterface: P[Def] =
    `extern` ~> `interface` ~/> idDef ~ maybeTypeParams ^^ ExternInterface.apply

  lazy val externFun: P[Def] =
    `extern` ~> (externCapture <~ `def`) ~/ idDef ~ params ~ (`:` ~> effectful) ~ ( `=` ~/> many(externBody)) ^^ {
      case pure ~ id ~ (tparams ~ vparams ~ bparams) ~ tpe ~ bodies =>
        ExternDef(pure, id, tparams, vparams, bparams, tpe, bodies)
    }

  lazy val externResource: P[Def] =
    (`extern` ~ `resource`) ~> (idDef ~ (`:` ~> blockType)) ^^ ExternResource.apply

  lazy val externBody: P[ExternBody] =
    (maybeFeatureFlag ~ ("""(?=[\"])""".r ~> commit(
              multilineString ^^ { s => Template(List(s), Nil) }
            | guard(regex(s"(?!${multi})".r)) ~> templateString(expr)
            | failure(s"Expected an extern definition, which can either be a single-line string (e.g., \"x + y\") or a multi-line string (e.g., $multi...$multi), potentially qualified")
            )) ^^ {
                case ff ~ body => ExternBody.StringExternBody(ff, body)
            }
    | featureFlag ~ ("{" ~> stmts <~ "}") ^^ { case ff ~ body => ExternBody.EffektExternBody(ff, body) }
    | featureFlag ~> failure(s"Expected an extern definition, which can either be a single-line string (e.g., \"x + y\") or a multi-line string (e.g., $multi...$multi)")
    )


  lazy val externCapture: P[CaptureSet] =
    ( "pure" ^^^ CaptureSet(Nil)
    | idRef ^^ { id => CaptureSet(List(id)) }
    | captureSet
    | success(CaptureSet(List(IdRef(List("effekt"), "io"))))
    )

  lazy val externInclude: P[Def] =
    ( `extern` ~> `include` ~/> maybeFeatureFlag ~ """\"([^\"]*)\"""".r ^^ { case ff ~ s => ExternInclude(ff, s.stripPrefix("\"").stripSuffix("\""), None) }
    | `extern` ~> maybeFeatureFlag ~ multilineString ^^ { case ff ~ contents => ExternInclude(ff, "", Some(contents)) }
    )


  /**
   * Parameters
   */
  lazy val params: P[List[Id] ~ List[ValueParam] ~ List[BlockParam]] =
    ( maybeTypeParams ~ valueParams ~ blockParams
    | maybeTypeParams ~ valueParams ~ success(List.empty[BlockParam])
    | maybeTypeParams ~ success(List.empty[ValueParam]) ~ blockParams
    | failure("Expected a parameter list (multiple value parameters or one block parameter)")
    )

  // params with optional types, currently only allowed for block args and operation implementations
  lazy val paramsOpt: P[List[Id] ~ List[ValueParam] ~ List[BlockParam]] =
    ( maybeTypeParams ~ valueParamsOpt ~ blockParamsOpt
    | maybeTypeParams ~ valueParamsOpt ~ success(List.empty[BlockParam])
    | maybeTypeParams ~ success(List.empty[ValueParam]) ~ blockParamsOpt
    | failure("Expected a parameter list (multiple value parameters or one block parameter; only type annotations of value parameters can be currently omitted)")
    )

  lazy val blockParams: P[List[BlockParam]] =
    some(`{` ~/> blockParam <~ `}`)

  lazy val blockParamsOpt: P[List[BlockParam]] =
    some(`{` ~/> blockParamOpt <~ `}`)

  lazy val valueParams: P[List[ValueParam]] =
    `(` ~/> manySep(valueParam, `,`) <~ `)`

  lazy val valueParamsOpt: P[List[ValueParam]] =
    `(` ~/> manySep(valueParamOpt, `,`) <~ `)`

  lazy val valueTypeAnnotation : P[ValueType] =
    ( `:` ~/> valueType
    | failure("Expected a type annotation")
    )

  lazy val valueParam : P[ValueParam] =
    idDef ~ valueTypeAnnotation ^^ { case id ~ tpe => ValueParam(id, Some(tpe)) : ValueParam }

  lazy val valueParamOpt: P[ValueParam] =
    idDef ~ (`:` ~> valueType).? ^^ { case id ~ tpe => ValueParam(id, tpe) : ValueParam }

  lazy val blockParam: P[BlockParam] =
    idDef ~ (`:` ~/> blockType) ^^ { case id ~ tpe => BlockParam(id, Some(tpe)) : BlockParam }

  lazy val blockParamOpt: P[BlockParam] =
    idDef ~ (`:` ~> blockType).? ^^ { case id ~ tpe => BlockParam(id, tpe) : BlockParam }

  lazy val typeParams: P[List[Id]] =
    `[` ~/> manySep(idDef, `,`) <~ `]`

  lazy val maybeTypeParams: P[List[Id]] =
    typeParams.? ^^ { o => o getOrElse Nil }

  /**
   * Arguments
   */

  lazy val maybeBlockArgs: P[List[Term]] =
    many(blockArg)

  lazy val blockArgs: P[List[Term]] =
    some(blockArg)

  lazy val blockArg: P[Term] =
    ( `{` ~> idRef <~ `}` ^^ Var.apply
    | functionArg
    )

  lazy val functionArg: P[BlockLiteral] =
    ( `{` ~> lambdaParams ~ (`=>` ~/> stmts <~ `}`) ^^ {
      case (tps, vps, bps) ~ body => BlockLiteral(tps, vps, bps, body) : BlockLiteral
    }
    | `{` ~> some(matchClause) <~ `}` ^^ { cs =>
      // TODO positions should be improved here and fresh names should be generated for the scrutinee
      // also mark the temp name as synthesized to prevent it from being listed in VSCode
      val name = "__tmpRes"
      val res: BlockLiteral = BlockLiteral(
        Nil,
        List(ValueParam(IdDef(name), None)),
        Nil,
        Return(Match(Var(IdRef(Nil, name)), cs, None)))
      res withPositionOf cs
    }
    | `{` ~> stmts <~ `}` ^^ { s => BlockLiteral(Nil, Nil, Nil, s) : BlockLiteral }
    | failure("Expected a block argument")
    )


  lazy val lambdaParams: P[(List[Id], List[ValueParam], List[BlockParam])] =
    ( paramsOpt ^^ { case tps ~ vps ~ bps => (tps, vps, bps) }
    | valueParamsOpt ^^ { ps => (Nil, ps, Nil) }
    | idDef ^^ { id => (Nil, List(ValueParam(id, None) : ValueParam), Nil) }
    )

  lazy val maybeValueArgs: P[List[Term]] =
    valueArgs.? ^^ { o => o.getOrElse(Nil) }

  lazy val valueArgs: P[List[Term]] =
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
          Return(Call(tgt, tps, vargs, List(BlockLiteral(Nil, params, Nil, body)) withPositionOf params))
       }
    | `with` ~> expr ~ (`;` ~> stmts) ^^ {
       case m@MethodCall(receiver, id, tps, vargs, bargs) ~ body =>
         Return(MethodCall(receiver, id, tps, vargs, bargs :+ (BlockLiteral(Nil, Nil, Nil, body) withPositionOf m)))
       case c@Call(callee, tps, vargs, bargs) ~ body =>
         Return(Call(callee, tps, vargs, bargs :+ (BlockLiteral(Nil, Nil, Nil, body) withPositionOf c)))
       case Var(id) ~ body =>
         val tgt = IdTarget(id) withPositionOf(id)
         Return(Call(tgt, Nil, Nil, (BlockLiteral(Nil, Nil, Nil, body) withPositionOf id) :: Nil))
       case term ~ body =>
         Return(Call(ExprTarget(term), Nil, Nil, (BlockLiteral(Nil, Nil, Nil, body) withPositionOf term) :: Nil))
    }
    )

  lazy val valDef: P[Def] =
     `val` ~> idDef ~ (`:` ~/> valueType).? ~ (`=` ~/> stmt) ^^ ValDef.apply

  lazy val varDef: P[Def] =
    `var` ~/> idDef ~ (`:` ~/> valueType).? ~ (`in` ~/> idRef).? ~ (`=` ~/> stmt) ^^ {
      case id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr)
      case id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr)
    }

  lazy val defDef: P[Def] =
    `def` ~/> idDef ~ (`:` ~/> blockType).? ~ (`=` ~/> expr) ^^ {
      case id ~ tpe ~ block => DefDef(id, tpe, block)
    }

  // TODO make the scrutinee a statement
  lazy val matchDef: P[Stmt] =
     `val` ~> matchPattern ~ many(`and` ~> matchGuard) ~ (`=` ~/> expr) ~ (`else` ~> stmt).? ~ (`;` ~> stmts) ^^ {
       case p ~ guards ~ sc ~ default ~ body =>
        Return(Match(sc, List(MatchClause(p, guards, body)), default)) withPositionOf p
     }

  lazy val typeAliasDef: P[Def] =
    `type` ~> idDef ~ maybeTypeParams ~ (`=` ~/> valueType) ^^ TypeDef.apply

  lazy val effectAliasDef: P[Def] =
    `effect` ~> idDef ~ maybeTypeParams ~ (`=` ~/> effects) ^^ EffectDef.apply

  lazy val dataDef: P[Def] =
    `type` ~> idDef ~ maybeTypeParams ~ (`{` ~/> manySep(constructor, `;`) <~ `}`) ^^ DataDef.apply

  lazy val recordDef: P[Def] =
    `record` ~/> idDef ~ maybeTypeParams ~ valueParams ^^ RecordDef.apply

  lazy val namespaceDef: P[Def] =
    `namespace` ~/> idDef ~ (`{` ~/> definitions <~ `}`) ^^ NamespaceDef.apply

  lazy val definitions: P[List[Def]] =
    ( `namespace` ~> idDef ~ (`{` ~/> definitions <~ `}`) ~ definitions  ^^ { case (id ~ defs ~ rest) => NamespaceDef(id, defs) :: rest }
    | `namespace` ~> idDef ~/ definitions ^^ { case (id ~ defs) => List(NamespaceDef(id, defs)) }
    | definition ~ definitions ^^ { case defn ~ defs => defn :: defs }
    | success(Nil)
    )

  lazy val constructor: P[Constructor] =
    idDef ~ maybeTypeParams ~ valueParams ^^ Constructor.apply

  /**
   * Expressions
   */
  lazy val expr: P[Term] = "?(" ~/> matchGuards <~ ")" ^^ { mgs =>
    If(mgs, Stmt.Return(Literal(true, symbols.builtins.TBoolean)), Stmt.Return(Literal(false, symbols.builtins.TBoolean)))
  } | matchExpr | assignExpr | orExpr | failure("Expected an expression")
  lazy val orExpr:  P[Term] = orExpr  ~ "||" ~/ andExpr ^^ thunkedBinaryOp | andExpr
  lazy val andExpr: P[Term] = andExpr ~ "&&" ~/ eqExpr ^^ thunkedBinaryOp | eqExpr
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
    | boxedExpr
    | unboxExpr
    | newExpr
    | primExpr
    )

  lazy val unboxExpr: P[Term] = `unbox` ~/> expr ^^ Unbox.apply

  lazy val newExpr: P[Term] = `new` ~/> implementation ^^ New.apply

  lazy val funCall: P[Term] =
    callTarget ~ some(arguments) ^^ {
      case target ~ ((targs ~ vargs ~ bargs) :: rest) =>
        rest.foldLeft[Term](Call(target, targs, vargs, bargs)) {
          case (expr, (targs ~ vargs ~ bargs)) => Call(ExprTarget(expr), targs, vargs, bargs)
        }
      case target ~ Nil => sys.error("should not happen since arguments cannot be nil.")
    }

  lazy val arguments: P[(List[ValueType] ~ List[Term] ~ List[Term])] =
    ( maybeTypeArgs ~ valueArgs ~ blockArgs
    | maybeTypeArgs ~ valueArgs ~ success(List.empty[Term])
    | maybeTypeArgs ~ success(List.empty[Term]) ~ blockArgs
    | typeArgs ~ success(List.empty[Term]) ~ success(List.empty[Term])
    )

  lazy val callTarget: P[CallTarget] =
    ( `(` ~> expr <~ `)` ^^ ExprTarget.apply
    | idRef ^^ IdTarget.apply
    )

  lazy val matchExpr: P[Term] =
    (accessExpr <~ `match` ~/ `{`) ~/ (many(matchClause) <~ `}`) ~/ (`else` ~/> stmt).? ^^ Match.apply

  lazy val doExpr: P[Term] =
    `do` ~/> idRef ~ arguments ^^ {
      case op ~ (targs ~ vargs ~ bargs) => Do(None, op, targs, vargs, bargs)
    }

  lazy val handleExpr: P[Term] =
    `try` ~/> stmt ~ some(handler) ^^ TryHandle.apply

  lazy val regionExpr: P[Term] =
    `region` ~/> idDef ~ stmt ^^ Region.apply

  lazy val handler: P[Handler] =
    ( `with` ~> (idDef <~ `:`).? ~ implementation ^^ {
      case capabilityName ~ impl =>
        val capability = capabilityName map { name => BlockParam(name, Some(impl.interface)): BlockParam }
        Handler(capability, impl)
      }
    )

  lazy val implementation: P[Implementation] =
    ( interfaceType ~ (`{` ~/> many(defClause) <~ `}`) ^^ {
      case effect ~ clauses =>
        Implementation(effect, clauses)
      }
    | idRef ~ maybeTypeParams ~ implicitResume ~ functionArg ^^ {
      case id ~ tparams ~ resume ~ BlockLiteral(_, vparams, bparams, body) =>
        val synthesizedId = IdRef(Nil, id.name)
        val interface = BlockTypeRef(id, Nil) withPositionOf id
        Implementation(interface, List(OpClause(synthesizedId, tparams, vparams, bparams, None, body, resume) withPositionOf id))
      }
    )

  lazy val defClause: P[OpClause] =
    (`def` ~/> idRef) ~ paramsOpt ~ (`:` ~/> effectful).? ~ implicitResume ~ (`=` ~/> functionBody) ^^ {
      case id ~ (tparams ~ vparams ~ bparams) ~ ret ~ resume ~ body => OpClause(id, tparams, vparams, bparams, ret, body, resume)
    }

  lazy val matchClause: P[MatchClause] =
    `case` ~/> matchPattern ~ many(`and` ~> matchGuard) ~ (`=>` ~/> stmts) ^^ MatchClause.apply

  lazy val matchGuard: P[MatchGuard] =
    ( expr ~ (`is` ~/> matchPattern) ^^ MatchGuard.PatternGuard.apply
    | expr ^^ MatchGuard.BooleanGuard.apply
    )

  lazy val matchGuards: P[List[MatchGuard]] =
    someSep(matchGuard, `and`)

  lazy val matchPattern: P[MatchPattern] =
    ( "_" ^^^ IgnorePattern()
    | literals ^^ { l => LiteralPattern(l) }
    | idRef ~ (`(` ~> manySep(matchPattern, `,`)  <~ `)`) ^^ TagPattern.apply
    | idDef ^^ AnyPattern.apply
    | `(` ~> matchPattern ~ (some(`,` ~> matchPattern) <~ `)`) ^^ { case f ~ r =>
        TagPattern(IdRef(List("effekt"), s"Tuple${r.size + 1}") withPositionOf f, f :: r)
      }
    )

  lazy val implicitResume: P[IdDef] = success(IdDef("resume"))

  lazy val assignExpr: P[Term] =
    idRef ~ (`=` ~> expr) ^^ Assign.apply

  lazy val ifExpr: P[Term] =
    `if` ~/> (`(` ~/> matchGuards <~ `)`) ~/ stmt ~ (`else` ~/> stmt | success(Return(UnitLit()))) ^^ If.apply

  lazy val whileExpr: P[Term] =
    `while` ~/> (`(` ~/> matchGuards <~ `)`) ~/ stmt ~ (`else` ~/> stmt).? ^^ While.apply

  lazy val primExpr: P[Term] =
    variable | literals | tupleLiteral | listLiteral | hole | `(` ~/> expr <~ `)`

  lazy val variable: P[Term] =
    idRef ^^ Var.apply

  lazy val hole: P[Term] =
    ( `<>` ^^^ Hole(Return(UnitLit()))
    | `<{` ~> stmts <~ `}>` ^^ Hole.apply
    )

  lazy val literals: P[Literal] =
    double | int | bool | unit | char | string

  lazy val int    = integerLiteral.flatMap { n =>
    try { val number = n.toLong;
      success(IntLit(number).withPositionOf(n))
    } catch { case e => failure("Not a 64bit integer literal.") }
  }
  lazy val bool   = `true` ^^^ BooleanLit(true) | `false` ^^^ BooleanLit(false)
  lazy val unit   = literal("()") ^^^ UnitLit()
  lazy val double = doubleLiteral ^^ { n => DoubleLit(n.toDouble) }
  lazy val string = // we need to replace certain characters that would otherwise mess up the respective syntax emitted by the backends
    ( multilineString ^^ { s => StringLit(s.replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")) }
    | stringLiteral ^^ { s => StringLit(s.substring(1, s.size - 1).replace("\\\n", "").replace("\\\r\n", "").replace("\t", "\\t")) }
    )
  lazy val char =
    ( charLiteral ^^ { CharLit.apply }
    | unicodeChar ^^ { CharLit.apply }
    )

  lazy val boxedExpr: P[Term] =
    `box` ~> captureSet.? ~ (idRef ^^ Var.apply | functionArg | newExpr) ^^ { case capt ~ block => Box(capt, block) }

  lazy val lambdaExpr: P[Term] =
    `fun` ~> valueParams ~ (`{` ~/> stmts <~ `}`)  ^^ { case ps ~ body => Box(None, BlockLiteral(Nil, ps, Nil, body)) }

  lazy val listLiteral: P[Term] =
    `[` ~> manySep(expr, `,`) <~ `]` ^^ { exprs => exprs.foldRight(NilTree) { ConsTree } withPositionOf exprs }

  lazy val tupleLiteral: P[Term] =
    `(` ~> expr ~ (`,` ~/> someSep(expr, `,`) <~ `)`) ^^ { case tup @ (first ~ rest) => TupleTree(first :: rest) withPositionOf tup }

  private def NilTree: Term =
    Call(IdTarget(IdRef(List(), "Nil")), Nil, Nil, Nil)

  private def ConsTree(el: Term, rest: Term): Term =
    Call(IdTarget(IdRef(List(), "Cons")), Nil, List(el, rest), Nil)

  private def TupleTree(args: List[Term]): Term =
    Call(IdTarget(IdRef(List("effekt"), s"Tuple${args.size}")), Nil, args, Nil)

  /**
   * Types and Effects
   */

  lazy val valueType: P[ValueType] =
    ( nocut(blockType) ~ (`at` ~/> captureSet) ^^ BoxedType.apply
    | primValueType
    )

  lazy val maybeValueTypes: P[List[ValueType]] =
    (`(` ~> manySep(valueType, `,`) <~ `)`).? ^^ { vps => vps.getOrElse(Nil) }

  lazy val primValueType: P[ValueType] =
    ( idRef ~ maybeTypeArgs ^^ ValueTypeRef.apply
    | `(` ~> valueType <~ `)`
    | `(` ~> valueType ~ (`,` ~/> some(valueType) <~ `)`) ^^ { case f ~ r => TupleTypeTree(f :: r) }
    | failure("Expected a value type")
    )

  lazy val captureSet: P[CaptureSet] = `{` ~> manySep(idRef, `,`) <~ `}` ^^ CaptureSet.apply

  lazy val blockType: P[BlockType] =
    ( maybeTypeParams ~ maybeValueTypes ~ many(blockTypeParam) ~ (`=>` ~/> primValueType) ~ maybeEffects ^^ {
      case tparams ~ vparams ~ bparams ~ t ~ effs => FunctionType(tparams, vparams, bparams, t, effs)
    }
    | some(blockTypeParam) ~ (`=>` ~/> primValueType) ~ maybeEffects ^^ { case tpes ~ ret ~ eff => FunctionType(Nil, Nil, tpes, ret, eff) }
    | primValueType ~ (`=>` ~/> primValueType) ~ maybeEffects ^^ { case t ~ ret ~ eff => FunctionType(Nil, List(t), Nil, ret, eff) }
    | (valueType <~ guard(`/`)) !!! "Effects not allowed here. Maybe you mean to use a function type `() => T / E`?"
    // TODO only allow this on parameters, not elsewhere...
    | interfaceType
    | `=>` ~/> primValueType ~ maybeEffects ^^ { case ret ~ eff => FunctionType(Nil, Nil, Nil, ret, eff) }
    | failure("Expected either a function type (e.g., (A) => B / {E} or => B) or an interface type (e.g., State[T]).")
    )

  lazy val blockTypeParam: P[(Option[IdDef], BlockType)] =
    `{` ~> (idDef <~ `:`).? ~ blockType <~ `}` ^^ { case id ~ tpe => (id, tpe) }

  lazy val interfaceType: P[BlockTypeRef] =
    ( idRef ~ maybeTypeArgs ^^ { case (id ~ targs) => BlockTypeRef(id, targs): BlockTypeRef }
    | failure("Expected an interface type")
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
     Call(IdTarget(IdRef(Nil, opName(op))), Nil, List(lhs, rhs), Nil)

  // thunkedBinaryOp(lhs, op, rhs) = op { lhs } { rhs }
  private def thunkedBinaryOp(lhs: Term, op: String, rhs: Term): Term =
     Call(IdTarget(IdRef(Nil, thunkedOpName(op))), Nil, Nil, List(BlockLiteral(Nil, Nil, Nil, Return(lhs)), BlockLiteral(Nil, Nil, Nil, Return(rhs))))

  private def thunkedOpName(op: String): String = op match {
    case "||" => "infixOr"
    case "&&" => "infixAnd"
  }

  private def opName(op: String): String = op match {
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
    ValueTypeRef(IdRef(List("effekt"), s"Tuple${tps.size}"), tps)

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

  /**
   * Parses the contents of a string and searches for unquotes ${ ... }
   *
   * returns the list of unquotes and their respectively preceding string.
   *
   *     "   BEFORE   ${ x }   AFTER "
   *      ^^^^^^^^^^^^  ^^^
   *        prefix     unquote
   */

  import scala.collection.mutable
  import scala.util.boundary
  import scala.util.boundary.break
  def templateString[T](contents: Parser[T]): Parser[Template[T]] =
    Parser { in =>
      boundary {
        val content = in.source.content
        val lastIndex = content.length
        var pos = in.offset

        // results
        val strings = mutable.ListBuffer.empty[String]
        val arguments = mutable.ListBuffer.empty[T]

        // helpers
        def eos: Boolean =
          pos >= lastIndex

        def unquoteStart: Boolean =
          !eos && content.charAt(pos) == '$' && content.charAt(pos + 1) == '{'

        def unquoteEnd: Boolean =
          !eos && content.charAt(pos) == '}'

        def skipUnquoteStart(): Unit =
          pos = pos + 2

        def skipUnquoteEnd(): Unit =
          if (!eos && unquoteEnd) { pos = pos + 1 }
          else break(kiama.parsing.Error("Expected '}' to close splice within string.", Input(in.source, pos)))

        def quote: Boolean =
          content.charAt(pos) == '"'

        def whitespace: Boolean = {
          val ch = content.charAt(pos)
          ch == ' ' || ch == '\t' ||  ch == '\n' ||  ch == '\r'
        }

        def skipWhitespace(): Unit =
          while (whitespace) { pos = pos + 1 }

        def parseQuote(): Unit =
          if (!eos && quote) { pos = pos + 1 }
          else break(Failure("Expected \" to start a string", Input(in.source, pos)))

        def parseStringPart(): Unit =
          val before = pos
          while (!eos && !quote && !unquoteStart) { pos = pos + 1 }
          strings.append(content.substring(before, pos))

        def parseUnquote(): Unit = {
          skipUnquoteStart()
          skipWhitespace()
          contents(Input(in.source, pos)) match {
            case Success(result, next) =>
              arguments.append(result)
              pos = next.offset
            case error: NoSuccess => break(error)
          }
          skipWhitespace()
          skipUnquoteEnd()
        }


        parseQuote()
        parseStringPart()
        while (unquoteStart) {
          parseUnquote()
          if (!quote) parseStringPart()
        }
        parseQuote()
        Success(Template(strings.toList, arguments.toList), Input(in.source, pos))
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
  // TODO add escape sequences
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
