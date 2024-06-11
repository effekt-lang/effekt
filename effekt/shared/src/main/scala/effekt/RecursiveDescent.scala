package effekt

import effekt.lexer.*
import effekt.lexer.TokenKind.{ `::` as PathSep, * }
import effekt.source.*
import kiama.util.{ Positions, Source, StringSource }

import scala.annotation.{ tailrec, targetName }
import scala.util.matching.Regex
import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break


case class ParseError2(message: String, position: Int) extends Throwable(message, null, false, false)

class RecursiveDescentParsers(positions: Positions, tokens: Seq[Token], filename: String = "") {

  import scala.collection.mutable.ListBuffer

  def fail(message: String): Nothing = throw ParseError2(message, position)

  def expect[T](message: String)(p: => T): T =
    try { p } catch { case e: ParseError2 => throw ParseError2(s"Expected $message but failed: ${e.message}", position) }

  // always points to the latest non-space position
  var position: Int = 0

  def peek: Token = tokens(position)

  // Negative lookahead
  def lookbehind(offset: Int): Token =
    tokens(position - offset)

  /**
   * Peeks n tokens ahead of the current one.
   */
  def peek(offset: Int): Token =
    @tailrec
    def go(position: Int, offset: Int): Token =
      if position >= tokens.length then fail("Unexpected end of file")

      tokens(position) match {
        case token if isSpace(token.kind) => go(position + 1, offset)
        case token if offset <= 0 => token
        case _ => go(position + 1, offset - 1)
      }

    go(position, offset)

  def peek(kind: TokenKind): Boolean =
    peek.kind == kind
  def peek(offset: Int, kind: TokenKind): Boolean =
    peek(offset).kind == kind

  def hasNext(): Boolean = position < tokens.length
  def next(): Token =
    val t = tokens(position)
    skip()
    t

  /**
   * Skips the current token and then all subsequent whitespace
   */
  def skip(): Unit = { position += 1; spaces() }

  def isSpace(kind: TokenKind): Boolean =
    kind match {
      case TokenKind.Space | TokenKind.Comment(_) | TokenKind.Newline => true
      case _ => false
    }

  @tailrec
  final def spaces(): Unit = if hasNext() then peek.kind match {
    case kind if isSpace(kind) => position += 1; spaces()
    case _ => ()
  }

  def consume(kind: TokenKind): Unit =
    // if !hasNext() then fail(s"Expected ${kind}, but reached end of file")
    val t = next()
    if (t.kind != kind) fail(s"Expected ${kind}, but got ${t}")

  /**
   * Guards `thn` by token `t` and consumes the token itself, if present.
   */
  inline def when[T](t: TokenKind)(thn: => T)(els: => T): T =
    if peek(t) then { consume(t); thn } else els

  inline def backtrack[T](p: => T): Option[T] =
    val before = position
    try { Some(p) } catch {
      case ParseError2(_, _) => position = before; None
    }

  /**
   * Tiny combinator DSL to sequence parsers
   */
  case class ~[+T, +U](_1: T, _2: U) {
    override def toString = s"(${_1}~${_2})"
  }

  extension [A](self: A) {
    @targetName("seq")
    inline def ~[B](other: B): (A ~ B) = new ~(self, other)
    inline def <~(t: TokenKind): A = { consume(t); self }
    inline def |(other: A): A = { backtrack(self).getOrElse(other) }
  }

  extension (self: TokenKind) {
    @targetName("seqRightToken")
    inline def ~>[R](other: => R): R = { consume(self); other }
  }

  extension (self: Unit) {
    @targetName("seqRightUnit")
    inline def ~>[R](other: => R): R = { other }
  }

  // tokens that delimit a statement
  def returnPosition: Boolean = peek(`}`) || peek(`case`) || peek(`}>`) || peek(EOF)

  /**
   * Statements
   */
  def stmts(): Stmt = peek.kind match {
    case `val`  => valStmt()
    case _ if isDefinition => DefStmt(definition(), semi() ~> stmts())
    case `with` => withStmt()
    case `var`  => DefStmt(varDef(), semi() ~> stmts())
    case `return` =>
      val result = `return` ~> Return(expr())
      maybeSemi()
      result
    case _ =>
      val e = expr()
      semi()
      if returnPosition then Return(e)
      else ExprStmt(e, stmts())
  }

  // ATTENTION: here the grammar changed (we added `with val` to disambiguate)
  // with val <ID> (: <TYPE>)? = <EXPR>; <STMTS>
  // with val (<ID> (: <TYPE>)?...) = <EXPR>
  // with <EXPR>; <STMTS>
  def withStmt(): Stmt = `with` ~> peek.kind match {
    case `val` => ???
    case _ => expr() ~ (semi() ~> stmts()) match {
       case m@MethodCall(receiver, id, tps, vargs, bargs) ~ body =>
         Return(MethodCall(receiver, id, tps, vargs, bargs :+ (BlockLiteral(Nil, Nil, Nil, body))))
       case c@Call(callee, tps, vargs, bargs) ~ body =>
         Return(Call(callee, tps, vargs, bargs :+ (BlockLiteral(Nil, Nil, Nil, body))))
       case Var(id) ~ body =>
         val tgt = IdTarget(id)
         Return(Call(tgt, Nil, Nil, (BlockLiteral(Nil, Nil, Nil, body)) :: Nil))
       case term ~ body =>
         Return(Call(ExprTarget(term), Nil, Nil, (BlockLiteral(Nil, Nil, Nil, body)) :: Nil))
    }
  }

  def maybeSemi(): Unit = if isSemi then semi()
  def isSemi: Boolean = peek.kind match {
    // \n   ; while
    //
    case `;` => true
    // foo }
    //     ^
    case t if returnPosition => true

    // \n   while
    //      ^
    case _ => lookbehind(1).kind == Newline
  }
  def semi(): Unit = peek.kind match {
    // \n   ; while
    //
    case `;` => consume(`;`)
    // foo }
    //     ^
    case t if returnPosition => ()

    // \n   while
    //      ^
    case _ if lookbehind(1).kind == Newline => ()

    case _ => fail("Expected ;")
  }

  def stmt(): Stmt =
    if peek(`{`) then braces { BlockStmt(stmts()) }
    else when(`return`) { Return(expr()) } { Return(expr()) }

  /**
   * Main entry point
   */
   def program(): ModuleDecl =
     // skip spaces at the start
     spaces()
     val res = ModuleDecl(moduleDecl(), manyWhile(includeDecl(), `import`), toplevels())
     if peek(`EOF`) then res else fail("Unexpected input")
     // failure("Required at least one top-level function or effect definition")

  def moduleDecl(): String =
    when(`module`) { moduleName() } { defaultModulePath }

  // we are purposefully not using File here since the parser needs to work both
  // on the JVM and in JavaScript
  def defaultModulePath: String =
    val baseWithExt = filename.split("[\\\\/]").last
    baseWithExt.split('.').head


  def includeDecl(): Include =
    Include(`import` ~> moduleName())

  def moduleName(): String =
    some(ident, `/`).mkString("/")

  def isToplevel: Boolean = peek.kind match {
    case `val` | `fun` | `def` | `type` | `effect` | `namespace` |
         `extern` | `effect` | `interface` | `type` | `record` => true
    case _ => false
  }

  def toplevel(): Def = peek.kind match {
    case `val`       => valDef()
    case `def`       => defDef()
    case `interface` => interfaceDef()
    case `type`      => typeOrAliasDef()
    case `record`    => recordDef()
    case `extern`    => externDef()
    case `effect`    => effectOrOperationDef()
    case `namespace` => namespaceDef()
    case `var`       => fail("Mutable variable declarations are currently not supported on the toplevel.")
    case _ => fail("Expected a top-level definition")
  }

  def toplevels(): List[Def] = manyWhile(toplevel(), isToplevel)

  def isDefinition: Boolean = peek.kind match {
    case `val` | `fun` | `def` | `type` | `effect` | `namespace` => true
    case `extern` | `effect` | `interface` | `type` | `record` =>
      val kw = peek.kind
      fail(s"Only supported on the toplevel: ${kw.toString} declaration.")
    case _ => false
  }

  def definition(): Def = peek.kind match {
    case `val`       => valDef()
    case `def`       => defDef()
    case `type`      => typeOrAliasDef()
    case `effect`    => effectDef()
    case `namespace` => namespaceDef()
    // TODO
    //     (`extern` | `effect` | `interface` | `type` | `record`).into { (kw: String) =>
    //        failure(s"Only supported on the toplevel: ${kw} declaration.")
    //      }
    case _ => fail("Expected definition")
  }

  def definitions(): List[Def] = manyWhile(definition(), isDefinition)

  def functionBody: Stmt = expect("the body of a function definition")(stmt())

  def valDef(): Def =
    ValDef(`val` ~> idDef(), maybeTypeAnnotation(), `=` ~> stmt())

  /**
   * In statement position, val-definitions can also be destructing:
   *   i.e. val (l, r) = point(); ...
   */
  def valStmt(): Stmt =
    def simpleLhs() = backtrack {
      `val` ~> idDef() ~ maybeTypeAnnotation() <~ `=`
    } map {
      case id ~ tpe => DefStmt(ValDef(id, tpe, stmt()), { semi(); stmts() })
    }
    def matchLhs() =
      `val` ~> matchPattern() ~ manyWhile(`and` ~> matchGuard(), `and`) <~ `=` match {
        case AnyPattern(id) ~ Nil => DefStmt(ValDef(id, None, stmt()), { semi(); stmts() })
        case p ~ guards =>
          val sc = expr()
          val default = when(`else`) { Some(stmt()) } { None }
          val body = semi() ~> stmts()
          Return(Match(sc, List(MatchClause(p, guards, body)), default))
      }

    simpleLhs() getOrElse matchLhs()


  def varDef(): Def =
      (`var` ~> idDef()) ~ maybeTypeAnnotation() ~ when(`in`) { Some(idRef()) } { None } ~ (`=` ~> stmt()) match {
        case id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr)
        case id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr)
      }

  def defDef(): Def =
    val id = consume(`def`) ~> idDef()

    def isBlockDef: Boolean = peek(`:`) || peek(`=`)

    if isBlockDef then
      // (: <VALUETYPE>)? `=` <EXPR>
      DefDef(id, when(`:`) { Some(blockType()) } { None }, `=` ~> expr())
    else
      // [...](<PARAM>...) {...} `=` <STMT>>
      val (tps, vps, bps) = params()
      FunDef(id, tps, vps, bps, maybeReturnAnnotation(), `=` ~> stmt())


  // right now: data type definitions (should be renamed to `data`) and type aliases
  def typeOrAliasDef(): Def =
    val id ~ tps = (`type` ~> idDef()) ~ maybeTypeParams()

    next().kind match {
      case `=` => TypeDef(id, tps, valueType())
      case `{` if peek(`}`) => DataDef(id, tps, Nil)
      case `{` => DataDef(id, tps, some(constructor, `;`) <~ `}`)
      case _ => ??? // TODO error message
    }

  def recordDef(): Def =
    RecordDef(`record` ~> idDef(), maybeTypeParams(), valueParams())

  def constructor(): Constructor =
    Constructor(idDef(), maybeTypeParams(), valueParams())

  // On the top-level both
  //    effect Foo = {}
  // and
  //    effect Foo(): Int
  // are allowed. Here we simply backtrack, since effect definitions shouldn't be
  // very long and cannot be nested.
  def effectOrOperationDef(): Def =
    backtrack { effectDef() } getOrElse { operationDef() }

  def effectDef(): Def =
    // effect <NAME> = <EFFECTS>
    EffectDef(`effect` ~> idDef(), maybeTypeParams(), `=` ~> effects())

  // effect <NAME>[...](...): ...
  def operationDef(): Def =
    `effect` ~> operation() match {
      case op =>
        // TODO is the `true` flag used at all anymore???
        InterfaceDef(IdDef(op.id.name), Nil, List(op), true)
    }

  def operation(): Operation =
    idDef() ~ params() ~ (`:` ~> effectful()) match {
      case id ~ (tps, vps, bps) ~ ret => Operation(id, tps, vps, bps, ret)
    }

  def interfaceDef(): InterfaceDef =
    InterfaceDef(`interface` ~> idDef(), maybeTypeParams(), `{` ~> manyWhile(`def` ~> operation(), `def`) <~ `}`, true)

  def namespaceDef(): Def =
    consume(`namespace`)
    val id = idDef()
    // namespace foo { <DEFINITION>* }
    if peek(`{`) then braces { NamespaceDef(id, definitions()) }
    // namespace foo
    // <DEFINITION>*
    else { semi(); NamespaceDef(id, definitions()) }


  def externDef(): Def = { peek(`extern`); peek(1).kind } match {
    case `type`      => externType()
    case `interface` => externInterface()
    case `resource`  => externResource()
    case `include`   => externInclude()
    case s: Str      => externString()
    case _ => externFun()
  }

  def externType(): Def =
    ExternType(`extern` ~> `type` ~> idDef(), maybeTypeParams())
  def externInterface(): Def =
    ExternInterface(`extern` ~> `interface` ~> idDef(), maybeTypeParams())
  def externResource(): Def =
    ExternResource(`extern` ~> `resource` ~> idDef(), `:` ~> blockType())
  def externInclude(): Def =
    ExternInclude(`extern` ~> `include` ~> path())

  def externString(): Def =
    consume(`extern`)
    next().kind match {
      case Str(contents, _) => ExternInclude("", Some(contents))
      case _ => fail("Expected string literal")
    }

  def externFun(): Def =
    (`extern` ~> maybeExternCapture()) ~ (`def` ~> idDef()) ~ params() ~ (`:` ~> effectful()) ~ (`=` ~> externBody()) match {
      case capt ~ id ~ (tps, vps, bps) ~ ret ~ body => ExternDef(capt, id, tps, vps, bps, ret, body)
    }

  def externBody(): Template[Term] =
    val first = string()
    val (exprs, strs) = manyWhile((`${` ~> expr() <~ `}`, string()), `${`).unzip
    Template(first :: strs, exprs)

  def maybeExternCapture(): CaptureSet =
    if peek(`{`) || peek(`pure`) || isVariable then externCapture()
    else CaptureSet(List(IdRef(List("effekt"), "io")))

  def externCapture(): CaptureSet =
    if peek(`{`) then captureSet()
    else if peek(`pure`) then `pure` ~> CaptureSet(Nil)
    else CaptureSet(List(idRef()))

  def path(): String = next().kind match {
    case Str(s, false) => s
    case _ => fail("Expected path as string literal.")
  }

  def string(): String = next().kind match {
    case Str(s, _) => s
    case _ => fail("Expected string literal.")
  }

  // TODO uncomment after rebase:
  //  def featureFlag(): FeatureFlag =
  //    ident() match {
  //      case "default" => FeatureFlag.Default
  //      case flag => FeatureFlag.NamedFeatureFlag(flag)
  //    }

  def maybeTypeAnnotation(): Option[ValueType] =
    if peek(`:`) then Some(typeAnnotation()) else None

  def maybeReturnAnnotation(): Option[Effectful] =
    when(`:`) { Some(effectful()) } { None }

  // TODO fail "Expected a type annotation"
  def typeAnnotation(): ValueType = `:` ~> valueType()



  def expr(): Term = peek.kind match {
    case _ => matchExpr()
  }

  def ifExpr(): Term =
    If(`if` ~> parens { matchGuards() },
      stmt(),
      when(`else`) { stmt() } { Return(UnitLit()) })

  def whileExpr(): Term =
    While(`while` ~> parens { matchGuards() },
      stmt(),
      when(`else`) { Some(stmt()) } { None })

  def doExpr(): Term =
    (`do` ~> idRef()) ~ arguments() match {
      case id ~ (targs, vargs, bargs) => Do(None, id, targs, vargs, bargs)
    }

  /*
  <tryExpr> ::= try { <stmts> } <handler>+
  <handler> ::= with (<idDef> :)? <implementation>
  <implementation ::= <interfaceType> { <opClause>+ }
  */
  def tryExpr(): Term =
    `try` ~> stmt() ~ someWhile(handler(), `with`) match {
      case s ~ hs => TryHandle(s, hs)
    }

  def regionExpr(): Term = Region(`region` ~> idDef(), stmt())

  def boxExpr(): Term = {
    val captures = `box` ~> backtrack(captureSet())
    val block = if (peek(`{`)) functionArg()
    else Var(idRef())
    Box(captures, block)
  }

  def unboxExpr(): Term = Unbox(`unbox` ~> expr())

  def newExpr(): Term = New(`new` ~> implementation())

  def handler(): Handler =
    `with` ~> backtrack(idDef() <~ `:`) ~ implementation() match {
      case capabilityName ~ impl => {
        val capability = capabilityName map { name => BlockParam(name, impl.interface): BlockParam }
        Handler(capability, impl)
      }
    }

  // This nonterminal uses limited backtracking: It parses the interface type multiple times.
  def implementation(): Implementation = {

    // Interface[...] {}
    def emptyImplementation() = backtrack { Implementation(interfaceType(), `{` ~> Nil <~ `}`) }

    // Interface[...] { def <NAME> = ... }
    def interfaceImplementation() = backtrack {
      val tpe = interfaceType()
      consume(`{`)
      if !peek(`def`) then fail("Expected at least one operation definition to implement this interface.")
      tpe
    } map { tpe =>
      Implementation(tpe, manyWhile(opClause(), `def`)) <~ `}`
    }

    // Interface[...] { () => ... }
    // Interface[...] { case ... => ... }
    def operationImplementation() = idRef() ~ maybeTypeParams() ~ functionArg() match {
      case (id ~ tps ~ BlockLiteral(_, vps, bps, body)) =>
        val synthesizedId = IdRef(Nil, id.name)
        val interface = BlockTypeRef(id, Nil): BlockTypeRef
        Implementation(interface, List(OpClause(synthesizedId, tps, vps, bps, None, body, implicitResume)))
    }

    emptyImplementation() orElse interfaceImplementation() getOrElse operationImplementation()
  }

  def opClause(): OpClause =
    (`def` ~> idRef()) ~ paramsOpt() ~ maybeReturnAnnotation() ~ (`=` ~> stmt()) match {
      case id ~ (tps, vps, bps) ~ ret ~ body =>
        // TODO the implicitResume needs to have the correct position assigned (maybe move it up again...)
        OpClause(id, tps, vps, bps, ret, body, implicitResume)
    }

  def implicitResume: IdDef = IdDef("resume")

  def matchClause(): MatchClause =
    MatchClause(`case` ~> matchPattern(), manyWhile(`and` ~> matchGuard(), `and`), `=>` ~> stmts())

  def matchGuards() = some(matchGuard, `and`)

  def matchGuard(): MatchGuard =
    expr() ~ when(`is`) { Some(matchPattern()) } { None } match {
      case e ~ Some(p) => MatchGuard.PatternGuard(e, p)
      case e ~ None    => MatchGuard.BooleanGuard(e)
    }

  def matchPattern(): MatchPattern = peek.kind match {
    case Ident("_") => skip(); IgnorePattern()
    case `(` => some(matchPattern, `(`, `,`, `)`) match {
      case p :: Nil => fail("Pattern matching on tuples requires more than one element")
      case ps => TagPattern(IdRef(List("effekt"), s"Tuple${ps.size}"), ps)
    }
    case _ if isVariable && peek(1, `(`) =>
      TagPattern(idRef(), many(matchPattern, `(`, `,`, `)`))
    case _ if isVariable =>
      AnyPattern(idDef())
    case _ if isLiteral => LiteralPattern(literal())
    case _ => fail("Expected pattern")
  }

  def matchExpr(): Term =
    var sc = assignExpr()
    while (peek(`match`)) {
       val clauses = `match` ~> braces { manyWhile(matchClause(), `case`) }
       val default = when(`else`) { Some(stmt()) } { None }
       sc = Match(sc, clauses, default)
    }
    sc

  def assignExpr(): Term = orExpr() match {
    case x @ Term.Var(id) => when(`=`) { Assign(id, expr()) } { x }
    case other => other
  }

  def orExpr(): Term = infix(andExpr, `||`)
  def andExpr(): Term = infix(eqExpr, `&&`)
  def eqExpr(): Term = infix(relExpr, `===`, `!==`)
  def relExpr(): Term = infix(addExpr, `<=`, `>=`, `<`, `>`)
  def addExpr(): Term = infix(mulExpr, `++`, `+`, `-`)
  def mulExpr(): Term = infix(callExpr, `*`, `/`)

  inline def infix(nonTerminal: () => Term, ops: TokenKind*): Term =
    var left = nonTerminal()
    while (ops.contains(peek.kind)) {
       val op = next().kind
       val right = nonTerminal()
       left = binaryOp(left, op, right)
    }
    left

  // === AST Helpers ===

  private def binaryOp(lhs: Term, op: TokenKind, rhs: Term): Term =
    // TODO after rebasing!
    // thunkedBinaryOp(lhs, op, rhs) = op { lhs } { rhs }
    //    if op == `||` || op == `&&` then
    //      Call(IdTarget(IdRef(Nil, opName(op))), Nil, Nil, List(BlockLiteral(Nil, Nil, Nil, Return(lhs)), BlockLiteral(Nil, Nil, Nil, Return(rhs))))
    //    else
    //      Call(IdTarget(IdRef(Nil, opName(op))), Nil, List(lhs, rhs), Nil)
    Call(IdTarget(IdRef(Nil, opName(op))), Nil, List(lhs, rhs), Nil)

  private def opName(op: TokenKind): String = op match {
    case `||` => "infixOr"
    case `&&` => "infixAnd"
    case `===` => "infixEq"
    case `!==` => "infixNeq"
    case `<` => "infixLt"
    case `>` => "infixGt"
    case `<=` => "infixLte"
    case `>=` => "infixGte"
    case `+` => "infixAdd"
    case `-` => "infixSub"
    case `*` => "infixMul"
    case `/` => "infixDiv"
    case `++` => "infixConcat"
    case _ => sys.error(s"Internal compiler error: not a valid operator ${op}")
  }

  private def TupleTypeTree(tps: List[ValueType]): ValueType =
    ValueTypeRef(IdRef(List("effekt"), s"Tuple${tps.size}"), tps)

  /**
   * This is a compound production for
   *  - member selection <EXPR>.<NAME>
   *  - method calls <EXPR>.<NAME>(...)
   *  - function calls <EXPR>(...)
   *
   * This way expressions like `foo.bar.baz()(x).bam.boo()` are
   * parsed with the correct left-associativity.
   */
  def callExpr(): Term = {
    var e = primExpr()

    while (peek(`.`) || isArguments)
      peek.kind match {
        // member selection (or method call)
        //   <EXPR>.<NAME>
        // | <EXPR>.<NAME>( ... )
        case `.` =>
          consume(`.`)
          val member = idRef()
          // method call
          if (isArguments) {
            val (targs, vargs, bargs) = arguments()
            e = Term.MethodCall(e, member, targs, vargs, bargs)
          } else {
            e = Term.MethodCall(e, member, Nil, Nil, Nil)
          }

        // function call
        case _ if isArguments =>
          val callee = e match {
            case Term.Var(id) => IdTarget(id)
            case other => ExprTarget(other)
          }
          val (targs, vargs, bargs) = arguments()
          e = Term.Call(callee, targs, vargs, bargs)

        // nothing to do
        case _ => ()
      }

    e
  }

  def isArguments: Boolean = peek(`(`) || peek(`[`) || peek(`{`)
  def arguments(): (List[ValueType], List[Term], List[Term]) =
    if (!isArguments) fail("Expected at least one argument section (types, values, or blocks)")
    (maybeTypeArgs(), maybeValueArgs(), maybeBlockArgs())

  def maybeTypeArgs(): List[ValueType] = if peek(`[`) then typeArgs() else Nil
  def maybeValueArgs(): List[Term] = if peek(`(`) then valueArgs() else Nil
  def maybeBlockArgs(): List[Term] = if peek(`{`) then blockArgs() else Nil

  def typeArgs(): List[ValueType] = some(valueType, `[`, `,`, `]`)
  def valueArgs(): List[Term] = many(expr, `(`, `,`, `)`)
  def blockArgs(): List[Term] = someWhile(blockArg(), `{`)

  /**
   * Note: for this nonterminal, we need some backtracking.
   */
  def blockArg(): Term =
    backtrack { `{` ~> Var(idRef()) <~ `}` } getOrElse { functionArg() }

  def functionArg(): BlockLiteral = braces {
    peek.kind match {
      // { case ... => ... }
      case `case` => someWhile(matchClause(), `case`) match { case cs =>
        // TODO positions should be improved here and fresh names should be generated for the scrutinee
        // also mark the temp name as synthesized to prevent it from being listed in VSCode
        val name = "__tmpRes"
        BlockLiteral(
          Nil,
          List(ValueParam(IdDef(name), None)),
          Nil,
          Return(Match(Var(IdRef(Nil, name)), cs, None))) : BlockLiteral
      }
      case _ =>
        // { (x: Int) => ... }
        backtrack { lambdaParams() <~ `=>` } map {
          case (tps, vps, bps) => BlockLiteral(tps, vps, bps, stmts()) : BlockLiteral
        } getOrElse {
          // { <STMTS> }
          BlockLiteral(Nil, Nil, Nil, stmts()) : BlockLiteral
        }
    }
  }

  def primExpr(): Term = peek.kind match {
    case `if`     => ifExpr()
    case `while`  => whileExpr()
    case `try`    => tryExpr()
    case `region` => regionExpr()
    case `box`    => boxExpr()
    case `unbox`  => unboxExpr()
    case `new`    => newExpr()
    case `do`                => doExpr()
    case _ if isLiteral      => literal()
    case _ if isVariable     => variable()
    case _ if isHole         => hole()
    case _ if isTupleOrGroup => tupleOrGroup()
    case _ if isListLiteral  => listLiteral()
    case _ => fail(s"Expected variables, literals, tuples, lists, holes or group")
  }

  def isListLiteral: Boolean = peek.kind match {
    case `[` => true
    case _ => false
  }
  def listLiteral(): Term =
    many(expr, `[`, `,`, `]`).foldRight(NilTree) { ConsTree }

  private def NilTree: Term =
    Call(IdTarget(IdRef(List(), "Nil")), Nil, Nil, Nil)

  private def ConsTree(el: Term, rest: Term): Term =
    Call(IdTarget(IdRef(List(), "Cons")), Nil, List(el, rest), Nil)

  def isTupleOrGroup: Boolean = peek(`(`)
  def tupleOrGroup(): Term =
    some(expr, `(`, `,`, `)`) match {
      case e :: Nil => e
      case xs => Call(IdTarget(IdRef(List("effekt"), s"Tuple${xs.size}")), Nil, xs.toList, Nil)
    }

  def isHole: Boolean = peek(`<>`) || peek(`<{`)
  def hole(): Term = peek.kind match {
    case `<>` => `<>` ~> Hole(Return(UnitLit()))
    case `<{` => `<{` ~> Hole(stmts()) <~ `}>`
    case _ => fail("Expected hole")
  }

  def isLiteral: Boolean = peek.kind match {
    case _: (Integer | Float | Str) => true
    case `true` => true
    case `false` => true
    case _ => isUnitLiteral
  }
  def literal(): Literal = peek.kind match {
    case Integer(v)         => skip(); IntLit(v)
    case Float(v)           => skip(); DoubleLit(v)
    case Str(s, multiline)  => skip(); StringLit(s)
    case `true`             => skip(); BooleanLit(true)
    case `false`            => skip(); BooleanLit(false)
    case t if isUnitLiteral => skip(); skip(); UnitLit()
    case t => fail("Expected a literal")
  }

  // Will also recognize ( ) as unit if we do not emit space in the lexer...
  private def isUnitLiteral: Boolean = peek(`(`) && peek(1, `)`)

  def isVariable: Boolean = isIdRef
  def variable(): Term = Var(idRef())

  def isIdRef: Boolean = isIdent

  def idRef(): IdRef = some(ident, PathSep) match {
    case ids => IdRef(ids.init, ids.last)
  }

  def idDef(): IdDef = IdDef(ident())

  def isIdent: Boolean = peek.kind match {
    case Ident(id) => true
    case _ => false
  }
  def ident(): String = next().kind match {
    case Ident(id) => id
    case _ => fail(s"Expected identifier")
  }


  /**
   * Types
   */

  def valueType(): ValueType = valueType2(true)

  /**
   * Uses backtracking!
   *
   * This is not very efficient. To parse a value type, we first parse a block type,
   * just to see that it either is no blocktype or it is not followed by an `at`
   * and just "looked" like a block type...
   *
   * The parameter [[boxedAllowed]] controls whether on the right a dangling `at`
   * can occur. This way we prevent parsing `() => S at {} at {}` and force users
   * to manually parenthesize.
   */
  private def valueType2(boxedAllowed: Boolean): ValueType = {
    def boxedBlock = backtrack {
      BoxedType(blockType2(false), `at` ~> captureSet())
    }
    if (boxedAllowed) { boxedBlock getOrElse atomicValueType() }
    else atomicValueType()
  }

  def atomicValueType(): ValueType = peek.kind match {
    case `(` => some(valueType, `(`, `,`, `)`) match {
      case tpe :: Nil => tpe
      case tpes => TupleTypeTree(tpes)
    }
    case _ => ValueTypeRef(idRef(), maybeTypeArgs())
  }


  /**
   * Uses backtracking!
   *
   * TODO improve errors
   *   i.e. fail("Expected either a function type (e.g., (A) => B / {E} or => B) or an interface type (e.g., State[T]).")
   */
  def blockType(): BlockType = blockType2(true)
  private def blockType2(boxedAllowed: Boolean): BlockType =

    def simpleFunType = backtrack {
      ValueTypeRef(idRef(), maybeTypeArgs()) <~ `=>`
    } map { tpe =>
      FunctionType(Nil, List(tpe), Nil, valueType2(boxedAllowed), maybeEffects())
    }

    def funType = backtrack {
      maybeTypeParams() ~ maybeValueTypes() ~ (maybeBlockTypeParams() <~ `=>`) ~ valueType2(boxedAllowed) ~ maybeEffects() match {
        case tparams ~ vparams ~ bparams ~ t ~ effs => FunctionType(tparams, vparams, bparams, t, effs)
      }
    }
    def parenthesized = backtrack { parens { blockType() } }

    simpleFunType orElse funType orElse parenthesized getOrElse interfaceType()

  def interfaceType(): BlockTypeRef =
    BlockTypeRef(idRef(), maybeTypeArgs()): BlockTypeRef
    // TODO error "Expected an interface type"

  def maybeTypeParams(): List[Id] = if peek(`[`) then typeParams() else Nil

  def typeParams(): List[Id] = some(idDef, `[`, `,`, `]`)

  def maybeBlockTypeParams(): List[(Option[IdDef], BlockType)] = if peek(`{`) then blockTypeParams() else Nil

  def blockTypeParams(): List[(Option[IdDef], BlockType)] = someWhile(blockTypeParam(), `{`)

  def blockTypeParam(): (Option[IdDef], BlockType) =
    braces { (backtrack { idDef() <~ `:` }, blockType()) }

  /*
  naming convention?:
    maybe[...]s: zero or more times
    [...]s: one or more times
    [...]opt: optional type annotation
  */

  def lambdaParams(): (List[Id], List[ValueParam], List[BlockParam]) =
    if isVariable then (Nil, List(ValueParam(idDef(), None)), Nil)  else paramsOpt()

  def params(): (List[Id], List[ValueParam], List[BlockParam]) =
    maybeTypeParams() ~ maybeValueParams() ~ maybeBlockParams() match {
      case tps ~ vps ~ bps => (tps, vps, bps)
    }

  def paramsOpt(): (List[Id], List[ValueParam], List[BlockParam]) =
    maybeTypeParams() ~ maybeValueParamsOpt() ~ maybeBlockParams() match {
      case (tps ~ vps ~ bps) =>
        // fail("Expected a parameter list (multiple value parameters or one block parameter; only type annotations of value parameters can be currently omitted)")
        (tps, vps, bps)
    }

  def maybeValueParamsOpt(): List[ValueParam] =
    if peek(`(`) then valueParamsOpt() else Nil

  def valueParamsOpt(): List[ValueParam] =
    many(valueParamOpt, `(`, `,`, `)`)

  def maybeValueParams(): List[ValueParam] =
    if peek(`(`) then valueParams() else Nil

  def valueParams(): List[ValueParam] =
    many(valueParam, `(`, `,`, `)`)

  def valueParam(): ValueParam =
    ValueParam(idDef(), Some(`:` ~> valueType()))

  def valueParamOpt(): ValueParam =
    ValueParam(idDef(), when(`:`) { Some(valueType()) } { None })

  def maybeBlockParams(): List[BlockParam] =
    manyWhile(`{` ~> blockParam() <~ `}`, `{`)

  def blockParams(): List[BlockParam] =
    someWhile(`{` ~> blockParam() <~ `}`, `{`)

  def blockParam(): BlockParam =
    BlockParam(idDef(), `:` ~> blockType())

  // TODO this needs to be implemented, once the PR is rebased onto master
  //  def blockParamOpt: BlockParam =
  //    BlockParam(idDef(), when(`:`)(Some(blockType()))(None))


  def maybeValueTypes(): List[ValueType] =
    if peek(`(`) then valueTypes() else Nil

  def valueTypes(): List[ValueType] = many(valueType, `(`, `,`, `)`)

  def captureSet(): CaptureSet = CaptureSet(many(idRef, `{`, `,` , `}`))

  def effectful(): Effectful = Effectful(valueType(), maybeEffects())

  def maybeEffects(): Effects = when(`/`) { effects() } { Effects.Pure }

  // TODO error "Expected an effect set"
  def effects(): Effects =
    if peek(`{`) then Effects(many(interfaceType, `{`, `,`, `}`))
    else Effects(interfaceType())



  /**
   * Helpers
   */


  /**
   * Repeats [[p]], separated by [[sep]] enclosed by [[before]] and [[after]]
   */
  inline def some[T](p: () => T, before: TokenKind, sep: TokenKind, after: TokenKind): List[T] =
    consume(before)
    val res = some(p, sep)
    consume(after)
    res

  inline def some[T](p: () => T, sep: TokenKind): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    components += p()
    while (peek(sep)) {
      consume(sep)
      components += p()
    }
    components.toList

  inline def someWhile[T](p: => T, lookahead: TokenKind): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    components += p
    while (peek(lookahead)) {
      components += p
    }
    components.toList

  inline def manyWhile[T](p: => T, lookahead: TokenKind): List[T] =
    manyWhile(p, peek(lookahead))

  inline def manyWhile[T](p: => T, predicate: => Boolean): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    while (predicate) {
      components += p
    }
    components.toList

  inline def parens[T](p: => T): T =
    consume(`(`)
    val res = p
    consume(`)`)
    res

  inline def braces[T](p: => T): T =
    consume(`{`)
    val res = p
    consume(`}`)
    res

  inline def many[T](p: () => T, before: TokenKind, sep: TokenKind, after: TokenKind): List[T] =
    consume(before)
    if (peek(after)) {
      consume(after)
      Nil
    } else {
      val components: ListBuffer[T] = ListBuffer.empty
      components += p()
      while (peek(sep)) {
        consume(sep)
        components += p()
      }
      consume(after)
      components.toList
    }
}
