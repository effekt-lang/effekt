package effekt

import effekt.lexer.*
import effekt.lexer.TokenKind.{ `::` as PathSep, * }

import effekt.source.*

import kiama.util.Positions

import scala.annotation.{ tailrec, targetName }
import scala.util.matching.Regex
import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break


case class ParseError2(message: String, position: Int) extends Throwable(message, null, false, false)

class RecursiveDescentParsers(positions: Positions, tokens: Seq[Token]) {

  import scala.collection.mutable.ListBuffer

  def fail(message: String): Nothing = throw ParseError2(message, position)

  def expect[T](message: String)(p: => T): T =
    try { p } catch { case e: ParseError2 => throw ParseError2(s"Expected $message but failed: ${e.message}", position) }

  // always points to the latest non-space position
  var position: Int = 0

  def peek: Token = tokens(position)
  def peek(offset: Int): Token = tokens(position + offset)
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

  @tailrec
  final def spaces(): Unit = if hasNext() then peek.kind match {
    case TokenKind.Space => position += 1; spaces()
    case TokenKind.Comment(_) => position += 1; spaces()
    case TokenKind.Newline => position += 1; spaces()
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

  /**
   * Statements
   */
  def stmts(): Stmt = peek.kind match {
    case _ if isDefinition => DefStmt(definition(), semi() ~> stmts())
    case `with` => withStmt()
    case `var`  => DefStmt(varDef(), semi() ~> stmts())
    case `return` => `return` ~> Return(expr())
    case _ =>
      val e = expr()
      val returnPosition = peek(`}`) || peek(`case`) || peek(EOF) // TODO EOF is just for testing
      if returnPosition then Return(e)
      else ExprStmt(e, { semi(); stmts() })
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

  def semi(): Unit = peek.kind match {
    // \n   ; while
    //
    case `;` => consume(`;`)
    // foo }
    //     ^
    case `}` | `case` => ()

    // \n   while
    //      ^
    case _ if peek(-1, Newline) => ()

    case _ => fail("Expected ;")
  }

  def stmt(): Stmt =
    if peek(`{`) then braces { stmts() }
    else when(`return`) { Return(expr()) } { Return(expr()) }

  def isDefinition: Boolean = peek.kind match {
    case `val` | `fun` | `def` | `type` | `effect` | `namespace` => true
    case `extern` | `effect` | `interface` | `type` | `record` =>
      val kw = peek.kind
      fail(s"Only supported on the toplevel: ${kw.toString} declaration.")
    case _ => false
  }

  def definition(): Def = peek.kind match {
    case `val` => valDef()
    case _ => fail("Expected definition")
  }

  def functionBody: Stmt = expect("the body of a function definition")(stmt())

  // TODO matchdef
  //  lazy val matchDef: P[Stmt] =
  //     `val` ~> matchPattern ~ many(`and` ~> matchGuard) ~ (`=` ~/> expr) ~ (`else` ~> stmt).? ~ (`;;` ~> stmts) ^^ {
  //       case p ~ guards ~ sc ~ default ~ body =>
  //        Return(Match(sc, List(MatchClause(p, guards, body)), default)) withPositionOf p
  //     }
  def valDef(): Def =
    ValDef(`val` ~> idDef(), maybeTypeAnnotation(), `=` ~> stmt())

  def varDef(): Def =
    (`var` ~> idDef()) ~ maybeTypeAnnotation() ~ when(`in`) { Some(idRef()) } { None } ~ (`=` ~> stmt()) match {
      case id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr)
      case id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr)
    }

  def maybeTypeAnnotation(): Option[ValueType] =
    if peek(`:`) then Some(typeAnnotation()) else None

  // TODO fail "Expected a type annotation"
  def typeAnnotation(): ValueType = `:` ~> valueType()

  def expr(): Term = peek.kind match {
    case `if`     => ifExpr()
    case `while`  => whileExpr()
    case `do`     => doExpr()
    case `try`    => tryExpr()
    case `region` => regionExpr()
    case `box`    => boxExpr()
    case `unbox`  => unboxExpr()
    case `new`    => newExpr()
    case _ => orExpr()
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
  <tryExpr> ::= try <stmts> <handler>+
  <handler> ::= with (<idDef> :)? <implementation>
  <implementation ::= <interfaceType> { <opClause>+ }
  */
  def tryExpr(): Term =
    `try` ~> stmt() ~ someWhile(handler(), `with`) match {
      case s ~ hs => TryHandle(s, hs)
    }

  def regionExpr(): Term = Region(`region` ~> idDef(), stmt())

  def boxExpr(): Term = ???

  def unboxExpr(): Term = Unbox(`unbox` ~> expr())

  def newExpr(): Term = New(`new` ~> implementation())

  def handler(): Handler =
    `with` ~> backtrack(idDef() <~ `:`) ~ implementation() match {
      case capabilityName ~ impl => {
        val capability = capabilityName map { name => BlockParam(name, impl.interface): BlockParam }
        Handler(capability, impl)
      }
    }

  def implementation(): Implementation = {
    backtrack(interfaceType()) match {
      // Interface[...] { def ... = { ... } def ... = { ... } }
      // Interface[...] {}
      case Some(intType) => Implementation(intType, manyWhile(opClause(), `def`))
      // Interface[...] { (...) { ... } => ... }
      case None => idRef() ~ maybeTypeParams() ~ functionArg() match {
        case id ~ tps ~ BlockLiteral(_, vps, bps, body) =>
          val synthesizedId = IdRef(Nil, id.name)
          val interface = BlockTypeRef(id, Nil): BlockTypeRef
          Implementation(interface, List(OpClause(synthesizedId, tps, vps, bps, None, body, implicitResume)))
      }
    }
  }

  def opClause(): OpClause =
    (`def` ~> idRef()) ~ paramsOpt() ~ when(`:`) { Some(effectful()) } { None } ~ (`=` ~> stmt()) match {
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
    case `__` => skip(); IgnorePattern()
    case `(` => some(matchPattern, `(`, `,`, `)`) match {
      case p :: Nil => fail("Pattern matching on tuples requires more than one element")
      case ps => TagPattern(IdRef(List("effekt"), s"Tuple${ps.size + 1}"), ps)
    }
    case _ if isVariable && peek(1, `(`) =>
      TagPattern(idRef(), many(matchPattern, `(`, `,`, `)`))
    case _ if isVariable =>
      AnyPattern(idDef())
    case _ if isLiteral => LiteralPattern(literal())
    case _ => fail("Expected pattern")
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
    // thunkedBinaryOp(lhs, op, rhs) = op { lhs } { rhs }
    if op == `||` || op == `&&` then
      Call(IdTarget(IdRef(Nil, opName(op))), Nil, Nil, List(BlockLiteral(Nil, Nil, Nil, Return(lhs)), BlockLiteral(Nil, Nil, Nil, Return(rhs))))
    else
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
            e = Term.Select(e, member)
          }

        // function call
        case _ if isArguments =>
          val callee = e match {
            case Term.Var(id) => IdTarget(id)
            case other => ExprTarget(other)
          }
          val (targs, vargs, bargs) = arguments()
          e = Term.Call(callee, Nil, vargs, Nil)

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

  def lambdaParams(): (List[Id], List[ValueParam], List[BlockParam]) = fail("NOT IMPLEMENTED")

  def primExpr(): Term = peek.kind match {
    case _ if isLiteral      => literal()
    case _ if isVariable     => variable()
    case _ if isHole         => hole()
    case _ if isTupleOrGroup => tupleOrGroup()
    case _ if isListLiteral  => listLiteral()
    case t => fail(s"Expected variables, literals, tuples, lists, holes or group but found")
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
    def atomicValueType = {
      peek.kind match {
        case `(` => some(valueType, `(`, `,`, `)`) match {
          case tpe :: Nil => tpe
          case tpes => TupleTypeTree(tpes)
        }
        case _ => ValueTypeRef(idRef(), maybeTypeArgs())
      }
    }
    if (boxedAllowed) { boxedBlock getOrElse atomicValueType }
    else atomicValueType
  }

  /**
   * Uses backtracking!
   *
   * TODO improve errors
   *   i.e. fail("Expected either a function type (e.g., (A) => B / {E} or => B) or an interface type (e.g., State[T]).")
   */
  def blockType(): BlockType = blockType2(true)
  private def blockType2(boxedAllowed: Boolean): BlockType =
    def funType = backtrack {
      maybeTypeParams() ~ maybeValueTypes() ~ (maybeBlockTypeParams() <~ `=>`) ~ valueType2(boxedAllowed) ~ maybeEffects() match {
        case tparams ~ vparams ~ bparams ~ t ~ effs => FunctionType(tparams, vparams, bparams, t, effs)
      }
    }
    def parenthesized = backtrack { parens { blockType() } }
    funType orElse parenthesized getOrElse interfaceType()

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

  def valueParams(): List[ValueParam] =
    many(valueParam, `(`, `,`, `)`)

  def valueParam(): ValueParam =
    ValueParam(idDef(), Some(`:` ~> valueType()))

  def valueParamOpt(): ValueParam =
    ValueParam(idDef(), when(`:`)(Some(valueType()))(None))

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

  def manyWhile[T](p: => T, lookahead: TokenKind): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    while (peek(lookahead)) {
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
