package effekt

import effekt.lexer.*
import effekt.lexer.TokenKind.{ `::` as PathSep, * }
import effekt.source.*
import effekt.context.Context
import kiama.parsing.{ ParseResult, Input }
import kiama.util.{ Positions, Source, StringSource, Range, Position }

import scala.annotation.{ tailrec, targetName }
import scala.util.matching.Regex
import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break


case class Fail(message: String, position: Int) extends Throwable(null, null, false, false)

class RecursiveDescent(positions: Positions, tokens: Seq[Token], source: Source) {

  import scala.collection.mutable.ListBuffer

  def parse(input: Input)(using C: Context): Option[ModuleDecl] =

    try {
      //println(input.tokens)
      //val before = System.currentTimeMillis()
      val res = Some(program())
      //val after = System.currentTimeMillis()
      //println(s"${input.source.name}: ${after - before}ms")

      res
    } catch {
      case Fail(msg, pos) =>
        val source = input.source
        val range = tokens.lift(pos) match {
          case Some(value) =>
            val from = source.offsetToPosition(value.start)
            val to = source.offsetToPosition(value.end + 1)
            Some(Range(from, to))
          case None =>
            val pos = Position(0, 0, source)
            Some(Range(pos, pos))
        }

        C.report(effekt.util.messages.ParseError(msg, range)) // fix error reporting
        None
    }

  // here we need to convert to kiamas error format since REPL is defined in kiama
  def parseRepl(input: Input)(using C: Context): ParseResult[Tree] =
    try { kiama.parsing.Success(repl(), input) } catch {
      case Fail(msg, pos) => kiama.parsing.Error(msg, input.copy(offset = pos))
    }


  // Interfacing with the token stream
  // ---------------------------------

  // always points to the latest non-space position
  var position: Int = 0

  def peek: Token = tokens(position)

  /**
   * Negative lookahead
   */
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

  // the previously consumed token
  var previous = tokens(position)

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
  def skip(): Unit =
    previous = tokens(position)
    position += 1;
    spaces()

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
    val positionBefore = position
    val t = next()

    if (t.kind != kind) {
      // we need to fail at the position before consuming
      position = positionBefore
      fail(s"Expected ${explain(kind)} but got ${explain(t.kind)}")
    }


  /* The actual parser itself
  * ------------------------
  * We use the following naming conventions for nonterminals:
  *
  *  - maybe[...]s: zero or more times
  *  - [...]s: one or more times
  *  - [...]opt: optional type annotation
  *
  * Furthermore, we try to adhere to the following conventions:
  * - Use `backtrack` with caution and as a last resort. Try to disambiguate the grammar by using `peek` and also try to
  *   only use `backtrack` on "shallow" non-terminals, that is, not on a non-terminal containing an expression,
  *   a statement or a type.
  * - For the same reason, there is no function `manyWhile[T](p: => T): List[T]` but only
  *   `manyWhile[T](p: => T, predicate: => Boolean): List[T]` as this one does not use backtracking.
  * - Use `fail` for reporting errors.
  * - Functions consuming tokens have an empty parameter list `()`, functions that do not, have no parameter list (e.g. peek)
  * - All non-terminals are to use the `nonterminal` function for keeping track of positions.
  */

  // tokens that delimit a statement
  def returnPosition: Boolean = peek(`}`) || peek(`case`) || peek(`}>`) || peek(EOF)

  /**
   * Statements
   */
  def stmts(): Stmt =
    nonterminal:
      peek.kind match {
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
    case `val` =>
      val params = (`val` ~> peek.kind match {
        case `(` => valueParamsOpt()
        case _ => List(valueParam()) // TODO copy position
      })
      desugarWith(params, `=` ~> expr(), semi() ~> stmts())

    case _ => desugarWith(Nil, expr(), semi() ~> stmts())
  }

  def desugarWith(params: List[ValueParam], call: Term, body: Stmt): Stmt = call match {
     case m@MethodCall(receiver, id, tps, vargs, bargs) =>
       Return(MethodCall(receiver, id, tps, vargs, bargs :+ (BlockLiteral(Nil, params, Nil, body))))
     case c@Call(callee, tps, vargs, bargs) =>
       Return(Call(callee, tps, vargs, bargs :+ (BlockLiteral(Nil, params, Nil, body))))
     case Var(id) =>
       val tgt = IdTarget(id)
       Return(Call(tgt, Nil, Nil, (BlockLiteral(Nil, params, Nil, body)) :: Nil))
     case term =>
       Return(Call(ExprTarget(term), Nil, Nil, (BlockLiteral(Nil, params, Nil, body)) :: Nil))
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
    nonterminal:
      if peek(`{`) then braces { BlockStmt(stmts()) }
      else when(`return`) { Return(expr()) } { Return(expr()) }


  /**
   * Main entry point for the repl.
   */
  def repl(): Tree =
    nonterminal:
       // skip spaces at the start
       spaces()
       val res = peek.kind match {
         case t if isToplevel => toplevel()
         case `import` => includeDecl()
         case _ => expr()
       }
       if peek(`EOF`) then res else fail("Unexpected end of input")

  /**
   * Main entry point
   */
   def program(): ModuleDecl =
     nonterminal:
       // skip spaces at the start
       spaces()
       val res = ModuleDecl(moduleDecl(), manyWhile(includeDecl(), `import`), toplevelDefs())
       if peek(`EOF`) then res else fail("Unexpected end of input")
       // failure("Required at least one top-level function or effect definition")

  def moduleDecl(): String =
    when(`module`) { moduleName() } { defaultModulePath }

  // we are purposefully not using File here since the parser needs to work both
  // on the JVM and in JavaScript
  def defaultModulePath: String =
    val baseWithExt = source.name.split("[\\\\/]").last
    baseWithExt.split('.').head


  def includeDecl(): Include =
    nonterminal:
      Include(`import` ~> moduleName())

  def moduleName(): String =
    some(ident, `/`).mkString("/")

  def isToplevel: Boolean = peek.kind match {
    case `val` | `fun` | `def` | `type` | `effect` | `namespace` |
         `extern` | `effect` | `interface` | `type` | `record` => true
    case _ => false
  }

  def toplevel(): Def =
    nonterminal:
      peek.kind match {
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

  def toplevelDefs(): List[Def] =
    peek.kind match {
      case `namespace` =>
        consume(`namespace`)
        val id = idDef()
        peek.kind match {
          case `{` =>
            val defs = braces(toplevelDefs())
            val df = toplevelDefs()
            NamespaceDef(id, defs) :: df
          case _   =>
            val defs = toplevelDefs()
            List(NamespaceDef(id, defs))
        }
      case _ =>
        if (isToplevel) toplevel() :: toplevelDefs()
        else Nil
    }

  def toplevels(): List[Def] =
    nonterminal:
      manyWhile(toplevel(), isToplevel)

  def isDefinition: Boolean = peek.kind match {
    case `val` | `def` | `type` | `effect` | `namespace` => true
    case `extern` | `effect` | `interface` | `type` | `record` =>
      val kw = peek.kind
      fail(s"Only supported on the toplevel: ${kw.toString} declaration.")
    case _ => false
  }

  def definition(): Def =
    nonterminal:
      peek.kind match {
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

  def definitions(): List[Def] =
    nonterminal:
      manyWhile(definition(), isDefinition)

  def functionBody: Stmt = stmt() // TODO error context: "the body of a function definition"

  def valDef(): Def =
    nonterminal:
      ValDef(`val` ~> idDef(), maybeValueTypeAnnotation(), `=` ~> stmt())

  /**
   * In statement position, val-definitions can also be destructing:
   *   i.e. val (l, r) = point(); ...
   */
  def valStmt(): Stmt =
    nonterminal:
      val startMarker = nonterminal { new {} }
      def simpleLhs() = backtrack {
        `val` ~> idDef() ~ maybeValueTypeAnnotation() <~ `=`
      } map {
        case id ~ tpe =>
          val binding = stmt()
          val valDef = ValDef(id, tpe, binding).withRangeOf(startMarker, binding)
          DefStmt(valDef, { semi(); stmts() })
      }
      def matchLhs() =
        `val` ~> matchPattern() ~ manyWhile(`and` ~> matchGuard(), `and`) <~ `=` match {
          case AnyPattern(id) ~ Nil =>
            val binding = stmt()
            val valDef = ValDef(id, None, binding).withRangeOf(startMarker, binding)
            DefStmt(valDef, { semi(); stmts() })
          case p ~ guards =>
            val sc = expr()
            val default = when(`else`) { Some(stmt()) } { None }
            val body = semi() ~> stmts()
            val clause = MatchClause(p, guards, body).withRangeOf(p, sc)
            val matching = Match(sc, List(clause), default).withRangeOf(startMarker, sc)
            Return(matching)
        }

      simpleLhs() getOrElse matchLhs()


  def varDef(): Def =
    nonterminal:
      (`var` ~> idDef()) ~ maybeValueTypeAnnotation() ~ when(`in`) { Some(idRef()) } { None } ~ (`=` ~> stmt()) match {
        case id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr)
        case id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr)
      }

  def defDef(): Def =
    nonterminal:
      val id = consume(`def`) ~> idDef()

      def isBlockDef: Boolean = peek(`:`) || peek(`=`)

      if isBlockDef then
        // (: <VALUETYPE>)? `=` <EXPR>
        DefDef(id, maybeBlockTypeAnnotation(), `=` ~> expr())
      else
        // [...](<PARAM>...) {...} `=` <STMT>>
        val (tps, vps, bps) = params()
        FunDef(id, tps, vps, bps, maybeReturnAnnotation(), `=` ~> stmt())


  // right now: data type definitions (should be renamed to `data`) and type aliases
  def typeOrAliasDef(): Def =
    nonterminal:
      val id ~ tps = (`type` ~> idDef()) ~ maybeTypeParams()

      peek.kind match {
        case `=` => `=` ~> TypeDef(id, tps, valueType())
        case _ => braces { DataDef(id, tps, manyWhile({ constructor() <~ semi() }, !peek(`}`))) }
      }

  def recordDef(): Def =
    nonterminal:
      RecordDef(`record` ~> idDef(), maybeTypeParams(), valueParams())

  def constructor(): Constructor =
    nonterminal:
      Constructor(idDef(), maybeTypeParams(), valueParams())

  // On the top-level both
  //    effect Foo = {}
  // and
  //    effect Foo(): Int
  // are allowed. Here we simply backtrack, since effect definitions shouldn't be
  // very long and cannot be nested.
  def effectOrOperationDef(): Def =
    nonterminal:
      backtrack { effectDef() } getOrElse { operationDef() }

  def effectDef(): Def =
    nonterminal:
      // effect <NAME> = <EFFECTS>
      EffectDef(`effect` ~> idDef(), maybeTypeParams(), `=` ~> effects())

  // effect <NAME>[...](...): ...
  def operationDef(): Def =
    nonterminal:
      `effect` ~> operation() match {
        case op =>
          // TODO is the `true` flag used at all anymore???
          InterfaceDef(IdDef(op.id.name), Nil, List(op), true)
      }

  def operation(): Operation =
    nonterminal:
      idDef() ~ params() ~ returnAnnotation() match {
        case id ~ (tps, vps, bps) ~ ret => Operation(id, tps, vps, bps, ret)
      }

  def interfaceDef(): InterfaceDef =
    nonterminal:
      InterfaceDef(`interface` ~> idDef(), maybeTypeParams(), `{` ~> manyWhile(`def` ~> operation(), `def`) <~ `}`, true)

  def namespaceDef(): Def =
    nonterminal:
      consume(`namespace`)
      val id = idDef()
      // namespace foo { <DEFINITION>* }
      if peek(`{`) then braces { NamespaceDef(id, definitions()) }
      // namespace foo
      // <DEFINITION>*
      else { semi(); NamespaceDef(id, definitions()) }


  def externDef(): Def =
    nonterminal:
      { peek(`extern`); peek(1).kind } match {
        case `type`      => externType()
        case `interface` => externInterface()
        case `resource`  => externResource()
        case `include`   => externInclude()
        // extern """..."""
        case s: Str      => externString()
        case Ident(_) | `pure` =>
          // extern IDENT def ...
          if (peek(2, `def`)) externFun()
          // extern IDENT """..."""
          else externString()
        // extern {...} def ...
        case _ => externFun()
      }

  def featureFlag(): FeatureFlag =
    next().kind match {
      case Ident("default") => FeatureFlag.Default
      case Ident(flag)      => FeatureFlag.NamedFeatureFlag(flag)
      case _                => fail("Expected identifier")
    }

  def maybeFeatureFlag(): FeatureFlag =
    nonterminal:
      backtrack(featureFlag()).getOrElse(FeatureFlag.Default)

  def externType(): Def =
    nonterminal:
      ExternType(`extern` ~> `type` ~> idDef(), maybeTypeParams())
  def externInterface(): Def =
    nonterminal:
      ExternInterface(`extern` ~> `interface` ~> idDef(), maybeTypeParams())
  def externResource(): Def =
    nonterminal:
      ExternResource(`extern` ~> `resource` ~> idDef(), blockTypeAnnotation())
  def externInclude(): Def =
    nonterminal:
      `extern` ~> `include` ~> ExternInclude(maybeFeatureFlag(), path().stripPrefix("\"").stripSuffix("\""), None)

  def externString(): Def =
    nonterminal:
      consume(`extern`)
      val ff = maybeFeatureFlag()
      next().kind match {
        case Str(contents, _) => ExternInclude(ff, "", Some(contents))
        case _ => fail("Expected string literal.")
      }

  def externFun(): Def =
    nonterminal:
      ((`extern` ~> maybeExternCapture()) ~ (`def` ~> idDef()) ~ params() ~ (returnAnnotation() <~ `=`)) match {
        case capt ~ id ~ (tps, vps, bps) ~ ret =>
          val bodies = manyWhile(externBody(), isExternBodyStart)
          ExternDef(capt, id, tps, vps, bps, ret, bodies)
      }

  def externBody(): ExternBody =
    nonterminal:
      peek.kind match {
        case _: Ident => peek(1).kind match {
          case `{` => ExternBody.EffektExternBody(featureFlag(), `{` ~> stmts() <~ `}`)
          case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template())
        }
        case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template())
      }

  private def isExternBodyStart: Boolean =
    peek.kind match {
      case Str(_, _) | Ident(_) | `{` => true
      case _                          => false
    }

  def template(): Template[Term] =
    nonterminal:
      // TODO handle case where the body is not a string, e.g.
      // Expected an extern definition, which can either be a single-line string (e.g., "x + y") or a multi-line string (e.g., """...""")
      val first = string()
      val (exprs, strs) = manyWhile((`${` ~> expr() <~ `}`, string()), `${`).unzip
      Template(first :: strs, exprs)

  def maybeExternCapture(): CaptureSet =
    nonterminal:
      if peek(`{`) || peek(`pure`) || isVariable then externCapture()
      else CaptureSet(List(IdRef(List("effekt"), "io")))

  def externCapture(): CaptureSet =
    nonterminal:
      if peek(`{`) then captureSet()
      else if peek(`pure`) then `pure` ~> CaptureSet(Nil)
      else CaptureSet(List(idRef()))

  def path(): String =
    nonterminal:
      next().kind match {
        case Str(s, false) => s
        case _ => fail("Expected path as string literal.")
      }

  def string(): String =
    nonterminal:
      next().kind match {
        case Str(s, _) => s
        case _ => fail("Expected string literal.")
      }


  def maybeValueTypeAnnotation(): Option[ValueType] =
    nonterminal:
      if peek(`:`) then Some(valueTypeAnnotation()) else None

  def maybeBlockTypeAnnotation(): Option[BlockType] =
    nonterminal:
      if peek(`:`) then Some(blockTypeAnnotation()) else None

  def maybeReturnAnnotation(): Option[Effectful] =
    nonterminal:
      when(`:`) { Some(effectful()) } { None }

  def returnAnnotation(): Effectful =
    if peek(`:`) then  `:` ~> effectful()
    else fail("Expected return type annotation")

  def valueTypeAnnotation(): ValueType =
    if peek(`:`) then  `:` ~> valueType()
    else fail("Expected a type annotation")

  def blockTypeAnnotation(): BlockType =
    if peek(`:`) then  `:` ~> blockType()
    else fail("Expected a type annotation")

  def expr(): Term = peek.kind match {
    case _ => matchExpr()
  }

  def ifExpr(): Term =
    nonterminal:
      If(`if` ~> parens { matchGuards() },
        stmt(),
        when(`else`) { stmt() } { Return(UnitLit()) })

  def whileExpr(): Term =
    nonterminal:
      While(`while` ~> parens { matchGuards() },
        stmt(),
        when(`else`) { Some(stmt()) } { None })

  def doExpr(): Term =
    nonterminal:
      (`do` ~> idRef()) ~ arguments() match {
        case id ~ (targs, vargs, bargs) => Do(None, id, targs, vargs, bargs)
      }

  /*
  <tryExpr> ::= try { <stmts> } <handler>+
  <handler> ::= with (<idDef> :)? <implementation>
  <implementation ::= <interfaceType> { <opClause>+ }
  */
  def tryExpr(): Term =
    nonterminal:
      `try` ~> stmt() ~ someWhile(handler(), `with`) match {
        case s ~ hs => TryHandle(s, hs)
      }

  def regionExpr(): Term =
    nonterminal:
      Region(`region` ~> idDef(), stmt())

  def boxExpr(): Term =
    nonterminal:
      val captures = `box` ~> backtrack(captureSet())
      val expr = if (peek(`{`)) functionArg()
        else if (peek(`new`)) newExpr()
        else Var(idRef())
      Box(captures, expr)


  // TODO deprecate
  def funExpr(): Term =
    nonterminal:
      `fun` ~> Box(None, BlockLiteral(Nil, valueParams(), Nil, braces { stmts() }))
    // TODO positions

  def unboxExpr(): Term =
    nonterminal:
      Unbox(`unbox` ~> expr())

  def newExpr(): Term =
    nonterminal:
      New(`new` ~> implementation())

  def handler(): Handler =
    nonterminal:
      `with` ~> backtrack(idDef() <~ `:`) ~ implementation() match {
        case capabilityName ~ impl =>
          val capability = capabilityName map { name => BlockParam(name, Some(impl.interface)): BlockParam }
          Handler(capability, impl)
      }

  // This nonterminal uses limited backtracking: It parses the interface type multiple times.
  def implementation(): Implementation =
    nonterminal:
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
      def operationImplementation() = idRef() ~ maybeTypeParams() ~ implicitResume ~ functionArg() match {
        case (id ~ tps ~ k ~ BlockLiteral(_, vps, bps, body)) =>
          val synthesizedId = IdRef(Nil, id.name).withPositionOf(id)
          val interface = BlockTypeRef(id, Nil).withPositionOf(id): BlockTypeRef
          val operation = OpClause(synthesizedId, tps, vps, bps, None, body, k).withRangeOf(id, body)
          Implementation(interface, List(operation))
      }

      emptyImplementation() orElse interfaceImplementation() getOrElse operationImplementation()

  def opClause(): OpClause =
    nonterminal:
      (`def` ~> idRef()) ~ paramsOpt() ~ maybeReturnAnnotation() ~ (`=` ~> stmt()) match {
        case id ~ (tps, vps, bps) ~ ret ~ body =>
          // TODO the implicitResume needs to have the correct position assigned (maybe move it up again...)
          OpClause(id, tps, vps, bps, ret, body, implicitResume)
      }

  def implicitResume: IdDef =
    nonterminal:
      IdDef("resume")

  def matchClause(): MatchClause =
    nonterminal:
      MatchClause(`case` ~> matchPattern(), manyWhile(`and` ~> matchGuard(), `and`), `=>` ~> stmts())

  def matchGuards() =
    nonterminal:
      some(matchGuard, `and`)

  def matchGuard(): MatchGuard =
    nonterminal:
      expr() ~ when(`is`) { Some(matchPattern()) } { None } match {
        case e ~ Some(p) => MatchGuard.PatternGuard(e, p)
        case e ~ None    => MatchGuard.BooleanGuard(e)
      }

  def matchPattern(): MatchPattern =
    nonterminal:
      peek.kind match {
        case `__` => skip(); IgnorePattern()
        case _ if isVariable  =>
          idRef() match {
            case id if peek(`(`) => TagPattern(id, many(matchPattern, `(`, `,`, `)`))
            case IdRef(Nil, name) => AnyPattern(IdDef(name)) // TODO positions
            case IdRef(_, name) => fail("Cannot use qualified names to bind a pattern variable")
          }
        case _ if isVariable =>
          AnyPattern(idDef())
        case _ if isLiteral => LiteralPattern(literal())
        case `(` => some(matchPattern, `(`, `,`, `)`) match {
          case p :: Nil => fail("Pattern matching on tuples requires more than one element")
          case ps => TagPattern(IdRef(List("effekt"), s"Tuple${ps.size}"), ps)
        }
        case _ => fail("Expected pattern")
      }

  def matchExpr(): Term =
    nonterminal:
      var sc = assignExpr()
      while (peek(`match`)) {
         val clauses = `match` ~> braces { manyWhile(matchClause(), `case`) }
         val default = when(`else`) { Some(stmt()) } { None }
         sc = Match(sc, clauses, default)
      }
      sc

  def assignExpr(): Term =
    nonterminal:
      orExpr() match {
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
    nonterminal:
      var left = nonTerminal()
      while (ops.contains(peek.kind)) {
         val op = next().kind
         val right = nonTerminal()
         left = binaryOp(left, op, right).withRangeOf(left, right)
      }
      left

  // === AST Helpers ===

  private def binaryOp(lhs: Term, op: TokenKind, rhs: Term): Term =
    nonterminal:
       if isThunkedOp(op) then
         Call(IdTarget(IdRef(Nil, opName(op))), Nil, Nil, List(BlockLiteral(Nil, Nil, Nil, Return(lhs)), BlockLiteral(Nil, Nil, Nil, Return(rhs))))
       else
         Call(IdTarget(IdRef(Nil, opName(op))), Nil, List(lhs, rhs), Nil)

  private def isThunkedOp(op: TokenKind): Boolean = op match {
    case `||` | `&&` => true
    case _           => false
  }

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
    // TODO positions!

  /**
   * This is a compound production for
   *  - member selection <EXPR>.<NAME>
   *  - method calls <EXPR>.<NAME>(...)
   *  - function calls <EXPR>(...)
   *
   * This way expressions like `foo.bar.baz()(x).bam.boo()` are
   * parsed with the correct left-associativity.
   */
  def callExpr(): Term = nonterminal {
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

  // argument lists cannot follow a linebreak:
  //   foo      ==    foo;
  //   ()             ()
  def isArguments: Boolean = lookbehind(1).kind != Newline && (peek(`(`) || peek(`[`) || peek(`{`))
  def arguments(): (List[ValueType], List[Term], List[Term]) =
    if (!isArguments) fail("Expected at least one argument section (types, values, or blocks)")
    (maybeTypeArgs(), maybeValueArgs(), maybeBlockArgs())

  def maybeTypeArgs(): List[ValueType] = if peek(`[`) then typeArgs() else Nil
  def maybeValueArgs(): List[Term] = if peek(`(`) then valueArgs() else Nil
  def maybeBlockArgs(): List[Term] = if peek(`{`) then blockArgs() else Nil

  def typeArgs(): List[ValueType] =
    nonterminal:
      some(valueType, `[`, `,`, `]`)
  def valueArgs(): List[Term] =
    nonterminal:
      many(expr, `(`, `,`, `)`)
  def blockArgs(): List[Term] =
    nonterminal:
      someWhile(blockArg(), `{`)

  /**
   * Note: for this nonterminal, we need some backtracking.
   */
  def blockArg(): Term =
    nonterminal:
      backtrack { `{` ~> Var(idRef()) <~ `}` } getOrElse { functionArg() }

  def functionArg(): BlockLiteral =
    nonterminal:
      braces {
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
    case `fun`    => funExpr()
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
    nonterminal:
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
    case _: (Integer | Float | Str | Chr) => true
    case `true` => true
    case `false` => true
    case _ => isUnitLiteral
  }
  def literal(): Literal =
    nonterminal:
      peek.kind match {
        case Integer(v)         => skip(); IntLit(v)
        case Float(v)           => skip(); DoubleLit(v)
        case Str(s, multiline)  => skip(); StringLit(s)
        case Chr(c)             => skip(); CharLit(c)
        case `true`             => skip(); BooleanLit(true)
        case `false`            => skip(); BooleanLit(false)
        case t if isUnitLiteral => skip(); skip(); UnitLit()
        case t => fail("Expected a literal")
      }

  // Will also recognize ( ) as unit if we do not emit space in the lexer...
  private def isUnitLiteral: Boolean = peek(`(`) && peek(1, `)`)

  def isVariable: Boolean = isIdRef
  def variable(): Term =
    nonterminal:
      Var(idRef())

  def isIdRef: Boolean = isIdent

  def idRef(): IdRef =
    nonterminal:
      some(ident, PathSep) match {
        case ids => IdRef(ids.init, ids.last)
      }

  def idDef(): IdDef =
    nonterminal:
      IdDef(ident())

  def isIdent: Boolean = peek.kind match {
    case Ident(id) => true
    case _ => false
  }
  def ident(): String =
    nonterminal:
      next().kind match {
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
  private def valueType2(boxedAllowed: Boolean): ValueType = nonterminal {
    def boxedBlock = backtrack {
      BoxedType(blockType2(false), `at` ~> captureSet())
    }
    if (boxedAllowed) { boxedBlock getOrElse atomicValueType() }
    else atomicValueType()
  }

  def atomicValueType(): ValueType =
    nonterminal:
      peek.kind match {
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
    nonterminal:

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

      def interface() =
        val res = interfaceType()
        if peek(`/`) then
          fail("Effects not allowed here. Maybe you mean to use a function type `() => T / E`?")
        else res

      simpleFunType orElse funType orElse parenthesized getOrElse interface()

  def interfaceType(): BlockTypeRef =
    nonterminal:
      BlockTypeRef(idRef(), maybeTypeArgs()): BlockTypeRef
    // TODO error "Expected an interface type"

  def maybeTypeParams(): List[Id] =
    nonterminal:
      if peek(`[`) then typeParams() else Nil

  def typeParams(): List[Id] =
    nonterminal:
      some(idDef, `[`, `,`, `]`)

  def maybeBlockTypeParams(): List[(Option[IdDef], BlockType)] =
    nonterminal:
      if peek(`{`) then blockTypeParams() else Nil

  def blockTypeParams(): List[(Option[IdDef], BlockType)] =
    nonterminal:
      someWhile(blockTypeParam(), `{`)

  def blockTypeParam(): (Option[IdDef], BlockType) =
    nonterminal:
      braces { (backtrack { idDef() <~ `:` }, blockType()) }

  def lambdaParams(): (List[Id], List[ValueParam], List[BlockParam]) =
    nonterminal:
      if isVariable then (Nil, List(ValueParam(idDef(), None)), Nil)  else paramsOpt()

  def params(): (List[Id], List[ValueParam], List[BlockParam]) =
    nonterminal:
      maybeTypeParams() ~ maybeValueParams() ~ maybeBlockParams() match {
        case tps ~ vps ~ bps => (tps, vps, bps)
      }

  def paramsOpt(): (List[Id], List[ValueParam], List[BlockParam]) =
    nonterminal:
      maybeTypeParams() ~ maybeValueParamsOpt() ~ maybeBlockParamsOpt() match {
        case (tps ~ vps ~ bps) =>
          // fail("Expected a parameter list (multiple value parameters or one block parameter; only type annotations of value parameters can be currently omitted)")
          (tps, vps, bps)
      }

  def maybeValueParamsOpt(): List[ValueParam] =
    nonterminal:
      if peek(`(`) then valueParamsOpt() else Nil

  def valueParamsOpt(): List[ValueParam] =
    nonterminal:
      many(valueParamOpt, `(`, `,`, `)`)

  def maybeValueParams(): List[ValueParam] =
    nonterminal:
      if peek(`(`) then valueParams() else Nil

  def valueParams(): List[ValueParam] =
    nonterminal:
      many(valueParam, `(`, `,`, `)`)

  def valueParam(): ValueParam =
    nonterminal:
      ValueParam(idDef(), Some(valueTypeAnnotation()))

  def valueParamOpt(): ValueParam =
    nonterminal:
      ValueParam(idDef(), maybeValueTypeAnnotation())

  def maybeBlockParams(): List[BlockParam] =
    nonterminal:
      manyWhile(`{` ~> blockParam() <~ `}`, `{`)

  def blockParams(): List[BlockParam] =
    nonterminal:
      someWhile(`{` ~> blockParam() <~ `}`, `{`)

  def maybeBlockParamsOpt(): List[BlockParam] =
    nonterminal:
      manyWhile(`{` ~> blockParamOpt() <~ `}`, `{`)

  def blockParamsOpt(): List[BlockParam] =
    nonterminal:
      someWhile(`{` ~> blockParamOpt() <~ `}`, `{`)

  def blockParam(): BlockParam =
    nonterminal:
      BlockParam(idDef(), Some(blockTypeAnnotation()))

  def blockParamOpt(): BlockParam =
    nonterminal:
      BlockParam(idDef(), when(`:`)(Some(blockType()))(None))


  def maybeValueTypes(): List[ValueType] =
    nonterminal:
      if peek(`(`) then valueTypes() else Nil

  def valueTypes(): List[ValueType] =
    nonterminal:
      many(valueType, `(`, `,`, `)`)

  def captureSet(): CaptureSet =
    nonterminal:
      CaptureSet(many(idRef, `{`, `,` , `}`))

  def effectful(): Effectful =
    nonterminal:
      Effectful(valueType(), maybeEffects())

  def maybeEffects(): Effects =
    nonterminal:
      when(`/`) { effects() } { Effects.Pure }

  // TODO error "Expected an effect set"
  def effects(): Effects =
    nonterminal:
      if peek(`{`) then Effects(many(interfaceType, `{`, `,`, `}`))
      else Effects(interfaceType())


  // Generic utility functions
  // -------------------------
  // ... for writing parsers.

  /**
   * Aborts parsing with the given message
   */
  def fail(message: String): Nothing = throw Fail(message, position)

  /**
   * Guards `thn` by token `t` and consumes the token itself, if present.
   */
  inline def when[T](t: TokenKind)(inline thn: => T)(inline els: => T): T =
    if peek(t) then { consume(t); thn } else els

  inline def backtrack[T](inline p: => T): Option[T] =
    val before = position
    try { Some(p) } catch {
      case Fail(_, _) => position = before; None
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

    @targetName("seqLeftToken")
    inline def <~(t: TokenKind): A = { consume(t); self }

    @targetName("seqLeftUnit")
    inline def <~(t: Unit): A = { self }

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
    someWhile(p, peek(lookahead))

  inline def someWhile[T](p: => T, predicate: => Boolean): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    components += p
    while (predicate) {
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


  // Positions

  inline def nonterminal[T](inline p: => T): T = {
    val startToken = peek
    val start = startToken.start
    val res = p
    val endToken = previous
    val end = endToken.end + 1 // since positions by lexer are inclusive, but kiama is exclusive

    //    val sc: Any = res
    //    if sc.isInstanceOf[Implementation] then sc match {
    //      case Implementation(_, List(op)) =>
    //        println(op)
    //        println(positions.getStart(op))
    //        println(positions.getFinish(op))
    //
    //        //        println(s"start: ${startToken.kind} (${source.offsetToPosition(start)})")
    //        //        println(s"end: ${endToken} (${source.offsetToPosition(end)})")
    //        //        println(s"peek: ${peek}")
    //
    //      case _ => ()
    //    }

    positions.setStart(res, source.offsetToPosition(start))
    positions.setFinish(res, source.offsetToPosition(end))

    res
  }

  extension [T](self: T) {
    inline def withPositionOf(other: Any): self.type = { positions.dupPos(other, self); self }
    inline def withRangeOf(first: Any, last: Any): self.type = { positions.dupRangePos(first, last, self); self }

    // Why did we need those?
    private def dupIfEmpty(from: Any, to: Any): Unit =
      if (positions.getStart(to).isEmpty) { positions.dupPos(from, to) }

    private def dupAll(from: Any, to: Any): Unit = to match {
      case t: Tree =>
        dupIfEmpty(from, t)
        t.productIterator.foreach { dupAll(from, _) }
      case t: Iterable[t] => t.foreach { dupAll(from, _) }
      case _ => ()
    }
  }
}
