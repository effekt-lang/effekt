package effekt

import effekt.lexer.*
import effekt.lexer.TokenKind.{`::` as PathSep, *}
import effekt.source.*
import effekt.context.Context
import effekt.source.Origin.Synthesized
import effekt.util.VirtualSource
import kiama.parsing.{Input, ParseResult}
import kiama.util.{Position, Positions, Range, Source}

import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

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
        val tokens = effekt.lexer.Lexer.lex(source)
        val parser = new Parser(C.positions, tokens, source)
        parser.parse(Input(source, 0))
      }
  } map { tree =>
    Parsed(source, tree)
  }
}


case class Fail(msg: String, position: Int) extends Throwable(null, null, false, false)
object Fail {
  def expectedButGot(expected: String, got: String, position: Int): Fail =
    Fail(s"Expected ${expected} but got ${got}", position)
}
case class SoftFail(message: String, positionStart: Int, positionEnd: Int)

class Parser(positions: Positions, tokens: Seq[Token], source: Source) {

  var softFails: ListBuffer[SoftFail] = ListBuffer[SoftFail]()

  private def report(msg: String, fromPosition: Int, toPosition: Int, source: Source = source)(using C: Context) = {
    val from = source.offsetToPosition(tokens(fromPosition).start)
    val to = source.offsetToPosition(tokens(toPosition).end + 1)
    val range = Range(from, to)
    C.report(effekt.util.messages.ParseError(msg, Some(range)))
  }

  def parse(input: Input)(using C: Context): Option[ModuleDecl] = {
    def reportSoftFails()(using C: Context): Unit =
      softFails.foreach {
        case SoftFail(msg, from, to) => report(msg, from, to, source = input.source)
      }

    try {
      //println(input.tokens)
      //val before = System.currentTimeMillis()
      val res = Some(program())
      //val after = System.currentTimeMillis()
      //println(s"${input.source.name}: ${after - before}ms")

      // Report soft fails
      reportSoftFails()
      if softFails.isEmpty then res else None
    } catch {
      case Fail(msg, pos) =>
        // Don't forget soft fails!
        reportSoftFails()

        report(msg, pos, pos, source = input.source)
        None
    }
  }

  // here we need to convert to kiamas error format since REPL is defined in kiama
  def parseRepl(input: Input)(using C: Context): ParseResult[Tree] =
    try { kiama.parsing.Success(repl(), input) } catch {
      case Fail(msg, pos) => kiama.parsing.Error(msg, input.copy(offset = pos))
    }

  var currentLabel: Option[String] = None

  extension[T](inline p: => T) inline def labelled(inline label: String): T = {
    val labelBefore = currentLabel
    if (currentLabel.isEmpty) {
      currentLabel = Some(label)
    }
    val res = p
    currentLabel = labelBefore
    res
  }

  // Interfacing with the token stream
  // ---------------------------------

  // always points to the latest non-space position
  var position: Int = 0

  def recover(tokenKind: TokenKind, tokenPosition: Int): TokenKind = tokenKind match {
    case TokenKind.Error(err) =>
      val recoveredKind = err match {
        case LexerError.UnterminatedStringLike(kind) => recover(kind, tokenPosition) // for nested errors
        case LexerError.UnterminatedComment => TokenKind.Comment("<unterminated>")
        case LexerError.EmptyCharLiteral => TokenKind.Chr('?')
        case LexerError.MultipleCodePointsInChar => TokenKind.Chr('?')
        case LexerError.InvalidIntegerFormat => TokenKind.Integer(0)
        case LexerError.InvalidDoubleFormat => TokenKind.Float(java.lang.Double.NaN)
        case _ => fail(err.message)
      }

      // NOTE: This relies on the fact that the `fail` above ^ throws!
      softFail(err.message, tokenPosition, tokenPosition)
      recoveredKind
    case _ => tokenKind
  }

  extension(token: Token) def failOnErrorToken(tokenPosition: Int = position): Token =
    token.kind match {
      case TokenKind.Error(err) => token.copy(kind = recover(token.kind, tokenPosition))
      case _                    => token
    }

  def peek: Token = tokens(position).failOnErrorToken(position)

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

      tokens(position).failOnErrorToken(position) match {
        case token if isSpace(token.kind) => go(position + 1, offset)
        case token if offset <= 0 => token
        case _ => go(position + 1, offset - 1)
      }

    go(position, offset)

  // the previously consumed token
  var previous: Option[Token] = {
    if position == 0 then None
    else Some(tokens(position).failOnErrorToken(position))
  }
  // The current position, the position after the previous token
  // The `+ 1` is needed since positions by lexer are inclusive, but kiama is exclusive
  def pos(): Int = previous.map(_.end + 1).getOrElse(0)

  def peek(kind: TokenKind): Boolean =
    peek.kind == kind
  def peek(offset: Int, kind: TokenKind): Boolean =
    peek(offset).kind == kind

  def hasNext(): Boolean = position < tokens.length
  def next(): Token =
    val t = tokens(position).failOnErrorToken(position)
    skip()
    t

  /**
   * Skips the current token and then all subsequent whitespace
   */
  def skip(): Unit =
    previous = Some(tokens(position))
    position += 1
    currentLabel = None
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
    if !hasNext() then fail(s"Expected ${kind}, but reached end of file")
    if (peek.kind != kind) {
      fail(explain(kind), peek.kind) // s"Expected ${explain(kind)} but got ${explain(t.kind)}")
    }
    val t = next()
    ()

  inline def expect[T](expected: String)(inline f: PartialFunction[TokenKind, T]): T =
    val kind = peek.kind
    if f.isDefinedAt(kind) then { skip(); f(kind) } else fail(expected, kind)

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
  def returnPosition: Boolean = peek(`}`) || peek(`}$`) || peek(`case`) || peek(`}>`) || peek(EOF)

  /**
   * Statements
   */
  def stmts(inBraces: Boolean): Stmt =
    nonterminal:
      (peek.kind match {
        case `val`  => valStmt(inBraces)
        case _ if isDefinition && inBraces => DefStmt(definition(), semi() ~> stmts(inBraces), span())
        case _ if isDefinition => fail("Definitions are only allowed, when enclosed in braces.")
        case `with` => withStmt(inBraces)
        case `var`  => DefStmt(varDef(), semi() ~> stmts(inBraces), span())
        case `return` =>
          // trailing semicolon only allowed when in braces
          `return` ~> Return(expr() <~ (if inBraces then maybeSemi()), span())
        case `}` | `}$` if inBraces => // Unexpected end of <STMTS> =>
          // insert a synthetic `return ()` into the block
          Return(UnitLit(span().emptyAfter.synthesized), span().emptyAfter.synthesized)
        // for now we do not allow multiple expressions in single-statement mode.
        // That is, we rule out
        //     def foo() =
        //       expr;
        //       expr
        case _ if !inBraces =>
          Return(expr(), span())
        case _ =>
          val e = expr()
          semi()
          if returnPosition then Return(e, span())
          else ExprStmt(e, stmts(inBraces), span())
      }) labelled "statements"

  // ATTENTION: here the grammar changed (we added `with val` to disambiguate)
  // with val <ID> (: <TYPE>)? = <EXPR>; <STMTS>
  // with val (<ID> (: <TYPE>)?...) = <EXPR>
  // with <EXPR>; <STMTS>
  def withStmt(inBraces: Boolean): Stmt = `with` ~> peek.kind match {
    case `val` =>
      val params = (`val` ~> peek.kind match {
        case `(` => valueParamsOpt()
        case _ => List(valueParamOpt()) // TODO copy position
      })
      desugarWith(params, Nil, `=` ~> expr(), semi() ~> stmts(inBraces), span())

    case `def` =>
      val params = (`def` ~> peek.kind match {
        case `{` => blockParamsOpt()
        case _ => List(blockParamOpt()) // TODO copy position
      })
      desugarWith(Nil, params, `=` ~> expr(), semi() ~> stmts(inBraces), span())

    case _ => desugarWith(Nil, Nil, expr(), semi() ~> stmts(inBraces), span())
  }

  def desugarWith(vparams: List[ValueParam], bparams: List[BlockParam], call: Term, body: Stmt, withSpan: Span): Stmt = call match {
     case m@MethodCall(receiver, id, tps, vargs, bargs, callSpan) =>
       Return(MethodCall(receiver, id, tps, vargs, bargs :+ (BlockLiteral(Nil, vparams, bparams, body, body.span.synthesized)), callSpan), withSpan.synthesized)
     case c@Call(callee, tps, vargs, bargs, callSpan) =>
       Return(Call(callee, tps, vargs, bargs :+ (BlockLiteral(Nil, vparams, bparams, body, body.span.synthesized)), callSpan), withSpan.synthesized)
     case Var(id, varSpan) =>
       val tgt = IdTarget(id)
       Return(Call(tgt, Nil, Nil, (BlockLiteral(Nil, vparams, bparams, body, body.span.synthesized)) :: Nil, varSpan), withSpan.synthesized)
     case Do(effect, id, targs, vargs, bargs, doSpan) =>
      Return(Do(effect, id, targs, vargs, bargs :+ BlockLiteral(Nil, vparams, bparams, body, body.span.synthesized), doSpan), withSpan.synthesized)
     case term =>
       Return(Call(ExprTarget(term), Nil, Nil, (BlockLiteral(Nil, vparams, bparams, body, body.span.synthesized)) :: Nil, term.span.synthesized), withSpan.synthesized)
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

    case _ => fail("Expected terminator: `;` or a newline")
  }

  def stmt(): Stmt =
    nonterminal:
      {
        if peek(`{`) then BlockStmt(braces { stmts(inBraces = true) }, span())
        else stmts(inBraces = false)
      } labelled "statement"

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
       // NOTE: This means we expected EOF, but there's still some stuff left over.
       if peek(`EOF`) then res else fail("Unexpected item")

  /**
   * Main entry point
   */
   def program(): ModuleDecl =
     nonterminal:
       // skip spaces at the start
       spaces()
       shebang()
       spaces()
       val (name, doc) = moduleDecl()
       val res = ModuleDecl(name, manyWhile(includeDecl(), `import`), toplevelDefs(), doc, span())

       if (!peek(`EOF`)) {
         // NOTE: This means we expected EOF, but there's still some _stuff_ left over.

         softFailWith("Expected top-level definition") {
           manyUntil({ skip() }, `EOF`)
         }
       }
       res

  @tailrec
  private def shebang(): Unit =
    peek.kind match {
      case Shebang(_) => consume(peek.kind); shebang()
      case _ => ()
    }

  def moduleDecl(): Tuple2[String, Doc] =
    documentedKind match {
      case `module` =>
        val doc = maybeDocumentation()
        consume(`module`)
        (moduleName(), doc)
      case _ => (defaultModulePath, None)
    }

  // we are purposefully not using File here since the parser needs to work both
  // on the JVM and in JavaScript
  def defaultModulePath: String =
    val baseWithExt = source.name.split("[\\\\/]").last
    baseWithExt.split('.').head


  def includeDecl(): Include =
    nonterminal:
      Include(`import` ~> moduleName(), span())

  def moduleName(): String =
    some(ident, `/`).mkString("/") labelled "module name"

  def isToplevel: Boolean = documentedKind match {
    case `val` | `fun` | `def` | `type` | `effect` | `namespace` |
         `extern` | `interface` | `type` | `record` | `var` => true
    case _ => false
  }

  def toplevel(): Def =
    nonterminal:
      documentedKind match {
        case `val`       => valDef()
        case `def`       => defDef()
        case `interface` => interfaceDef()
        case `type`      => typeOrAliasDef()
        case `record`    => recordDef()
        case `extern`    => externDef()
        case `effect`    => effectOrOperationDef()
        case `namespace` => namespaceDef()
        case `var`       => backtrack {
          softFailWith("Mutable variable declarations are currently not supported on the toplevel.") {
            varDef()
          }
        } getOrElse fail("Mutable variable declarations are currently not supported on the toplevel.")
        case _ => fail("Expected a top-level definition")
      }

  def toplevelDefs(): List[Def] =
    documentedKind match {
      case `namespace` =>
        val doc = maybeDocumentation()
        consume(`namespace`)
        val id = idDef()
        peek.kind match {
          case `{` =>
            val defs = braces(toplevelDefs())
            val df = toplevelDefs()
            NamespaceDef(id, defs, doc, span()) :: df
          case _   =>
            val defs = toplevelDefs()
            List(NamespaceDef(id, defs, doc, span()))
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
    case `extern` | `interface` | `type` | `record` =>
      val kw = peek.kind
      fail(s"Only supported on the toplevel: ${kw.toString} declaration.")
    case _ => false
  }

  def definition(): Def =
    nonterminal:
      documentedKind match {
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
      documented { doc =>
        ValDef(`val` ~> idDef(), maybeValueTypeAnnotation(), `=` ~> stmt(), doc, span())
      }

  /**
   * In statement position, val-definitions can also be destructing:
   *   i.e. val (l, r) = point(); ...
   */
  def valStmt(inBraces: Boolean): Stmt =
    nonterminal:
      val doc = maybeDocumentation()
      val startPos = pos()
      val startMarker = nonterminal { new {} }
      def simpleLhs() = backtrack {
        // Make sure there's either a `:` or `=` next, otherwise goto `matchLhs`
        def canCut: Boolean = peek(`:`) || peek(`=`)
        `val` ~> idDef() <~ (if !canCut then fail("Expected a `:` or a `=`"))
      } map { id =>
          val tpe = maybeValueTypeAnnotation() <~ `=`
          val binding = stmt()
          val endPos = pos()
          val valDef = ValDef(id, tpe, binding, doc, Span(source, startPos, endPos)).withRangeOf(startMarker, binding)
          DefStmt(valDef, { semi(); stmts(inBraces) }, span())
      }
      def matchLhs() =
        maybeDocumentation() ~ (`val` ~> matchPattern()) ~ manyWhile(`and` ~> matchGuard(), `and`) <~ `=` match {
          case doc ~ AnyPattern(id, _) ~ Nil =>
            val binding = stmt()
            val endPos = pos()
            val valDef = ValDef(id, None, binding, doc, Span(source, startPos, endPos)).withRangeOf(startMarker, binding)
            DefStmt(valDef, { semi(); stmts(inBraces) }, span())
          case doc ~ p ~ guards =>
            // matches do not support doc comments, so we ignore `doc`
            val sc = expr()
            val endPos = pos()
            val default = when(`else`) { Some(stmt()) } { None }
            val body = semi() ~> stmts(inBraces)
            val clause = MatchClause(p, guards, body, Span(source, p.span.from, sc.span.to)).withRangeOf(p, sc)
            val matching = Match(List(sc), List(clause), default, Span(source, startPos, endPos, Synthesized)).withRangeOf(startMarker, sc)
            Return(matching, span().synthesized)
        }

      simpleLhs() getOrElse matchLhs()


  def varDef(): Def =
    nonterminal:
      maybeDocumentation() ~ (`var` ~> idDef()) ~ maybeValueTypeAnnotation() ~ when(`in`) { Some(idRef()) } { None } ~ (`=` ~> stmt()) match {
        case doc ~ id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr, doc, span())
        case doc ~ id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr, doc, span())
      }

  def defDef(): Def =
    nonterminal:
      val doc = maybeDocumentation()
      val id = consume(`def`) ~> idDef()

      def isBlockDef: Boolean = peek(`:`) || peek(`=`)

      if isBlockDef then
        // (: <VALUETYPE>)? `=` <EXPR>
        DefDef(id, maybeBlockTypeAnnotation(), `=` ~> expr(), doc, span())
      else
        // [...](<PARAM>...) {...} `=` <STMT>>
        val (tps, vps, bps) = params()
        FunDef(id, tps, vps, bps, maybeReturnAnnotation(), `=` ~> stmt(), doc, span())


  // right now: data type definitions (should be renamed to `data`) and type aliases
  def typeOrAliasDef(): Def =
    nonterminal:
      val doc = maybeDocumentation()
      val id ~ tps = (`type` ~> idDef()) ~ maybeTypeParams()

      peek.kind match {
        case `=` => `=` ~> TypeDef(id, tps.unspan, valueType(), doc, span())
        case _ => DataDef(id, tps, braces { manyUntil({ constructor() <~ semi() }, `}`) }, doc, span())
      }

  def recordDef(): Def =
    nonterminal:
      documented { doc =>
        RecordDef(`record` ~> idDef(), maybeTypeParams(), valueParams(), doc, span())
      }

  def constructor(): Constructor =
    nonterminal:
      documented { doc =>
        Constructor(idDef(), maybeTypeParams(), valueParams(), doc, span()) labelled "constructor"
      }

  // On the top-level both
  //    effect Foo = {}
  // and
  //    effect Foo(): Int
  // are allowed. Here we simply backtrack, since effect definitions shouldn't be
  // very long and cannot be nested.
  def effectOrOperationDef(): Def = {
    // We used to use `effect Foo { def a(); def b(); ... }` for multi-operation interfaces,
    // but now we use `interface Foo { ... }` instead.
    // If we can't parse `effectDef` or `operationDef`, we should try parsing an interface with the wrong keyword
    // and report an error to the user if the malformed interface would be valid.
    def interfaceDefUsingEffect(): Maybe[InterfaceDef] =
      backtrack(restoreSoftFails = false):
        softFailWith("Unexpected 'effect', did you mean to declare an interface of multiple operations using the 'interface' keyword?"):
          interfaceDef(`effect`)

    nonterminal:
      backtrack { effectDef() }
        .orElse { interfaceDefUsingEffect() }
        .getOrElse { operationDef() } // The `operationDef` should be last as to not cause spurious errors later.
  }

  def effectDef(): Def =
    nonterminal:
      // effect <NAME> = <EFFECTS>
      documented { doc =>
        EffectDef(`effect` ~> idDef(), maybeTypeParams().unspan, `=` ~> effects(), doc, span())
      }

  // effect <NAME>[...](...): ...
  def operationDef(): Def =
    nonterminal:
      val doc = maybeDocumentation()
      `effect` ~> operation(doc) match {
        case op @ Operation(id, tps, vps, bps, ret, opDoc, opSpan) =>
          InterfaceDef(IdDef(id.name, id.span) withPositionOf op, tps, List(Operation(id, Many.empty(tps.span.synthesized), vps, bps, ret, opDoc, opSpan) withPositionOf op), doc, span())
      }

  def operation(doc: Doc): Operation =
    nonterminal:
      idDef() ~ params() ~ returnAnnotation() match {
        case id ~ (tps, vps, bps) ~ ret => Operation(id, tps, vps.unspan, bps.unspan, ret, doc, span())
      }

  def interfaceDef(keyword: TokenKind = `interface`): InterfaceDef =
    nonterminal:
      documented { doc =>
        // TODO
        // InterfaceDef(keyword ~> idDef(), maybeTypeParams(),
        //   `{` ~> manyWhile(documented { opDoc => `def` ~> operation(opDoc) }, documentedKind == `def`) <~ `}`, doc, span())
        InterfaceDef(keyword ~> idDef(), maybeTypeParams(),
          `{` ~> manyUntil(documented { opDoc => { `def` ~> operation(opDoc) } labelled "} or another operation declaration" }, `}`) <~ `}`, doc, span())
      }

  def namespaceDef(): Def =
    nonterminal:
      val doc = maybeDocumentation()
      consume(`namespace`)
      val id = idDef()
      // namespace foo { <DEFINITION>* }
      if peek(`{`) then NamespaceDef(id, braces { definitions() }, doc, span())
      // namespace foo
      // <DEFINITION>*
      else { semi(); NamespaceDef(id, definitions(), doc, span()) }


  def externDef(): Def =
    nonterminal:
      val doc = maybeDocumentation()
      { peek(`extern`); peek(1).kind } match {
        case `type`      => externType(doc)
        case `interface` => externInterface(doc)
        case `resource`  => externResource(doc)
        case `include`   => externInclude(doc)
        // extern """..."""
        case s: Str      => externString(doc)
        case Ident(_) | `pure` =>
          // extern IDENT def ...
          if (peek(2, `def`)) externFun(doc)
          // extern IDENT """..."""
          else externString(doc)
        // extern {...} def ...
        case _ => externFun(doc)
      }

  def featureFlag(): FeatureFlag = {
    expect("feature flag identifier") {
      case Ident("default") => FeatureFlag.Default(span())
      case Ident(flag)      => FeatureFlag.NamedFeatureFlag(flag, span())
    }
  }

  def maybeFeatureFlag(): FeatureFlag =
    nonterminal:
      backtrack(featureFlag()).getOrElse(FeatureFlag.Default(span()))

  def externType(doc: Doc): Def =
    ExternType(`extern` ~> `type` ~> idDef(), maybeTypeParams(), doc, span())
  def externInterface(doc: Doc): Def =
    ExternInterface(`extern` ~> `interface` ~> idDef(), maybeTypeParams().unspan, doc, span())
  def externResource(doc: Doc): Def =
    ExternResource(`extern` ~> `resource` ~> idDef(), blockTypeAnnotation(), doc, span())
  def externInclude(doc: Doc): Def =
    consume(`extern`)
    consume(`include`)
    val posAfterInclude = pos()
    ExternInclude(maybeFeatureFlag(), path().stripPrefix("\"").stripSuffix("\""), None, IdDef("", Span(source, posAfterInclude, posAfterInclude, Synthesized)), doc=doc, span=span())

  def externString(doc: Doc): Def =
    nonterminal:
      consume(`extern`)
      val posAfterExtern = pos()
      val ff = maybeFeatureFlag()
      expect("string literal") {
        case Str(contents, _) => ExternInclude(ff, "", Some(contents), IdDef("", Span(source, posAfterExtern, posAfterExtern, Synthesized)), doc, span())
      }

  def externFun(doc: Doc): Def =
    nonterminal:
      ((`extern` ~> maybeExternCapture()) ~ (`def` ~> idDef()) ~ params() ~ (returnAnnotation() <~ `=`)) match {
        case capt ~ id ~ (tps, vps, bps) ~ ret =>
          val bodies = manyWhile(externBody(), isExternBodyStart)
          ExternDef(capt, id, tps, vps, bps, ret, bodies, doc, span())
      }

  def externBody(): ExternBody =
    nonterminal:
      peek.kind match {
        case _: Ident => (peek(1).kind match {
          case `{` => ExternBody.EffektExternBody(featureFlag(), `{` ~> stmts(inBraces = true) <~ `}`, span())
          case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template(), span())
        }) labelled "extern body (string or block)"
        case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template(), span())
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
      val (exprs, strs) = manyWhile((`${` ~> expr() <~ `}$`, string()), `${`).unzip
      Template(first :: strs, exprs)

  def documented[T](p: Doc => T): T =
    p(maybeDocumentation())

  def maybeDocumentation(): Doc =
    nonterminal:
      peek.kind match
        case DocComment(_) =>
          val docComments = manyWhile({
            val msg = peek.kind match {
              case DocComment(message) => message
              case _ => ""
            }
            consume(peek.kind)
            msg
          }, peek.kind.isInstanceOf[DocComment])

          if (docComments.isEmpty) None
          else Some(docComments.mkString("\\n"))
        case _ => None

  def documentedKind(position: Int): TokenKind = peek(position).kind match {
    case DocComment(_) => documentedKind(position + 1)
    case k => k
  }

  def documentedKind: TokenKind = documentedKind(0)

  def maybeExternCapture(): CaptureSet =
    nonterminal:
      val posn = pos()
      if peek(`{`) || peek(`pure`) || isVariable then externCapture()
      else CaptureSet(List(IdRef(List("effekt"), "io", Span(source, posn, posn, Synthesized))), span())

  def externCapture(): CaptureSet =
    nonterminal:
      if peek(`{`) then captureSet()
      else if peek(`pure`) then `pure` ~> CaptureSet(Nil, span())
      else CaptureSet(List(idRef()), span())

  def path(): String =
    nonterminal:
      expect("path as string literal") {
        case Str(s, false) => s
      }

  def string(): String =
    nonterminal:
      expect("string literal") {
        case Str(s, _) => s
      }

  def maybeValueTypeAnnotation(): Option[ValueType] =
    nonterminal:
      if peek(`:`) then Some(valueTypeAnnotation()) else None

  def maybeBlockTypeAnnotation(): Option[BlockType] =
    nonterminal:
      if peek(`:`) then Some(blockTypeAnnotation()) else None

  def maybeReturnAnnotation(): Maybe[Effectful] =
    nonterminal:
      when(`:`) { Maybe.Some(effectful(), span()) } { Maybe.None(span()) }

  def returnAnnotation(): Effectful =
    if peek(`:`) then  `:` ~> effectful()
    else fail("return type annotation", peek.kind)

  def valueTypeAnnotation(): ValueType =
    if peek(`:`) then  `:` ~> valueType()
    else fail("a type annotation", peek.kind)

  def blockTypeAnnotation(): BlockType =
    if peek(`:`) then  `:` ~> blockType()
    else fail("a type annotation", peek.kind)

  def expr(): Term = peek.kind match {
    case _ => matchExpr() labelled "expression"
  }

  def ifExpr(): Term =
    nonterminal:
      If(`if` ~> parens { matchGuards().unspan },
        stmt(),
        when(`else`) { stmt() } { Return(UnitLit(span().emptyAfter), span().emptyAfter) }, span())

  def whileExpr(): Term =
    nonterminal:
      While(`while` ~> parens { matchGuards().unspan },
        stmt(),
        when(`else`) { Some(stmt()) } { None },
        span())

  def doExpr(): Term =
    nonterminal:
      (`do` ~> idRef()) ~ arguments() match {
        case id ~ (targs, vargs, bargs) => Do(None, id, targs, vargs, bargs, span())
      }

  /*
  <tryExpr> ::= try { <stmts> } <handler>+
  <handler> ::= with (<idDef> :)? <implementation>
  <implementation ::= <interfaceType> { <opClause>+ }
  */
  def tryExpr(): Term =
    nonterminal:
      `try` ~> stmt() ~ someWhile(handler(), `with`) match {
        case s ~ hs => TryHandle(s, hs.unspan, span())
      }

  def regionExpr(): Term =
    nonterminal:
      Region(`region` ~> idDef(), stmt(), span())

  def boxExpr(): Term = {
    nonterminal:
      consume(`box`)
      val expr = if (peek(`{`)) functionArg()
      else callExpr()
      val captures = backtrack {
        `at` ~> captureSet()
      }
      Box(captures, expr, span())
  }

  // TODO deprecate
  def funExpr(): Term =
    nonterminal:
      val blockLiteral = `fun` ~> BlockLiteral(Nil, valueParams().unspan, Nil, braces { stmts(inBraces = true) }, span())
      Box(Maybe.None(Span(source, pos(), pos(), Synthesized)), blockLiteral, blockLiteral.span.synthesized)

  def unboxExpr(): Term =
    nonterminal:
      Unbox(`unbox` ~> expr(), span())

  def newExpr(): Term =
    nonterminal:
      New(`new` ~> implementation(), span())

  def handler(): Handler =
    nonterminal:
      `with` ~> backtrack(idDef() <~ `:`) ~ implementation() match {
        case capabilityName ~ impl =>
          val capability = capabilityName map { name => BlockParam(name, Some(impl.interface), name.span.synthesized): BlockParam }
          Handler(capability.unspan, impl, span())
      }

  // This nonterminal uses limited backtracking: It parses the interface type multiple times.
  def implementation(): Implementation =
    nonterminal:
      // Interface[...] {}
      def emptyImplementation() = backtrack { Implementation(blockTypeRef(), `{` ~> Nil <~ `}`, span()) }

      // Interface[...] { def <NAME> = ... }
      def interfaceImplementation() = backtrack {
        val tpe = blockTypeRef()
        consume(`{`)
        if !peek(`def`) then fail("Expected at least one operation definition to implement this interface.")
        tpe
      } map { tpe =>
        Implementation(tpe, manyUntil(opClause() labelled "operation clause", `}`) <~ `}`, span())
      }

      // Interface[...] { () => ... }
      // Interface[...] { case ... => ... }
      def operationImplementation() = idRef() ~ maybeTypeArgs() ~ implicitResume ~ functionArg() match {
        case (id ~ tps ~ k ~ BlockLiteral(_, vps, bps, body, _)) =>
          val synthesizedId = IdRef(Nil, id.name, id.span.synthesized).withPositionOf(id)
          val interface = TypeRef(id, tps, id.span.synthesized).withPositionOf(id)
          val operation = OpClause(synthesizedId, Nil, vps, bps, None, body, k, Span(source, id.span.from, body.span.to, Synthesized)).withRangeOf(id, body)
          Implementation(interface, List(operation), span())
      }

      (emptyImplementation() orElse interfaceImplementation() getOrElse operationImplementation()) labelled "interface implementation (starting with its name)"

  def opClause(): OpClause =
    nonterminal:
      (`def` ~> idRef()) ~ paramsOpt() ~ maybeReturnAnnotation() ~ (`=` ~> stmt()) match {
        case id ~ (tps, vps, bps) ~ ret ~ body =>
         if (isSemi) {
           semi()

           if (!peek(`}`) && !peek(`def`) && !peek(EOF)) {
             softFailWith("Unexpected tokens after operation definition. Expected either a new operation definition or the end of the implementation.") {
               // consume until the next `def` or `}` or EOF
               while (!peek(`}`) && !peek(`def`) && !peek(EOF)) {
                 next()
               }
             }
           }
         }

         // TODO the implicitResume needs to have the correct position assigned (maybe move it up again...)
         OpClause(id, tps, vps, bps, ret.unspan, body, implicitResume, span())
      }

  def implicitResume: IdDef =
    nonterminal:
      IdDef("resume", span())

  def matchClause(): MatchClause =
    nonterminal:
      val patterns = `case` ~> some(matchPattern, `,`)
      val pattern: MatchPattern = patterns match {
        case Many(List(pat), _) => pat
        case pats => MultiPattern(pats.unspan, pats.span)
      }
      MatchClause(
        pattern,
        manyWhile(`and` ~> matchGuard(), `and`),
        // allow a statement enclosed in braces or without braces
        // both is allowed since match clauses are already delimited by `case`
        `=>` ~> (if (peek(`{`)) { stmt() } else { stmts(inBraces = true) }),
        span()
      )

  def matchGuards() =
    nonterminal:
      some(matchGuard, `and`)

  def matchGuard(): MatchGuard =
    nonterminal:
      expr() ~ when(`is`) { Some(matchPattern()) } { None } match {
        case e ~ Some(p) => MatchGuard.PatternGuard(e, p, span())
        case e ~ None    => MatchGuard.BooleanGuard(e, span())
      }

  def matchPattern(): MatchPattern =
    nonterminal:
      peek.kind match {
        case `__` => skip(); IgnorePattern(span())
        case _ if isVariable  =>
          idRef() match {
            case id if peek(`(`) => TagPattern(id, many(matchPattern, `(`, `,`, `)`).unspan, span())
            case IdRef(Nil, name, span) => AnyPattern(IdDef(name, span), span)
            case IdRef(_, name, _) => fail("Cannot use qualified names to bind a pattern variable")
          }
        case _ if isVariable =>
          AnyPattern(idDef(), span())
        case _ if isLiteral => LiteralPattern(literal(), span())
        case `(` => some(matchPattern, `(`, `,`, `)`) match {
          case Many(p :: Nil , _) => fail("Pattern matching on tuples requires more than one element")
          case Many(ps, _) => TagPattern(IdRef(List("effekt"), s"Tuple${ps.size}", span().synthesized), ps, span())
        }
        case k => fail("pattern", k)
      }

  def matchExpr(): Term =
    nonterminal:
      var sc = assignExpr()
      while (peek(`match`)) {
         val clauses = `match` ~> braces { manyWhile(matchClause(), `case`) }
         val default = when(`else`) { Some(stmt()) } { None }
         sc = Match(List(sc), clauses, default, span())
      }
      sc

  def assignExpr(): Term =
    nonterminal:
      orExpr() match {
        case x @ Term.Var(id, _) => when(`=`) { Assign(id, expr(), span()) } { x }
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
         val op = next()
         val right = nonTerminal()
         left = binaryOp(left, op, right).withRangeOf(left, right)
      }
      left

  // === AST Helpers ===

  private def binaryOp(lhs: Term, op: Token, rhs: Term): Term =
    nonterminal:
       if isThunkedOp(op.kind) then
         Call(
           IdTarget(IdRef(Nil, opName(op.kind), op.span(source).synthesized)),
           Nil, Nil,
           List(
             BlockLiteral(Nil, Nil, Nil, Return(lhs, lhs.span), lhs.span.synthesized),
             BlockLiteral(Nil, Nil, Nil, Return(rhs, rhs.span), rhs.span.synthesized),
           ),
           Span(source, lhs.span.from, rhs.span.to, Synthesized)
         )
       else
         Call(
           IdTarget(IdRef(Nil, opName(op.kind), op.span(source).synthesized)),
           Nil, List(ValueArg.Unnamed(lhs), ValueArg.Unnamed(rhs)), Nil,
           Span(source, lhs.span.from, rhs.span.to, Synthesized)
         )

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

  def TypeTuple(tps: Many[Type]): Type =
    TypeRef(IdRef(List("effekt"), s"Tuple${tps.size}", tps.span.synthesized), tps, tps.span.synthesized)

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
    nonterminal:
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
              e = Term.MethodCall(e, member, targs, vargs, bargs, span())
            } else {
              e = Term.MethodCall(e, member, Nil, Nil, Nil, span())
            }

          // function call
          case _ if isArguments =>
            val callee = e match {
              case Term.Var(id, _) => IdTarget(id)
              case other => ExprTarget(other)
            }
            val (targs, vargs, bargs) = arguments()
            e = Term.Call(callee, targs, vargs, bargs, span())

          // nothing to do
          case _ => ()
        }

      e
  }

  // argument lists cannot follow a linebreak:
  //   foo      ==    foo;
  //   ()             ()
  def isArguments: Boolean = lookbehind(1).kind != Newline && (peek(`(`) || peek(`[`) || peek(`{`))
  def arguments(): (List[ValueType], List[ValueArg], List[Term]) =
    if (!isArguments) fail("at least one argument section (types, values, or blocks)", peek.kind)
    (maybeTypeArgs().unspan, maybeValueArgs(), maybeBlockArgs())

  def maybeTypeArgs(): Many[ValueType] =
    nonterminal:
      if peek(`[`) then typeArgs() else Many.empty(span())
  def maybeValueArgs(): List[ValueArg] = if peek(`(`) then valueArgs() else Nil
  def maybeBlockArgs(): List[Term] = if peek(`{`) then blockArgs() else Nil

  inline def valueArg(): ValueArg = {
    if (isIdent && peek(1).kind == `=`) {
      nonterminal:
        (ident() <~ `=`) ~ expr() match {
          case name ~ v => ValueArg.Named(name, v, span())
        }
    } else { ValueArg.Unnamed(expr()) }
  }
  def typeArgs(): Many[ValueType] =
    nonterminal:
      some(valueType, `[`, `,`, `]`)
  def valueArgs(): List[ValueArg] =
    nonterminal:
      many(valueArg, `(`, `,`, `)`).unspan
  def blockArgs(): List[Term] =
    nonterminal:
      someWhile(blockArg(), `{`).unspan

  /**
   * Note: for this nonterminal, we need some backtracking.
   */
  def blockArg(): Term =
    nonterminal:
      backtrack { `{` ~> variable() <~ `}` } getOrElse { functionArg() }

  def functionArg(): BlockLiteral =
    nonterminal:
      braces {
        peek.kind match {
          // { case ... => ... }
          case `case` => someWhile(matchClause(), `case`) match { case cs =>
            val arity = cs match {
              case Many(MatchClause(MultiPattern(ps, _), _, _, _) :: _, _) => ps.length
              case _ => 1
            }
            // TODO fresh names should be generated for the scrutinee
            // also mark the temp name as synthesized to prevent it from being listed in VSCode
            val names = List.tabulate(arity){ n => s"__arg${n}" }
            BlockLiteral(
              Nil,
              names.map { name => ValueParam(IdDef(name, Span.missing(source)), None, Span.missing(source)) },
              Nil,
              Return(Match(names.map{ name => Var(IdRef(Nil, name, Span.missing(source)), Span.missing(source)) }, cs.unspan, None, Span.missing(source)), Span.missing(source)),
              Span.missing(source)
            )
          }
          case _ =>
            // { (x: Int) => ... }
            backtrack { lambdaParams() <~ `=>` } map {
              case (tps, vps, bps) => BlockLiteral(tps, vps, bps, stmts(inBraces = true), Span.missing(source)) : BlockLiteral
            } getOrElse {
              // { <STMTS> }
              BlockLiteral(Nil, Nil, Nil, stmts(inBraces = true), Span.missing(source)) : BlockLiteral
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
    case _ if isString       => templateString()
    case _ if isLiteral      => literal()
    case _ if isVariable     =>
      peek(1).kind match {
        case _: Str => templateString()
        case _ => variable()
      }
    case _ if isHole         => hole()
    case _ if isTupleOrGroup => tupleOrGroup()
    case _ if isListLiteral  => listLiteral()
    case `${` =>
      nonterminal:
        val startPosition = position
        val _ = manyUntil({ skip() }, `}$`)
        val endPosition = position
        if peek(`}$`) then consume(`}$`) else if peek(`}`) then consume(`}`)

        val msg = s"Unexpected splice: string interpolation $${ ... } is only allowed in strings!"
        softFail(msg, startPosition, endPosition)

        StringLit("<splice>", span().synthesized) // HACK(jiribenes, 2025-07-01): We should really make a Tree.Error for these situations.

    case k => fail("variables, literals, tuples, lists, holes or group", k)
  }

  def isListLiteral: Boolean = peek.kind match {
    case `[` => true
    case _ => false
  }
  def listLiteral(): Term =
    nonterminal:
      manyTrailing(expr, `[`, `,`, `]`).foldRight(NilTree) { ConsTree }

  private def NilTree: Term =
    Call(IdTarget(IdRef(List(), "Nil", Span.missing(source))), Nil, Nil, Nil, Span.missing(source))

  private def ConsTree(el: Term, rest: Term): Term =
    Call(IdTarget(IdRef(List(), "Cons", Span.missing(source))), Nil, List(ValueArg.Unnamed(el), ValueArg.Unnamed(rest)), Nil, Span.missing(source))

  def isTupleOrGroup: Boolean = peek(`(`)
  def tupleOrGroup(): Term =
    nonterminal:
      some(expr, `(`, `,`, `)`) match {
        case Many(e :: Nil, _) => e
        case Many(xs, _) => Call(IdTarget(IdRef(List("effekt"), s"Tuple${xs.size}", span().synthesized)), Nil, xs.map(ValueArg.Unnamed), Nil, span().synthesized)
      }

  def isHole: Boolean = peek.kind match {
    case `<>` => true
    case `<{` => true
    case HoleStr(_) => true
    case _ => false
  }
  def hole(): Term = {
    nonterminal:
      peek.kind match {
        case `<>` => `<>` ~> Hole(IdDef("hole", span().synthesized), Template(Nil, Nil), span())
        case `<{` => {
          val s = `<{` ~> stmts(inBraces = true) <~ `}>`
          Hole(IdDef("hole", span().synthesized), Template(Nil, List(s)), span())
        }
        case _: HoleStr => {
          val body = holeTemplate()
          Hole(IdDef("hole", span().synthesized), body, span())
        }
        case k => fail("hole", k)
      }
    }

  def holeTemplate(): Template[Stmt] =
    nonterminal:
      val first = holeString()
      val (s, strs) = manyWhile((`${` ~> stmts(inBraces = true) <~ `}$`, holeString()), `${`).unzip
      Template(first :: strs, s)

  def holeString(): String =
    nonterminal:
      expect("natural language text") {
        case HoleStr(s) => s
      }

  def isLiteral: Boolean = peek.kind match {
    case _: (Integer | Float | Str | Chr) => true
    case `true` => true
    case `false` => true
    case _ => isUnitLiteral
  }

  def isString: Boolean = peek.kind match {
    case _: Str => true
    case _      => false
  }

  def templateString(): Term =
    nonterminal:
      backtrack(idRef()) ~ template() match {
        // We do not need to apply any transformation if there are no splices _and_ no custom handler id is given
        case Maybe(None, _) ~ Template(str :: Nil, Nil) => StringLit(str, Span.missing(source))
        // s"a${x}b${y}" ~> s { do literal("a"); do splice(x); do literal("b"); do splice(y); return () }
        case id ~ Template(strs, args) =>
          val target = id.getOrElse(IdRef(Nil, "s", id.span.synthesized))
          val doLits = strs.map { s =>
            Do(None, IdRef(Nil, "literal", Span.missing(source)), Nil, List(ValueArg.Unnamed(StringLit(s, Span.missing(source)))), Nil, Span.missing(source))
          }
          val doSplices = args.map { arg =>
            Do(None, IdRef(Nil, "splice", Span.missing(source)), Nil, List(ValueArg.Unnamed(arg)), Nil, Span.missing(source))
          }
          val body = interleave(doLits, doSplices)
            .foldRight(Return(UnitLit(Span.missing(source)), Span.missing(source))) { (term, acc) => ExprStmt(term, acc, Span.missing(source)) }
          val blk = BlockLiteral(Nil, Nil, Nil, body, Span.missing(source))
          Call(IdTarget(target), Nil, Nil, List(blk), Span.missing(source))
      }

  // TODO: This should use `expect` as it follows the same pattern.
  // However, we currently cannot use `expect` here as the unit literal consists of two tokens
  def literal(): Literal =
    nonterminal:
      peek.kind match {
        case Integer(v)         => skip(); IntLit(v, span())
        case Float(v)           => skip(); DoubleLit(v, span())
        case Str(s, multiline)  => skip(); StringLit(s, span())
        case Chr(c)             => skip(); CharLit(c, span())
        case `true`             => skip(); BooleanLit(true, span())
        case `false`            => skip(); BooleanLit(false, span())
        case t if isUnitLiteral => skip(); skip(); UnitLit(span())
        case t => fail("a literal", t)
      }

  // Will also recognize ( ) as unit if we do not emit space in the lexer...
  private def isUnitLiteral: Boolean = peek(`(`) && peek(1, `)`)

  def isVariable: Boolean = isIdRef
  def variable(): Term =
    nonterminal:
      Var(idRef(), span())

  def isIdRef: Boolean = isIdent

  def idRef(): IdRef =
    nonterminal:
      some(ident, PathSep) match {
        case ids => IdRef(ids.init, ids.last, span())
      }

  def idDef(): IdDef =
    nonterminal:
      IdDef(ident(), span())

  def isIdent: Boolean = peek.kind match {
    case Ident(id) => true
    case _ => false
  }
  def ident(): String =
    nonterminal:
      expect("identifier") { case Ident(id) => id }

  /*
   * Type grammar by precedence:
   *
   * Type ::= Id ('[' Type ',' ... ']')?                                           refType
   *
   *       | '(' Type ',' ... ')'                                                  atomicType
   *       | '(' Type ')'
   *
   *       | Type '=>' Type ('/' SetType)?                                         functionType
   *       | '(' Type ',' ... ')' ('{' Type '}')* '=>' Type ('/' SetType)?
   *
   *       | Type 'at' Id                                                          boxedType
   *       | Type 'at' '{' Id ',' ... '}'
   *       | Type ('/' SetType)?
   *
   * SetType ::= Type
   *        | '{' Type ',' ... '}'
   */
  def effects(): Effects = {
    nonterminal:
      if (peek(`{`)) {
        val effects = many(refType, `{`, `,`, `}`)
        Effects(effects.unspan, effects.span)
      }
      else
        Effects(List(refType()), span())
  } labelled "effect set"

  def maybeEffects(): Effects = {
    nonterminal:
      when(`/`) {
        effects()
      } {
        Effects.Pure(span().synthesized)
      }
  }

  def refType(): TypeRef =
    nonterminal:
      TypeRef(idRef(), maybeTypeArgs(), span())

  // Parse atomic types: Tuples, parenthesized types, type references (highest precedence)
  private def atomicType(): Type =
    nonterminal:
      peek.kind match {
        case `(` =>
          some(boxedType, `(`, `,`, `)`) match {
            case Many(tpe :: Nil, _) => tpe
            case tpes => TypeTuple(tpes)
          }
        case _ => refType()
      }

  // Parse function types (middle precedence)
  private def functionType(): Type = {
    // Complex function type: [T]*(Int, String)*{Exc} => Int / {Effect}
    def functionTypeComplex: Maybe[Type] = backtrack {
      maybeTypeParams() ~ maybeValueTypes() ~ (maybeBlockTypeParams() <~ `=>`)
    } map { case tparams ~ vparams ~ bparams =>
      (atomicType() labelled "return type") ~ maybeEffects() match {
        case  t ~ effs => FunctionType(tparams, vparams, bparams, t, effs, span())
      }
    }

    // Simple function type: Int => Int
    def functionTypeSimple: Maybe[Type] = backtrack {
      refType() <~ `=>`
    } map { tpe =>
      FunctionType(Many.empty(tpe.span.emptyAfter), Many(List(tpe), tpe.span.emptyAfter), Many.empty(tpe.span.emptyAfter), atomicType(), maybeEffects(), span())
    }

    // Try to parse each function type variant, fall back to basic type if none match
    nonterminal:
      functionTypeSimple orElse functionTypeComplex getOrElse atomicType()
  }

  // Parse boxed types and effectfuls (lowest precedence)
  // "Top-level" parser for a generic type.
  private def boxedType(): Type = {
    nonterminal:
      // Parse the function type first
      val tpe = functionType()

      // TODO: these should probably be in a loop to parse as many `at`s and `\`s as possible?
      val boxed = when(`at`) {
        BoxedType(tpe, captureSet() labelled "capture set", span())
      } {
        tpe
      }

      if (peek(`/`)) {
        Effectful(boxed, maybeEffects(), span())
      } else {
        boxed
      }
  }

  // NOTE: ValueType, BlockType are just aliases for Type.
  def blockType(): BlockType = boxedType() labelled "block type"
  def valueType(): ValueType = boxedType() labelled "value type"

  // Completely specialized for TypeRef: we only parse `refType` here, we don't go through the whole hierarchy.
  // This results in slightly worse errors, but massively simplifies the design.
  inline def blockTypeRef(): TypeRef = refType()

  // Somewhat specialized: we parse a normal type, if it's not a ${tpe} / ${effs},
  // then pretend the effect set is empty. This seems to work out fine :)
  def effectful(): Effectful = {
    nonterminal:
      (boxedType() match
        case eff: Effectful => eff
        case tpe => {
          Effectful(tpe, Effects.Pure(Span(source, pos(), pos(), Synthesized)), span())
        }) labelled "return-type and effects"
  }

  def maybeTypeParams(): Many[Id] =
    nonterminal:
      if peek(`[`) then typeParams() else Many.empty(span())

  def typeParams(): Many[Id] =
    nonterminal:
      some(idDef, `[`, `,`, `]`)

  def maybeBlockTypeParams(): Many[(Maybe[IdDef], Type)] =
    nonterminal:
      if peek(`{`) then blockTypeParams() else Many.empty(span())

  def blockTypeParams(): Many[(Maybe[IdDef], Type)] =
    nonterminal:
      someWhile(blockTypeParam(), `{`)

  def blockTypeParam(): (Maybe[IdDef], Type) =
    nonterminal:
      braces { (backtrack { idDef() <~ `:` }, blockType()) }


  def lambdaParams(): (List[Id], List[ValueParam], List[BlockParam]) =
    nonterminal:
      if isVariable then (Nil, List(ValueParam(idDef(), None, span())), Nil)  else paramsOpt()

  def params(): (Many[Id], Many[ValueParam], Many[BlockParam]) =
    nonterminal:
      maybeTypeParams() ~ maybeValueParams() ~ maybeBlockParams() match {
        case tps ~ vps ~ bps => (tps, vps, bps)
      }

  def paramsOpt(): (List[Id], List[ValueParam], List[BlockParam]) =
    nonterminal:
      maybeTypeParams() ~ maybeValueParamsOpt() ~ maybeBlockParamsOpt() match {
        case (tps ~ vps ~ bps) =>
          // fail("Expected a parameter list (multiple value parameters or one block parameter; only type annotations of value parameters can be currently omitted)")
          (tps.unspan, vps, bps)
      }

  def maybeValueParamsOpt(): List[ValueParam] =
    nonterminal:
      if peek(`(`) then valueParamsOpt() else Nil

  def valueParamsOpt(): List[ValueParam] =
    nonterminal:
      many(valueParamOpt, `(`, `,`, `)`).unspan

  def maybeValueParams(): Many[ValueParam] =
    nonterminal:
      if peek(`(`) then valueParams() else Many.empty(span())

  def valueParams(): Many[ValueParam] =
    nonterminal:
      many(valueParam, `(`, `,`, `)`)

  def valueParam(): ValueParam =
    nonterminal:
      ValueParam(idDef(), Some(valueTypeAnnotation()), span())

  def valueParamOpt(): ValueParam =
    nonterminal:
      ValueParam(idDef(), maybeValueTypeAnnotation(), span())

  def maybeBlockParams(): Many[BlockParam] =
    nonterminal:
      Many(manyWhile(`{` ~> blockParam() <~ `}`, `{`), span())

  def blockParams(): List[BlockParam] =
    nonterminal:
      someWhile(`{` ~> blockParam() <~ `}`, `{`).unspan

  def maybeBlockParamsOpt(): List[BlockParam] =
    nonterminal:
      manyWhile(`{` ~> blockParamOpt() <~ `}`, `{`)

  def blockParamsOpt(): List[BlockParam] =
    nonterminal:
      someWhile(`{` ~> blockParamOpt() <~ `}`, `{`).unspan

  def blockParam(): BlockParam =
    nonterminal:
      BlockParam(idDef(), Some(blockTypeAnnotation()), span())

  def blockParamOpt(): BlockParam =
    nonterminal:
      BlockParam(idDef(), when(`:`)(Some(blockType()))(None), span())

  def maybeValueTypes(): Many[Type] =
    nonterminal:
      if peek(`(`) then valueTypes() else Many.empty(span())

  def valueTypes(): Many[Type] =
    nonterminal:
      many(valueType, `(`, `,`, `)`)

  def captureSet(): CaptureSet =
    nonterminal:
      CaptureSet(many(idRef, `{`, `,` , `}`).unspan, span())

  // Generic utility functions
  // -------------------------
  // ... for writing parsers.

  /**
   * Aborts parsing with the given message
   */
  def fail(expected: String, got: TokenKind): Nothing =
    throw Fail.expectedButGot(currentLabel.getOrElse { expected }, explain(got), position)

  def fail(msg: String): Nothing =
    throw Fail(msg, position)

  def softFail(message: String, start: Int, end: Int): Unit = {
    softFails += SoftFail(message, start, end)
  }

  inline def softFailWith[T](inline message: String)(inline p: => T): T = {
    val startPosition = position
    val result = p
    val endPosition = position

    if (startPosition == endPosition) {
      softFail(message, startPosition, startPosition)
    } else {
      softFail(message, startPosition, endPosition - 1)
    }
    result
  }

  /**
   * Guards `thn` by token `t` and consumes the token itself, if present.
   */
  inline def when[T](t: TokenKind)(inline thn: => T)(inline els: => T): T =
    if peek(t) then { consume(t); thn } else els

  inline def backtrack[T](inline restoreSoftFails: Boolean = true)(inline p: => T): Maybe[T] =
    val before = position
    val beforePrevious = previous
    val labelBefore = currentLabel
    val softFailsBefore = softFails.clone()
    try { Maybe.Some(p, span(tokens(before).end)) } catch {
      case Fail(_, _) => {
        position = before
        previous = beforePrevious
        currentLabel = labelBefore
        if restoreSoftFails then softFails = softFailsBefore
        Maybe.None(Span(source, pos(), pos(), Synthesized))
      }
    }
  inline def backtrack[T](inline p: => T): Maybe[T] = backtrack(restoreSoftFails = true)(p)

  def interleave[A](xs: List[A], ys: List[A]): List[A] = (xs, ys) match {
    case (x :: xs, y :: ys) => x :: y :: interleave(xs, ys)
    case (Nil, ys) => ys
    case (xs, Nil) => xs
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
  inline def some[T](p: () => T, before: TokenKind, sep: TokenKind, after: TokenKind): Many[T] =
    nonterminal:
      consume(before)
      val res = some(p, sep)
      consume(after)
      Many(res.unspan, span())

  inline def some[T](p: () => T, sep: TokenKind): Many[T] =
    nonterminal:
      val components: ListBuffer[T] = ListBuffer.empty
      components += p()
      while (peek(sep)) {
        consume(sep)
        components += p()
      }
      Many(components.toList, span())

  inline def someWhile[T](p: => T, lookahead: TokenKind): Many[T] =
    someWhile(p, peek(lookahead))

  inline def someWhile[T](p: => T, predicate: => Boolean): Many[T] =
    nonterminal:
      val components: ListBuffer[T] = ListBuffer.empty
      components += p
      while (predicate) {
        components += p
      }
      Many(components.toList, span())

  inline def manyWhile[T](p: => T, lookahead: TokenKind): List[T] =
    manyWhile(p, peek(lookahead))

  inline def manyUntil[T](p: => T, lookahead: TokenKind): List[T] =
    manyUntil(p, peek(lookahead))

  inline def manyWhile[T](p: => T, predicate: => Boolean): List[T] =
    val components: ListBuffer[T] = ListBuffer.empty
    while (predicate) {
      components += p
    }
    components.toList

  inline def manyUntil[T](p: => T, predicate: => Boolean): List[T] =
    manyWhile(p, !predicate)

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

  inline def many[T](p: () => T, before: TokenKind, sep: TokenKind, after: TokenKind): Many[T] =
    nonterminal:
      consume(before)
      if (peek(after)) {
        consume(after)
        Many.empty(span())
      } else {
        val components: ListBuffer[T] = ListBuffer.empty
        components += p()
        while (peek(sep)) {
          consume(sep)
          components += p()
        }
        consume(after)
        Many(components.toList,span())
      }


  inline def manyTrailing[T](p: () => T, before: TokenKind, sep: TokenKind, after: TokenKind): List[T] =
    consume(before)
    if (peek(after)) {
      consume(after)
      Nil
    } else if (peek(sep)) {
      consume(sep)
      consume(after)
      Nil
    } else {
      val components: ListBuffer[T] = ListBuffer.empty
      components += p()
      while (peek(sep)) {
        consume(sep)

        if (!peek(after)) {
          components += p()
        }
      }
      consume(after)
      components.toList
    }


  // Positions

  // the "span" effect used to compute the positions of nodes
  inline def span(): Span =
    span(_start.value)

  // creates a Span with a given start and the end position of the previous token
  inline def span(start : Int ): Span =
    val end = pos()

    // We need some special handling in the case where we did not consume any tokens.
    // In this case, we have that start > end.
    // This can be seen in the following example:
    //
    //     previous
    //        /   peek
    //       /    /
    //┌──┐ ┌──┐  ┌┐┌┐
    // def foo   = <>
    //        |
    //      we want to produce an empty span here for the omitted parameters and return type
    //
    // When parsing the optional parameters and the optional return type, we get the following values:
    // previous = "foo", peek = "=", _start = peek.start, end = previous.end +1
    // therefore, in this case start is greater than end.

    if start > end then {
      // We did not consume anything (except for whitespace), so we generate an empty span just after the previous token
      Span(source, pos(), pos())
    } else {
      Span(source, start, end)
    }

  /* Hack: Resolving Kiama positions takes a lot of time, let's cache `offset` ~> `kiama.Position`.
   * This makes the parser about 30% faster. */
  private val positionCache = scala.collection.mutable.HashMap[Int, Position]()
  private def getPosition(offset: Int): Position =
    positionCache.getOrElseUpdate(offset, source.offsetToPosition(offset))

  // the handler for the "span" effect.
  private val _start: scala.util.DynamicVariable[Int] = scala.util.DynamicVariable(0)
  inline def nonterminal[T](inline p: => T): T = _start.withValue(peek.start) {
    val startToken = peek
    val start = startToken.start
    val res = p
    val end = pos()

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

    val startPos = getPosition(start)
    val endPos = getPosition(end)

    // recursively add positions to subtrees that are not yet annotated
    // this is better than nothing and means we have positions for desugared stuff
    def annotatePositions(res: Any): Unit = res match {
      case l: List[_] =>
        if (positions.getRange(l).isEmpty) {
          positions.setStart(l, startPos)
          positions.setFinish(l, endPos)
          l.foreach(annotatePositions)
        }
      case t: Tree =>
        val recurse = positions.getRange(t).isEmpty
        if(positions.getStart(t).isEmpty) positions.setStart(t, startPos)
        if(positions.getFinish(t).isEmpty) positions.setFinish(t, endPos)
        t match {
          case p: Product if recurse =>
            p.productIterator.foreach { c =>
              annotatePositions(c)
            }
          case _ => ()
        }
      case _ => ()
    }
    annotatePositions(res)

    // still annotate, in case it is not Tree
    positions.setStart(res, startPos)
    positions.setFinish(res, endPos)

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
