package effekt

import effekt.context.Context
import effekt.lexer.*
import effekt.lexer.TokenKind.{`::` as PathSep, *}
import effekt.source.*
import effekt.source.Origin.Synthesized
import effekt.util.VirtualSource
import kiama.parsing.{Input, ParseResult}
import kiama.util.{Position, Range, Source}

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/**
 * String templates containing unquotes `${... : T}`
 */
case class Template[+T](strings: List[String], args: List[T]) {
  def map[R](f: T => R): Template[R] = Template(strings, args.map(f))
}

case class SpannedTemplate[T](strings: List[Spanned[String]], args: List[Spanned[T]]) {
  def map[R](f: T => R): SpannedTemplate[R] = SpannedTemplate(strings, args.map(_.map(f)))
  def unspan: Template[T] = Template(strings.map(_.unspan), args.map(_.unspan))
}

case class Spanned[T](unspan: T, span: Span) {
  def map[R](f: T => R): Spanned[R] = Spanned(f(unspan), span)
}

object Parser extends Phase[Source, Parsed] {

  val phaseName = "parser"

  def run(source: Source)(implicit C: Context): Option[PhaseResult.Parsed] = source match {
    case VirtualSource(decl, _) => Some(decl)
    case source =>
      Context.timed(phaseName, source.name) {
        val tokens = effekt.lexer.Lexer.lex(source)
        val parser = new Parser(tokens, source)
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

class Parser(tokens: Seq[Token], source: Source) {

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
  def stmts(inBraces: Boolean = false): Stmt =
    nonterminal:
      (peek.kind match {
        case `{` => BlockStmt(braces { stmts(inBraces = true) }, span())
        case `val`  => valStmt(inBraces)
        case _ if isDefinition && inBraces => DefStmt(definition(), semi() ~> stmts(inBraces), span())
        case _ if isDefinition => fail("Definitions are only allowed, when enclosed in braces.")
        case `with` => withStmt(inBraces)
        case `var`  => DefStmt(varDef(noInfo()), semi() ~> stmts(inBraces), span())
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

  // with val <PATTERN> (, <PATTERN>)* = <EXPR>; <STMTS>
  // with def <BLOCKPARAM> = <EXPR>; <STMTS>
  // with <EXPR>; <STMTS>
  def withStmt(inBraces: Boolean): Stmt = `with` ~> peek.kind match {
    case `val` =>
      consume(`val`)
      val patterns = some(matchPattern, `,`)
      val call = `=` ~> expr()
      val body = semi() ~> stmts(inBraces)
      desugarWithPatterns(patterns, call, body, span())

    case `def` =>
      val params = (`def` ~> peek.kind match {
        case `{` => blockParamsOpt()
        case _ => List(blockParamOpt()) // TODO copy position
      })
      val call = `=` ~> expr()
      val body = semi() ~> stmts(inBraces)
      val blockLit: BlockLiteral = BlockLiteral(Nil, Nil, params, body, body.span.synthesized)
      desugarWith(call, blockLit, span())

    case _ =>
      val call = expr()
      val body = semi() ~> stmts(inBraces)
      val blockLit: BlockLiteral = BlockLiteral(Nil, Nil, Nil, body, body.span.synthesized)
      desugarWith(call, blockLit, span())
  }

  // Desugar `with val` with pattern(s)
  def desugarWithPatterns(patterns: Many[MatchPattern], call: Term, body: Stmt, withSpan: Span): Stmt = {
    // Check if all patterns are simple variable bindings or ignored
    val allSimpleVars = patterns.unspan.forall {
      case AnyPattern(_, _) => true
      case IgnorePattern(_) => true
      case _ => false
    }

    val blockLit: BlockLiteral = if (allSimpleVars) {
      // Simple case: all patterns are just variable names (or ignored)
      // Desugar to: call { (x, y, _, ...) => body }
      val vparams: List[ValueParam] = patterns.unspan.map {
        case AnyPattern(id, span) => ValueParam(id, None, span)
        case _ => sys.error("impossible: checked above")
      }
      BlockLiteral(Nil, vparams, Nil, body, body.span.synthesized)
    } else {
      // Complex case: at least one pattern needs matching
      // Desugar to: call { case pat1, pat2, ...  => body }
      // This requires one argument per pattern, matching against multiple scrutinees
      val patternList = patterns.unspan
      val names = List.tabulate(patternList.length) { n => s"__withArg${n}" }
      val argSpans = patternList.map(_.span)

      val vparams: List[ValueParam] = names.zip(argSpans).map { (name, span) =>
        ValueParam(IdDef(name, span.synthesized), None, span.synthesized)
      }
      val scrutinees = names.zip(argSpans).map { (name, span) =>
        Var(IdRef(Nil, name, span.synthesized), span.synthesized)
      }

      val pattern: MatchPattern = patterns match {
        case Many(List(single), _) => single
        case Many(ps, span) => MultiPattern(ps, span)
      }

      val clause = MatchClause(pattern, Nil, body, Span(source, pattern.span.from, body.span.to, Synthesized))
      val matchExpr = Match(scrutinees, List(clause), None, withSpan.synthesized)
      val matchBody = Return(matchExpr, withSpan.synthesized)
      BlockLiteral(Nil, vparams, Nil, matchBody, withSpan.synthesized)
    }

    desugarWith(call, blockLit, withSpan)
  }

  def desugarWith(call: Term, blockLit: BlockLiteral, withSpan: Span): Stmt = call match {
    case MethodCall(receiver, id, tps, vargs, bargs, callSpan) =>
      Return(MethodCall(receiver, id, tps, vargs, bargs :+ blockLit, callSpan), withSpan.synthesized)
    case Call(callee, tps, vargs, bargs, callSpan) =>
      Return(Call(callee, tps, vargs, bargs :+ blockLit, callSpan), withSpan.synthesized)
    case Var(id, varSpan) =>
      Return(Call(IdTarget(id), Nil, Nil, blockLit :: Nil, varSpan), withSpan.synthesized)
    case Do(id, targs, vargs, bargs, doSpan) =>
      Return(Do(id, targs, vargs, bargs :+ blockLit, doSpan), withSpan.synthesized)
    case term =>
      Return(Call(ExprTarget(term), Nil, Nil, blockLit :: Nil, term.span.synthesized), withSpan.synthesized)
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
        else when(`return`) { Return(expr(), span()) } { Return(expr(), span()) }
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

      // potential documentation for the file / module
      documented { info =>
        val (name, moduleInfo, unusedInfo) = peek.kind match {
          case `module` =>
            consume(`module`)
            (moduleName(), info, None)
          case `import` if info.nonEmpty =>
            softFail("Imports cannot be documented", span().from, span().to)
            (defaultModulePath, noInfo(), None)
          case _ =>
            (defaultModulePath, noInfo(), Some(info))
        }

        val imports = manyWhile(includeDecl(), `import`)

        val definitions = unusedInfo match {
          case Some(info) if info.nonEmpty => toplevelDefs(info)
          case _ => toplevelDefs()
        }

        if (!peek(`EOF`)) {
          // NOTE: This means we expected EOF, but there's still some _stuff_ left over.
          softFailWith("Expected top-level definition") {
            manyUntil({ skip() }, `EOF`)
          }
        }

        ModuleDecl(name, imports, definitions, moduleInfo.onlyDoc().doc, span())
      }

  @tailrec
  private def shebang(): Unit =
    peek.kind match {
      case Shebang(_) => consume(peek.kind); shebang()
      case _ => ()
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

  def isToplevel: Boolean = peek.kind match {
    case `val` | `def` | `type` | `effect` | `namespace` | `interface` | `type` | `record` | `var` | `include` | `extern` => true
    case _ => false
  }

  def toplevel(): Def =
    documented: doc =>
      toplevelDef(doc)

  private def toplevelDef(info: Info): Def =
      peek.kind match {
        case _ if info.isExtern.nonEmpty => externDef(info)
        case `val`       => valDef(info)
        case `def`       => defDef(info)
        case `interface` => interfaceDef(info)
        case `type`      => typeOrAliasDef(info)
        case `record`    => recordDef(info)
        case `effect`    => effectOrOperationDef(info)
        case `namespace` => namespaceDef(info)
        case `var`       => backtrack {
          softFailWith("Mutable variable declarations are currently not supported on the toplevel.") {
            varDef(info)
          }
        } getOrElse fail("Mutable variable declarations are currently not supported on the toplevel.")
        case _ => fail("Expected a top-level definition")
      }

  private def toplevelDefs(): List[Def] =
    documented: info =>
      toplevelDefs(info)

  private def toplevelDefs(info: Info): List[Def] =
    nonterminal:
      peek.kind match {
        case `namespace` =>
          consume(`namespace`)
          val id = idDef()
          peek.kind match {
            case `{` =>
              val defs = braces(toplevelDefs())
              val df = toplevelDefs()
              NamespaceDef(id, defs, info, span()) :: df
            case _   =>
              val defs = toplevelDefs()
              List(NamespaceDef(id, defs, info, span()))
          }
        case _ if info.isExtern.nonEmpty || isToplevel =>
          nonterminal { toplevelDef(info) } :: toplevelDefs()
        case _ => Nil
      }

  def isDefinition: Boolean = peek.kind match {
    case `val` | `def` | `type` | `effect` => true
    case `interface` | `type` | `record` =>
      val kw = peek.kind
      fail(s"Only supported on the toplevel: ${kw.toString} declaration.")
    case _ => false
  }

  def definition(): Def =
    documented: info =>
      peek.kind match {
        case `val`       => valDef(info)
        case `def`       => defDef(info)
        case `type`      => typeOrAliasDef(info)
        case `effect`    => effectDef(info)
        case _ => fail("Expected definition")
      }

  def definitions(): List[Def] =
    nonterminal:
      manyWhile(definition(), isDefinition)

  def functionBody: Stmt = stmts() // TODO error context: "the body of a function definition"

  def valDef(info: Info): Def =
    ValDef(`val` ~> idDef(), maybeValueTypeAnnotation(), `=` ~> stmt(), info.onlyDoc(), span())

  /**
   * In statement position, val-definitions can also be destructing:
   *   i.e. val (l, r) = point(); ...
   */
  def valStmt(inBraces: Boolean): Stmt =
    documented: info =>
      val startPos = peek.start
      def simpleLhs() = backtrack {
        // Make sure there's either a `:` or `=` next, otherwise goto `matchLhs`
        def canCut: Boolean = peek(`:`) || peek(`=`)
        `val` ~> idDef() <~ (if !canCut then fail("Expected a `:` or a `=`"))
      } map { id =>
          val tpe = maybeValueTypeAnnotation() <~ `=`
          val binding = stmt()
          val endPos = pos()
          val valDef = ValDef(id, tpe, binding, info.onlyDoc(), Span(source, startPos, endPos))
          DefStmt(valDef, { semi(); stmts(inBraces) }, span())
      }
      def matchLhs() =
        (`val` ~> matchPattern()) ~ manyWhile(`and` ~> matchGuard(), `and`) <~ `=` match {
          case AnyPattern(id, _) ~ Nil =>
            val binding = stmt()
            val endPos = pos()
            val valDef = ValDef(id, None, binding, info.onlyDoc(), Span(source, startPos, endPos))
            DefStmt(valDef, { semi(); stmts(inBraces) }, span())
          case p ~ guards =>
            // matches do not support doc comments, so we ignore `info`
            val sc = expr()
            val endPos = pos()
            val default = when(`else`) { Some(stmt()) } { None }
            val body = semi() ~> stmts(inBraces)
            val clause = MatchClause(p, guards, body, Span(source, p.span.from, sc.span.to))
            val matching = Match(List(sc), List(clause), default, Span(source, startPos, endPos, Synthesized))
            Return(matching, span().synthesized)
        }

      simpleLhs() getOrElse matchLhs()


  def varDef(info: Info): Def =
    (`var` ~> idDef()) ~ maybeValueTypeAnnotation() ~ when(`in`) { Some(idRef()) } { None } ~ (`=` ~> stmt()) match {
      case id ~ tpe ~ Some(reg) ~ expr => RegDef(id, tpe, reg, expr, info.onlyDoc(), span())
      case id ~ tpe ~ None ~ expr      => VarDef(id, tpe, expr, info.onlyDoc(), span())
    }

  def defDef(info: Info): Def =
    val id = consume(`def`) ~> idDef()

    def isBlockDef: Boolean = peek(`:`) || peek(`=`) || peek(`at`)

    if isBlockDef then
      // (: <VALUETYPE>)? `=` <EXPR>
      DefDef(id, maybeCaptureSet(), maybeBlockTypeAnnotation(), `=` ~> expr(), info, span())
    else
      // [...](<PARAM>...) {...} `=` <STMT>>
      val (tps, vps, bps) = params()
      val captures = maybeCaptureSet()
      FunDef(id, tps, vps, bps, captures, maybeReturnAnnotation(), `=` ~> stmts(), info, span())


  // right now: data type definitions (should be renamed to `data`) and type aliases
  def typeOrAliasDef(info: Info): Def =
    val id ~ tps = (`type` ~> idDef()) ~ maybeTypeParams()

    peek.kind match {
      case `=` => `=` ~> TypeDef(id, tps.unspan, valueType(), info, span())
      case _ => DataDef(id, tps, braces { manyUntil({ constructor() <~ semi() }, `}`) }, info, span())
    }

  def recordDef(info: Info): Def =
    RecordDef(`record` ~> idDef(), maybeTypeParams(), valueParams(), info, span())

  def constructor(): Constructor =
    documented: info =>
      Constructor(idDef(), maybeTypeParams(), valueParams(), info.onlyDoc().doc, span()) labelled "constructor"

  // On the top-level both
  //    effect Foo = {}
  // and
  //    effect Foo(): Int
  // are allowed. Here we simply backtrack, since effect definitions shouldn't be
  // very long and cannot be nested.
  def effectOrOperationDef(info: Info): Def = {
    // We used to use `effect Foo { def a(); def b(); ... }` for multi-operation interfaces,
    // but now we use `interface Foo { ... }` instead.
    // If we can't parse `effectDef` or `operationDef`, we should try parsing an interface with the wrong keyword
    // and report an error to the user if the malformed interface would be valid.
    def interfaceDefUsingEffect(): Maybe[InterfaceDef] =
      backtrack(restoreSoftFails = false):
        softFailWith("Unexpected 'effect', did you mean to declare an interface of multiple operations using the 'interface' keyword?"):
          interfaceDef(info, `effect`)

    backtrack { effectDef(info) }
      .orElse { interfaceDefUsingEffect() }
      .getOrElse { operationDef(info) } // The `operationDef` should be last as to not cause spurious errors later.
  }

  def effectDef(info: Info): Def =
    // effect <NAME> = <EFFECTS>
    EffectDef(`effect` ~> idDef(), maybeTypeParams().unspan, `=` ~> effects(), info, span())

  // effect <NAME>[...](...): ...
  def operationDef(info: Info): Def =
    `effect` ~> operation(info) match {
      case op @ Operation(id, tps, vps, bps, ret, opDoc, opSpan) =>
        InterfaceDef(
          IdDef(id.name, id.span),
          tps,
          List(Operation(id, Many.empty(tps.span.synthesized), vps, bps, ret, opDoc, opSpan)),
          info,
          span())
    }

  def operation(info: Info): Operation =
    nonterminal:
      idDef() ~ params() ~ returnAnnotation() match {
        case id ~ (tps, vps, bps) ~ ret => Operation(id, tps, vps.unspan, bps.unspan, ret, info.onlyDoc().doc, span())
      }

  def interfaceDef(info: Info, keyword: TokenKind = `interface`): InterfaceDef =
    InterfaceDef(keyword ~> idDef(), maybeTypeParams(),
      `{` ~> manyUntil(documented { opInfo => { `def` ~> operation(opInfo) } labelled "} or another operation declaration" }, `}`) <~ `}`, info, span())

  def namespaceDef(info: Info): Def =
    consume(`namespace`)
    val id = idDef()
    // namespace foo { <DEFINITION>* }
    if peek(`{`) then NamespaceDef(id, braces { definitions() }, info.onlyDoc(), span())
    // namespace foo
    // <DEFINITION>*
    else { semi(); NamespaceDef(id, definitions(), info.onlyDoc(), span()) }

  def externDef(): Def =
    documented: info =>
      externDef(info)

  def externDef(info: Info): Def =
    info.assertExtern()
    peek.kind match {
      case `type`      => externType(info)
      case `interface` => externInterface(info)
      case `resource`  => externResource(info)
      case `include`   => externInclude(info)
      case `def`       => externFun(info)
      // extern """..."""
      case _           => externString(info)
      // extern js """..."""
    }

  // reinterpret a parsed capture as a feature flag
  def featureFlagFromCapture(capt: Option[CaptureSet]): FeatureFlag = capt match {
    case Some(CaptureSet(IdRef(Nil, "default", span) :: Nil, _)) => FeatureFlag.Default(span)
    case Some(CaptureSet(IdRef(Nil, flag, span) :: Nil, _)) => FeatureFlag.NamedFeatureFlag(flag, span)
    case None => FeatureFlag.Default(span())
    case _ => fail(s"Not a valid feature flag: ${capt}")
  }

  def featureFlag(): FeatureFlag =
    expect("feature flag identifier") {
      case Ident("default") => FeatureFlag.Default(span())
      case Ident(flag)      => FeatureFlag.NamedFeatureFlag(flag, span())
    }

  def maybeFeatureFlag(): FeatureFlag =
    nonterminal:
      backtrack(featureFlag()).getOrElse(FeatureFlag.Default(span()))

  def externType(info: Info): Def =
    ExternType(`type` ~> idDef(), maybeTypeParams(), info, span())
  def externInterface(info: Info): Def =
    ExternInterface(`interface` ~> idDef(), maybeTypeParams().unspan, info, span())
  def externResource(info: Info): Def =
    ExternResource(`resource` ~> idDef(), blockTypeAnnotation(), info, span())
  def externInclude(info: Info): Def =
    consume(`include`)
    val posAfterInclude = pos()
    val ff = maybeFeatureFlag()
    ExternInclude(ff, path().stripPrefix("\"").stripSuffix("\""), None, IdDef("", Span(source, posAfterInclude, posAfterInclude, Synthesized)), info=info, span=span())

  def externString(info: Info): Def =
    val posAfterExtern = pos()
    val ff = maybeFeatureFlag()
    expect("string literal") {
      case Str(contents, _) => ExternInclude(ff, "", Some(contents), IdDef("", Span(source, posAfterExtern, posAfterExtern, Synthesized)), info, span())
    }

  def externFun(info: Info): Def =
    (`def` ~> idDef() ~ params() ~ maybeCaptureSet() ~ (returnAnnotation() <~ `=`)) match {
      case id ~ (tps, vps, bps) ~ cpt ~ ret =>
        val bodies = manyWhile(externBody(), isExternBodyStart)
        val captures = cpt.getOrElse(defaultCapture(cpt.span.synthesized))
        ExternDef(id, tps, vps, bps, captures, ret, bodies, info, span())
    }

  def externBody(): ExternBody =
    nonterminal:
      peek.kind match {
        case _: Ident => (peek(1).kind match {
          case `{` => ExternBody.EffektExternBody(featureFlag(), `{` ~> stmts(inBraces = true) <~ `}`, span())
          case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template().unspan, span())
        }) labelled "extern body (string or block)"
        case _ => ExternBody.StringExternBody(maybeFeatureFlag(), template().unspan, span())
      }

  private def isExternBodyStart: Boolean =
    peek.kind match {
      case Str(_, _) | Ident(_) | `{` => true
      case _                          => false
    }

  def template(): SpannedTemplate[Term] =
    nonterminal:
      // TODO handle case where the body is not a string, e.g.
      // Expected an extern definition, which can either be a single-line string (e.g., "x + y") or a multi-line string (e.g., """...""")
      val first = spanned(string())
      val (exprs, strs) = manyWhile((`${` ~> spanned(expr()) <~ `}$`, spanned(string())), `${`).unzip
      SpannedTemplate(first :: strs, exprs)

  def spanned[T](p: => T): Spanned[T] =
    nonterminal:
      Spanned(p, span())

  def documented[T](p: Info => T): T =
    nonterminal:
      p(info())

  private def maybeDocumentation(): Doc =
    peek.kind match {
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
    }

  // /// some documentation
  // private
  // extern
  def info(): Info =
    nonterminal {
      val doc = maybeDocumentation()
      val isPrivate = nonterminal {
        when(`private`) { Maybe.Some((), span()) } { Maybe.None(span()) }
      }
      val isExtern = nonterminal {
        when(`extern`) { Maybe.Some((), span()) } { Maybe.None(span()) }
      }
      Info(doc, isPrivate, isExtern)
    }

  def noInfo(): Info = Info.empty(Span(source, pos(), pos(), Synthesized))

  extension (info: Info) {
    def assertExtern(): Unit = info.isExtern match {
      case Maybe(None, span) => softFail("Extern definitions require `extern` modifier", position, position) // info.isExtern.span.from, info.isExtern.span.to)
      case _ => ()
    }

    def onlyDoc(): Info =
      if (info.isExtern.nonEmpty) softFail("Modifier `extern` is not allowed", position, position) // , info.isExtern.span.from, info.isExtern.span.to)
      if (info.isPrivate.nonEmpty) softFail("Modifier `private` is not allowed", position, position) // , info.isExtern.span.from, info.isExtern.span.to)

      info
  }

  def defaultCapture(span: Span): CaptureSet =
    CaptureSet(List(IdRef(List("effekt"), "io", span)), span)

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

  def maybeCaptureSet(): Maybe[CaptureSet] =
    nonterminal:
      when(`at`) { Maybe.Some(captureSet(), span()) } { Maybe.None(span()) }

  def maybeValueTypeAnnotation(): Option[ValueType] =
    nonterminal:
      if peek(`:`) then Some(valueTypeAnnotation()) else None

  def maybeBlockTypeAnnotation(): Maybe[BlockType] =
    nonterminal:
      if peek(`:`) then Maybe.Some(blockTypeAnnotation(), span()) else Maybe.None(span())

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
        stmts(),
        when(`else`) { stmts() } { Return(UnitLit(span().emptyAfter), span().emptyAfter) }, span())

  def whileExpr(): Term =
    nonterminal:
      While(`while` ~> parens { matchGuards().unspan },
        stmts(),
        when(`else`) { Some(stmts()) } { None },
        span())

  def doExpr(): Term =
    nonterminal:
      (`do` ~> idRef()) ~ arguments() match {
        case id ~ (targs, vargs, bargs) => Do(id, targs, vargs, bargs, span())
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
          val synthesizedId = IdRef(Nil, id.name, id.span.synthesized)
          val interface = TypeRef(id, tps, id.span.synthesized)
          val operation = OpClause(synthesizedId, Nil, vps, bps, None, body, k, Span(source, id.span.from, body.span.to, Synthesized))
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
        `=>` ~> stmts(inBraces = true),
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
         left = binaryOp(left, op, right)
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
            nonterminal:
              val argSpans = cs match {
                case Many(MatchClause(MultiPattern(ps, _), _, _, _) :: _, _) => ps.map(_.span)
                case p => List(p.span)
              }
              // TODO fresh names should be generated for the scrutinee
              // also mark the temp name as synthesized to prevent it from being listed in VSCode
              val names = List.tabulate(argSpans.length){ n => s"__arg${n}" }
              BlockLiteral(
                Nil,
                names.zip(argSpans).map { (name, span) => ValueParam(IdDef(name, span.synthesized), None, span.synthesized) },
                Nil,
                Return(
                  Match(
                    names.zip(argSpans).map{ (name, span) => Var(IdRef(Nil, name, span.synthesized), span.synthesized) },
                    cs.unspan,
                    None,
                    span().synthesized
                  ), span().synthesized),
                span().synthesized
              )
          }
          case _ =>
            // { (x: Int) => ... }
            nonterminal:
              backtrack { lambdaParams() <~ `=>` } map {
                case (tps, vps, bps) => BlockLiteral(tps, vps, bps, stmts(inBraces = true), span()) : BlockLiteral
              } getOrElse {
                // { <STMTS> }
                BlockLiteral(Nil, Nil, Nil, stmts(inBraces = true), span()) : BlockLiteral
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
      manyTrailing(() => spanned(expr()), `[`, `,`, `]`).foldRight(NilTree) { ConsTree }

  private def NilTree: Term =
    Call(IdTarget(IdRef(List(), "Nil", span())), Nil, Nil, Nil, span())

  private def ConsTree(el: Spanned[Term], rest: Term): Term =
    Call(
      IdTarget(IdRef(List(), "Cons", el.span.synthesized)),
      Nil,
      List(ValueArg.Unnamed(el.unspan), ValueArg.Unnamed(rest)),
      Nil,
      el.span.synthesized
    )

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
        case Maybe(None, _) ~ SpannedTemplate(str :: Nil, Nil) => StringLit(str.unspan, str.span)
        // s"a${x}b${y}" ~> s { do literal("a"); do splice(x); do literal("b"); do splice(y); return () }
        case id ~ SpannedTemplate(strs, args) =>
          val target = id.getOrElse(IdRef(Nil, "s", id.span.synthesized))
          val doLits = strs.map { s =>
            Do(
              IdRef(Nil, "literal", s.span.synthesized),
              Nil,
              List(ValueArg.Unnamed(StringLit(s.unspan, s.span))),
              Nil,
              s.span.synthesized
            )
          }
          val doSplices = args.map { arg =>
            Do(
              IdRef(Nil, "splice", arg.span.synthesized),
              Nil,
              List(ValueArg.Unnamed(arg.unspan)),
              Nil,
              arg.span.synthesized
            )
          }
          val body = interleave(doLits, doSplices)
            .foldRight(
              Return(UnitLit(span().synthesized), span().synthesized)
            ) { (term, acc) => ExprStmt(term, acc, term.span.synthesized) }
          val blk = BlockLiteral(Nil, Nil, Nil, body, span().synthesized)
          Call(IdTarget(target), Nil, Nil, List(blk), span().synthesized)
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
      peek.kind match {
        case `{` => CaptureSet(many(idRef, `{`, `,` , `}`).unspan, span())
        case _ if isIdRef => CaptureSet(List(idRef()), span())
        case t => fail("Expected a capture set.", t)
        }

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
    p
  }
}
