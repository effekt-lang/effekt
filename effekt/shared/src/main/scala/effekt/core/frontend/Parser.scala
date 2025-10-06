package effekt
package core.frontend

import effekt.lexer.*
import effekt.lexer.TokenKind.{ `::` as PathSep, * }

import effekt.util.messages.ErrorReporter
import scala.annotation.{tailrec, targetName}
import kiama.util.{ Source, StringSource }
import core.frontend.Stmt.*
import core.frontend.Pure.*

case class Fail(message: String, position: Int) extends Throwable(null, null, false, false)

class Parser(tokens: Seq[Token], source: Source) {

  import scala.collection.mutable.ListBuffer

  // Interfacing with the token stream
  // ---------------------------------

  // always points to the latest non-space position
  var position: Int = 0

  extension(token: Token) def failOnErrorToken: Token = token.kind match {
    case TokenKind.Error(err) => fail(err.msg)
    case _ => token
  }

  def peek: Token = tokens(position).failOnErrorToken

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

      tokens(position).failOnErrorToken match {
        case token if isSpace(token.kind) => go(position + 1, offset)
        case token if offset <= 0 => token
        case _ => go(position + 1, offset - 1)
      }

    go(position, offset)

  // the previously consumed token
  var previous = tokens(position)

  def pos() = previous.end + 1
  def peek(kind: TokenKind): Boolean =
    peek.kind == kind
  def peek(offset: Int, kind: TokenKind): Boolean =
    peek(offset).kind == kind

  def hasNext(): Boolean = position < tokens.length
  def next(): Token =
    val t = tokens(position).failOnErrorToken
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

  // tokens that delimit a statement
  def returnPosition: Boolean = peek(`}`) || peek(`case`) || peek(`}>`) || peek(EOF)



  // Soft keywords
  // -------------

  val `make` = Ident("make")
  val `io`   = Ident("io")

  val softKeywords = List("make", "io")
  // pure is already a keyword

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

  /**
   * Main entry point
   */
   def program(): ModuleDecl =
     // skip spaces at the start
     spaces()
     val res = ModuleDecl(declarations(), toplevels())
     if peek(`EOF`) then res else fail("Unexpected end of input")


  def toplevels(): List[Toplevel] = manyWhile(toplevel(), isToplevel)

  def toplevel(): Toplevel = peek.kind match {
    case `def` =>
      skip()
      val id = ident()
      when(`=`)
        { Toplevel.Def(id, block()) }
        // function shorthand: def foo(): Int = {}
        { Toplevel.Def(id, function()) }
    case `val` => Toplevel.Val(`val` ~> ident(), `=` ~> stmt())
    case _ => fail("Not a valid toplevel definition")
  }

  // def foo (): Int = { ... }
  //         ^^^^^^^^^^^^^^^^^
  def function(): Block.BlockLit =
    Block.BlockLit(maybeTypeParams(), maybeValueParams(), maybeBlockParams(), maybeReturnType(), `=` ~> stmt())

  def maybeReturnType(): Option[Type] =
    when(`:`) { Some(valueType()) } { None }

  def isToplevel: Boolean = peek.kind match {
    case `def` | `val` => true
    case _ => false
  }

  def declarations(): List[Declaration] =
    manyWhile(declaration(), isDeclaration)

  def declaration(): Declaration =
    peek.kind match {
      case `type` => Declaration.Data(`type` ~> ident(), maybeTypeParams(), many(constructor, `{`, `;`, `}`))
      case `interface` => Declaration.Interface(`interface` ~> ident(), maybeTypeParams(), braces { manyWhile(`def` ~> property(), `def`) })
      case _ => fail("Not a valid declaration")
    }

  def isDeclaration: Boolean = peek.kind match {
    case `type` | `interface` => true
    case _ => false
  }

  def constructor(): Constructor = Constructor(ident(), maybeTypeParams(), many(field, `(`, `,`, `)`))

  def property(): Property = Property(ident(), `:` ~> blockType())

  def field(): Field = Field(ident(), `:` ~> valueType())

  // Statements
  // ----------
  def stmts(): Stmt =
    peek.kind match {
      case `val` => Val(`val` ~> ident(), `=` ~> stmt(), semi() ~> stmts())
      case `let` => Let(`let` ~> ident(), `=` ~> expr(), semi() ~> stmts())
      case `def` =>
        skip()
        val id = ident()
        when(`=`)
          { Def(id, block(), semi() ~> stmts()) }
          // function shorthand:
          { Def(id, function(), semi() ~> stmts()) }
      case `return` =>
        val result = `return` ~> Return(pureExpr())
        maybeSemi()
        result
      case _ =>
        expr() match {
          // call
          case ValueVar(id) if peek(`(`) || peek(`[`) || peek(`{`) =>
            val (targs, vargs, bargs) = arguments()
            App(Block.BlockVar(id), targs, vargs, bargs)
          // method call
          case ValueVar(id) if peek(`.`) =>
            consume(`.`)
            val method = ident()
            val (targs, vargs, bargs) = arguments()
            Invoke(Block.BlockVar(id), method, targs, vargs, bargs)

          case pure: Pure if returnPosition => semi() ~> Return(pure)
          case expr: Expr if returnPosition => Let("tmp", expr, Return(Pure.ValueVar("tmp")))
          case other => Let("_", other, semi() ~> stmts())
        }
    }

  def stmt(): Stmt =
    peek.kind match {
      case `{` => braces { stmts() }
      case `return` => skip() ~> Return(pureExpr())
      case _ => expr() match {
        // call
        case ValueVar(id) if peek(`(`) || peek(`[`) || peek(`{`) =>
          val (targs, vargs, bargs) = arguments()
          App(Block.BlockVar(id), targs, vargs, bargs)
        // method call
        case ValueVar(id) if peek(`.`) =>
          consume(`.`)
          val method = ident()
          val (targs, vargs, bargs) = arguments()
          Invoke(Block.BlockVar(id), method, targs, vargs, bargs)
        case pure: Pure => semi() ~> Return(pure)
        case expr => Let("tmp", expr, Return(Pure.ValueVar("tmp")))
      }
    }

  def isHole: Boolean = peek(`<>`)

  def hole(): Stmt =
    peek.kind match {
      case `<>` => `<>` ~> Hole()
      case _ => fail("Expected hole")
    }


  // Expressions
  // -----------

  def expr(): Expr = peek.kind match {
    case `io` => (skip() ~> ident() ~ arguments()) match {
      case id ~ (targs, vargs, bargs) => DirectApp(id, targs, vargs, bargs)
    }
    case _ => pureExpr()
  }

  def isPure: Boolean = isLiteral || peek(`make`) || peek(`pure`) || peek(`box`) || isVariable


  // Pure Expressions
  // ----------------

  def pureExpr(): Pure =
    peek.kind match {
      // TODO throw error if bargs are not empty
      case `make` => (skip() ~> ident() ~ arguments()) match {
        case id ~ (targs, vargs, bargs) => Make(id, targs, vargs)
      }
      case `pure` => (skip() ~> ident() ~ arguments()) match {
        case id ~ (targs, vargs, bargs) => PureApp(id, targs, vargs)
      }
      case `box` => skip() ~> Box(block())
      case _ if isVariable => variable()
      case _ if isLiteral  => literal()
      case _ => fail(s"Expected pure expression")
    }

  def literal(): Literal =
    peek.kind match {
      case Integer(v)         => skip(); Literal(v, effekt.core.Type.TInt)
      case Float(v)           => skip(); Literal(v, effekt.core.Type.TDouble)
      case Str(v, multiline)  => skip(); Literal(v, effekt.core.Type.TString)
      case Chr(v)             => skip(); Literal(v, effekt.core.Type.TChar)
      case `true`             => skip(); Literal(true, effekt.core.Type.TBoolean)
      case `false`            => skip(); Literal(false, effekt.core.Type.TBoolean)
      case t if isUnitLiteral => skip(); skip(); Literal((), effekt.core.Type.TUnit)
      case t => fail("Expected a literal")
    }

  def typeParams(): List[Id] = some(ident, `[`, `,`, `]`)

  def maybeTypeParams(): List[Id] = if peek(`[`) then typeParams() else Nil

  def maybeValueParams(): List[Param] =
      if peek(`(`) then valueParams() else Nil

  def valueParams(): List[Param] =
      many(valueParam, `(`, `,`, `)`)

  def valueParam(): Param =
      Param(ident(),`:` ~> valueType())

  def maybeBlockParams(): List[Param] =
      manyWhile(`{` ~> blockParam() <~ `}`, `{`)

  def blockParams(): List[Param] =
      someWhile(`{` ~> blockParam() <~ `}`, `{`)

  def blockParam(): Param =
      Param(ident(),`:` ~> blockType())

  def string(): String =
    next().kind match {
      case Str(s, _) => s
      case _ => fail("Expected string literal.")
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

  // Will also recognize ( ) as unit if we do not emit space in the lexer...
  private def isUnitLiteral: Boolean = peek(`(`) && peek(1, `)`)

  def isVariable: Boolean = isIdent
  def variable(): Pure = Pure.ValueVar(id())

  def id(): Id = ident()

  def isIdent: Boolean = peek.kind match {
    case Ident(id) => !softKeywords.contains(id)
    case _ => false
  }

  def ident(): String =
    next().kind match {
      case Ident(id) => id
      case _ => fail(s"Expected identifier")
    }


  // Blocks
  // ------

  def block(): Block = peek.kind match {
    case `unbox` => Block.Unbox(`unbox` ~> pureExpr())
    case `new` => Block.New(`new` ~> implementation())
    case `{` => blocklit()
    case Ident(id) => Block.BlockVar(ident())
    case _ => fail(s"Expected block")
  }

  // { (x: Int) => return 42 }
  def blocklit(): Block.BlockLit =
    braces { Block.BlockLit(maybeTypeParams(), maybeValueParams(), maybeBlockParams(), None, `=>` ~> stmts()) }

  def implementation(): Implementation =
    Implementation(refType(), braces { manyWhile(operation(), `def`) })

  def operation(): Operation = Operation(`def` ~> ident(), function())

  // Arguments
  def isArguments: Boolean = lookbehind(1).kind != Newline && (peek(`(`) || peek(`[`) || peek(`{`))
  def arguments(): (List[Type], List[Pure], List[Block]) =
    if (!isArguments) fail("Expected at least one argument section (types, values, or blocks)")
    (maybeTypeArgs(), maybeValueArgs(), maybeBlockArgs())

  def maybeTypeArgs(): List[Type] = if peek(`[`) then typeArgs() else Nil
  def maybeValueArgs(): List[Pure] = if peek(`(`) then valueArgs() else Nil
  def maybeBlockArgs(): List[Block] = if peek(`{`) then blockArgs() else Nil

  def typeArgs(): List[Type] = some(valueType, `[`, `,`, `]`)
  def valueArgs(): List[Pure] = many(pureExpr, `(`, `,`, `)`)
  def blockArgs(): List[Block] = someWhile(blockArg(), `{`)

  def blockArg(): Block =
      backtrack { `{` ~> Block.BlockVar(id()) <~ `}` } getOrElse { blocklit() }

  // Types

  def valueType(): Type = boxedType()
  def blockType(): Type = functionType()

  // Parse function types (middle precedence)
  def functionType(): Type = {
    // Complex function type: [T]*(Int, String)*{Exc} => Int / {Effect}
    def functionTypeComplex: Option[Type] = backtrack {
      maybeTypeParams() ~ maybeValueTypes() ~ (maybeBlockTypeParams() <~ `=>`)
    } map { case tparams ~ vparams ~ bparams =>
      refType() match {
        case  t => Type.Function(tparams, vparams, bparams, t)
      }
    }

    // Simple function type: Int => Int
    def functionTypeSimple: Option[Type] = backtrack {
      refType() <~ `=>`
    } map { tpe =>
      Type.Function(Nil, List(tpe), Nil, refType())
    }

    // Try to parse each function type variant, fall back to basic type if none match
    functionTypeSimple orElse functionTypeComplex getOrElse refType()
  }

  def refType(): Type.Ref =
    Type.Ref(ident(), maybeTypeArgs())

  def captures(): Captures = many(ident, `{`, `,`, `}`).toSet


  def maybeValueTypes() = if peek(`(`) then valueTypes() else Nil
  def valueTypes(): List[Type] = many(valueType, `(`, `,`, `)`)

  def maybeBlockTypeParams(): List[(Option[Id], Type)] =
      if peek(`{`) then blockTypeParams() else Nil

  def blockTypeParams(): List[(Option[Id], Type)] =
      someWhile(blockTypeParam(), `{`)

  def blockTypeParam(): (Option[Id], Type) =
      braces { (backtrack { ident() <~ `:` }, blockType()) }

  def boxedType(): Type =
    val tpe = functionType()
    when(`at`) { Type.Boxed(tpe, captures()) } { tpe }


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
}

def shouldParse(input: String)(tpe: Type): Unit =
  val source = StringSource(input)
  val lexer = effekt.lexer.Lexer(source)
  val tokens = lexer.run()
  val parser = new core.frontend.Parser(tokens, source)
  try {
    val result = parser.valueType()
    if (result != tpe) {
      assert(false, s"\nExpected: ${tpe}\nGot     : ${result}")
    }
  } catch {
    case Fail(msg, pos) => println(s"Parse error (${pos}): ${msg}")
  }

def shouldParse(input: String)(decl: Declaration): Unit =
  val source = StringSource(input)
  val lexer = effekt.lexer.Lexer(source)
  val tokens = lexer.run()
  val parser = new core.frontend.Parser(tokens, source)
  try {
    val result = parser.declaration()
    if (result != decl) {
      assert(false, s"\nExpected: ${decl}\nGot     : ${result}")
    }
  } catch {
    case Fail(msg, pos) => println(s"Parse error (${pos}): ${msg}")
  }

def shouldParseProgram(input: String): Unit =
  val source = StringSource(input)
  val lexer = effekt.lexer.Lexer(source)
  val tokens = lexer.run()
  val parser = new core.frontend.Parser(tokens, source)
  try {
    val result = parser.program()
    println(result)
  } catch {
    case Fail(msg, pos) =>
      println(s"Parse error (${pos}): ${msg}")
      println(tokens(pos))
  }

@main
def testParser =

  shouldParse(
    """ type List[T] {
      |   Nil();
      |   Cons(head: T, tail: List[T])
      | }
      |""".stripMargin
  )(
    Declaration.Data("List", List("T"),
      List(Constructor("Nil", Nil, Nil),
      Constructor("Cons", Nil, List(Field("head", Type.Ref("T", Nil)), Field("tail", Type.Ref("List",List(Type.Ref("T", Nil))))))))
  )

  shouldParse("Int")(Type.Ref("Int", Nil))

  shouldParse("Int => String")(Type.Function(Nil, Type.Ref("Int", Nil) :: Nil, Nil, Type.Ref("String", Nil)))

  shouldParse("(Int) => String")(Type.Function(Nil, Type.Ref("Int", Nil) :: Nil, Nil, Type.Ref("String", Nil)))

  shouldParse("() => String")(Type.Function(Nil, Nil, Nil, Type.Ref("String", Nil)))

  shouldParse(
    """ interface Counter {
      |   def get: () => Int
      |   def inc: () => Unit
      | }
      | """.stripMargin
  )(
    Declaration.Interface("Counter", Nil, List(
      Property("get", Type.Function(Nil, Nil, Nil, Type.Ref("Int", Nil))),
      Property("inc", Type.Function(Nil, Nil, Nil, Type.Ref("Unit", Nil)))))
  )

  shouldParseProgram(
    """type List[A] { Nil(); Cons(hd: A, tl: List[A]) }
      |
      |def map[A, B](l: List[A]) { f: A => B }: List[B] = map[A, B](l) { (a: A) => f(a) }
      |
      |def inc(n: Int): Int = pure infixAdd(n, 1)
      |
      |def f = { () => 42 }
      |
      |def foo = unbox box unbox box bar
      |
      |def main(): Unit = {
      | def foo = new Counter {
      |   def inc() = ()
      |   def get() = 43
      | }
      |
      | return ()
      |}
      |""".stripMargin
  )


def testParser2 =
  val source = StringSource(
    """ val x = { return 42 };
      | return x
      |""".stripMargin)

//    """io println("hello");
//      |io println("world");
//      |val x = { return 42 };
//      |val y = 5;
//      |let f = pure infixAdd(pure infixAdd(1, 2), 3)
//      |val c = make Cons(true, make Cons(2, make Nil()))
//      |c
//      |""".stripMargin)

  val lexer = effekt.lexer.Lexer(source)
  val tokens = lexer.run()
  val parser = new core.frontend.Parser(tokens, source)
  try {
    val result = parser.stmts()
    val typed = typer.typecheck(result)(using typer.Context.empty)
    println(util.show(typed.term))
    println(util.show(typed.tpe))
    println(util.show(typed.captures))
  } catch {
    case Fail(msg, pos) => println(s"Parse error (${pos}): ${msg}")
  }

