package effekt

import effekt.lexer.TokenKind.*
import effekt.lexer.{Lexer, Token, TokenKind}
import effekt.source.*
import effekt.source.Origin.Synthesized
import kiama.util.{Positions, Source, StringSource}
import munit.Location

// DSL for creating code snippets with span annotations
object SpanSyntax {
  implicit class StringOps(val content: String) extends AnyVal {

    def sourceAndPosition: (Source, Int) = {
      val (source, positions) = content.sourceAndPositions

      if (positions.length != 1)
        throw new IllegalArgumentException("Exactly one marker line (with '" + "↑" + "') is required.")

      (source, positions.head)
    }

    def sourceAndSpan: (Source, Span) = {
      val (source, positions) = content.sourceAndPositions
      if (positions.length != 2)
        throw new IllegalArgumentException("Exactly two marker lines (with '" + "↑" + "') are required.")
      val start = positions(0)
      val end = positions(1)
      // The end of the span is exclusive, so we need to increment the character position.
      val span = Span(source, start, end + 1)
      (source, span)
    }

    def sourceAndPositions: (Source, Seq[Int]) = {
      val lines = content.stripMargin.split("\n").toBuffer
      val positions = scala.collection.mutable.ArrayBuffer[Int]()
      var lineIdx = 0
      var lineBytePos = 0
      while (lineIdx < lines.length) {
        val line = lines(lineIdx)
        if (line.contains("↑")) {
          if (lineIdx == 0)
            throw new IllegalArgumentException("Marker on first line cannot refer to a previous line.")
          // There may be multiple markers on the same line, so we need to record all of them.
          for (i <- line.indices if line(i) == '↑') {

            // Consider the following example snippet:
            //┌────────────────┐  = lines(lineIdx - 1).length  + 1
            //|def foo() ... \n|  = lineBytePos
            //     ↑
            positions += (lineBytePos - ( lines(lineIdx - 1).length  + 1 ) + i)
          }
          lines.remove(lineIdx)
          // adjust index because of removal
          lineIdx -= 1
        } else {
          // add the line length incremented by 1 for the new line \n symbol
          lineBytePos += lines(lineIdx).length + 1
        }
        lineIdx += 1
      }
      val newContent = lines.mkString("\n")
      (StringSource(newContent), positions.toList)
    }
  }

}


class RecursiveDescentTests extends munit.FunSuite {

  def parser(input: String, positions: Positions)(using munit.Location): RecursiveDescent = {
    val source = StringSource(input, "")
    val lexer = effekt.lexer.Lexer(source)
    val tokens = lexer.run()
    // TODO catch LexerError exception?
    new RecursiveDescent(positions, tokens, source)
  }

  def parse[R](input: String, f: RecursiveDescent => R, positions: Positions = new Positions())(using munit.Location): R =
    try {
      val p = parser(input, positions)
      val result = f(p)
      assert(p.peek(TokenKind.EOF), s"Did not consume everything: ${p.peek}")
      result
    } catch {
      case Fail(msg, pos) =>
        fail(s"Unexpected parse error (token index ${pos}): ${msg}")
    }

  def parseExpr(input: String, positions: Positions = new Positions())(using munit.Location): Term =
    parse(input, _.expr())

  def parseStmt(input: String, positions: Positions = new Positions())(using munit.Location): Stmt =
    parse(input, _.stmt())

  def parseStmts(input: String, positions: Positions = new Positions())(using munit.Location): Stmt =
    parse(input, _.stmts())

  def parseMatchPattern(input: String, positions: Positions = new Positions())(using munit.Location): MatchPattern =
    parse(input, _.matchPattern())

  def parseMatchClause(input: String, positions: Positions = new Positions())(using munit.Location): MatchClause =
    parse(input, _.matchClause())

  def parseValueType(input: String, positions: Positions = new Positions())(using munit.Location): ValueType =
    parse(input, _.valueType())

  def parseBlockType(input: String, positions: Positions = new Positions())(using munit.Location): BlockType =
    parse(input, _.blockType())

  def parseOpClause(input: String, positions: Positions = new Positions())(using munit.Location): OpClause =
    parse(input, _.opClause())

  def parseImplementation(input: String, positions: Positions = new Positions())(using munit.Location): Implementation =
    parse(input, _.implementation())

  def parseTry(input: String, positions: Positions = new Positions())(using munit.Location): Term =
    parse(input, _.tryExpr())

  def parseParams(input: String, positions: Positions = new Positions())(using munit.Location): (Many[Id], Many[ValueParam], Many[BlockParam]) =
    parse(input, _.params())

  def parseLambdaParams(input: String, positions: Positions = new Positions())(using munit.Location): (List[Id], List[ValueParam], List[BlockParam]) =
    parse(input, _.lambdaParams())

  def parseDefinition(input: String, positions: Positions = new Positions())(using munit.Location): Def =
    parse(input, _.definition())

  def parseDefinitions(input: String, positions: Positions = new Positions())(using munit.Location): List[Def] =
    parse(input, _.definitions())

  def parseToplevel(input: String, positions: Positions = new Positions())(using munit.Location): Def =
    parse(input, _.toplevel())

  def parseProgram(input: String, positions: Positions = new Positions())(using munit.Location): ModuleDecl =
    parse(input, _.program())

  def parseExternDef(input: String, positions: Positions = new Positions())(using munit.Location): Def =
    parse(input, _.externDef())

  // Custom asserts
  //
  //

  def equalsModuloSpans(a: Any, b: Any): Boolean = {
    def eq(x: Any, y: Any): Boolean = (x, y) match {
      case (_: Span, _: Span) =>
        true

      case (xs: Iterable[_], ys: Iterable[_]) =>
        xs.size == ys.size && xs.iterator.zip(ys.iterator).forall { (x, y) => eq(x, y) }

      case (p1: Product, p2: Product) if p1.getClass == p2.getClass =>
        val f1 = p1.productIterator.toList
        val f2 = p2.productIterator.toList
        (f1.length == f2.length) && f1.zip(f2).forall((eq _).tupled)

      case _ =>
        x == y
    }

    eq(a, b)
  }

  def assertEqualModuloSpans[A, B](
    obtained: A,
    expected: B,
  )(implicit loc: Location, ev: B <:< A): Unit = {
    if (!equalsModuloSpans(obtained, expected)) {
      failComparison(
        "Trees are not the same modulo spans",
        munitPrint(obtained),
        munitPrint(expected),
      )(loc)
    }
  }

  def assertNotEqualModuloSpans[A <: Tree, B <: Tree](
      obtained: A,
      expected: B,
    )(implicit loc: Location, ev: B <:< A): Unit = {
    if (equalsModuloSpans(obtained, expected)) {
      failComparison(
        "Trees are the same modulo spans, but expected them to be different",
        munitPrint(obtained),
        munitPrint(expected),
      )(loc)
    }
  }

  // Snippet and span DSL
  //
  //

  import effekt.SpanSyntax.StringOps

  test("Correct cursor position") {
    val (_, cursor) =
      raw"""def main() = { println("Hello, world!") }
           |    ↑
           |""".sourceAndPosition

    assertEquals(cursor, 4)
  }

  test("Missing cursor throws exception") {
    intercept[IllegalArgumentException] {
      raw"""
           |def main() = { println("Hello, world!") }
           |""".sourceAndPosition
    }
  }

  test("Correct multiline span") {
    val (source, span) =
      raw"""There is some content here.
           |    ↑
           |And here.
           |    ↑
           |""".sourceAndSpan

    val textWithoutSpan =
      raw"""There is some content here.
           |And here.""".stripMargin

    assertEquals(span.from, 4)
    assertEquals(span.to, 33)
    assertEquals(source.content, textWithoutSpan)
  }

  test("Correct newline spans") {
    val (_, pos : Int) =
      "\n\n\n\n↑".sourceAndPosition

    assertEquals(pos, 3)
  }

  // Peek tests
  //
  //

  test("Peeking") {
    implicit def toToken(t: TokenKind): Token = Token(0, 0, t)
    def peek(tokens: Seq[Token], offset: Int): Token =
      new RecursiveDescent(new Positions, tokens, StringSource("", "test")).peek(offset)

    val tokens = List[Token](`(`, Space, Newline, `)`, Space, `=>`, EOF)
    assertEquals(peek(tokens, 0).kind, `(`)
    assertEquals(peek(tokens, 1).kind, `)`)
    assertEquals(peek(tokens, 2).kind, `=>`)
    assertEquals(peek(tokens, 3).kind, EOF)
  }

  test("Simple expressions") {
    parseExpr("42")
    parseExpr("f")
    parseExpr("f(a)")
    parseExpr("f(a, 42)")
    parseExpr("\\u0000")

    parseExpr("l.foreach { _ => 42 }")
    {
      val (source, positions) =
        raw"""loop { f }
             |↑   ↑  ↑↑
             |""".sourceAndPositions

      assertEquals(
        parseExpr(source.content),
        Call(IdTarget(IdRef(Nil, "loop", Span(source, positions(0), positions(1)))), Nil, Nil, List(Var(IdRef(Nil, "f", Span(source, positions(2), positions(3)))))))
    }

    assertNotEqualModuloSpans(
      parseExpr("f.m(a, 42)"),
      parseExpr("(f.m)(a, 42)"))

    assertEqualModuloSpans(
      parseExpr("f(a, 42)()"),
      parseExpr("(f(a, 42))()"))

    {
      val (source, positions) =
        raw"""foo.bar
             |↑  ↑↑  ↑
             |""".sourceAndPositions
      assertEquals(
        parseExpr(source.content),
        // At the moment uniform function call syntax is always a method call
        MethodCall(Var(IdRef(Nil, "foo", Span(source, positions(0), positions(1)))), IdRef(Nil, "bar", Span(source, positions(2), positions(3))), Nil, Nil, Nil))
    }

    parseExpr("resume(42)")
    parseExpr("in(42)")

    parseExpr("fun() { foo(()) }")

    parseExpr("10.seconds")

    parseExpr("[1,2,3]")
    parseExpr("[3,2,1,]")
    parseExpr("[]")
    parseExpr("[,]")
    intercept[Throwable] { parseExpr("[,1]") }
  }

  test("Boxing") {
    parseExpr("box f")
    parseExpr("unbox f")
    assertEqualModuloSpans(
      parseExpr("unbox box f"),
      parseExpr("unbox (box f)")
    )
    assertNotEqualModuloSpans(
      parseExpr("box { 42 }"),
      parseExpr("box {} { 42 }")
    )
    parseExpr("box { (x: Int) => x }")
    parseExpr("box new Fresh { def fresh() = \"42\" }")
    parseExpr("box foo()")
    parseExpr("box bar(1)")
    parseExpr("box baz(quux)")

    // { f } is parsed as a capture set and not backtracked.
    intercept[Throwable] { parseExpr("box { f }") }
  }

  test("Pattern matching") {
    parseExpr(
      """do raise(RuntimeError(), msg) match {}
        |""".stripMargin)

    parseExpr(
      """x match {
        |  case 0 => 42
        |  case _ => <{ "Test" }>
        |}
        |""".stripMargin)

    parseExpr(
          """x match {
            |  case 0 => { 42 }
            |  case 1 => { 1; 2 }
            |  case _ => 42
            |}
            |""".stripMargin)

    parseExpr(
      """x match {
        |  case Some(b) => acc = Cons(b, acc)
        |}
        |""".stripMargin)
  }

  test("Qualified names") {
    {
      val (source, span) =
        raw"""map
             |↑ ↑
             |""".sourceAndSpan
      assertEquals(parseExpr("map"), Var(IdRef(List(), "map", span)))
    }
    {
      val (source, span) =
        raw"""list::map
             |↑       ↑
             |""".sourceAndSpan
      assertEquals(parseExpr("list::map"), Var(IdRef(List("list"), "map", span)))
    }
    {
      val (source, span) =
        raw"""list::internal::map
             |↑                 ↑
             |""".sourceAndSpan
      assertEquals(parseExpr("list::internal::map"), Var(IdRef(List("list", "internal"), "map", span)))
    }
    {
      val (source, span) =
        raw"""list::internal::test
             |↑                  ↑
             |""".sourceAndSpan
      assertEquals(parseExpr("list::internal::test"), Var(IdRef(List("list", "internal"), "test", span)))
    }
  }

  test("Operator precedence") {
    parseExpr("1 + 1")

    assertEqualModuloSpans(
      parseExpr("1 + 2 + 3"),
      parseExpr("(1 + 2) + 3"))

    assertEqualModuloSpans(
      parseExpr("1 + 2 * 3"),
      parseExpr("1 + (2 * 3)"))

    assertEqualModuloSpans(
      parseExpr("1 + 2 * 3 == 4 + 5"),
      parseExpr("(1 + (2 * 3)) == (4 + 5)"))

    parseExpr("i = i + 1")

    parseExpr("compare(x, y) && go(next)")

    parseExpr("foo || bar")
  }

  test("Dangling else") {
    assertEqualModuloSpans(
      parseExpr("if (42) if (42) x else y"),
      parseExpr("if (42) (if (42) x else y) else ()"))
  }

  test("Simple statements") {
    parseStmt("42")
    parseStmt("return 42")
    parseStmt("{ f(); return 43 }")
    parseStmt("{ f(); 43 }")
  }

  test("Compound statements") {
    parseStmts(
      """ val x = { 42; 43 };
        | val y = f(x);
        | y
        |""".stripMargin)

    parseStmts(
      """with foo().baz;
        |bar()
        |""".stripMargin)

    parseStmts(
      """var x = baz;
        |return x
        |""".stripMargin)

    parseStmts(
      """var x in r = baz;
        |return x
        |""".stripMargin)

    // trailing semicolon
    parseStmts(
      """return x;
        |""".stripMargin)

    parseStmts("fun() { x = x + 1; x }")
  }

  test("Definition statements") {
    parseStmts(
      """val x = 42
        |type T = Int
        |()
        |""".stripMargin)

    parseStmts("val (left, right) = list; return left")

    parseStmts("val g: () => Unit / Exc at {exc} = fun() { closure() }; ()")
  }

  test("Semicolon insertion") {
    parseStmts("f(); return x")
    parseStmts(
      """var x = { baz }
        |return x
        |""".stripMargin)

    assertEqualModuloSpans(
      parseStmts(
        """f()
          |g()
          |""".stripMargin),
      parseStmts(
        """f();
          |return g()
          |""".stripMargin))

      parseStmts(
        """ val next = f() // Comment
          | g()
          |""".stripMargin)

    assertEqualModuloSpans(
      parseStmts(
        """f()
          |
          |()
          |""".stripMargin),
      parseStmts(
        """f();
          |
          |()
          |""".stripMargin))
  }

  test("Simple patterns") {
    parseMatchPattern("x")
    parseMatchPattern("Cons(x, y)")
    assertEqualModuloSpans(
      parseMatchPattern("_"),
      IgnorePattern())
    parseMatchPattern("Cons(x, Cons(x, Nil()))")

    {
      val (source, positions) =
        raw"""(left, Cons(x, right))
             |↑↑   ↑ ↑   ↑↑↑ ↑    ↑ ↑
             |""".sourceAndPositions
      assertEquals(
        parseMatchPattern("(left, Cons(x, right))"),
        TagPattern(IdRef(List("effekt"), "Tuple2", Span(source, positions(0), positions(9), Synthesized)),
          List(AnyPattern(IdDef("left", Span(source, positions(1), positions(2)))),
            TagPattern(
              IdRef(List(), "Cons", Span(source, positions(3), positions(4))),
              List(AnyPattern(IdDef("x", Span(source, positions(5), positions(6)))),
                AnyPattern(IdDef("right", Span(source, positions(7), positions(8)))))
            ))))
    }
  }

  test("Block arguments") {
    parseExpr("map {f}")
    parseExpr("map {list::f}")
    parseExpr("map {f} {g}")
    parseExpr("map { f } { g }")
    parseExpr("map(x) { f } { g }")
    parseExpr("map(x) { return 42 }")
    parseExpr("map(x) { map(x) { return 42 } }")
  }

  test("Value types") {
    {
      val (source, positions) =
        raw"""Int
             |↑  ↑↑
             |""".sourceAndPositions
      assertEquals(
        parseValueType(source.content),
        TypeRef(IdRef(Nil, "Int", Span(source, positions(0), positions(1))), Many.empty(Span(source, positions(1), positions(2)))))
    }
    parseValueType("List[Int]")
    parseValueType("list::List[Int]")
    parseValueType("list::List[effekt::Int]")
  }

  test("Block types") {
    parseBlockType("Exc")
    parseBlockType("State[S]")
    parseBlockType("State[Int]")
    parseBlockType("() => Int")
    parseBlockType("(Int) => Int")
    parseBlockType("Int => Int")

    {
      val (source, positions) =
        raw"""(Int, String) => Int
             |↑↑  ↑ ↑     ↑↑↑  ↑  ↑↑
             |""".sourceAndPositions
      assertEquals(
        parseBlockType(source.content),
        FunctionType(Many.empty(Span(source, positions(0), positions(1))),
          Many(List(TypeRef(IdRef(Nil, "Int", Span(source, positions(1), positions(2))), Many.empty(Span(source, positions(2), positions(2)))),
            TypeRef(IdRef(Nil, "String", Span(source, positions(3), positions(4))), Many.empty(Span(source, positions(4), positions(4))))), Span(source, positions(0), positions(5))),
          Many.empty(Span(source, positions(5), positions(6))), TypeRef(IdRef(Nil, "Int", Span(source, positions(7), positions(8))), Many.empty(Span(source, positions(8), positions(9)))), Effects(Nil)))
    }

    parseBlockType("(Int, String) => Int / Exc")
    parseBlockType("[T](Int, String) => Int / { Exc, State[T] }")
    parseBlockType("=> Int")
    parseBlockType("{Exc} => Int")
    parseBlockType("{Exc} {Amb} => Int")
    parseBlockType("({Exc} {Amb} => Int)")
    parseBlockType("{Exc} {amb : Amb} => Int")
    parseBlockType("{exc:Exc} => Int")
    parseBlockType("[T] => T") // Not sure we want this...

    parseValueType("Exc at { a, b, c }")
    intercept[Throwable] { parseBlockType("Exc / Eff") }
    intercept[Throwable] { parseBlockType("Exc / {}") }

    parseValueType("() => (Exc at {}) / {} at { a, b, c }")

    assertEqualModuloSpans(
      parseValueType("() => Int at { a, b, c }"),
      parseValueType("(() => Int) at { a, b, c }"))

    // we purposefully cannot parse this:
    intercept[Throwable] { parseValueType("() => Int at { a, b, c } at {}") }

    parseValueType("() => (Int at { a, b, c }) at {}")
    parseValueType("(() => Int) at { a, b, c }")
    parseValueType("(() => Int at {}) => Int at { a, b, c }")

    parseValueType("() => Unit / Socket at {io, async, global}")
  }

  test("Params") {
    parseParams("()")
    intercept[Throwable] { parseParams("(x, y)") }
    parseParams("[A, B](x: A, y: B)")
    intercept[Throwable] { parseParams("[]") }
    // is this desirable?
    parseParams("[A]")


    parseLambdaParams("a")
    parseLambdaParams("(a)")
    parseLambdaParams("(a: Int)")
    parseLambdaParams("[T](a: Int)")
    parseLambdaParams("[T](a)")
    parseLambdaParams("[T]")
    parseLambdaParams("{f: Exc}")
    parseLambdaParams("[T]{f: Exc}")
  }

  test("Match clauses") {
    parseMatchClause("case x => 42")
    parseMatchClause("case Foo(x, y) => 42")
    parseMatchClause("case Foo(x, y) and x == 43 => 42")
    parseMatchClause("case Foo(Bar(42, true), _) and x == 43 => 42")
    parseMatchClause("case _ => 42")
    parseMatchClause("case a and a is Foo(bar) => 42")
    parseMatchClause("case outer::inner::Constructor(x) => x + 1")
    parseMatchClause("case () => 2")
  }

  test("Op clauses") {
    parseOpClause("def foo() = 42")
    parseOpClause("def foo[T]() = 42")
    parseOpClause("def foo[T](a) = 42")
    parseOpClause("def foo[T](a: Int) = 42")
    parseOpClause("def foo[T](a: Int, b) = 42")
    // TODO rebase!
    // parseOpClause("def foo[T](){f} = 42")
  }

  test("Implementations") {
    {
      val (source, span) =
        raw"""Foo {}
             |↑ ↑
             |""".sourceAndSpan
      assertEquals(
        parseImplementation(source.content),
        Implementation(TypeRef(IdRef(Nil, "Foo", span), Nil), Nil))
    }
    parseImplementation("Foo[T] {}")
    parseImplementation("Foo[T] { def bar() = 42 }")
    parseImplementation(
      """Foo[T] {
        |  def bar() = 42
        |  def baz() = 42
        |}""".stripMargin)

    parseImplementation("Foo[T] { case Get(x) => 43 }")

    // Doesn't work yet
    parseImplementation("Foo { x => 43 }".stripMargin)

    {
      val (source, positions) =
        raw"""Foo { 43 }
             |↑  ↑↑
             |""".sourceAndPositions
      assertEquals(
        parseImplementation("Foo { 43 }"),
        Implementation(
          TypeRef(IdRef(Nil, "Foo", Span(source, positions(0), positions(1))), Nil),
          List(OpClause(IdRef(Nil, "Foo", Span(source, positions(0), positions(1))), Nil, Nil, Nil, None,
            Return(Literal(43, symbols.builtins.TInt)), IdDef("resume", Span(source, positions(1), positions(2)))))))
    }
  }

  test("Try expressions") {
    parseTry("try 42 with Eff { 42 }")
    parseTry("try { 42 } with Eff { 42 }")
    parseTry("try { 42 } with Empty {}")
    parseTry("try { 42 } with Eff { def op(x) = x + 42 }")
    parseTry(
      """try {
      |  val x = 42
      |  do op(x + 1)
      |} with Eff[A] {
      |  def op(x: A) = x
      |}
      """.stripMargin
    )
    parseTry(
      """try { do op(42) }
      with Eff1 {}
      with Eff2 { case Get(x) => x + 1 }
      with Eff3 { def op(x, y) = { x } def op2() = { () }}
      """
    )
  }

  test("Type definition") {
    parseDefinition("type A = Int")
    parseDefinition("type A[X] = Int")
    parseDefinition("type A[X] { Foo() }")
    parseDefinition(
      """type A[X] {
        |  Foo();
        |  Bar()
        |}
        |""".stripMargin)

    parseDefinition(
      """type DATATYPE[X] {
        |  Foo()
        |  Bar()
        |}
        |""".stripMargin)
  }

  test("Namespaces") {
    parseDefinitions(
      """val x = 4
        |val y = 5
        |""".stripMargin)

    val nested = parseDefinitions(
      """namespace list {
        |  val x = 4
        |  val y = 5
        |}
        |""".stripMargin)

    val semi = parseDefinitions(
      """namespace list;
        |val x = 4
        |val y = 5
        |""".stripMargin)

    assertEqualModuloSpans(nested, semi)

    val nested2 = parseDefinitions(
      """namespace list {
        |  namespace internal {
        |
        |    val x = 4
        |    val y = 5
        |  }
        |}
        |""".stripMargin)

    val semi2 = parseDefinitions(
      """namespace list;
        |namespace internal;
        |
        |val x = 4
        |val y = 5
        |""".stripMargin)

    val semiInsertion = parseDefinitions(
      """namespace list
        |namespace internal
        |
        |val x = 4
        |val y = 5
        |""".stripMargin)

    assertEqualModuloSpans(nested2, semi2)
    assertEqualModuloSpans(nested2, semiInsertion)

    parseDefinitions(
      """val x = {
        |  namespace foo;
        |  val y = 4;
        |  foo::y
        |}
        |""".stripMargin)
  }

  test("Definitions") {
    {
      val (source, positions) =
        raw"""def foo = f
             |    ↑  ↑  ↑↑
             |""".sourceAndPositions

      assertEquals(
        parseDefinition(source.content),
        DefDef(IdDef("foo", Span(source, positions(0), positions(1))), None, Var(IdRef(Nil, "f", Span(source, positions(2), positions(3))))))
    }

    parseDefinition(
        """def foo: Exc = f
          |""".stripMargin)

    parseDefinition(
        """def foo() = e
          |""".stripMargin)

    parseDefinition(
        """def foo[T] = e
          |""".stripMargin)

    parseDefinition(
        """def foo[T](x: Int) = e
          |""".stripMargin)

    parseDefinition(
        """def foo[T](x: Int): String / {} = e
          |""".stripMargin)

    parseDefinition(
        """def op(): Int => Int at {greeter} / Greet = f
          |""".stripMargin
    )
  }

  test("Function definition"){
    val (source, positions) =
      raw"""def foo[T1, T2](x: T1, y: T2){b: => Unit}: Unit = <>
           |       ↑       ↑             ↑           ↑     ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.tparams.span, Span(source, positions(0), positions(1)))
    assertEquals(funDef.vparams.span, Span(source, positions(1), positions(2)))
    assertEquals(funDef.bparams.span, Span(source, positions(2), positions(3)))
    assertEquals(funDef.ret.span, Span(source, positions(3), positions(4)))
  }

  test("Function definition with effects"){
    val (source, positions) =
      raw"""def foo{b: => Unit / bar}: Unit / bar = <>
           |       ↑                 ↑           ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, positions(0), positions(1)))
    assertEquals(funDef.ret.span, Span(source, positions(1), positions(2)))
  }

  test("Function definition without return type") {
    val (source, positions) =
      raw"""def foo{b: => Unit / bar} = <>
           |       ↑                 ↑↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, positions(0), positions(1)))
    assertEquals(funDef.ret.span, Span(source, positions(1), positions(2)))
  }

  test("Function definition with whitespaces instead of return type") {
    val (source, positions) =
      raw"""def foo{b: => Unit / bar}      = <>
           |       ↑                 ↑     ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, positions(0), positions(1)))
    assertEquals(funDef.ret.span, Span(source, positions(1), positions(2)))
  }

  test("Toplevel definitions") {
    parseToplevel("def foo() = ()")
    parseToplevel("effect Foo = {}")
    parseToplevel("effect Foo(): Int")
    parseToplevel("interface Foo {}")

    parseToplevel(
      """interface State {
        |  def get(): Int
        |  def set(n: Int): Unit
        |}
        |""".stripMargin)

    parseToplevel(
      """interface State[T] {
        |  def get(): T
        |  def set(n: T): Unit
        |}
        |""".stripMargin)

    parseToplevel(
      """extern include "foo/text.txt"
        |""".stripMargin)

    parseToplevel("extern type Foo[S]")
    parseToplevel("extern resource foo: Foo")

    parseToplevel(
      """extern "function() { console.log('hey!'); }"
        |""".stripMargin)

    parseToplevel(
      """extern def foo(): Int = "bar ${test} baz ${bam}"
        |""".stripMargin)
  }

  test("Record definitions") {
    val rec =
      raw"""record Vec2d[T](x: T, y: T)
           |       ↑    ↑  ↑           ↑""".stripMargin
    val (source, positions) = rec.sourceAndPositions

    val definition = parseToplevel(source.content)
    val recordDef = definition match {
      case rd@RecordDef(id, tparams, fields) => rd
      case other =>
        throw new IllegalArgumentException(s"Expected RecordDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(recordDef.id.span, Span(source, positions(0), positions(1)))
    assertEquals(recordDef.tparams.span, Span(source, positions(1), positions(2)))
    assertEquals(recordDef.fields.span, Span(source, positions(2), positions(3)))
  }

  test("Programs") {
    // this is from examples/pos/builtins
    parseProgram(
      """module examples/pos/builtins
        |
        |type Color { Red(); Green(); Blue() }
        |
        |def main() = {
        |    println(1);
        |    println("foo");
        |    println(true);
        |    println(1 == 2);
        |    inspect(Red())
        |}
        |""".stripMargin)

    parseProgram(
      """// test comment
        |def foo() = 42
        |""".stripMargin)

    parseProgram(
      """module trailing_comment
        |
        |def main() = ()
        |
        |// foo""".stripMargin)
  }

  test("Extern definition") {
    parseExternDef("extern {io} def read(s: String): Int = default { 42 } js { 1 + 1 } chez { 42 }")
    parseExternDef("extern \"console.log(42)\"")
    parseExternDef("extern \"\"\"console.log(42)\"\"\"")
    parseExternDef("extern type Complex")
    parseExternDef("extern type State[A]")
    parseExternDef("extern interface State[A]")
    parseExternDef("extern resource withFile: [A](String) { () => A } => A")
    parseExternDef("extern include \"path/to/file\"")
    parseExternDef("extern js \"\"\"console.log(42)\"\"\"")
    parseExternDef("extern pure def read(s: String): String = default { s }")
    parseExternDef("extern def read(s: String): String = \"${s}\"")
    parseProgram(
      "extern def println(value: String): Unit =" +
      "js \"$effekt.println(${value})\"" +
      "chez \"(println_impl ${value})\"" +
      "llvm \"\"\"call void @c_io_println_String(%Pos %value); ret %Pos zeroinitializer ; Unit\"\"\"" + "\n" +
      "extern js \"\"\" function \"\"\""
    )
  }
}
