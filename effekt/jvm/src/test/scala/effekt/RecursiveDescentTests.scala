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
    val tokens = effekt.lexer.Lexer.lex(source)
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

  def parseReturnAnnotation(input: String, positions: Positions = new Positions())(using munit.Location): Effectful =
    parse(input, _.returnAnnotation())

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

  def equalModuloSpans(a: Any, b: Any): Boolean = {
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
    if (!equalModuloSpans(obtained, expected)) {
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
    if (equalModuloSpans(obtained, expected)) {
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

  // Parsing tests
  //
  //

  test("Simple expressions") {
    parseExpr("42")
    parseExpr("f")
    parseExpr("f(a)")
    parseExpr("f(a, 42)")
    parseExpr("\\u0000")

    parseExpr("l.foreach { _ => 42 }")
    {
      val (source, pos) =
        raw"""loop { f }
             |↑   ↑  ↑↑ ↑
             |""".sourceAndPositions

      assertEquals(
        parseExpr(source.content),
        Call(
          IdTarget(IdRef(Nil, "loop", Span(source, pos(0), pos(1)))),
          Nil, Nil,
          List(
            Var(IdRef(Nil, "f", Span(source, pos(2), pos(3))), Span(source, pos(2), pos(3)))
          ),
          Span(source, pos(0), pos.last)
        )
      )
    }

    assertNotEqualModuloSpans(
      parseExpr("f.m(a, 42)"),
      parseExpr("(f.m)(a, 42)"))

    assertEqualModuloSpans(
      parseExpr("f(a, 42)()"),
      parseExpr("(f(a, 42))()"))

    {
      val (source, pos) =
        raw"""foo.bar
             |↑  ↑↑  ↑
             |""".sourceAndPositions
      assertEquals(
        parseExpr(source.content),
        // At the moment uniform function call syntax is always a method call
        MethodCall(
          Var(IdRef(Nil, "foo", Span(source, pos(0), pos(1))), Span(source, pos(0), pos(1))),
          IdRef(Nil, "bar", Span(source, pos(2), pos(3))), Nil, Nil, Nil,
          Span(source, pos(0), pos.last)
        )
      )
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

  test("Strings") {
    parseExpr("\"\"")
    parseExpr("\"hello\"")
    parseExpr("\"Hello, ${name}\"")
    parseExpr("\"My name is ${person.lastName}, ${person.firstName} ${person.lastName}\"")
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
      parseExpr("box { 42 } at {}")
    )
    parseExpr("box { (x: Int) => x }")
    parseExpr("box new Fresh { def fresh() = \"42\" }")
    parseExpr("box foo()")
    parseExpr("box bar(1)")
    parseExpr("box baz(quux)")
    parseExpr("box { (x, y) => compareByteString(x, y) } at {io, global}")

    {
      val (source, pos) =
        raw"""box { (x, y) => compareByteString(x, y) }
             |                                         ↑
             |""".sourceAndPosition
      val b = parseExpr(source.content)
      b match {
        case Term.Box(c, _, _) => assertEquals(c.span, Span(source, pos, pos, Synthesized))
        case other =>
          throw new IllegalArgumentException(s"Expected Box but got ${other.getClass.getSimpleName}")
      }
    }

    // { f } is parsed as a capture set and not backtracked.
    intercept[Throwable] { parseExpr("box { f }") }
  }

  test("Holes") {
    parseExpr("<>")
    parseExpr("<\" natural language text \">")
    parseExpr("<\" natural language text with { braces } \">")
    parseExpr("<\" natural language text with terms like ${ 1 + 1 } inside \">")
    parseExpr("<\" deeply ${ 1 + <\" nested stuff ${ 2 } \"> + 3 } \">")
    parseExpr("<\" you can use statements like ${ val foo = 42 } inside \">")
    parseExpr("<\"\">")
    parseExpr("<\" ${ x }\">")

    {
      val (source, expectedSpan) =
        """<" let's check that the span is correct ">
          |↑                                        ↑
          |""".sourceAndSpan
      val hole = parseExpr(source.content)
      hole match {
        case Term.Hole(_, _, span) =>
          assertEquals(span, expectedSpan)
        case other =>
          throw new IllegalArgumentException(s"Expected Hole but got ${other.getClass.getSimpleName}")
      }
    }

    {
      val (source, expectedSpan) =
        """<>
          |↑↑
          |""".sourceAndSpan

      val hole = parseExpr(source.content)
      hole match {
        case Term.Hole(_, _, span) =>
          println(hole)
          assertEquals(span, expectedSpan)
        case other =>
          throw new IllegalArgumentException(s"Expected Hole but got ${other.getClass.getSimpleName}")
      }
    }
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
      assertEquals(parseExpr(source.content), Var(IdRef(List(), "map", span), span))
    }
    {
      val (source, span) =
        raw"""list::map
             |↑       ↑
             |""".sourceAndSpan
      assertEquals(parseExpr(source.content), Var(IdRef(List("list"), "map", span), span))
    }
    {
      val (source, span) =
        raw"""list::internal::map
             |↑                 ↑
             |""".sourceAndSpan
      assertEquals(parseExpr(source.content), Var(IdRef(List("list", "internal"), "map", span), span))
    }
    {
      val (source, span) =
        raw"""list::internal::test
             |↑                  ↑
             |""".sourceAndSpan
      assertEquals(parseExpr(source.content), Var(IdRef(List("list", "internal"), "test", span), span))
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

  test("Pattern-matching val parses with correct span") {
    val (source, pos) =
      raw"""val (left, right) = list; return left
           |    ↑↑   ↑ ↑    ↑↑  ↑   ↑ ↑      ↑   ↑
           |""".sourceAndPositions
    val expected = Return(Match(
      List(Var(IdRef(List(), "list", Span(source,pos(6),pos(7))),Span(source,pos(6),pos(7)))),
      List(
        MatchClause(
          TagPattern(
            IdRef(List("effekt"),"Tuple2",Span(source,pos(0),pos(5),Synthesized)),
            List(AnyPattern(IdDef("left", Span(source,pos(1),pos(2))), Span(source,pos(1),pos(2))),
              AnyPattern(IdDef("right",Span(source,pos(3),pos(4))), Span(source,pos(3),pos(4)))),
            Span(source,pos(0),pos(5))
          ),
          List(),
          Return(Var(IdRef(List(),"left",Span(source,pos(9),pos(10))),
            Span(source,pos(9),pos(10))),
            Span(source,pos(8),pos(10))),
          Span(source,pos(0),pos(7))
        )
      ),
      None,
      Span(source,0,pos(7), Synthesized)
    ),Span(source,0,pos.last, Synthesized));
    assertEquals(parseStmts(source.content), expected)
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
      IgnorePattern(Span.missing))
    parseMatchPattern("Cons(x, Cons(x, Nil()))")

    {
      val (source, pos) =
        raw"""(left, Cons(x, right))
             |↑↑   ↑ ↑   ↑↑↑ ↑    ↑↑↑
             |""".sourceAndPositions
      assertEquals(
        parseMatchPattern(source.content),
        TagPattern(IdRef(List("effekt"), "Tuple2", Span(source, pos(0), pos.last, Synthesized)),
          List(AnyPattern(IdDef("left", Span(source, pos(1), pos(2))), Span(source, pos(1), pos(2))),
            TagPattern(
              IdRef(List(), "Cons", Span(source, pos(3), pos(4))),
              List(AnyPattern(IdDef("x", Span(source, pos(5), pos(6))), Span(source, pos(5), pos(6))),
                AnyPattern(IdDef("right", Span(source, pos(7), pos(8))), Span(source, pos(7), pos(8)))),
              Span(source, pos(3), pos(9))
            )),
          Span(source, pos(0), pos.last)
        )
      )
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

  test("Return annotations") {
    parseReturnAnnotation(": Int")
    parseReturnAnnotation(": Int / Exc")
    parseReturnAnnotation(": Int / { Exc }")
    parseReturnAnnotation(": Int / { Exc, State }")
    {
      val (source, pos) =
        raw""": Int / { Exc, State[T] }
             |  ↑  ↑  ↑ ↑  ↑ ↑    ↑↑↑↑ ↑
             |""".sourceAndPositions
      assertEquals(
        parseReturnAnnotation(source.content),
        Effectful(
          TypeRef(
            IdRef(Nil, "Int", Span(source, pos(0), pos(1))),
            Many.empty(Span(source, pos(1), pos(1))),
            Span(source, pos(0), pos(1))
          ),
          Effects(
            List(
              TypeRef(
                IdRef(Nil, "Exc", Span(source, pos(3), pos(4))),
                Many.empty(Span(source, pos(4), pos(4))),
                Span(source, pos(3), pos(4))
              ),
              TypeRef(
                IdRef(Nil, "State", Span(source, pos(5), pos(6))),
                Many(List(
                  TypeRef(
                    IdRef(Nil, "T", Span(source, pos(7), pos(8))),
                    Many.empty(Span(source, pos(8), pos(8))),
                    Span(source, pos(7), pos(8))
                  )
                ), Span(source, pos(6), pos(9))),
                Span(source, pos(5), pos(9))
              )
            ),
            Span(source, pos(2), pos.last)
          ),
          Span(source, pos.head, pos.last)
        )
      )
    }
  }

  test("Value types") {
    {
      val (source, pos) =
        raw"""Int
             |↑  ↑
             |""".sourceAndPositions
      assertEquals(
        parseValueType(source.content),
        TypeRef(IdRef(Nil, "Int", Span(source, pos(0), pos(1))), Many.empty(Span(source, pos(1), pos(1))), Span(source, pos(0), pos(1))))
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
      val (source, pos) =
        raw"""(Int, String) => Int
             |↑↑  ↑ ↑     ↑↑   ↑  ↑
             |""".sourceAndPositions
      assertEquals(
        parseBlockType(source.content),
        FunctionType(Many.empty(Span(source, pos(0), pos(0))),
          Many(List(
            TypeRef(IdRef(Nil, "Int", Span(source, pos(1), pos(2))), Many.empty(Span(source, pos(2), pos(2))), Span(source, pos(1), pos(2))),
            TypeRef(IdRef(Nil, "String", Span(source, pos(3), pos(4))), Many.empty(Span(source, pos(4), pos(4))), Span(source, pos(3), pos(4)))
          ), Span(source, pos(0), pos(5))),
          Many.empty(Span(source, pos(5), pos(5))),
          TypeRef(IdRef(Nil, "Int", Span(source, pos(6), pos(7))), Many.empty(Span(source, pos(7), pos(7))), Span(source, pos(6), pos(7))),
          Effects(Nil, Span(source, pos.last, pos.last, Synthesized)),
          Span(source, pos(0), pos.last)
        )
      )
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
      val (source, pos) =
        raw"""Foo {}
             |↑  ↑  ↑
             |""".sourceAndPositions
      assertEquals(
        parseImplementation(source.content),
        Implementation(
          TypeRef(IdRef(Nil, "Foo", Span(source, pos(0), pos(1))), Many.empty(Span(source, pos(1), pos(1))), Span(source, pos(0), pos(1))),
          Nil,
          Span(source, pos(0), pos(2)))
      )
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
      val (source, pos) =
        raw"""Foo { 43 }
             |↑  ↑  ↑ ↑ ↑
             |""".sourceAndPositions
      assertEquals(
        parseImplementation(source.content),
        Implementation(
          TypeRef(IdRef(Nil, "Foo", Span(source, pos(0), pos(1))), Many.empty(Span(source, pos(1), pos(1))), Span(source, pos(0), pos(1), Synthesized)),
          List(OpClause(IdRef(Nil, "Foo", Span(source, pos(0), pos(1), Synthesized)), Nil, Nil, Nil, None,
            Return(Literal(43, symbols.builtins.TInt, Span(source, pos(2), pos(3))), Span(source, pos(2), pos(3))), IdDef("resume", Span(source, pos(1), pos(1))),
            Span(source, pos(0), pos(3), Synthesized))
          ),
          Span(source, pos(0), pos.last)
        ))
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

  test("Try handler capability parses with correct span") {
    val (source, pos) =
      raw"""try { 42 } with eff: Eff { 42 }
           |                ↑  ↑
           |""".sourceAndPositions

    val tryExpr = parseTry(source.content) match {
      case t: Term.TryHandle => t
      case other =>
        throw new IllegalArgumentException(s"Expected Try but got ${other.getClass.getSimpleName}")
    }

    assertEquals(tryExpr.handlers.head.capability.get.span, Span(source, pos(0), pos(1), Synthesized))
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
      """/// type A
        |type A[X] {
        |  /// Foo of A
        |  Foo();
        |
        |  /// Bar of A
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
      val (source, pos) =
        raw"""def foo = f
             |    ↑  ↑  ↑↑
             |""".sourceAndPositions

      assertEquals(
        parseDefinition(source.content),
        DefDef(
          IdDef("foo", Span(source, pos(0), pos(1))),
          None,
          Var(IdRef(Nil, "f", Span(source, pos(2), pos(3))), Span(source, pos(2), pos(3))),
          None, Span(source, 0, pos.last)))
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
    val (source, pos) =
      raw"""def foo[T1, T2](x: T1, y: T2){b: => Unit}: Unit = <>
           |       ↑       ↑             ↑           ↑     ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, doc, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.tparams.span, Span(source, pos(0), pos(1)))
    assertEquals(funDef.vparams.span, Span(source, pos(1), pos(2)))
    assertEquals(funDef.bparams.span, Span(source, pos(2), pos(3)))
    assertEquals(funDef.ret.span, Span(source, pos(3), pos(4)))
  }

  test("Function definition with effects"){
    val (source, pos) =
      raw"""def foo{b: => Unit / bar}: Unit / bar = <>
           |       ↑                 ↑           ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, doc, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, pos(0), pos(1)))
    assertEquals(funDef.ret.span, Span(source, pos(1), pos(2)))
  }

  test("Function definition without return type") {
    val (source, pos) =
      raw"""def foo{b: => Unit / bar} = <>
           |       ↑                 ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, doc, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, pos(0), pos(1)))
    assertEquals(funDef.ret.span, Span(source, pos(1), pos(1)))
  }

  test("Function definition with whitespaces instead of return type") {
    val (source, pos) =
      raw"""def foo{b: => Unit / bar}      = <>
           |       ↑                 ↑
           |""".sourceAndPositions

    val definition = parseDefinition(source.content)

    val funDef = definition match {
      case fd@FunDef(id, tparams, vparams, bparams, ret, body, doc, span) => fd
      case other =>
        throw new IllegalArgumentException(s"Expected FunDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(funDef.bparams.span, Span(source, pos(0), pos(1)))
    assertEquals(funDef.ret.span, Span(source, pos(1), pos(1)))
  }

  test("Value definition parses with correct span") {
    val (source, span) =
      raw"""val foo = 5
           |↑         ↑
           |""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val valDef = definition match {
      case vd@ValDef(id, annot, binding, doc, span) => vd
      case other =>
        throw new IllegalArgumentException(s"Expected ValDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(valDef.span, span)
  }

  test("Value definition and docs parses with correct span") {
    val (source, span) =
      raw"""/// Some doc comment
           |↑
           |val foo = 5
           |          ↑
           |""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val valDef = definition match {
      case vd@ValDef(id, annot, binding, doc, span) => vd
      case other =>
        throw new IllegalArgumentException(s"Expected ValDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(valDef.span, span)
  }


  test("Region definition parses with correct span") {
    val (source, span) =
      raw"""var foo: Int in bar = 5; ()
           |↑                     ↑
           |""".sourceAndSpan

    val definition = parseStmts(source.content)

    val regDef = definition match {
      case DefStmt(rd@RegDef(id, annot, region, binding, doc, span), _, _) => rd
      case other =>
        throw new IllegalArgumentException(s"Expected RegDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(regDef.span, span)
  }

  test("Var definition parses with correct span") {
    val (source, span) =
      raw"""var foo = 7; ()
           |↑         ↑
           |""".sourceAndSpan

    val definition = parseStmts(source.content)

    val varDef = definition match {
      case DefStmt(vd@VarDef(id, annot, binding, doc, span), _, _) => vd
      case other =>
        throw new IllegalArgumentException(s"Expected VarDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(varDef.span, span)
  }

  test("Namespace definition parses with correct span") {
    val (source, span) =
      raw"""namespace Foo {
           |↑
           |}
           |↑""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val nsDef = definition match {
      case nd@NamespaceDef(id, defs, doc, span) => nd
      case other =>
        throw new IllegalArgumentException(s"Expected NamespaceDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(nsDef.span, span)
  }

  test("Interface definition parses with correct span") {
    val (source, span) =
      raw"""interface IBar {}
           |↑               ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val intDef = definition match {
      case id@InterfaceDef(name, tparams, ops, doc, span) => id
      case other =>
        throw new IllegalArgumentException(s"Expected InterfaceDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(intDef.span, span)
  }

  test("Data definition parses with correct span") {
    val (source, span) =
      raw"""type Option[T] {
           |↑
           |  Some();
           |}
           |↑""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val dataDef = definition match {
      case dd@DataDef(id, tparams, ctors, doc, span) => dd
      case other =>
        throw new IllegalArgumentException(s"Expected DataDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(dataDef.span, span)
  }

  test("Record definition parses with correct span") {
    val (source, span) =
      raw"""record R(x: Int)
           |↑              ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val recDef = definition match {
      case rd@RecordDef(id, tparams, fields, doc, span) => rd
      case other =>
        throw new IllegalArgumentException(s"Expected RecordDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(recDef.span, span)
  }

  test("Type alias definition parses with correct span") {
    val (source, span) =
      raw"""type Matrix[T] = List[List[T]]
           |↑                            ↑
           |""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val typeDef = definition match {
      case td@TypeDef(id, tparams, tpe, doc, span) => td
      case other =>
        throw new IllegalArgumentException(s"Expected TypeDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(typeDef.span, span)
  }

  test("Effect definition parses with correct span") {
    val (source, span) =
      raw"""effect Set = { Get, Put }
           |↑                       ↑
           |""".sourceAndSpan

    val definition = parseDefinition(source.content)

    val effDef = definition match {
      case ed@EffectDef(id, tparams, effs, doc, span) => ed
      case other =>
        throw new IllegalArgumentException(s"Expected EffectDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(effDef.span, span)
  }

  test("Effect operation definition parses with correct span") {
    val (source, pos) =
      raw"""effect emit[A](value: A): Unit
           |↑      ↑   ↑  ↑               ↑
           |""".sourceAndPositions

    val definition = parseToplevel(source.content)

    val intDef = definition match {
      case id@InterfaceDef(name, tparams, ops, doc, span) => id
      case other =>
        throw new IllegalArgumentException(s"Expected InterfaceDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(intDef.tparams.span, Span(source, pos(2), pos(3)))
    assertEquals(intDef.ops.head.span, Span(source, pos(1), pos.last))
    assertEquals(intDef.ops.head.tparams.span, Span(source, pos(2), pos(3), Synthesized))
    assertEquals(intDef.span, Span(source, pos(0), pos.last))
  }

  test("Extern type definition parses with correct span") {
    val (source, span) =
      raw"""extern type Foo
           |↑             ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val extType = definition match {
      case et@ExternType(id, tparams, doc, span) => et
      case other =>
        throw new IllegalArgumentException(s"Expected ExternType but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extType.span, span)
  }

  test("Extern interface definition parses with correct span") {
    val (source, span) =
      raw"""extern interface IFace[T]
           |↑                       ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val extIfc = definition match {
      case ei@ExternInterface(id, tparams, doc, span) => ei
      case other =>
        throw new IllegalArgumentException(s"Expected ExternInterface but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extIfc.span, span)
  }

  test("Extern resource definition parses with correct span") {
    val (source, span) =
      raw"""extern resource Res: Int
           |↑                      ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val extRes = definition match {
      case er@ExternResource(id, tpe, doc, span) => er
      case other =>
        throw new IllegalArgumentException(s"Expected ExternResource but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extRes.span, span)
  }

  test("Extern include definition parses with correct span") {
    val (source, span) =
      raw"""extern include "foo.effekt"
           |↑                         ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val extInc = definition match {
      case ei@ExternInclude(flag, path, contents, id, doc, span) => ei
      case other =>
        throw new IllegalArgumentException(s"Expected ExternInclude but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extInc.span, span)
  }

  test("Extern function definition parses with correct span") {
    val (source, span) =
      raw"""extern def foo(): Int = "body"
           |↑                            ↑
           |""".sourceAndSpan

    val definition = parseToplevel(source.content)

    val extFun = definition match {
      case ef@ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, doc, span) => ef
      case other =>
        throw new IllegalArgumentException(s"Expected ExternDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extFun.span, span)
  }

  test("Extern effect‐body parses with correct span") {
    val (source, pos) =
      raw"""extern def foo(): Int = default { foo() }
           |↑                       ↑      ↑         ↑
           |""".sourceAndPositions

    val definition = parseToplevel(source.content)

    val extDef = definition match {
      case ed: ExternDef => ed
      case other =>
        throw new IllegalArgumentException(s"Expected ExternDef but got ${other.getClass.getSimpleName}")
    }

    assertEquals(extDef.bodies.head.span, Span(source, pos(1), pos.last))
    assertEquals(extDef.bodies.head.featureFlag.span, Span(source, pos(1), pos(2)))
    assertEquals(extDef.span, Span(source, pos(0), pos.last))
  }

  test("Toplevel definitions") {
    parseToplevel(
      """/// foo function
        |def foo() = ()
        |""".stripMargin)
    parseToplevel("def foo() = ()")
    parseToplevel(
      """/// Foo effect
        |effect Foo = {}
        |""".stripMargin)
    parseToplevel("effect Foo = {}")
    parseToplevel("effect Foo(): Int")
    parseToplevel(
      """/// Foo interface
        |interface Foo {}
        |""".stripMargin)
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
      """/// State interface
        |interface State {
        |  /// get operation
        |  def get(): Int
        |
        |  /// set operation
        |  def set(n: Int): Unit
        |}
        |""".stripMargin)

    parseToplevel(
      """extern include "foo/text.txt"
        |""".stripMargin)
    parseToplevel(
      """/// include extern file
        |extern include "foo/text.txt"
        |""".stripMargin)

    parseToplevel("extern type Foo[S]")
    parseToplevel(
      """/// extern Foo type
        |extern type Foo[S]
        |""".stripMargin)
    parseToplevel("extern resource foo: Foo")
    parseToplevel(
      """/// extern Foo resource
        |extern resource foo: Foo
        |""".stripMargin)

    parseToplevel(
      """extern "function() { console.log('hey!'); }"
        |""".stripMargin)
    parseToplevel(
      """/// extern function
        |extern "function() { console.log('hey!'); }"
        |""".stripMargin)

    parseToplevel(
      """extern def foo(): Int = "bar ${test} baz ${bam}"
        |""".stripMargin)
    parseToplevel(
      """/// extern foo definition
        |extern def foo(): Int = "bar ${test} baz ${bam}"
        |""".stripMargin)

    parseToplevel(
      """/// list namespace
        |namespace list {
        |  val x = 4
        |  val y = 5
        |}
        |""".stripMargin)

  }

  test("Record definitions") {
    val rec =
      raw"""record Vec2d[T](x: T, y: T)
           |       ↑    ↑  ↑           ↑""".stripMargin
    val (source, pos) = rec.sourceAndPositions

    val definition = parseToplevel(source.content)
    val recordDef = definition match {
      case rd@RecordDef(id, tparams, fields, doc, span) => rd
      case other =>
        throw new IllegalArgumentException(s"Expected RecordDef but got ${other.getClass.getSimpleName}")
    }
    assertEquals(recordDef.id.span, Span(source, pos(0), pos(1)))
    assertEquals(recordDef.tparams.span, Span(source, pos(1), pos(2)))
    assertEquals(recordDef.fields.span, Span(source, pos(2), pos(3)))
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

    parseProgram(
      """/// Module with Doc Comment
        |module commented_module
        |
        |def main() = ()
        |""".stripMargin)

    parseProgram(
      """#!/usr/bin/env effekt
        |
        |def main() = ()
        |""".stripMargin)

    parseProgram(
      """#!/usr/bin/env nix
        |#!nix run effekt-nix
        |
        |def main() = ()
        |""".stripMargin)
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
