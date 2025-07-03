package effekt

import effekt.lexer.TokenKind.*
import effekt.lexer.{Lexer, TokenKind, LexerError }
import effekt.lexer.LexerError.*

import kiama.util.StringSource

import munit.Location

class LexerTests extends munit.FunSuite {

  def assertTokensEq(prog: String, expected: TokenKind*)(using Location): Unit = {
    val tokens = Lexer.lex(StringSource(prog, ""))
    assertEquals(tokens.map { t => t.kind }, expected.toVector)
  }

  def assertSuccess(prog: String)(using Location): Unit =
    val tokens = Lexer.lex(StringSource(prog, ""))
    assertEquals(tokens, tokens.filterNot { t => t.isError }, "Expected no error tokens!")

  def assertFailure(prog: String)(using Location): Unit =
    val tokens = Lexer.lex(StringSource(prog, ""))
    // NOTE(jiribenes, 2025-07-01): Can we do better here? The error is a bit confusing tbh.
    assertNotEquals(tokens, tokens.filterNot { t => t.isError }, "Expected at least one error token, but didn't found any")

  test("function definition") {
    val prog =
      """def f[A](x: A, y: A): () => (A, A) at {} =
        |    return box { () => (x, y) }
        |""".stripMargin
    assertTokensEq(
      prog,
      `def`, Ident("f"), `[`, Ident("A"), `]`,
      `(`, Ident("x"), `:`, Ident("A"), `,`, Ident("y"), `:`, Ident("A"), `)`,
      `:`, `(`, `)`, `=>`, `(`, Ident("A"), `,`, Ident("A"), `)`, `at`, `{`, `}`, `=`, Newline,
      `return`, `box`, `{`, `(`, `)`, `=>`, `(`, Ident("x"), `,`, Ident("y"), `)`, `}`, Newline,
      EOF
    )
  }

  test("braces") {
    // only rejected in the parser!
    assertSuccess("${}")
    assertSuccess("\" before ${ ${} } after\"")

    {
      val prog = "${ ${} }"

      assertTokensEq(
        prog,
        `${`, `${`, `}$`, `}$`,
        EOF
      )
    }

    assertSuccess("}")
    assertSuccess("{}}")
    assertSuccess("{ 42 ")
    assertSuccess("\"${}}}}\"")
    assertSuccess("\"}\"")
    assertSuccess("\" before ${ \"${ 42 }\" } after\"")
  }

  test("numbers") {
    val num = "12.34 100 200 123.345 1 -12.34 -100 -123.345 -1"
    assertTokensEq(
      num,
      Float(12.34), Integer(100), Integer(200), Float(123.345), Integer(1),
      Float(-12.34), Integer(-100), Float(-123.345), Integer(-1),
      EOF
    )
  }

  test("big numbers") {
    assertFailure("9223372036854775808")
    assertTokensEq("9223372036854775807", Integer(9223372036854775807L), EOF)
    assertFailure("-9223372036854775809")
    assertTokensEq("-9223372036854775808", Integer(-9223372036854775808L), EOF)
    // the max double value is 1.7976931348623157E308, without "e notation/scientific notation" support in the lexer,
    // we refrain from writing it down in full length here

    // This once failed independently while the tests above worked, so it's nice to test this too.
    val num = "foo(9223372036854775808)"
    assertTokensEq(
      num,
      Ident("foo"), `(`, Error(InvalidIntegerFormat), `)`,
      EOF
    )
  }

  test("symbols") {
    val prog = "=> <= < >= > / * - && ++ +"
    assertTokensEq(
      prog,
      `=>`, `<=`, `<`, `>=`, `>`, `/`, `*`, `-`, `&&`, `++`, `+`, EOF
    )
  }

  test("single line strings") {
    val prog = """ "hello, world" "" """
    assertTokensEq(
      prog,
      Str("hello, world", false),
      Str("", false),
      EOF
    )
    assertTokensEq("\"\\\"hello\\\"\"", Str("\"hello\"", false), EOF)
    assertTokensEq("\" \\n\"", Str(" \n", false), EOF)
    assertTokensEq("\" \\t \"", Str(" \t ", false), EOF)
    assertFailure("\"\\k\"")
    assertTokensEq("\"\\u001b\"", Str("\u001b", false), EOF)
    assertTokensEq("\"\\u{001b}\"", Str("\u001b", false), EOF)
    assertTokensEq("'\\\''", Chr(39), EOF)
    assertTokensEq("\"'\"", Str("'", false), EOF)
    assertTokensEq("'\\\"'", Chr(34), EOF)
    assertTokensEq("\"\\\"\"", Str("\"", false), EOF)
  }

  test("characters") {
    assertTokensEq("' '", Chr(' '), EOF)
    assertTokensEq("'😅'", Chr(0x1F605), EOF)
    assertTokensEq("'\\\\'", Chr('\\'), EOF)
    assertTokensEq("'\\n'", Chr('\n'), EOF)
    assertTokensEq("'\\t'", Chr('\t'), EOF)
    assertTokensEq("'\\u0000'", Chr(0), EOF)
    assertTokensEq("'\\uFFFF'", Chr(0xFFFF), EOF)
    assertTokensEq("'\\u10FFFF'{ val", Chr(0x10FFFF), `{`, `val`, EOF)
    assertTokensEq("val s ='\\u0000'+ 1", `val`, Ident("s"), `=`, Chr(0), `+`, Integer(1), EOF)
    assertTokensEq("''", Error(LexerError.EmptyCharLiteral), EOF)
  }

  test("multi line strings") {
    val prog =
      "\"\"\"val def interface \"\" \"\n" +
      "continues here \t \r\n" +
      " and is end\n" +
      " \"\"\""
    assertTokensEq(
      prog,
      Str("val def interface \"\" \"\ncontinues here \t \r\n and is end\n ", true),
      EOF
    )
    assertTokensEq("\" \"\"\"", Str(" ", false), Str("", false), EOF)
    assertTokensEq(
      "\"\"\" \"",
      Error(UnterminatedStringLike(Str(" \"", true))),
      EOF
    )

    assertTokensEq(
      "\"\"\"",
      Error(UnterminatedStringLike(Str("", true))),
      EOF
    )
  }

  test("quoted single-line string") {
    val prog = """ "this is a quote ${ xs.map { x => " ${x + 1}" } } after the quote" """
    assertTokensEq(
      prog,
      Str("this is a quote ",false),
      `${`, Ident("xs"), `.`, Ident("map"),
      `{`, Ident("x"), `=>`, Str(" ",false),
      `${`, Ident("x"), `+`, Integer(1), TokenKind.`}$`,
      Str("",false), TokenKind.`}`, TokenKind.`}$`,
      Str(" after the quote",false),
      EOF
    )
  }

  test("missing interpolation") {
    val prog = """ "hello ${ xs.map { x => "${x + 1}" } " """
    assertTokensEq(
      prog,
      Str("hello ", multiline = false),
      `${`, Ident("xs"), `.`, Ident("map"),
      `{`, Ident("x"), `=>`,
      Str("", multiline = false),
      `${`, Ident("x"), `+`, Integer(1), `}$`,
      Str("", multiline = false),
      `}`,
      Error(UnterminatedStringLike(Str(" ", multiline = false))),
      Error(UnterminatedInterpolation(depth = 1)),
      EOF
    )
  }

  test("simple interpolated string") {
    assertTokensEq(
      "\"Hello ${ 42 } world\"",
      Str("Hello ", false),
      `${`, Integer(42), `}$`,
      Str(" world", false),
      EOF
    )
  }

  test("interpolated string") {
    val prog = """ "${x + 1}${x + 2}" """
    assertTokensEq(
      prog,
      Str("", false),
      `${`, Ident("x"), `+`, Integer(1), TokenKind.`}$`,
      Str("", false),
      `${`, Ident("x"), `+`, Integer(2), TokenKind.`}$`,
      Str("", false),
      EOF
    )
  }

  test("interpolated hole string") {
    val prog = "<\" natural language text with terms like ${ 1 + 1 } inside \">"
    assertTokensEq(
      prog,
      HoleStr(" natural language text with terms like "),
      `${`, Integer(1), `+`, Integer(1), `}$`,
      HoleStr(" inside "),
      EOF
    )
  }

  test("multiline string holes") {
    val prog1: String =
      """<" Here it starts
        |  and here it continues ">
        |""".stripMargin
    assertTokensEq(
      prog1,
      HoleStr(" Here it starts\n  and here it continues "), Newline, EOF
    )

    val prog2: String =
      """<" Oh look, a number!
        |   ${ 42 }
        |  and here it continues ">
        |""".stripMargin
    assertTokensEq(
      prog2,
      HoleStr(" Oh look, a number!\n   "),
      `${`, Integer(42), `}$`,
      HoleStr("\n  and here it continues "), Newline,
      EOF
    )

    val prog3: String =
      """effect foo(): Int
        |effect bar(): String
        |def baz(x: Int): Int / {foo, bar} = <"
        |  This is some natural language text.
        |  ${ do foo() }
        |  Some more text.
        |  ${ do bar() }
        |">
        |""".stripMargin

    assertSuccess(prog3)
  }

  test("quoted multi-line string") {
    val prog =
      "\"\"\"multi-line quote\n" +
      "${x + 1}, ${y + 1} \n" +
      "\"\"\""
    assertTokensEq(
      prog,
      Str("multi-line quote\n", true),
      `${`, Ident("x"), `+`, Integer(1), `}$`, Str(", ", true), `${`, Ident("y"), `+`, Integer(1), `}$`, Str(" \n", true),
      EOF
    )
  }

  test("simple nested interpolated string") {
    val prog = "\"${\"${1}\"2}\""
    assertSuccess(prog)
  }

  test("trivial comment") {
    val prog = "//"
    assertTokensEq(
      prog,
      Comment(""), EOF
    )
  }

  test("unicode comment") {
    val prog = "// ≤"
    assertTokensEq(
      prog,
      Comment(" ≤"), EOF
    )
  }

  test("singleline comment") {
    val prog =
      """interface Eff { def operation(): Unit }
        |// val x = 1 def while / \t still\n comment
        |val x = 2""".stripMargin
    assertTokensEq(
      prog,
      `interface`, Ident("Eff"), `{`, `def`, Ident("operation"), `(`, `)`, `:`, Ident("Unit"), `}`, Newline,
      Comment(" val x = 1 def while / \\t still\\n comment"), Newline,
      `val`, Ident("x"), `=`, Integer(2),
      EOF
    )
  }

  test("multiline comment") {
    val prog =
      """val x = 42
        |/* comment begin def return /* * /
        |still comment
        |here's the end
        |*/
        |def main(): Unit = ()""".stripMargin

    assertTokensEq(
      prog,
      `val`, Ident("x"), `=`, Integer(42), Newline,
      Comment(" comment begin def return /* * /\nstill comment\nhere's the end\n"), Newline,
      `def`, Ident("main"), `(`, `)`, `:`, Ident("Unit"), `=`, `(`, `)`,
      EOF
    )
  }

  test("newline") {
    val prog =
      """ val next = f() // Comment
      |g()
      |""".stripMargin
    assertTokensEq(
      prog,
      `val`, Ident("next"), `=`, Ident("f"), `(`, `)`, Comment(" Comment"), Newline,
      Ident("g"), `(`, `)`, Newline,
      EOF
    )
  }

  test("empty") {
    val prog = ""
    assertTokensEq(prog, EOF)
  }

  test("ignore whitespace") {
    val prog =
      """// interface definition
        |  interface
        |
        |     Eff[A,
        |  B] { def operation
        |  [C]
        |  (x:    C)  :
        |
        |  (A, B)
        |
        |  }
        |""".stripMargin
    assertTokensEq(
      prog,
      Comment(" interface definition"), Newline,
      `interface`, Newline, Newline, Ident("Eff"), `[`, Ident("A"), `,`, Newline, Ident("B"), `]`, `{`,
      `def`, Ident("operation"), Newline, `[`, Ident("C"), `]`, Newline, `(`, Ident("x"), `:`, Ident("C"), `)`, `:`, Newline, Newline, `(`, Ident("A"), `,`, Ident("B"), `)`, Newline,
      Newline,
      `}`, Newline,
      EOF
    )
  }

  test("resilience") {
    val prog =
      """
        |val a = ${}
        |val b = 123456789012345678901234567890
        |val c = "
        |inspect(c)
        |""".stripMargin

    assertTokensEq(
      prog,
      Newline,
      `val`, Ident("a"), `=`, `${`, `}$`, Newline,
      `val`, Ident("b"), `=`, Error(InvalidIntegerFormat), Newline,
      `val`, Ident("c"), `=`, Error(UnterminatedStringLike(Str("", multiline = false))), Newline,
      Ident("inspect"), `(`, Ident("c"), `)`, Newline,
      EOF
    )
  }

  def assertSuccessFile(filename: String): Unit = {
    // val start = System.nanoTime()
    val file = scala.io.Source.fromFile(filename).mkString
    assertSuccess(file)
    // println(s"${filename}: ${(System.nanoTime() - start) * 1e-9}")
  }

  test("big stdlib files") {
    assertSuccessFile("libraries/common/list.effekt")
    assertSuccessFile("libraries/common/effekt.effekt")
    assertSuccessFile("libraries/common/stringbuffer.effekt")
    assertSuccessFile("libraries/common/exception.effekt")
    assertSuccessFile("libraries/common/array.effekt")
    assertSuccessFile("libraries/common/bytearray.effekt")
    assertSuccessFile("libraries/common/char.effekt")
    assertSuccessFile("libraries/common/stream.effekt")
    assertSuccessFile("libraries/common/map.effekt")
  }
}
