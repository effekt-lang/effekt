package effekt

import effekt.lexer.TokenKind.*
import effekt.lexer.{Lexer, Token, TokenKind, LexerError }
import effekt.lexer.LexerError.*

import kiama.util.{ StringSource }

import munit.Location

class LexerTests extends munit.FunSuite {

  def assertTokensEq(prog: String, expected: TokenKind*)(using Location): Unit = {
    val tokens = Lexer(StringSource(prog, "")).run()
    tokens.zipAll(expected, null, null).foreach((t1, t2) => assertEquals(t1.kind, t2))
  }

  def assertSuccess(prog: String)(using Location): Unit =
    try {
      Lexer(StringSource(prog, "")).run()
    } catch {
        case LexerError(msg, start, end) => fail(s"$msg at $start...$end")
    }

  def assertFailure(prog: String)(using Location): Unit =
    try {
      Lexer(StringSource(prog, "")).run()
      fail(s"expected an lexer error but found none")
    } catch {
      case LexerError(_, _, _) => ()
    }

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
    assertFailure("${}")
    assertFailure("\" before ${ ${} } after\"")

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
  }

  test("characters") {
    assertTokensEq("' '", Chr(' '), EOF)
    assertTokensEq("'😅'", Chr(0x1F605), EOF)
    assertTokensEq("'\\\\'", Chr('\\'), EOF)
    assertTokensEq("'\\n'", Chr('\n'), EOF)
    assertTokensEq("'\\t'", Chr('\t'), EOF)
    assertTokensEq("\\u0000", Chr(0), EOF)
    assertTokensEq("\\uFFFF", Chr(0xFFFF), EOF)
    assertTokensEq("\\u10FFFF{ val", Chr(0x10FFFF), `{`, `val`, EOF)
    assertTokensEq("val s =\\u0000+ 1", `val`, Ident("s"), `=`, Chr(0), `+`, Integer(1), EOF)
    assertTokensEq("''", Error(LexerError("Empty character literal", 0, 2)), EOF)
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
    assertFailure("\"\"\"")
    assertFailure("\"\"\" \"")
  }

  test("quoted single-line string") {
    val prog = """ "this is a quote ${ xs.map { x => " ${x + 1}" } } after the quote" """
    assertTokensEq(
      prog,
      Str("this is a quote ",false),
      `${`, Ident("xs"), `.`, Ident("map"),
      `{`, Ident("x"), `=>`, Str(" ",false),
      `${`, Ident("x"), `+`, Integer(1), TokenKind.`}`,
      Str("",false), TokenKind.`}`, TokenKind.`}`,
      Str(" after the quote",false),
      EOF
    )
  }

  test("interpolated string") {
    val prog = """ "${x + 1}${x + 2}" """
    assertTokensEq(
      prog,
      Str("", false),
      `${`, Ident("x"), `+`, Integer(1), TokenKind.`}`,
      Str("", false),
      `${`, Ident("x"), `+`, Integer(2), TokenKind.`}`,
      Str("", false),
      EOF
    )
  }

  test("quoted multi-line string") {
    val prog =
      "\"\"\"multi-line quote\n" +
      "${x + 1}, ${y + 1} \n" +
      "\"\"\""
    assertTokensEq(
      prog,
      Str("multi-line quote\n", true),
      `${`, Ident("x"), `+`, Integer(1), `}`, Str(", ", true), `${`, Ident("y"), `+`, Integer(1), `}`, Str(" \n", true),
      EOF
    )
  }

  test("simple nested interpolated string") {
    val prog = "\"${\"${1}\"2}\""
    assertSuccess(prog)
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

  test("big file") {
    // val start = System.nanoTime()
    val file = scala.io.Source.fromFile("libraries/common/list.effekt").mkString
    assertSuccess(file)
    // println((System.nanoTime() - start) * 1e-9)
  }
}
