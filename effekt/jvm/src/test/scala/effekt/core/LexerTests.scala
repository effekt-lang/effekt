package effekt.core
import effekt.{Lexer, Position, Token, TokenKind}
import effekt.TokenKind.*

class LexerTests extends munit.FunSuite {
  def assertTokensEq(prog: String, expected: TokenKind*): Unit = {
    val res = Lexer(prog).run()
    assert(res.length == expected.length, "wrong number of tokens")
    res.zip(expected).foreach((t1, t2) => assertEquals(t1.kind, t2))
  }

  test("function definition") {
    val prog =
      """def f[A](x: A, y: A): () => (A, A) at {} =
        |    return box { () => (x, y) }
        |""".stripMargin
    assertTokensEq(
      prog, 
      Def, Ident("f"), `[`, Ident("A"), `]`,
      `(`, Ident("x"), `:`, Ident("A"), `,`, Ident("y"), `:`, Ident("A"), `)`,
      `:`, `(`, `)`, `=>`, `(`, Ident("A"), `,`, Ident("A"), `)`, At, `{`, `}`, `=`,
      Return, Box, `{`, `(`, `)`, `=>`, `(`, Ident("x"), `,`, Ident("y"), `)`, `}`,
      EOF
    )
  }

  test("numbers") {
    val num = "12.34 100. 200 123.345 2345.6 1"
    assertTokensEq(
      num,
      Float(12.34), Float(100.0), Integer(200), Float(123.345), Float(2345.6), Integer(1), EOF
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
    val prog = """ "hello, world" "" "\"hello\""  """
    assertTokensEq(
      prog,
      Str("hello, world"),
      Str(""),
      Str("\\\"hello\\\""),
      EOF
    )
  }
  
  test("multi line strings") {
    val prog =
      "\"\"\"val def interface \"\" \"\n" +
      "continues here \t \r\n" +
      " and is end\n" + 
      " \"\"\""
    assertTokensEq(
      prog,
      Str("val def interface \"\" \"\ncontinues here \t \r\n and is end\n "),
      EOF
    )
  }
  
  test("singleline comment") {
    val prog =
      """interface Eff { def operation(): Unit }
        |// val x = 1 def while / \t still\n comment
        |val x = 2""".stripMargin
    assertTokensEq(
      prog,
      Interface, Ident("Eff"), `{`, Def, Ident("operation"), `(`, `)`, `:`, Ident("Unit"), `}`,
      Comment(" val x = 1 def while / \\t still\\n comment"),
      Val, Ident("x"), `=`, Integer(2), EOF
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
      Val, Ident("x"), `=`, Integer(42),
      Comment(" comment begin def return /* * /\nstill comment\nhere's the end\n"),
      Def, Ident("main"), `(`, `)`, `:`, Ident("Unit"), `=`, `(`, `)`,
      EOF
    )
  }
  
  test("empty") {
    val prog = ""
    assertTokensEq(prog, EOF)
  }
  
  test("malformed") {
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
      Comment(" interface definition"),
      Interface, Ident("Eff"), `[`, Ident("A"), `,`, Ident("B"), `]`, `{`,
      Def, Ident("operation"), `[`, Ident("C"), `]`, `(`, Ident("x"), `:`, Ident("C"), `)`, `:`, `(`, Ident("A"), `,`, Ident("B"), `)`,
      `}`,
      EOF
    )
  }
}