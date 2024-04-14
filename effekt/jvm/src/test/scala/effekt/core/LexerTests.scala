package effekt.core
import effekt.{Lexer, Position, Token, TokenKind}
import effekt.TokenKind.*

import scala.collection.mutable.ListBuffer

class LexerTests extends munit.FunSuite {
  def assertTokensEq(prog: String, expected: TokenKind*): Unit = {
    val (res, err) = Lexer(prog).run()
    println(res)
    if (err.isDefined) fail(s"Lexing failed with error ${err.get}")
    assert(res.length == expected.length, s"wrong number of tokens: obtained ${res.length}, expected ${expected.length}")
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
  
  test("quoted single-line string") {
    val prog = """"this is a quote ${xs.map { x => "${x + 1}" }} after the quote""""
    assertTokensEq(
      prog,
      QuotedStr(List(
        Token(1,16,Str("this is a quote ")), Token(17,18,`${`), Token(19,20,Ident("xs")), Token(21,21,`.`), Token(22,24,Ident("map")), Token(26,26,`{`), Token(28,28,Ident("x")), Token(30,31,`=>`), 
        Token(33,42,QuotedStr(List(Token(34,35,`${`), Token(36,36,Ident("x")), Token(38,38,`+`), Token(40,40,Integer(1)), Token(41,41,`}$`)))),
        Token(44,44,`}`), Token(45,45,`}$`), Token(46,62,Str(" after the quote")))
      ),
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
