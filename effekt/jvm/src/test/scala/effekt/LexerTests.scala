package effekt

import effekt.lexer.TokenKind.*
import effekt.lexer.{Lexer, Position, Token, TokenKind}

import scala.collection.mutable.ListBuffer

import munit.Location

class LexerTests extends munit.FunSuite {
  def assertTokensEq(prog: String, expected: TokenKind*)(using Location): Unit = {
    val (tokens, err) = Lexer(prog).run()
    println(tokens)
    val tokensWithoutWhite = tokens.filter {
      case Token(_, _, Newline) | Token(_, _, Space) => false
      case _ => true
    } 
    if (err.isDefined) fail(s"Lexing failed with error ${err.get}")
    assert(tokensWithoutWhite.length == expected.length, s"wrong number of tokens: obtained ${tokensWithoutWhite.length}, expected ${expected.length}")
    tokensWithoutWhite.zip(expected).foreach((t1, t2) => assertEquals(t1.kind, t2))
  }

  def assertSuccess(prog: String)(using Location): Unit = {
    val (tokens, err) = Lexer(prog).run()
    if (err.isDefined) {
      fail(err.toString)
    }
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
      `:`, `(`, `)`, `=>`, `(`, Ident("A"), `,`, Ident("A"), `)`, `at`, `{`, `}`, `=`,
      `return`, `box`, `{`, `(`, `)`, `=>`, `(`, Ident("x"), `,`, Ident("y"), `)`, `}`
    )
  }

  test("numbers") {
    val num = "12.34 100. 200 123.345 2345.6 1"
    assertTokensEq(
      num,
      Float(12.34), Float(100.0), Integer(200), Float(123.345), Float(2345.6), Integer(1)
    )
  }
  
  test("symbols") {
    val prog = "=> <= < >= > / * - && ++ +"
    assertTokensEq(
      prog,
      `=>`, `<=`, `<`, `>=`, `>`, `/`, `*`, `-`, `&&`, `++`, `+`
    )
  }
  
  test("single line strings") {
    val prog = """ "hello, world" "" "\"hello\""  """
    assertTokensEq(
      prog,
      Str("hello, world", false),
      Str("", false),
      Str("\\\"hello\\\"", false)
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
      Str("val def interface \"\" \"\ncontinues here \t \r\n and is end\n ", true)
    )
  }
  
  test("quoted single-line string") {
    val prog = """"this is a quote ${xs.map { x => "${x + 1}" }} after the quote""""
    assertTokensEq(
      prog,
      TemplateStr(List(
        Token(1,16,Str("this is a quote ", false)), Token(17,18,`${`), Token(19,20,Ident("xs")), Token(21,21,`.`), Token(22,24,Ident("map")), Token(26,26,`{`), Token(28,28,Ident("x")), Token(30,31,`=>`), 
        Token(33,42,TemplateStr(List(Token(34,35,`${`), Token(36,36,Ident("x")), Token(38,38,`+`), Token(40,40,Integer(1)), Token(41,41,`}$`)))),
        Token(44,44,`}`), Token(45,45,`}$`), Token(46,61,Str(" after the quote", false)))
      )
    )
  }
  
  test("quoted multi-line string") {
    val prog =
      "\"\"\"multi-line quote\n" +
      "${x + 1}, ${y + 1} \n" +
      "\"\"\""
    assertTokensEq(
      prog,
      TemplateStr(List(
        Token(3,19,Str("multi-line quote\n", true)), 
        Token(20,21,`${`), Token(22,22,Ident("x")), Token(24,24,`+`), Token(26,26,Integer(1)), Token(27,27,`}$`),
        Token(28,29,Str(", ", true)), 
        Token(30,31,`${`), Token(32,32,Ident("y")), Token(34,34,`+`), Token(36,36,Integer(1)), Token(37,37,`}$`),
        Token(38,39,Str(" \n", true))))
    )
  }

  test("singleline comment") {
    val prog =
      """interface Eff { def operation(): Unit }
        |// val x = 1 def while / \t still\n comment
        |val x = 2""".stripMargin
    assertTokensEq(
      prog,
      `interface`, Ident("Eff"), `{`, `def`, Ident("operation"), `(`, `)`, `:`, Ident("Unit"), `}`,
      Comment(" val x = 1 def while / \\t still\\n comment"),
      `val`, Ident("x"), `=`, Integer(2)
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
      `val`, Ident("x"), `=`, Integer(42),
      Comment(" comment begin def return /* * /\nstill comment\nhere's the end\n"),
      `def`, Ident("main"), `(`, `)`, `:`, Ident("Unit"), `=`, `(`, `)`
    )
  }
  
  test("empty") {
    val prog = ""
    assertTokensEq(prog)
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
      `interface`, Ident("Eff"), `[`, Ident("A"), `,`, Ident("B"), `]`, `{`,
      `def`, Ident("operation"), `[`, Ident("C"), `]`, `(`, Ident("x"), `:`, Ident("C"), `)`, `:`, `(`, Ident("A"), `,`, Ident("B"), `)`,
      `}`
    )
  }

  test("big file") {
    val start = System.nanoTime()
    val file = scala.io.Source.fromFile("libraries/js/immutable/list.effekt").mkString
    assertSuccess(file)
    //println((System.nanoTime() - start) * 1e-9)
  }
}
