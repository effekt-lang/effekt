package effekt

import effekt.source.*
import effekt.symbols.*

import effekt.lexer.{ Lexer, Position, Token, TokenKind }
import effekt.lexer.TokenKind.*

import kiama.util.{ Positions, StringSource }

import munit.Location

class RecursiveDescentTests extends munit.FunSuite {

  def parser(input: String, positions: Positions)(using munit.Location): RecursiveDescentParsers = {
    val lexer = effekt.lexer.Lexer(input)
    val (tokens, error) = lexer.run()
    if (error.nonEmpty) fail(s"Lexer errors: ${error}")
    new RecursiveDescentParsers(positions, tokens)
  }

  def parse[R](input: String, f: RecursiveDescentParsers => R, positions: Positions = new Positions())(using munit.Location): R =
    try {
      val p = parser(input, positions)
      val result = f(p)
      assert(p.peek(TokenKind.EOF), "Did not consume everything")
      result
    } catch {
      case ParseError2(msg, pos) =>
        fail(s"Unexpected parse error (token index ${pos}): ${msg}")
    }

  def parseExpr(input: String, positions: Positions = new Positions())(using munit.Location): Term =
    parse(input, _.expr())

  def parseStmt(input: String, positions: Positions = new Positions())(using munit.Location): Stmt =
    parse(input, _.stmt())

  def parseStmts(input: String, positions: Positions = new Positions())(using munit.Location): Stmt =
    parse(input, _.stmts())

  test("Simple expressions") {
    parseExpr("42")
    parseExpr("f")
    parseExpr("f(a)")
    parseExpr("f(a, 42)")

    assertNotEquals(
      parseExpr("f.m(a, 42)"),
      parseExpr("(f.m)(a, 42)"))

    assertEquals(
      parseExpr("f(a, 42)()"),
      parseExpr("(f(a, 42))()"))
  }

  test("Dangling else") {
    assertEquals(
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
  }
}
