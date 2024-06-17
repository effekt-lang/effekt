package effekt

import effekt.source.*
import effekt.symbols.*

import effekt.lexer.{ Lexer, Token, TokenKind }
import effekt.lexer.TokenKind.*

import kiama.util.{ Positions, StringSource }
import kiama.parsing. { ParseResult, Input, Failure, Success, Error => ParseError }

import munit.Location

class ParserTests extends munit.FunSuite {

  /* def parseExpr(input: String): ParseResult[Term] =
    val source = StringSource(input)
    val lexer = Lexer(input)
    val (tokens, error) = lexer.run()
    println(tokens)

    if (error.nonEmpty) fail(s"Lexer errors: ${error}")

    val positions = Positions()
    val p = new EffektParsers(positions)
    p.parseAll(p.expr, TokenInput(tokens, 0, source, { case Token(s, _, _) => s }))

  def assertSuccessExpr(input: String, expected: Term)(using Location): Unit =
    parseExpr(input) match {
      case Success(result, next) => println(result); assertEquals(result, expected)
      case failure: Failure => fail(s"Expected ${expected}, but got error: ${failure.message}")
      case error: ParseError => fail(s"Expected ${expected}, but got error: ${error.message}")
    }

  def assertSuccessExpr(input: String)(using Location): Unit =
    parseExpr(input) match {
      case Success(result, next) => println(result)
      case failure: Failure => fail(s"Expected expression, but got error: ${failure.message}")
      case error: ParseError => fail(s"Expected expression, but got error: ${error.message}")
    }

  def assertFailureExpr(input: String)(using Location): Unit =
    parseExpr(input) match {
      case Success(result, next) => fail(s"Expected error, but got: ${result}")
      case failure: Failure => ()
      case error: ParseError => ()
    }

  test("simple parsing") {
    import Token.*

    assertSuccessExpr("42", Literal(42, builtins.TInt))
    assertSuccessExpr("true", Literal(true, builtins.TBoolean))
    assertSuccessExpr("1 + 1")
    assertFailureExpr("1 +- 1")
    assertSuccessExpr("box { () => def f(x: Int): Int = x; val x = y + 1; someFunction(x) }")
  } */
}