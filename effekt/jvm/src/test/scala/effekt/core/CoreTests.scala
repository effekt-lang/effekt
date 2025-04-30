package effekt
package core
import munit.Location
import kiama.parsing.{ NoSuccess, Success }

/** Base class for tests of [[core]]-related stuff.
 * Provides helpers to parse inputs and test for alpha-equivalence(*),
 * plus a given [[TestContext]] that should work for simple cases (at least).
 */
trait CoreTests extends munit.FunSuite {

  protected def defaultNames: Map[String, _root_.effekt.symbols.Symbol] = symbols.builtins.rootTypes ++ symbols.builtins.rootCaptures

  def shouldBeEqual(obtained: ModuleDecl, expected: ModuleDecl, clue: => Any)(using Location) =
    assertEquals(obtained, expected, {
      s"""${clue}
        |=====================
        |Got:
        |----
        |${effekt.core.PrettyPrinter.format(obtained).layout}
        |
        |Expected:
        |---------
        |${effekt.core.PrettyPrinter.format(expected).layout}
        |
        |""".stripMargin
    })

  def shouldBeEqual(obtained: Stmt, expected: Stmt, clue: => Any)(using Location) =
    assertEquals(obtained, expected, {
      s"""${clue}
        |=====================
        |Got:
        |----
        |${effekt.core.PrettyPrinter.format(obtained)}
        |
        |Expected:
        |---------
        |${effekt.core.PrettyPrinter.format(expected)}
        |
        |""".stripMargin
    })

  def assertAlphaEquivalent(obtained: ModuleDecl,
                            expected: ModuleDecl,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = Names(defaultNames))(using Location): Unit = {
    val renamer = TestRenamer(names, "$")
    shouldBeEqual(renamer(obtained), renamer(expected), clue)
  }
  def assertAlphaEquivalentStatements(obtained: Stmt,
                            expected: Stmt,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = Names(defaultNames))(using Location): Unit = {
    val renamer = TestRenamer(names, "$")
    shouldBeEqual(renamer(obtained), renamer(expected), clue)
  }
  def parse(input: String,
            nickname: String = "input",
            names: Names = Names(defaultNames))(using Location): ModuleDecl = {
    CoreParsers.module(input, names) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) => fail(s"Parsing ${nickname} had trailing garbage: " +
        s"${next.source.toString.substring(next.offset)}")
      case err: NoSuccess =>
        val pos = err.next.position
        fail(s"Parsing ${nickname} failed\n[${pos.line}:${pos.column}] ${err.message}")
    }
  }

  def parseStatement(input: String,
            nickname: String = "input",
            names: Names = Names(defaultNames))(using Location): Stmt = {
    CoreParsers.statement(input, names) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) => fail(s"Parsing ${nickname} had trailing garbage: " +
        s"${next.source.toString.substring(next.offset)}")
      case err: NoSuccess =>
        val pos = err.next.position
        fail(s"Parsing ${nickname} failed\n[${pos.line}:${pos.column}] ${err.message}")
    }
  }
}

trait CoreTransformationTests extends CoreTests {
  def transform(input: ModuleDecl): ModuleDecl

  def assertTransformsTo(input: String, expected: String,
                         clue: => Any = "transformation result is not the expected one",
                         names: Names = Names(defaultNames))(using Location): Unit = {
    val pInput = parse(input, "input", names = names)
    val pExpected = parse(expected, "expected result", names = names)
    val obtained = transform(pInput)
    assertAlphaEquivalent(obtained, pExpected, clue, names = names)
  }
}
