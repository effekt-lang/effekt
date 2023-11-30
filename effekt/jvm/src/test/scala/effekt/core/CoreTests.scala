package effekt.core
import effekt.symbols
import munit.Location
import kiama.parsing.{NoSuccess, Success}

trait CoreTests extends munit.FunSuite {

  protected val defaultNames = new Names(symbols.builtins.rootTypes ++ symbols.builtins.rootTerms ++ symbols.builtins.rootCaptures)

  def assertAlphaEquivalent(obtained: ModuleDecl,
                            expected: ModuleDecl,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = defaultNames)(using Location): Unit = {
    val renamer = Renamer(names)
    assertEquals(renamer(obtained), renamer(expected), clue=clue)
  }
  def parse(input: String,
            nickname: String = "input",
            names: Names = defaultNames): ModuleDecl = {
    CoreParsers.module(input, names) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) => fail(s"Parsing ${nickname} had trailing garbage: " +
        s"${next.source.toString.substring(next.offset)}")
      case err: NoSuccess => fail(s"Parsing ${nickname} failed: ${err.message}")
    }
  }

  protected given testContext: TestContext = new TestContext
}

trait CoreTransformationTests extends CoreTests {
  def transform(input: ModuleDecl): ModuleDecl

  def assertTransformsTo(input: String, expected: String,
                         clue: => Any = "transformation result is not the expected one",
                         names: Names = defaultNames)(using Location): Unit = {
    val pInput = parse(input, "input", names = names)
    val pExpected = parse(expected, "expected result", names = names)
    val obtained = transform(pInput)
    assertAlphaEquivalent(obtained, pExpected, clue, names = names)
  }
}