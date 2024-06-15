package effekt.core
import effekt.{symbols, Phase}
import munit.Location
import kiama.parsing.{NoSuccess, Success}
import effekt.PhaseResult.CoreTransformed

/** Base class for tests of [[core]]-related stuff.
 * Provides helpers to parse inputs and test for alpha-equivalence(*),
 * plus a given [[TestContext]] that should work for simple cases (at least).
 */
trait CoreTests extends munit.FunSuite {

  protected def defaultNames = symbols.builtins.rootTypes ++ symbols.builtins.rootTerms ++ symbols.builtins.rootCaptures

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
    val renamer = Renamer(names, "$")
    shouldBeEqual(renamer(obtained), renamer(expected), clue)
  }
  def assertAlphaEquivalentStatements(obtained: Stmt,
                            expected: Stmt,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = Names(defaultNames))(using Location): Unit = {
    val renamer = Renamer(names, "$")
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

  protected given testContext: TestContext = new TestContext
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
/** [[CoreTransformationTests]] for the common case of testing a specific [[Phase]]. */
trait CorePhaseTests[P <: Phase[CoreTransformed, CoreTransformed]](phase: P) extends CoreTransformationTests {

  protected val theSource: kiama.util.Source = new kiama.util.Source {
    override def name: String = "(core test)"
    override def content: String =
      throw NotImplementedError("The original Effekt source is not available in core tests.")
  }
  protected val theSourceModuleDecl: effekt.source.ModuleDecl = effekt.source.ModuleDecl("(core test)", Nil, Nil) // FIXME sentinel value

  // TODO Maybe fix this up so we can do findPrelude. Cmp. PolymorphismBoxingTests for a hacky way to do this
  protected val theSourceModule: effekt.symbols.Module = effekt.symbols.Module(theSourceModuleDecl, theSource)

  override def transform(input: ModuleDecl): ModuleDecl = {
    testContext.in {
      testContext.module = theSourceModule
      // (source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.core.ModuleDecl)
      phase.run(CoreTransformed(theSource, theSourceModuleDecl, theSourceModule, input))(using testContext) match {
        case Some(CoreTransformed(source, tree, mod, core)) => core
        case None => fail(s"Phase ${phase.phaseName} failed on test input")
      }
    }
  }
}
