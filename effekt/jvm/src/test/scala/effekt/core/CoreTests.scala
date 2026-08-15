package effekt.core
import effekt.{Phase, symbols}
import munit.Location
import kiama.parsing.{NoSuccess, Success}
import effekt.PhaseResult.CoreTransformed
import effekt.source.Span
import java.io.File
import java.nio.file.Files

sealed trait CoreIRPass { def header: String }
final case class CoreIRTransform(header: String, run: ModuleDecl => ModuleDecl) extends CoreIRPass
final case class CoreIRAnalysis(header: String, run: ModuleDecl => String) extends CoreIRPass

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
        |${effekt.core.ReparsablePrettyPrinter.format(obtained).layout}
        |
        |Expected:
        |---------
        |${effekt.core.ReparsablePrettyPrinter.format(expected).layout}
        |
        |""".stripMargin
    })

  def shouldBeEqual(obtained: Stmt, expected: Stmt, clue: => Any)(using Location) =
    assertEquals(obtained, expected, {
      s"""${clue}
        |=====================
        |Got:
        |----
        |${effekt.core.ReparsablePrettyPrinter.format(obtained)}
        |
        |Expected:
        |---------
        |${effekt.core.ReparsablePrettyPrinter.format(expected)}
        |
        |""".stripMargin
    })

  def assertAlphaEquivalent(obtained: ModuleDecl,
                            expected: ModuleDecl,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = Names(defaultNames))(using Location): Unit = {
    val renamer = TestRenamer(names, preserveUserAnnotatedPrefix=false)
    val obtainedRenamed = renamer(obtained)
    val expectedRenamed = renamer(expected)
    val obtainedPrinted = effekt.core.ReparsablePrettyPrinter.format(obtainedRenamed).layout
    val expectedPrinted = effekt.core.ReparsablePrettyPrinter.format(expectedRenamed).layout
    assertEquals(obtainedPrinted, expectedPrinted)
  }
  def assertAlphaEquivalentStatements(obtained: Stmt,
                            expected: Stmt,
                            clue: => Any = "values are not alpha-equivalent",
                            names: Names = Names(defaultNames))(using Location): Unit = {
    val renamer = TestRenamer(names, preserveUserAnnotatedPrefix=false)
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

  /** Runs ordered analysis and transformation expectations embedded in Core `.ir` files. */
  protected final def registerCoreIRTests(directory: File, passes: CoreIRPass*): Unit = {
    val byHeader = passes.map(pass => pass.header -> pass).toMap
    val separator = """(?m)^///\s*(.+)$""".r

    directory.listFiles()
      .filter(_.getName.endsWith(".ir"))
      .sortBy(_.getName)
      .foreach { file =>
        test(file.getName) {
          val content = Files.readString(file.toPath)
          val sections = separator.split(content).toList.map(_.trim)
          val headers = separator.findAllMatchIn(content).map(_.group(1).trim).toList

          assert(headers.nonEmpty, "fixture has no /// steps")
          assertEquals(sections.size, headers.size + 1)

          val defaultModule = s"core/tests/${file.getName.stripSuffix(".ir")}"
          def asModule(source: String, path: String): String =
            if source.trim.startsWith("module ") then source else s"module $path\n\n$source"

          var current = parse(asModule(sections.head, defaultModule), "initial Core IR")
          headers.zip(sections.tail).zipWithIndex.foreach { case ((header, expected), index) =>
            val clue = s"${file.getName} step ${index + 1} ($header)"
            byHeader.getOrElse(header, fail(s"Unknown Core IR test step: '$header'")) match {
              case CoreIRAnalysis(_, run) =>
                assertNoDiff(run(current).trim, expected.trim, clue)

              case CoreIRTransform(_, run) =>
                val obtained = run(current)
                val expectedTree = parse(asModule(expected, current.path), s"expected after $header")
                assertAlphaEquivalent(obtained, expectedTree, clue)
                current = obtained
            }
          }
        }
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
  protected val theSourceModuleDecl: effekt.source.ModuleDecl = effekt.source.ModuleDecl("(core test)", Nil, Nil, None, Span.missing(theSource)) // FIXME sentinel value

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
