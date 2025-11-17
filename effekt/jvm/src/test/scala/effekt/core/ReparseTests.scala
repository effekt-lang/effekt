package effekt.core

import effekt.*
import effekt.PhaseResult.CoreTransformed
import effekt.context.{Context, IOModuleDB}
import effekt.util.PlainMessaging
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{Source, StringSource}
import munit.Location
import sbt.io.*
import sbt.io.syntax.*

import java.io.File

/*
// * This test suite ensures that the core pretty-printer always produces reparsable code.
 */
class ReparseTests extends CoreTests {
  object plainMessaging extends PlainMessaging
  object context extends Context with IOModuleDB {
    val messaging = plainMessaging

    object frontend extends CompileToCore

    override lazy val compiler = frontend.asInstanceOf
  }

  // The sources of all test files are stored here:
  def examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  def ignored: Set[File] = Set(
    // Missing include: text/pregexp.scm
    File("examples/pos/simpleparser.effekt"),
    // Cannot find source for unsafe/cont
    File("examples/pos/propagators.effekt"),
    // Bidirectional effects that mention the same effect recursively are not (yet) supported.
    File("examples/pos/bidirectional/selfrecursion.effekt")
  )

  def positives: Set[File] = Set(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "benchmarks",
  )

  def runTests() = positives.foreach(runPositiveTestsIn)

  def runPositiveTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      f => if (!ignored.contains(f)) {
        test(s"${f.getPath}") {
          toCoreThenReparse(f)
        }
      }
    }

  def toCoreThenReparse(input: File) = {
    val content = IO.read(input)
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)
    val (_, _, coreMod: ModuleDecl) = context.frontend.compile(StringSource(content, "input.effekt"))(using context).map {
      case (_, decl) => decl
    }.getOrElse {
      val errors = plainMessaging.formatMessages(context.messaging.buffer)
      sys error errors
    }
    val printed = core.PrettyPrinter.format(coreMod).layout
    val reparsed: ModuleDecl = parse(printed)(using Location.empty)
    val renamer = TestRenamer(Names(defaultNames))
    val lhs = renamer(reparsed)
    val rhs = renamer(coreMod)
    val lhsPrinted = core.PrettyPrinter.format(lhs).layout
    val rhsPrinted = core.PrettyPrinter.format(rhs).layout
    assertEquals(lhsPrinted, rhsPrinted)
  }

  def foreachFileIn(file: File)(test: File => Unit): Unit =
    file match {
      case f if f.isDirectory =>
        f.listFiles.foreach(foreachFileIn(_)(test))
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".effekt.md") =>
        test(f)
      case _ => ()
    }

  runTests()
}

/**
 * A "backend" that simply outputs the core module for a given Effekt source program.
 */
class CompileToCore extends Compiler[(Id, symbols.Module, ModuleDecl)] {
  def extension = ".effekt-core.ir"

  // Support all the feature flags so that we can test all extern declarations
  override def supportedFeatureFlags: List[String] = List("vm", "js", "jsNode", "chez", "llvm")

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = None

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = None

  override def compile(source: Source)(using C: Context): Option[(Map[String, String], (Id, symbols.Module, ModuleDecl))] =
    Optimized.run(source).map { res => (Map.empty, res) }

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.ensureMainExists(mod)
      (mainSymbol, mod, core)
  }
}
