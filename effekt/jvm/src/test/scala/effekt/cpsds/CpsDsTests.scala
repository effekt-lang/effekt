package effekt
package cpsds

import java.io.File
import sbt.io.*
import sbt.io.syntax.*
import kiama.parsing.{ NoSuccess, Success }
import munit.Location
import effekt.core.{ Id, Names }

enum Step {
  /** Run a transform and check the result against expectedSource. */
  case Transform(pass: TransformPass, expectedSource: String)
  /** Run a transform without checking (empty body); updates the current tree. */
  case TransformOnly(pass: TransformPass)
  /** Run an analysis on the current tree, check against expectedOutput. */
  case Analyze(analysis: AnalysisPass, expectedOutput: String)
}

enum TransformPass(val header: String, val run: (String, ModuleDecl, Id) => ModuleDecl) {
  case Inline extends TransformPass("INLINE",
    (_, input, mainId) => Inliner.transform(mainId, input))
  case StaticArguments extends TransformPass("STATIC_ARGUMENTS",
    (_, input, _) => cpsds.StaticArguments.transform(input))
  case Simplify extends TransformPass("SIMPLIFY",
    (_, input, _) => Simplifier.transform(input))
  case SinkBlocks extends TransformPass("SINK_BLOCKS",
    (_, input, mainId) => BlockSinking.transform(input, mainId))
  case DropParameters extends TransformPass("DROP_PARAMETERS",
    (_, input, _) => ???)
}

enum AnalysisPass(val header: String, val run: (String, ModuleDecl, Id) => String) {
  case Flows extends AnalysisPass("FLOWS",
    (name, input, main) => {
      val flow = input.flows
      input.definitions.collect {
        case d: ToplevelDefinition.Def =>
          s"${d.id}\n---\n${flowAnalysis.solve(d, main).show}"
        case _ => ()
      }.mkString("\n")
    })
}

class CpsDsTests extends munit.FunSuite {

  def examplesDir = new File("examples") / "cps"

  val defaultNames = Map(
    "main" -> Id("main", -1),
    "add"  -> Id("add", -2),
    "sub"  -> Id("sub", -3),
    "eq"   -> Id("eq", -4),
    "println"  -> Id("println", -5),
    "show" -> Id("show", -6),
    "Nil" -> Id("Nil", -7),
    "Cons" -> Id("Cons", -8),
    "lt"   -> Id("lt", -9),
  )

  def parse(input: String, nickname: String = "input")(using Location): ModuleDecl = {
    Parser.module(input, Names(defaultNames)) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) =>
        fail(s"Parsing $nickname had trailing garbage: ${next.source.toString.substring(next.offset)}")
      case err: NoSuccess =>
        val pos = err.next.position
        fail(s"Parsing $nickname failed\n[${pos.line}:${pos.column}] ${err.message}")
    }
  }

  private val allTransforms: Map[String, TransformPass] =
    TransformPass.values.map(p => p.header -> p).toMap

  private val allAnalyses: Map[String, AnalysisPass] =
    AnalysisPass.values.map(a => a.header -> a).toMap

  def parseStepHeader(name: String)(using Location): Either[TransformPass, AnalysisPass] = {
    val trimmed = name.trim
    allTransforms.get(trimmed).map(Left(_))
      .orElse(allAnalyses.get(trimmed).map(Right(_)))
      .getOrElse(fail(s"Unknown step: '$trimmed'"))
  }

  def splitTestFile(content: String)(using Location): (String, List[Step], List[TransformPass]) = {
    val separator = """(?m)^///\s*(.+)$""".r

    val parts = separator.split(content).toList.map(_.trim)
    val stepNames = separator.findAllMatchIn(content).map(_.group(1)).toList

    if parts.isEmpty then fail("Test file is empty")
    if stepNames.isEmpty then fail("Test file has no /// separators")
    if parts.size != stepNames.size + 1 then
      fail(s"Expected ${stepNames.size + 1} sections but found ${parts.size}")

    val initialSource = parts.head
    val steps = stepNames.zip(parts.tail).map { case (name, body) =>
      parseStepHeader(name) match {
        case Left(pass) =>
          if body.isEmpty then Step.TransformOnly(pass)
          else Step.Transform(pass, body)
        case Right(analysis) => Step.Analyze(analysis, body)
      }
    }

    steps.lastOption match {
      case Some(Step.TransformOnly(_)) =>
        // Drop the trailing TransformOnly steps; we'll run them in a dedicated test
        val (checkedSteps, trailingRuns) = {
          val reversed = steps.reverse
          val trailing = reversed.takeWhile(_.isInstanceOf[Step.TransformOnly]).reverse
          val checked = reversed.dropWhile(_.isInstanceOf[Step.TransformOnly]).reverse
          (checked, trailing.collect { case Step.TransformOnly(p) => p })
        }
        (initialSource, checkedSteps, trailingRuns)
      case _ =>
        (initialSource, steps, Nil)
    }
  }

  private def findMain(input: ModuleDecl): Id =
    input.definitions.collectFirst {
      case ToplevelDefinition.Def(id, _, _) if id.name.name == "main" => id
    }.getOrElse {
      input.definitions.head match {
        case ToplevelDefinition.Def(id, _, _) => id
        case ToplevelDefinition.Val(id, _, _, _) => id
      }
    }

  def assertAlphaEquivalent(obtained: ModuleDecl, expected: ModuleDecl, clue: => Any)(using Location): Unit = {
    val renamer = new TestRenamer
    val obtainedRenamed = renamer(obtained)
    val expectedRenamed = renamer(expected)
    def obtainedStr = PrettyPrinter.format(obtainedRenamed).layout
    def expectedStr = PrettyPrinter.format(expectedRenamed).layout
    assertEquals(obtainedStr, expectedStr, {
      s"""$clue
         |=====================
         |Got:
         |----
         |$obtainedStr
         |
         |Expected:
         |---------
         |$expectedStr
         |""".stripMargin
    })
  }

  def testFile(file: File): Unit = {
    val content = scala.io.Source.fromFile(file).mkString
    val filename = file.getName

    val (initialSource, steps, trailingPasses) = splitTestFile(content)

    var currentTree: ModuleDecl = null
    var currentRenamer: TestRenamer = null

    test(s"$filename: parse initial program") {
      currentRenamer = new TestRenamer()
      currentTree = currentRenamer(parse(initialSource, "initial IR"))
    }

    steps.zipWithIndex.foreach { case (step, idx) =>
      step match {
        case Step.TransformOnly(pass) =>
          test(s"$filename step ${idx + 1}: ${pass.header} (unchecked)") {
            assert(currentTree != null, "previous step must have succeeded")
            val mainId = findMain(currentTree)
            currentTree = pass.run(filename, currentTree, mainId)
          }

        case Step.Transform(pass, expectedSource) =>
          test(s"$filename step ${idx + 1}: ${pass.header}") {
            assert(currentTree != null, "previous step must have succeeded")
            val expected = currentRenamer(parse(expectedSource, s"expected after step ${idx + 1} (${pass.header})"))
            val mainId = findMain(currentTree)
            val obtained = pass.run(filename, currentTree, mainId)
            assertAlphaEquivalent(obtained, expected,
              s"$filename step ${idx + 1} (${pass.header}) produced unexpected result")
            currentTree = expected
          }

        case Step.Analyze(analysis, expectedOutput) =>
          test(s"$filename step ${idx + 1}: ${analysis.header}") {
            assert(currentTree != null, "previous step must have succeeded")
            val mainId = findMain(currentTree)
            val obtained = analysis.run(filename, currentTree, mainId)
            assertNoDiff(obtained.trim, expectedOutput.trim,
              s"$filename step ${idx + 1} (${analysis.header}) produced unexpected output")
          }
      }
    }

    if trailingPasses.nonEmpty then {
      val pipelineDesc = trailingPasses.map(_.header).mkString(" → ")
      test(s"$filename: $pipelineDesc (no expected output yet)") {
        assert(currentTree != null, "previous step must have succeeded")
        for pass <- trailingPasses do {
          val mainId = findMain(currentTree)
          currentTree = pass.run(filename, currentTree, mainId)
        }
        val result = PrettyPrinter.format(currentTree).layout
        fail(s"No expected output after $pipelineDesc. Got:\n\n$result")
      }
    }
  }

  if examplesDir.exists() && examplesDir.isDirectory then
    examplesDir.listFiles()
      .filter(_.getName.endsWith(".ir"))
      .sorted
      .foreach(testFile)
  else
    test("examples/cps directory not found".ignore) { () }
}
