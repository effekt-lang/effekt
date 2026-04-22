package effekt
package cpsds

import java.io.File
import sbt.io.*
import sbt.io.syntax.*
import kiama.parsing.{NoSuccess, Success}
import munit.Location
import effekt.core.{Id, Names}

enum Step {
  case Transform(pass: TransformPass, expectedSource: String)
  case Analyze(analysis: AnalysisPass, expectedOutput: String)
}

enum TransformPass {
  case Inline
  case StaticArguments
  case Simplify
  case SinkBlocks
  case DropParameters
}

enum AnalysisPass {
  case Flows
}

class CpsDsTests extends munit.FunSuite {

  def examplesDir = new File("examples") / "cps"

  val defaultNames = Map(
    "main" -> Id("main", -1),
    "add"  -> Id("add", -2),
    "sub"  -> Id("sub", -3),
    "eq"  -> Id("eq", -4),
    "println"  -> Id("println", -5)
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

  def parseStepHeader(name: String)(using Location): Either[TransformPass, AnalysisPass] = name.trim match {
    case "INLINE"            => Left(TransformPass.Inline)
    case "STATIC_ARGUMENTS"  => Left(TransformPass.StaticArguments)
    case "SIMPLIFY"          => Left(TransformPass.Simplify)
    case "SINK_BLOCKS"       => Left(TransformPass.SinkBlocks)
    case "DROP_PARAMETERS"   => Left(TransformPass.DropParameters)
    case "FLOWS"             => Right(AnalysisPass.Flows)
    case other               => fail(s"Unknown step: '$other'")
  }

  def splitTestFile(content: String)(using Location): (String, List[Step]) = {
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
        case Left(pass)     => Step.Transform(pass, body)
        case Right(analysis) => Step.Analyze(analysis, body)
      }
    }

    (initialSource, steps)
  }

  def runTransform(pass: TransformPass, input: ModuleDecl): ModuleDecl = pass match {
    case TransformPass.Inline =>
      val mainId = findMain(input)
      Inliner.transform(mainId, input)
    case TransformPass.StaticArguments =>
      StaticArguments.transform(input)
    case TransformPass.Simplify =>
      Simplifier.transform(input)
    case TransformPass.SinkBlocks =>
      val mainId = findMain(input)
      BlockSinking.transform(input, mainId)
    case TransformPass.DropParameters =>
      ???
  }

  def runAnalysis(analysis: AnalysisPass, input: ModuleDecl): String = analysis match {
    case AnalysisPass.Flows =>
      input.flows.show
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

    val (initialSource, steps) = splitTestFile(content)

    // Track the current program across steps. Transformations update it;
    // analyses leave it unchanged. We use a var that is captured by each
    // test closure — munit runs tests sequentially within a suite, so the
    // ordering is deterministic.
    var currentTree: ModuleDecl = null
    var currentRenamer: TestRenamer = null

    test(s"$filename: parse initial program") {
      currentRenamer = new TestRenamer()
      currentTree = currentRenamer(parse(initialSource, "initial IR"))
    }

    steps.zipWithIndex.foreach { case (step, idx) =>
      step match {
        case Step.Transform(pass, expectedSource) =>
          test(s"$filename step ${idx + 1}: $pass") {
            assert(currentTree != null, "previous step must have succeeded")
            val expected = currentRenamer(parse(expectedSource, s"expected after step ${idx + 1} ($pass)"))
            val obtained = runTransform(pass, currentTree)
            assertAlphaEquivalent(obtained, expected,
              s"$filename step ${idx + 1} ($pass) produced unexpected result")
            // Update current tree for subsequent steps
            currentTree = expected
          }

        case Step.Analyze(analysis, expectedOutput) =>
          test(s"$filename step ${idx + 1}: $analysis") {
            assert(currentTree != null, "previous step must have succeeded")
            val obtained = runAnalysis(analysis, currentTree)
            assertEquals(obtained.trim, expectedOutput.trim,
              s"$filename step ${idx + 1} ($analysis) produced unexpected output")
            // Analysis does NOT update currentTree
          }
      }
    }
  }

  // Discover and register all .ir files in examples/cps/
  if examplesDir.exists() && examplesDir.isDirectory then
    examplesDir.listFiles()
      .filter(_.getName.endsWith(".ir"))
      .sorted
      .foreach(testFile)
  else
    test("examples/cps directory not found".ignore) { () }
}
