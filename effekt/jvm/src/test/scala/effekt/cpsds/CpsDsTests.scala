package effekt
package cpsds

import java.io.File
import sbt.io.*
import sbt.io.syntax.*
import kiama.parsing.{NoSuccess, Success}
import munit.Location
import effekt.core.{Id, Names}

enum Pass {
  case Inline
  case StaticArguments
  case Simplify
}

case class PassTest(pass: Pass, input: ModuleDecl, expected: ModuleDecl)

class CpsDsTests extends munit.FunSuite {

  def examplesDir = new File("examples") / "cps"

  def parse(input: String, nickname: String = "input")(using Location): ModuleDecl = {
    Parser.module(input) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) =>
        fail(s"Parsing $nickname had trailing garbage: ${next.source.toString.substring(next.offset)}")
      case err: NoSuccess =>
        val pos = err.next.position
        fail(s"Parsing $nickname failed\n[${pos.line}:${pos.column}] ${err.message}")
    }
  }

  def parsePass(name: String)(using Location): Pass = name.trim match {
    case "INLINE" => Pass.Inline
    case "STATIC_ARGUMENTS" => Pass.StaticArguments
    case "SIMPLIFY" => Pass.Simplify
    case other => fail(s"Unknown pass: '$other'")
  }

  def parseTestFile(content: String)(using Location): List[PassTest] = {
    val separator = """(?m)^///\s*(.+)$""".r

    val parts = separator.split(content).toList.map(_.trim)
    val passNames = separator.findAllMatchIn(content).map(_.group(1)).toList

    if parts.isEmpty then fail("Test file is empty")
    if passNames.isEmpty then fail("Test file has no /// separators")
    if parts.size != passNames.size + 1 then
      fail(s"Expected ${passNames.size + 1} sections but found ${parts.size}")

    val initial = parse(parts.head, "initial IR")
    val tests = List.newBuilder[PassTest]

    var currentInput = initial
    passNames.zip(parts.tail).zipWithIndex.foreach { case ((passName, expectedSource), idx) =>
      val pass = parsePass(passName)
      val expected = parse(expectedSource, s"expected after step ${idx + 1} ($passName)")
      tests += PassTest(pass, currentInput, expected)
      currentInput = expected
    }

    tests.result()
  }

  def runPass(pass: Pass, input: ModuleDecl): ModuleDecl = pass match {
    case Pass.Inline =>
      val mainId = input.definitions.collectFirst {
        case ToplevelDefinition.Def(id, _, _) if id.name.name == "main" => id
      }.getOrElse {
        input.definitions.head match {
          case ToplevelDefinition.Def(id, _, _) => id
          case ToplevelDefinition.Val(id, _, _, _) => id
          case ToplevelDefinition.Let(id, _) => id
        }
      }
      Inliner.transform(mainId, input)
    case Pass.StaticArguments =>
      StaticArguments.transform(input)
    case Pass.Simplify =>
      Simplifier.transform(input)
  }

  def assertAlphaEquivalent(obtained: ModuleDecl, expected: ModuleDecl, clue: => Any)(using Location): Unit = {
    val names = Names(Map.empty)
    val obtainedRenamed = TestRenamer(names)(obtained)
    val expectedRenamed = TestRenamer(names)(expected)
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

    val tests = parseTestFile(content)

    tests.zipWithIndex.foreach { case (PassTest(pass, input, expected), idx) =>
      test(s"$filename step ${idx + 1}: $pass") {
        val obtained = runPass(pass, input)
        assertAlphaEquivalent(obtained, expected,
          s"$filename step ${idx + 1} ($pass) produced unexpected result")
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
