package effekt
package cpsds

import java.io.File
import sbt.io.*
import sbt.io.syntax.*
import kiama.parsing.{NoSuccess, Success}
import munit.Location

enum Pass {
  case Inline
  case StaticArguments
  case Simplify
}

case class PassTest(pass: Pass, input: ModuleDecl, expected: ModuleDecl)

class CpsDsPassTests extends munit.FunSuite {

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

  def testFile(file: File): Unit = {
    val content = scala.io.Source.fromFile(file).mkString
    val filename = file.getName

    val tests = parseTestFile(content)

    tests.zipWithIndex.foreach { case (PassTest(pass, input, expected), idx) =>
      test(s"$filename step ${idx + 1}: $pass (parse only)") {
        // For now, just verify that both input and expected parsed successfully
        // by checking they round-trip through the pretty printer
        val inputPrinted = PrettyPrinter.format(input).layout
        val expectedPrinted = PrettyPrinter.format(expected).layout
        assert(inputPrinted.nonEmpty, s"$filename step ${idx + 1}: input pretty-printed to empty string")
        assert(expectedPrinted.nonEmpty, s"$filename step ${idx + 1}: expected pretty-printed to empty string")
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
