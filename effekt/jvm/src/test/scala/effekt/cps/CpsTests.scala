package effekt
package cps

import java.io.File
import sbt.io.*
import sbt.io.syntax.*
import kiama.parsing.{ NoSuccess, Success }
import munit.Location
import effekt.context.Context
import effekt.core.{ DeclarationContext, Id, Names, TestContext }
import effekt.generator.{ chez, js }

/**
 * Golden tests for the backends, starting from CPS programs in `examples/cps/`.
 *
 * Each file consists of an initial program, followed by sections `/// BACKEND`
 * with the expected output of that backend.
 */
enum Backend(val header: String, val run: ModuleDecl => String) {
  case JavaScript extends Backend("JAVASCRIPT", input => {
      given Context = new TestContext
      given DeclarationContext = new DeclarationContext(input.declarations, Nil)
      js.TransformerCps.resetNames()
      val generated = js.TransformerCps.toJS(input, Nil)
      js.PrettyPrinter.format(generated.stmts).layout.linesIterator.map(_.stripTrailing).mkString("\n")
    })
  case Chez extends Backend("CHEZ", input => {
      given Context = new TestContext
      chez.PrettyPrinter.format(input.definitions.map(chez.TransformerCPS.toChez)).layout
        .linesIterator.map(_.stripTrailing).mkString("\n")
    })
}

class CpsTests extends munit.FunSuite {

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
    "Triple" -> Id("Triple", -10),
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

  private val allBackends: Map[String, Backend] =
    Backend.values.map(b => b.header -> b).toMap

  def splitTestFile(content: String)(using Location): (String, List[(Backend, String)]) = {
    val separator = """(?m)^///\s*(.+)$""".r

    val parts = separator.split(content).toList.map(_.trim)
    val stepNames = separator.findAllMatchIn(content).map(_.group(1).trim).toList

    if parts.isEmpty then fail("Test file is empty")
    if stepNames.isEmpty then fail("Test file has no /// separators")
    if parts.size != stepNames.size + 1 then
      fail(s"Expected ${stepNames.size + 1} sections but found ${parts.size}")

    val steps = stepNames.zip(parts.tail).map { case (name, body) =>
      (allBackends.getOrElse(name, fail(s"Unknown step: '$name'")), body)
    }
    (parts.head, steps)
  }

  def testFile(file: File): Unit = {
    val content = scala.io.Source.fromFile(file).mkString
    val filename = file.getName

    val (initialSource, steps) = splitTestFile(content)

    steps.foreach { case (backend, expectedOutput) =>
      test(s"$filename: ${backend.header}") {
        val input = new TestRenamer()(parse(initialSource, "initial IR"))
        assertNoDiff(backend.run(input).trim, expectedOutput.trim,
          s"$filename (${backend.header}) produced unexpected output")
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
