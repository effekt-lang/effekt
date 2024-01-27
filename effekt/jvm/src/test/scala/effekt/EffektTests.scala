package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._
import scala.sys.process.*

import scala.language.implicitConversions

trait EffektTests extends munit.FunSuite {

  // The name of the backend as it is passed to the --backend flag.
  def backendName: String

  def output: File = new File(".") / "out" / "tests" / getClass.getName.toLowerCase

  // The sources of all testfiles are stored here:
  def examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  def ignored: List[File] = List()

  // Folders to discover and run tests in
  def positives: List[File] = List()

  def negatives: List[File] = List()

  def runTestFor(input: File, expected: String): Unit =
    test(input.getPath + s" (${backendName})") {
      assertNoDiff(run(input), expected)
    }

  def run(input: File): String =
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", backendName,
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(input.getPath, configs)
    configs.stringEmitter.result()


  def runTests() =
    Backend.backend(backendName).runner.checkSetup() match {
      case Left(msg) => test(s"${this.getClass.getName}: ${msg}".ignore) { () }
      case Right(value) =>
        negatives.foreach(runPositiveTestsIn)
        positives.foreach(runPositiveTestsIn)
    }

  def runPositiveTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, None) => sys error s"Missing checkfile for ${f.getPath}"
      case (f, Some(expected)) => runTestFor(f, expected)
    }

  def foreachFileIn(dir: File)(test: (File, Option[String]) => Unit): Unit = //describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory && !ignored.contains(f) =>
        runPositiveTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".effekt.md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        if (ignored.contains(f)) {
          ignored.contains(f)
        } else {
          val checkfile = path / (baseName + ".check")
          val expected = if checkfile.exists() then Some(IO.read(checkfile)) else None

          test(f, expected)
        }
      case _ => ()
    }

  runTests()
}
