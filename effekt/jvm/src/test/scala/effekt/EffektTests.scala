package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._
import scala.sys.process.*

import scala.language.implicitConversions

trait EffektTests extends munit.FunSuite {

  def output: File = new File(".") / "out" / "tests" / getClass.getName.toLowerCase

  // The sources of all testfiles are stored here:
  def examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  def ignored: List[File] = List()

  // Folders to discover and run tests in
  def included: List[File] = List()

  def runTestFor(input: File, check: File, expectedResult: String): Unit

  def canRun(): Boolean

  def runTests() =
    if canRun() then
      included.foreach(runPositiveTestsIn)
    else
      test(s"${this.getClass.getName}: Binary not found!".ignore) { () }

  def runPositiveTestsIn(dir: File): Unit = //describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory && !ignored.contains(f) =>
        runPositiveTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        if (ignored.contains(f)) {
          test(f.getName.ignore) { () }
        } else {
          val contents = IO.read(checkfile)
          runTestFor(f, checkfile, contents)
        }

      case _ => ()
    }

  // utils to check whether a command is available
  def canRunExecutable(command: String*): Boolean =
    try { Process(command).run(ProcessIO(out => (), in => (), err => ())).exitValue() == 0 } catch { _ => false }

  runTests()
}
