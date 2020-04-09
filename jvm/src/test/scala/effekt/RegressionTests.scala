package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class RegressionTests extends AnyFunSpec {

  val srcFolder = new File("examples")
  val posFiles = (srcFolder / "pos") ** "*.effekt"
  val negFiles = (srcFolder / "neg") ** "*.effekt"

  describe("Positive tests") {
    for (file <- posFiles.get) {
      val path = file.getParentFile
      val baseName = file.getName.stripSuffix(".effekt")
      val checkfile = path / (baseName + ".check")

      it(file.getPath) {
        val out = interpret(file.getPath)
        val expected = if (checkfile.exists()) { IO.read(checkfile).toString } else { "" }
        assert(expected == out)
      }
    }
  }

  describe("Negative tests") {
    for (file <- negFiles.get) {
      val path = file.getParentFile
      val baseName = file.getName.stripSuffix(".effekt")
      val checkfile = path / (baseName + ".check")

      it(file.getPath) {
        val out = interpret(file.getPath)

        if (checkfile.exists()) {
          val expected = IO.read(checkfile).toString
          assert(expected == out)
        } else {
          ??? // TODO check output for messages.. for now that would be good enough for
          // a neg test
        }
      }
    }
  }

  def interpret(filename: String): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "lib"))
    configs.verify()
    compiler.compileFile(filename, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }

  /**
   * Generates the check files from the actual outputs.
   *
   * Call from sbt with:
   *    > project effektJVM
   *    > test:console
   *    scala> new effekt.RegressionTests().generateCheckFiles()
   *
   * Check afterwards with:
   *    git diff
   */
  def generateCheckFiles(): Unit = {
    println("Generating check files by running the tests. This can take a while...\n")
    for (file <- (posFiles.get ++ negFiles.get)) {
      val path = file.getParentFile
      val baseName = file.getName.stripSuffix(".effekt")
      val checkfile = path / (baseName + ".check")

      val out = interpret(file.getPath)
      IO.write(checkfile, out)
    }
  }

}
