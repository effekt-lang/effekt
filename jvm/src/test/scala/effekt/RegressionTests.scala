package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class RegressionTests extends AnyFunSpec with TestUtils {

  runTestsIn(examplesDir)

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory => runTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        it(f.getName) {
          val out = interpret(f)

          if (checkfile.exists()) {
            assert(IO.read(checkfile).toString == out)
          }
        }
      case _ => ()
    }
  }
}

trait TestUtils {

  // The sources of all testfiles are stored here:
  lazy val examplesDir = new File("examples")

  val compiler = new effekt.Driver {}
  val configs = compiler.createConfig(Seq("--lib", "lib"))
  configs.verify()
  compiler.compileFile("examples/features/adt.md", configs)

  def interpret(file: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "lib"))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }

  /**
   * Generates the check files from the actual outputs.
   *
   * Call from sbt with:
   *    > project effektJVM
   *    > test:console
   *    scala> effekt.TestUtils.generateCheckFiles()
   *
   * Check afterwards with:
   *    git diff
   */
  def generateCheckFilesIn(dir: File, regenerateAll: Boolean): Unit = {
    dir.listFiles.foreach {
      case f if f.isDirectory => generateCheckFilesIn(f, regenerateAll)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")
        val checkfile = path / (baseName + ".check")

        if (regenerateAll || f.lastModified() > checkfile.lastModified()) {
          println(s"Writing checkfile for ${f}")
          val out = interpret(f)
          // save checkfile in source folder (e.g. examples/)
          IO.write(checkfile, out)
        }
      case _ => ()
    }
  }

  def generateCheckFiles(regenerateAll: Boolean = false): Unit = {
    generateCheckFilesIn(examplesDir, regenerateAll)
  }
}
object TestUtils extends TestUtils
