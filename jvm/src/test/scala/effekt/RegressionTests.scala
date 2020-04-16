package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class RegressionTests extends AnyFunSpec {

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory => runTestsIn(f)
      case f if f.getName.endsWith(".effekt") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        it(f.getName) {
          val out = interpret(f.getPath)

          if (checkfile.exists()) {
            assert(IO.read(checkfile).toString == out)
          }
        }
      case _ => ()
    }
  }

  runTestsIn(new File("examples"))

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
  def generateCheckFilesIn(dir: File): Unit = {
    println(s"Generating check files in folder: ${dir.getPath}")
    dir.listFiles.foreach {
      case f if f.isDirectory => generateCheckFilesIn(f)
      case f if f.getName.endsWith(".effekt") =>
        println(s"Writing checkfile for ${f}")
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".effekt")
        val checkfile = path / (baseName + ".check")

        val out = interpret(f.getPath)
        IO.write(checkfile, out)
      case _ => ()
    }
  }

  def generateCheckFiles(): Unit = generateCheckFilesIn(new File("examples"))

}
