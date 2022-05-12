package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class JavaScriptTests extends AnyFunSpec with TestUtils {

  runTestsIn(examplesDir)

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory && !ignored.contains(f) => runTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        if (ignored.contains(f)) {
          ignore(f.getName) { () }
        } else it(f.getName) {
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

  lazy val ignored: List[File] = List(
    examplesDir / "neg" / "lambdas",
    examplesDir / "neg" / "issue50.effekt"
  )

  // The sources of all testfiles are stored here:
  lazy val examplesDir = new File("examples")

  //  val compiler = new effekt.Driver {}
  //  val configs = compiler.createConfig(Seq("--lib", "libraries/js/monadic"))
  //  configs.verify()
  //  compiler.compileFile("test.effekt", configs)
  //  effekt.util.Task.dump()

  def interpret(file: File): String = {
    // this resets the caches before each test:
    // effekt.util.Task.reset()
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "libraries/js/monadic"))
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
      case f if f.isDirectory && !ignored.contains(f) => generateCheckFilesIn(f, regenerateAll)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")
        val checkfile = path / (baseName + ".check")

        val isIgnored = ignored.contains(f)
        val shouldGenerate = regenerateAll || f.lastModified() > checkfile.lastModified()
        if (!isIgnored && shouldGenerate) {
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
