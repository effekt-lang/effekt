package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class JavaScriptTests extends EffektTests {

  override lazy val ignored: List[File] = List(
    examplesDir / "llvm"
  )

  def runTestFor(f: File, expected: String) =
    it(f.getName) {
      val out = interpretJS(f)
      assert(expected == out)
    }

  def interpretJS(file: File): String = {
    // this resets the caches before each test:
    // effekt.util.Task.reset()
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "libraries/js/monadic"))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    removeAnsiColors(configs.stringEmitter.result())
  }
}

object TestUtils extends JavaScriptTests {

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
          val out = interpretJS(f)
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
