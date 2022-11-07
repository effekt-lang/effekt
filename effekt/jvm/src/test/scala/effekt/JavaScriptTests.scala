package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  override lazy val ignored: List[File] = List(
    examplesDir / "llvm"
  )

  def runTestFor(input: File, check: File, expected: String): Unit =
    test(input.getPath) {
      val out = interpretJS(input)
      assertNoDiff(out, expected, s"Output running '${input.getPath}' differed from check file '${check.getPath}'.")
    }

  def interpretJS(file: File): String = {
    // this resets the caches before each test:
    // effekt.util.Task.reset()
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "libraries/js/monadic"))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result()
  }
}

object TestUtils {

  object jsTests extends JavaScriptTests
  import jsTests.*

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
