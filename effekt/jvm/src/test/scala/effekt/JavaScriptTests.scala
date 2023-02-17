package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  override def included: List[File] = List(
    examplesDir / "pos",
    examplesDir / "neg",
    examplesDir / "casestudies",
    examplesDir / "benchmarks"
  )

  override def ignored: List[File] = List(
    // we deprecated locally defined type and effect declarations, for now.
    examplesDir / "neg" / "existential_effect_leaks.effekt",
    examplesDir / "neg" / "scoped.effekt",
  )

  def runTestFor(input: File, check: File, expected: String): Unit =
    test(input.getPath) {
      val out = interpretJS(input)
      assertNoDiff(out, expected, s"Output running '${input.getPath}' differed from check file '${check.getPath}'.")
    }

  def interpretJS(file: File): String = {
    // Reset the caches before each test. This is necessary to
    // avoid dependencies compiled with one compiler instance to be reused when
    // compiled with another compiler instance / config / backend.
    //
    // It does however slow down the tests, since the standard library has to be
    // compiled each and every time.
    effekt.util.Task.reset()
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--backend", "js",
      "--Koutput", "string",
      "--out", output.getPath))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result()
  }

  def canRun() = JSRunner.checkSetup().isRight
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

          // Save checkfile in source folder (e.g. examples/)
          // We remove ansi colors to make check files human-readable.
          IO.write(checkfile, removeAnsiColors(out))
        }
      case _ => ()
    }
  }

  def generateCheckFiles(regenerateAll: Boolean = false): Unit = {
    generateCheckFilesIn(examplesDir, regenerateAll)
  }

  def removeAnsiColors(text: String): String = text.replaceAll("\u001B\\[[;\\d]*m", "")
}
