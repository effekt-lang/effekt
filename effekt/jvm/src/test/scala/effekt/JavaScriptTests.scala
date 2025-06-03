package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  def backendName = "js"

  override def positives: Set[File] = Set(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "benchmarks",
  )

  override def negatives: Set[File] = Set(
    examplesDir / "neg"
  )

  override lazy val withoutOptimizations: Set[File] = Set(
    // contifying under reset
    examplesDir / "pos" / "issue842.effekt",
    examplesDir / "pos" / "issue861.effekt",

    // syntax error (multiple declaration)
    examplesDir / "pos" / "parser.effekt",
    examplesDir / "pos" / "probabilistic.effekt",
  )

  override def ignored: Set[File] = super.ignored ++ Set(
    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",
  )
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
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".effekt.md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")
        val checkfile = path / (baseName + ".check")

        val isIgnored = ignored.contains(f)
        val shouldGenerate = regenerateAll || f.lastModified() > checkfile.lastModified()
        if (!isIgnored && shouldGenerate) {
          println(s"Writing checkfile for ${f}")
          val out = run(f, true)

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
