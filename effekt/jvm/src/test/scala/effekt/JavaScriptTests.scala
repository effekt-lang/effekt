package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  def backendName = "js"

  override def positives: List[File] = List(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "benchmarks",
  )

  override def negatives: List[File] = List(
    examplesDir / "neg"
  )

  // global missing
  // examples/pos/capture/selfregion.effekt
  // examples/benchmarks/are_we_fast_yet/storage.effekt
  override def ignored: List[File] = List(
    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",

    // bidirectional
    examplesDir / "pos" / "object" / "higher_order_compose_op.effekt",
    examplesDir / "pos" / "bidirectional",
    examplesDir / "pos" / "type_omission_op.effekt",

    // toplevel val / run
    examplesDir / "pos" / "toplevelval.effekt",
    examplesDir / "pos" / "raytracer.effekt",
    examplesDir / "casestudies" / "naturalisticdsls.effekt.md",
    examplesDir / "casestudies" / "anf.effekt.md"
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
          val out = run(f)

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
