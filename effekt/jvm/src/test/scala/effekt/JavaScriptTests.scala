package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  def backendName = "js"

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

    // Missing features in new direct style backend:
    // ---------------------------------------------

    // (local) mutable variables
    examplesDir / "pos" / "nim.effekt",
    examplesDir / "pos" / "propagators.effekt",
    examplesDir / "benchmarks" / "triples.effekt",
    examplesDir / "benchmarks" / "nqueens.effekt",
    examplesDir / "benchmarks" / "simple_counter.effekt",
    examplesDir / "benchmarks" / "tree.effekt",
    examplesDir / "casestudies"/ "prettyprinter.md",
    examplesDir / "casestudies"/ "lexer.md",
    examplesDir / "casestudies"/ "parser.md",
    examplesDir / "casestudies"/ "naturalisticdsls.md",
    examplesDir / "casestudies"/ "buildsystem.md",
    examplesDir / "casestudies"/ "anf.md",
    examplesDir / "pos" / "issue108.effekt",
    examplesDir / "pos" / "overloading.effekt",
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "type_parameters_blocks.effekt",
    examplesDir / "pos" / "stacksafe.effekt",
    examplesDir / "pos" / "build.effekt",
    examplesDir / "pos" / "type_parameter_blocks.effekt",
    examplesDir / "pos" / "capture" / "mbed.effekt",
    examplesDir / "pos" / "mutable.effekt",
    examplesDir / "pos" / "sideeffects.effekt",
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "simpleparser.effekt",
    examplesDir / "pos" / "higherorder_io_control.effekt",
    examplesDir / "pos" / "dequeue.effekt",
    examplesDir / "pos" / "matchblock.effekt",
    examplesDir / "pos" / "multihandler.effekt",
    examplesDir / "pos" / "state.effekt",
    examplesDir / "pos" / "parser.effekt",
    examplesDir / "pos" / "stream_push.effekt",
    examplesDir / "pos" / "stream_pull.effekt",
    examplesDir / "pos" / "probabilistic.effekt",
    examplesDir / "pos" / "multieffects.effekt",
    examplesDir / "pos" / "imports.effekt",
    examplesDir / "pos" / "withstatement.effekt",
    examplesDir / "pos" / "raytracer.effekt",
    examplesDir / "pos" / "lambdas" / "generators.effekt",
    examplesDir / "pos" / "lambdas" / "localstate.effekt",
    examplesDir / "pos" / "lambdas" / "scheduler.effekt",

    // bidirectional
    examplesDir / "pos" / "bidirectional" / "iterators.effekt",
    examplesDir / "pos" / "bidirectional" / "pingpong.effekt",

    // regions
    examplesDir / "benchmarks" / "generator.effekt",
    examplesDir / "pos" / "recursiveobject.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "liftinference.effekt",
    examplesDir / "pos" / "capture" / "selfregion.effekt"

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
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
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
