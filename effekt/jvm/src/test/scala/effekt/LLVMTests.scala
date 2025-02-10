package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends EffektTests {

  def backendName = "llvm"

  override def valgrind = sys.env.get("EFFEKT_VALGRIND").nonEmpty
  override def debug = sys.env.get("EFFEKT_DEBUG").nonEmpty

  override lazy val positives: List[File] = List(
    examplesDir / "llvm",
    examplesDir / "pos",
    examplesDir / "benchmarks",
  )

  /**
   * Documentation of currently failing tests because of missing features
   * and their reason
   */
  lazy val missingFeatures: List[File] = List(
    // Regex
    examplesDir / "pos" / "simpleparser.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "capture" / "mbed.effekt",

    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",

    // Only JS (tests should be moved to a JS folder)
    examplesDir / "pos" / "genericcompare.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    examplesDir / "pos" / "maps.effekt",
    examplesDir / "pos" / "capture" / "resources.effekt",
    examplesDir / "pos" / "io",

    // higher order foreign functions are not supported
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt",

    // See PR #355
    examplesDir / "llvm" / "string_toint.effekt",

    // Generic equality
    examplesDir / "pos" / "issue429.effekt",

    // Generic comparison
    examplesDir / "pos" / "issue733.effekt",
  )

  override lazy val ignored: List[File] = missingFeatures ++ noValgrind(examplesDir)
}

/**
 * Documentation of tests that succeed in running, but fail valgrind
 * and their reason
 */
def noValgrind(examplesDir: File): List[File] = List(
  examplesDir / "llvm" / "prompt-duplication.effekt",
)

class LLVMNoValgrindTests extends EffektTests {
  def backendName = "llvm"

  override def valgrind = false
  override def debug = false

  override lazy val positives: List[File] = noValgrind(examplesDir)
}
