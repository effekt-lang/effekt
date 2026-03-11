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

  override lazy val positives: Set[File] = Set(
    examplesDir / "llvm",
    examplesDir / "pos",
    examplesDir / "benchmarks",
  )

  /**
   * Documentation of currently failing tests because of missing features
   * and their reason
   */
  lazy val missingFeatures: Set[File] = Set(
    // Regex
    examplesDir / "pos" / "simpleparser.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "capture" / "mbed.effekt",

    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",

    // no generic inspect
    examplesDir / "casestudies",

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

    // re-entrant resumption
    examplesDir / "pos" / "issue1203.effekt",
    examplesDir / "pos" / "issue1203b.effekt",
  )

  override lazy val withoutOptimizations: Set[File] = Set(
    // contifying under reset
    examplesDir / "pos" / "issue842.effekt",
    examplesDir / "pos" / "issue861.effekt",

    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "capture" / "selfregion.effekt",
    examplesDir / "benchmarks" / "other" / "generator.effekt",
    examplesDir / "pos" / "bidirectional" / "typeparametric.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "storage.effekt",

    // top-level object definition
    examplesDir / "pos" / "object" / "if_control_effect.effekt",
    examplesDir / "pos" / "lambdas" / "toplevel_objects.effekt",
    examplesDir / "pos" / "type_omission_op.effekt",
    examplesDir / "pos" / "bidirectional" / "higherorderobject.effekt",
    examplesDir / "pos" / "bidirectional" / "res_obj_boxed.effekt",
    examplesDir / "pos" / "bidirectional" / "effectfulobject.effekt",
  )

  override lazy val ignored: Set[File] = missingFeatures ++ noValgrind(examplesDir) ++ super.ignored
}

/**
 * Documentation of tests that succeed in running, but fail valgrind
 * and their reason
 */
def noValgrind(examplesDir: File): Set[File] = Set(
  examplesDir / "llvm" / "prompt-duplication.effekt",
)

class LLVMNoValgrindTests extends EffektTests {
  def backendName = "llvm"

  override def valgrind = false
  override def debug = false

  override lazy val positives: Set[File] = noValgrind(examplesDir)
}
