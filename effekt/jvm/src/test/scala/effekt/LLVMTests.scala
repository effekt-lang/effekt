package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends EffektTests {

  def backendName = "llvm"

  override lazy val positives: List[File] = List(
    examplesDir / "llvm"
  )

  /**
   * Documentation of currently failing tests in pos and their reason
   */
  lazy val failingTestsInPos: List[File] = List(

    // BUGS
    // ----

    // seg faults!
    examplesDir / "pos" / "issue108.effekt",

    // boxing
    examplesDir / "benchmarks" / "church_exponentiation.effekt",

    // missing dealiasing of `def f = g`
    examplesDir / "pos" / "defdef.effekt",

    // unsure
    examplesDir / "pos" / "parametrized.effekt",
    examplesDir / "ml" / "probabilistic.effekt", // crashes with "PANIC: Reached a hole in the program"

    // MISSING FEATURES
    // ----------------

    // now show instance for records / datatypes
    examplesDir / "pos" / "builtins.effekt",
    examplesDir / "pos" / "namespaces.effekt",
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "either.effekt",

    // inspect
    examplesDir / "pos" / "probabilistic.effekt",
    examplesDir / "pos" / "nim.effekt",

    // arrays
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "raytracer.effekt",

    // Regex
    examplesDir / "pos" / "simpleparser.effekt",

    // tuples
    examplesDir / "pos" / "records.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "toplevelval.effekt",
    examplesDir / "pos" / "capture" / "mbed.effekt",

    // foreign functions with block arguments
    examplesDir / "pos" / "liftinference.effekt",

    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",
    examplesDir / "pos" / "unsafe_cont.effekt",

    // Only JS (tests should be moved to a JS folder)
    examplesDir / "pos" / "genericcompare.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    examplesDir / "pos" / "maps.effekt",
    examplesDir / "pos" / "capture" / "resources.effekt",
    examplesDir / "pos" / "io",

    // Bidirectional effects do not work in general
    examplesDir / "pos" / "bidirectional",

    // first class functions closing over capabilities
    examplesDir / "pos" / "capture" / "borrows.effekt",
    examplesDir / "pos" / "capture" / "optimizing_unbox.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "lambdas" / "annotated.effekt",
    examplesDir / "pos" / "lambdas" / "scheduler.effekt",
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt",

    // higher order foreign functions are not supported
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt",

  )

  override lazy val ignored: List[File] = List(

    // See PR #355
    examplesDir / "llvm" / "string_toint.effekt",
  )
}
