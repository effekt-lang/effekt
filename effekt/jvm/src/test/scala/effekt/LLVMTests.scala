package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends EffektTests {

  def backendName = "llvm"

  override def valgrind = sys.env.get("EFFEKT_VALGRIND").nonEmpty

  override lazy val positives: List[File] = List(
    examplesDir / "llvm",
    examplesDir / "pos",
    examplesDir / "benchmarks",
  )

  lazy val bugs: List[File] = List(
    // names not sanitized (even?)
    examplesDir / "pos" / "special_names.effekt",

    // sanitizer/valgrind: segfault
    examplesDir / "pos" / "parametrized.effekt",

    // Valgrind leak, unclear
    examplesDir / "llvm" / "polymorphism_map.effekt",
    examplesDir / "pos" / "type_parameters_blocks.effekt",
    
    // Valgrind leak in array_new
    examplesDir / "benchmarks" / "are_we_fast_yet" / "sieve.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "nbody.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "bounce.effekt", // + c_ref_fresh
    examplesDir / "benchmarks" / "are_we_fast_yet" / "towers.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "queens.effekt",
  )

  /**
   * Documentation of currently failing tests and their reason
   */
  lazy val missingFeatures: List[File] = List(

    // now show instance for records / datatypes
    examplesDir / "pos" / "builtins.effekt",
    examplesDir / "pos" / "namespaces.effekt",
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "either.effekt",

    // inspect
    examplesDir / "pos" / "probabilistic.effekt",
    examplesDir / "pos" / "nim.effekt",
    examplesDir / "pos" / "exists.effekt",

    // arrays
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "raytracer.effekt",
    examplesDir / "pos" / "issue319.effekt",
    examplesDir / "pos" / "array",

    // Regex
    examplesDir / "pos" / "simpleparser.effekt",

    // tuples
    examplesDir / "pos" / "records.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "toplevelval.effekt",
    examplesDir / "pos" / "capture" / "mbed.effekt",
    examplesDir / "pos" / "lib_test.effekt",

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
    examplesDir / "pos" / "type_omission_op.effekt",
    examplesDir / "pos" / "object"/ "higher_order_compose_op.effekt",

    // first class functions closing over capabilities
    examplesDir / "pos" / "capture" / "state_eff.effekt",
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt",
    examplesDir / "pos" / "file.effekt",

    // higher order foreign functions are not supported
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt",

    // See PR #355
    examplesDir / "llvm" / "string_toint.effekt",

    // Generic equality
    examplesDir / "pos" / "issue429.effekt",

    // Math operations
    examplesDir / "pos" / "doubles.effekt",
  )

  override lazy val ignored: List[File] = bugs ++ missingFeatures
}
