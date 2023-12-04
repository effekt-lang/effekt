package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.language.implicitConversions

abstract class ChezSchemeTests extends EffektTests {

  override def included: List[File] = List(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "chez",
    examplesDir / "benchmarks"
  )

  // Test files which are to be ignored (since features are missing or known bugs exist)
  override def ignored: List[File] = List(
    examplesDir / "llvm",

    examplesDir / "ml",

    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "issue319.effekt",
    examplesDir / "pos" / "maps.effekt",

    // bidirectional effects are not yet supported in our Chez backend
    examplesDir / "pos" / "bidirectional",

    // unsafe continuations are not yet supported in our Chez backend
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // the number representations differ in JS and Chez
    examplesDir / "casestudies" / "ad.md",

    // in the CallCC variant, we cannot have toplevel vals at the moment (their bindings need to be wrapped in `(run (thunk ...))`
    // see comment on commit 61492d9
    examplesDir / "casestudies" / "anf.md",

    // indexOf and lastIndexOf are not implemented in text/string
    examplesDir / "pos" / "string" / "indexOf.effekt",

    // missing array-related functions & methods
    examplesDir / "pos" / "array" / "list_conversion.effekt",
    examplesDir / "pos" / "array" / "sum.effekt",

    // we do not need to run the negative tests for the other backends
    examplesDir / "neg",

    examplesDir / "pos" / "infer",

    examplesDir / "pos" / "lambdas",

    examplesDir / "pos" / "multiline_extern_definition.effekt", // the test is specific to JS

    examplesDir / "pos" / "io", // async io is only implemented for monadic JS
  )
}

class ChezSchemeMonadicTests extends ChezSchemeTests {
  def backendName = "chez-monadic"
}

class ChezSchemeCallCCTests extends ChezSchemeTests {
  def backendName = "chez-callcc"
}
class ChezSchemeLiftTests extends ChezSchemeTests {

  def backendName = "chez-lift"

  override def ignored: List[File] = super.ignored ++ List(
    // global mutable state is not yet supported

    // regions are not yet supported
    examplesDir / "benchmarks" / "generator.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "capture" / "selfregion.effekt",

    // known issues:
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt", // doesn't work with lift inference, yet
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt" // ffi is passed evidence, which it does not need
  )
}
