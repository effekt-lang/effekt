package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.language.implicitConversions

abstract class ChezSchemeTests extends EffektTests {

  override def positives: List[File] = List(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "chez",
    examplesDir / "benchmarks"
  )

  // Test files which are to be ignored (since features are missing or known bugs exist)
  override def ignored: List[File] = List(

    examplesDir / "llvm",

    // bidirectional handlers
    examplesDir / "pos" / "maps.effekt",

    // bidirectional effects are not yet supported in our Chez backend
    examplesDir / "pos" / "bidirectional",
    examplesDir / "pos" / "object",
    examplesDir / "pos" / "type_omission_op.effekt",

    // unsafe continuations are not yet supported in our Chez backend
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // the number representations differ in JS and Chez
    examplesDir / "casestudies" / "ad.effekt.md",
    examplesDir / "casestudies" / "inference.effekt.md",

    // in the CallCC variant, we cannot have toplevel vals at the moment (their bindings need to be wrapped in `(run (thunk ...))`
    // see comment on commit 61492d9
    examplesDir / "casestudies" / "anf.effekt.md",

    // we do not need to run the negative tests for the other backends
    examplesDir / "neg",

    examplesDir / "pos" / "infer",

    examplesDir / "pos" / "lambdas",

    examplesDir / "pos" / "multiline_extern_definition.effekt", // the test is specific to JS

    examplesDir / "pos" / "io", // async io is only implemented for monadic JS

    examplesDir / "pos" / "genericcompare.effekt", // genericCompare is only implemented for JS

    examplesDir / "pos" / "issue429.effekt",
  )
}

class ChezSchemeMonadicTests extends ChezSchemeTests {
  def backendName = "chez-monadic"
}

class ChezSchemeCallCCTests extends ChezSchemeTests {
  def backendName = "chez-callcc"
}
