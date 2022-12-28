package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.language.implicitConversions

abstract class ChezSchemeTests extends EffektTests {

  override def included: List[File] = List(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "chez"
  )

  // Test files which are to be ignored (since features are missing or known bugs exist)
  override def ignored: List[File] = List(
    examplesDir / "llvm",

    examplesDir / "ml",

    examplesDir / "pos" / "arrays.effekt",
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

    // we do not need to run the negative tests for the other backends
    examplesDir / "neg",

    examplesDir / "pos" / "infer",

    examplesDir / "pos" / "lambdas",

    examplesDir / "pos" / "multiline_extern_definition.effekt", // the test is specific to JS

    examplesDir / "pos" / "io", // async io is only implemented for monadic JS
  )

  def interpretCS(file: File, variant: String): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", s"chez-$variant",
      "--includes", "libraries/chez/common",
      "--includes", ".",
      "--lib", s"libraries/chez/$variant",
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result()
  }
}

class ChezSchemeMonadicTests extends ChezSchemeTests {
  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (monadic)") {
      val out = interpretCS(input, "monadic")
      assertNoDiff(out, expected)
    }
  }
}

class ChezSchemeCallCCTests extends ChezSchemeTests {
  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (callcc)") {
      val out = interpretCS(input, "callcc")
      assertNoDiff(out, expected)
    }
  }
}
class ChezSchemeLiftTests extends ChezSchemeTests {
  override def ignored: List[File] = super.ignored ++ List(
    // known issues:
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt", // doesn't work with lift inference, yet
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt" // ffi is passed evidecen, which it does not need
  )

  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (lift)") {
      val out = interpretCS(input, "lift")
      assertNoDiff(out, expected)
    }
  }
}
