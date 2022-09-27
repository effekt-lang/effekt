package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class ChezSchemeTests extends EffektTests {

  // Test files which are to be ignored (since features are missing or known bugs exist)
  override lazy val ignored: List[File] = List(
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
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt", // doesn't work with lift inference, yet

    examplesDir / "pos" / "multiline_extern_definition.effekt" // the test is specific to JS
  )

  def runTestFor(f: File, expected: String) = {
    it(f.getName + " (callcc)") {
      val out = interpretCS(f, "callcc")
      assert(expected == out)
    }
    it(f.getName + " (lift)") {
      val out = interpretCS(f, "lift")
      assert(expected == out)
    }
    it(f.getName + " (monadic)") {
      val out = interpretCS(f, "monadic")
      assert(expected == out)
    }
  }

  def interpretCS(file: File, variant: String): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", s"chez-$variant",
      "--includes", "libraries/chez/common",
      "--includes", ".",
      "--lib", s"libraries/chez/$variant"
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    removeAnsiColors(configs.stringEmitter.result())
  }
}
