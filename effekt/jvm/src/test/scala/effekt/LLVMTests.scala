package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends EffektTests {

  override lazy val included: List[File] = List(
    examplesDir / "llvm",
  )

  /**
   * Documentation of currently failing tests in pos and their reason
   */
  lazy val failingTestsInPos: List[File] = List(

    // now show instance for records / datatypes
    examplesDir / "pos" / "builtins.effekt",
    examplesDir / "pos" / "namespaces.effekt",
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "either.effekt",

    // missing dealiasing of `def f = g`
    examplesDir / "pos" / "defdef.effekt",

    // option
    examplesDir / "pos" / "raytracer.effekt",

    // lists
    examplesDir / "pos" / "higherorder_io_control.effekt",
    examplesDir / "pos" / "type_parameters_blocks.effekt",
    examplesDir / "pos" / "withstatement.effekt",
    examplesDir / "pos" / "build.effekt",
    examplesDir / "pos" / "overloading.effekt",
    examplesDir / "pos" / "matchblock.effekt",
    examplesDir / "pos" / "sideeffects.effekt",

    // arrays
    examplesDir / "pos" / "arrays.effekt",

    // text/string & string concatenation
    examplesDir / "pos" / "matchdef.effekt",
    examplesDir / "pos" / "simpleparser.effekt",
    examplesDir / "pos" / "parametrized.effekt",
    examplesDir / "pos" / "probabilistic.effekt",
    examplesDir / "pos" / "parser.effekt",

    // tuples
    examplesDir / "pos" / "records.effekt",
    examplesDir / "pos" / "existentials.effekt",

    // holes
    examplesDir / "pos" / "infer" / "infer_blockvars.effekt",
    examplesDir / "pos" / "emptymatch.effekt",

    // multi handlers
    examplesDir / "pos" / "multihandler.effekt",
    examplesDir / "pos" / "multieffects.effekt",

    // multiple methods
    examplesDir / "pos" / "effectalias.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "toplevelval.effekt",

    // foreign functions with block arguments
    examplesDir / "pos" / "liftinference.effekt",

    // probably issue 207
    examplesDir / "pos" / "stream_push.effekt",
    examplesDir / "pos" / "matching.effekt",

    // others
    examplesDir / "pos" / "issue108.effekt",
    examplesDir / "pos" / "propagators.effekt",
    examplesDir / "pos" / "nim.effekt",
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "dequeue.effekt",
    examplesDir / "pos" / "stream_pull.effekt",
    examplesDir / "pos" / "matchhandler.effekt",
    examplesDir / "pos" / "mutualrecursion.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    examplesDir / "pos" / "maps.effekt",
    examplesDir / "pos" / "state.effekt",
    examplesDir / "pos" / "bug1.effekt",


    // whole folders
    examplesDir / "pos" / "bidirectional",
    examplesDir / "pos" / "capture",
    examplesDir / "pos" / "lambdas",
    examplesDir / "pos" / "io",
    examplesDir / "pos" / "infer",
    examplesDir / "pos" / "polymorphic",
  )

  override lazy val ignored: List[File] = List(
    // computes the wrong results
    examplesDir / "llvm" / "nested.effekt",

    // Issue #207
    examplesDir / "llvm" / "polymorphism_blockparams.effekt",
  )

  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (llvm)") {
      val out = runLLVM(input)
      assertNoDiff(out, expected)
    }
  }

  def runLLVM(f: File): String = {
    // Reset the caches before each test. This is necessary to
    // avoid dependencies compiled with one compiler instance to be reused when
    // compiled with another compiler instance / config / backend.
    //
    // It does however slow down the tests, since the standard library has to be
    // compiled each and every time.
    effekt.util.Task.reset()
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "llvm",
      "--lib", "libraries/llvm",
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result()
  }

  def canRun() =
    canRunExecutable("llc", "--version") ||
    canRunExecutable("llc-15", "--version") ||
    canRunExecutable("llc-12", "--version")
}
