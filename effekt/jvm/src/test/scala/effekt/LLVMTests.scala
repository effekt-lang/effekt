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

    // missing dealiasing of `def f = g`
    examplesDir / "pos" / "defdef.effekt",

    // missing multiple operations (zero to be precise)
    examplesDir / "pos" / "effectalias.effekt",

    // type polymorphic data types
    examplesDir / "pos" / "effectalias.effekt",
    examplesDir / "pos" / "emptymatch.effekt",
    examplesDir / "pos" / "either.effekt",
    examplesDir / "pos" / "matching.effekt",

    // type polymorphic functions
    examplesDir / "pos" / "infer" / "infer_handler.effekt",
    examplesDir / "pos" / "poly1.effekt",
    examplesDir / "pos" / "imports.effekt",

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
    examplesDir / "pos" / "lists.effekt",

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

    // multi handlers
    examplesDir / "pos" / "multihandler.effekt",
    examplesDir / "pos" / "multieffects.effekt",

    // toplevel def and let bindings
    examplesDir / "pos" / "toplevelval.effekt",

    // foreign functions with block arguments
    examplesDir / "pos" / "liftinference.effekt",

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

    // polymorphic effect operations not supported, yet
    examplesDir / "llvm" / "choice.effekt",
    examplesDir / "llvm" / "triples.effekt",
  )

  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (llvm)") {
      val out = runLLVM(input)
      assertNoDiff(out, expected)
    }
  }

  def runLLVM(f: File): String = {
    // TODO flaky body
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
