package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.language.implicitConversions

class MLTests extends EffektTests {

  override lazy val included: List[File] = List(
    examplesDir / "ml"
  )

  override lazy val ignored: List[File] = List(

    // Broken tests
    examplesDir / "ml" / "naturalisticdsls.md",
    examplesDir / "ml" / "probabilistic.effekt",

    // Tests with box
    //    examplesDir / "pos" / "capture" / "defdef.effekt",
    examplesDir / "pos" / "capture" / "mbed.effekt",
    //    examplesDir / "pos" / "capture" / "optimizing_unbox.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "capture" / "resources.effekt",
    //    examplesDir / "pos" / "capture" / "selfregion.effekt",
    //    examplesDir / "pos" / "infer" / "infer_boxed.effekt",
    //    examplesDir / "pos" / "infer" / "infer_effect_polymorphic.effekt",
    examplesDir / "pos" / "issue108.effekt",
    //    examplesDir / "pos" / "lambdas" / "annotated.effekt",
    //    examplesDir / "pos" / "lambdas" / "effects.effekt",
    //    examplesDir / "pos" / "lambdas" / "generators.effekt",
    //    examplesDir / "pos" / "lambdas" / "higherorder.effekt",
    //    examplesDir / "pos" / "lambdas" / "localstate.effekt",
    examplesDir / "pos" / "lambdas" / "scheduler.effekt",
    //    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt",
    examplesDir / "benchmarks" / "generator.effekt",

    // polymorphic effect operation not supported
    examplesDir / "pos" / "existentials.effekt",
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "bidirectional" / "pingpong.effekt",
    examplesDir / "pos" / "bidirectional" / "iterators.effekt",

    // mutual recursion
    examplesDir / "pos" / "mutualrecursion.effekt",

    // heap
    examplesDir / "casestudies" / "ad.md",

    // regex
    examplesDir / "casestudies" / "anf.md",
    examplesDir / "casestudies" / "lexer.md",
    examplesDir / "casestudies" / "parser.md",
    examplesDir / "casestudies" / "prettyprinter.md",
    examplesDir / "pos" / "simpleparser.effekt",

    // cont
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // array api
    examplesDir / "pos" / "raytracer.effekt",

    // async
    examplesDir / "pos" / "io" / "async_file_io.effekt",

    // mut map
    examplesDir / "pos" / "maps.effekt",

    // Unclear issue
    examplesDir / "casestudies" / "buildsystem.md",
    examplesDir / "casestudies" / "naturalisticdsls.md",
    examplesDir / "features" / "adt.md",
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "build.effekt",
    examplesDir / "pos" / "emptymatch.effekt",
//    examplesDir / "pos" / "imports.effekt",
    examplesDir / "pos" / "liftinference.effekt",
//    examplesDir / "pos" / "lists.effekt",
//    examplesDir / "pos" / "matchdef.effekt",
    examplesDir / "pos" / "multieffects.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    examplesDir / "pos" / "parametrized.effekt",
    examplesDir / "pos" / "parser.effekt",
    examplesDir / "pos" / "probabilistic.effekt",
    examplesDir / "pos" / "stream_pull.effekt",
  )

  def runTestFor(input: java.io.File, check: File, expected: String): Unit =
    test(input.getPath + " (ml)") {
      val out = runML(input)
      assertNoDiff(out, expected)
    }

  def runML(input: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "ml",
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(input.getPath, configs)
    configs.stringEmitter.result()
  }
}
