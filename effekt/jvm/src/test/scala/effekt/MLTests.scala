package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

class MLTests extends EffektTests {

  def backendName: String = "ml"

  override lazy val positives: List[File] = List(
    examplesDir / "ml",
    examplesDir / "pos",
    examplesDir / "features"
  )

  override lazy val ignored: List[File] = List(

    // Regression with monomorphization
    examplesDir / "ml" / "raytracer.effekt",

    // `gametree` uses `resume` in a different region (under `var i` in `range`)
    examplesDir / "ml" / "nim.effekt",

    // monomorphization of global state is not yet implemented (minified version of original raytracer)
    examplesDir / "ml" / "global.effekt",

    // (Mutually) recursive functions, which are monomorphized
    // And: unknown problem with evidence monomorphization
    examplesDir / "ml" / "probabilistic.effekt",

    examplesDir / "pos" / "object",
    // Tests with box
    examplesDir / "pos" / "capture" / "mbed.effekt",
    examplesDir / "pos" / "file.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "capture" / "resources.effekt",
    examplesDir / "pos" / "capture" / "selfregion.effekt",
    examplesDir / "pos" / "issue108.effekt",

    examplesDir / "pos" / "lambdas" / "generators.effekt",
    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt",
    examplesDir / "pos" / "lambdas" / "scheduler.effekt",


    // region-based memory management is not yet supported (monomorphization would only work for type monomorphic regions)
    examplesDir / "benchmarks" / "other" / "generator.effekt",

    // missing "show" instance
    examplesDir / "pos" / "nim.effekt",
    examplesDir / "pos" / "builtins.effekt",
    examplesDir / "pos" / "either.effekt",
    examplesDir / "pos" / "namespaces.effekt",
    examplesDir / "pos" / "exists.effekt", // now show instance for existentials

    // polymorphic effect operation not supported
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "bidirectional",
    examplesDir / "pos" / "type_omission_op.effekt",

    // effect-polymorphic recursion
    examplesDir / "benchmarks" / "other" / "variadic_combinators.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "nbody.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "queens.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "bounce.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "mandelbrot.effekt",

    // heap
    examplesDir / "casestudies" / "ad.effekt..md",

    // regex
    examplesDir / "casestudies" / "anf.effekt.md",
    examplesDir / "casestudies" / "lexer.effekt.md",
    examplesDir / "casestudies" / "parser.effekt.md",
    examplesDir / "casestudies" / "prettyprinter.effekt.md",
    examplesDir / "pos" / "simpleparser.effekt",

    // cont
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // array api
    examplesDir / "pos" / "raytracer.effekt",
    examplesDir / "pos" / "issue319.effekt",
    examplesDir / "pos" / "array" / "list_conversion.effekt",
    examplesDir / "pos" / "array" / "sum.effekt",

    // mutable map
    examplesDir / "pos" / "maps.effekt",

    // Unclear issue
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt",
    examplesDir / "casestudies" / "buildsystem.effekt.md",
    examplesDir / "casestudies" / "naturalisticdsls.effekt.md",
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "build.effekt",
    examplesDir / "pos" / "liftinference.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "storage.effekt",

    examplesDir / "pos" / "probabilistic.effekt",

    examplesDir / "pos" / "genericcompare.effekt", // genericCompare is only implemented for JS
  )
}
