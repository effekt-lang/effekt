package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

class MLTests extends EffektTests {

  def backendName: String = "ml"

  override lazy val positives: List[File] = List(
    examplesDir / "ml",
    examplesDir / "benchmarks",
    examplesDir / "pos",
    examplesDir / "features"
  )

  override lazy val ignored: List[File] = List(

    // Error messages differ due to missing positions
    examplesDir / "pos" / "patternmatching" / "matching-defaults.effekt",
    examplesDir / "pos" / "unused_effects.effekt",
    examplesDir / "pos" / "twice-explicit.effekt",
    examplesDir / "pos" / "state.effekt",
    examplesDir / "pos" / "bidirectional" / "selfrecursion.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",

    // Regression with monomorphization
    examplesDir / "ml" / "raytracer.effekt",

    // `gametree` uses `resume` in a different region (under `var i` in `range`)
    examplesDir / "ml" / "nim.effekt",
    examplesDir / "ml" / "non_scoped_resume.effekt", // minified version

    // monomorphization of global state is not yet implemented (minified version of original raytracer)
    examplesDir / "ml" / "global.effekt",

    // (Mutually) recursive functions, which are monomorphized
    // And: unknown problem with evidence monomorphization
    examplesDir / "ml" / "probabilistic.effekt",

    examplesDir / "pos" / "object",
    // Tests with box
    examplesDir / "pos" / "capture" / "defdef.effekt",
    examplesDir / "pos" / "capture" / "mbed.effekt",
    examplesDir / "pos" / "file.effekt",
    //    examplesDir / "pos" / "capture" / "optimizing_unbox.effekt",
    examplesDir / "pos" / "capture" / "regions.effekt",
    examplesDir / "pos" / "capture" / "resources.effekt",
    examplesDir / "pos" / "capture" / "selfregion.effekt",
    //    examplesDir / "pos" / "infer" / "infer_boxed.effekt",
    //    examplesDir / "pos" / "infer" / "infer_effect_polymorphic.effekt",
    examplesDir / "pos" / "issue108.effekt",

    examplesDir / "pos" / "lambdas",
    //    examplesDir / "pos" / "lambdas" / "annotated.effekt",
    //    examplesDir / "pos" / "lambdas" / "effects.effekt",
    //    examplesDir / "pos" / "lambdas" / "generators.effekt",
    //    examplesDir / "pos" / "lambdas" / "higherorder.effekt",
    //    examplesDir / "pos" / "lambdas" / "localstate.effekt",
    //    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt",


    // region-based memory management is not yet supported (monomorphization would only work for type monomorphic regions)
    examplesDir / "pos" / "recursiveobject.effekt",

    examplesDir / "benchmarks" / "generator.effekt",

    // missing "show" instance
    examplesDir / "pos" / "nim.effekt",
    examplesDir / "pos" / "builtins.effekt",
    examplesDir / "pos" / "either.effekt",
    examplesDir / "pos" / "namespaces.effekt",
    examplesDir / "pos" / "overloading.effekt",
    examplesDir / "pos" / "dequeue.effekt",
    examplesDir / "pos" / "matchblock.effekt",
    examplesDir / "pos" / "polymorphic" / "exceptions.effekt",
    examplesDir / "pos" / "exists.effekt", // now show instance for existentials

    // polymorphic effect operation not supported
    examplesDir / "pos" / "existentials.effekt",
    examplesDir / "pos" / "triples.effekt",
    examplesDir / "pos" / "bidirectional",
    examplesDir / "benchmarks" / "variadic_combinators.effekt",

    // mutual recursion
    examplesDir / "pos" / "mutualrecursion.effekt",

    // heap
    examplesDir / "casestudies" / "ad.effekt..md",

    // regex
    examplesDir / "casestudies" / "anf.effekt.md",
    examplesDir / "casestudies" / "lexer.effekt.md",
    examplesDir / "casestudies" / "parser.effekt.md",
    examplesDir / "casestudies" / "prettyprinter.effekt.md",
    examplesDir / "benchmarks" / "pretty.effekt",
    examplesDir / "pos" / "simpleparser.effekt",

    // cont
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // array api
    examplesDir / "pos" / "raytracer.effekt",
    examplesDir / "pos" / "issue319.effekt",
    examplesDir / "pos" / "array" / "list_conversion.effekt",
    examplesDir / "pos" / "array" / "sum.effekt",

    // async
    examplesDir / "pos" / "io" / "async_file_io.effekt",

    // mutable map
    examplesDir / "pos" / "maps.effekt",

    // Unclear issue
    examplesDir / "pos" / "capture" / "ffi_blocks.effekt",
    examplesDir / "casestudies" / "buildsystem.effekt.md",
    examplesDir / "casestudies" / "naturalisticdsls.effekt.md",
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "build.effekt",
    examplesDir / "pos" / "emptymatch.effekt",
    examplesDir / "pos" / "liftinference.effekt",
    examplesDir / "pos" / "multieffects.effekt",
    examplesDir / "pos" / "multiline_extern_definition.effekt",
    // unkown issue - 'Mutual definitions are currently not supported by this backend'
    examplesDir / "pos" / "patternmatching" / "matching-while.effekt",

    examplesDir / "pos" / "probabilistic.effekt",

    examplesDir / "pos" / "genericcompare.effekt", // genericCompare is only implemented for JS
  )
}
