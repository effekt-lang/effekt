package effekt
package core

import java.io.File
import effekt.util.messages.FatalPhaseError

class LambdaSetTests extends CoreTests {

  test("analysis requires monomorphic Core") {
    val input = parse("""
      module core/tests/polymorphic

      def identity['A](x: 'A) = {
        return x: 'A
      }
    """)

    intercept[FatalPhaseError](LambdaSets.analyze(input))
  }

  private def show(input: ModuleDecl): String =
    LambdaSets.show(LambdaSets.analyze(input))

  private def monomorphize(input: ModuleDecl): ModuleDecl = {
    val preprocessed = Mono.preprocess(input)
    Mono.specialize(preprocessed, Mono.solve(Mono.collect(preprocessed)))
  }

  private def specialize(input: ModuleDecl): ModuleDecl = {
    val result = LambdaSets.transform(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/lambda-sets"),
    CoreIRAnalysis("LAMBDA_SETS", show),
    CoreIRAnalysis("MONO_LAMBDA_SETS", input => show(monomorphize(input))),
    CoreIRTransform("LAMBDA_SPECIALIZE", specialize),
    CoreIRTransform("MONO_LAMBDA_SPECIALIZE", input => specialize(monomorphize(input)))
  )
}
