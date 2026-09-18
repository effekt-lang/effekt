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

  test("recursive lambda-set quotient terminates") {
    // X = { a(Y, X), b(X, X) }
    // Y = { a(X, X), e }
    // Global recoloring alternates the colors of X and Y. Monotone
    // refinement separates them once and never merges them again.
    val colors = LambdaSets.stablePartition(2) { (id, colors) =>
      id match {
        case 0 => Set(
          "a" -> Vector(colors(1), colors(0)),
          "b" -> Vector(colors(0), colors(0)))
        case 1 => Set(
          "a" -> Vector(colors(0), colors(0)),
          "e" -> Vector.empty)
      }
    }

    assertNotEquals(colors(0), colors(1))
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
