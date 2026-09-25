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

  private def mainCapture(module: ModuleDecl): Captures = module.definitions.collectFirst {
    case Toplevel.Def(id, block) if id.name.name == "main" => block.capt
  }.get

  test("nominal dispatch preserves latent effects") {
    val input = parse("""
      module core/tests/dispatch_effects
      extern {io} def touch(value: Int): Unit = js ""
      def first() = { (touch: (Int) => Unit @ {io})(1) }
      def second() = { (touch: (Int) => Unit @ {io})(2) }
      def main(flag: Bool) = {
        val action = if (flag: Bool) {
          return box {io} (first: () => Unit @ {io})
        } else {
          return box {io} (second: () => Unit @ {io})
        };
        (unbox action: () => Unit at {io})()
      }
    """)
    val result = specialize(input)
    assertEquals(mainCapture(result), mainCapture(input))
  }

  test("unboxing a represented object reconstructs its block interface") {
    val input = parse("""
      module core/tests/boxed_object
      interface Reader { read: () => Int }
      extern {io} def touch(value: Int): Unit = js ""
      def main(n: Int) = {
        let boxed = box {io} new Reader {
          def read() = {
            val ignored = (touch: (Int) => Unit @ {io})(n: Int);
            return n: Int
          }
        }
        def reader = unbox boxed: Reader at {io}
        (reader: Reader @ {io}).read: () => Int()
      }
    """)
    assertEquals(mainCapture(specialize(input)), mainCapture(input))
  }

  test("nominal representations identify alpha-equivalent block types") {
    val input = parse("""
      module core/tests/boxed_alpha
      def use(){action: (Int){f: (Int) => Int} => Int} = {
        (action: (Int){f: (Int) => Int} => Int @ {action})(42){
          (x: Int) => { return x: Int }
        }
      }
      def main() = {
        let boxed = box {} { (x: Int){ignored: (Int) => Int} => return x: Int }
        (use: (){action: (Int){f: (Int) => Int} => Int} => Int @ {})(){
          (unbox boxed: (Int){ignored: (Int) => Int} => Int at {})
        }
      }
    """)
    specialize(input)
  }

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
