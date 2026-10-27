package effekt.core

import java.io.File

class ArityRaisingTests extends CoreTests {

  private def raise(input: ModuleDecl): ModuleDecl = {
    val result = ArityRaising.transform(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/arity-raising"),
    CoreIRTransform("ARITY_RAISE", raise)
  )
}
