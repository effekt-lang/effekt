package effekt.core

import java.io.File

class SpecializeTests extends CoreTests {

  private def specialized(input: ModuleDecl): ModuleDecl = {
    val result = specialization(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/specialize"),
    CoreIRTransform("SPECIALIZE", specialized)
  )
}
