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
    CoreIRAnalysis("RECURSIVE", input => recursive(names(input)).toList.map(_.name.name).sorted.mkString("\n")),
    CoreIRTransform("SPECIALIZE", specialized)
  )
}
