package effekt
package core

import java.io.File

import effekt.core.optimizer.{ ReturnsTo, cleanup, contify, identify, returnsTo, sink }

class ContificationTests extends CoreTests {

  private def checked(transform: ModuleDecl => ModuleDecl)(input: ModuleDecl): ModuleDecl = {
    val result = transform(input)
    result.typecheck()
    result
  }

  private var analysis: ReturnsTo = null

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRAnalysis("RETURNS_TO", input => { analysis = returnsTo(input); analysis.show }),
    CoreIRTransform("CONTIFY", contify(_, analysis)),
    CoreIRTransform("SINK", checked(sink(_, analysis))),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
