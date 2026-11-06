package effekt
package core

import java.io.File

import effekt.core.optimizer.{ cleanup, contify, identify, returnsTo, sink }

class ContificationTests extends CoreTests {

  private def checked(transform: ModuleDecl => ModuleDecl)(input: ModuleDecl): ModuleDecl = {
    val result = transform(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRTransform("SINK", checked(sink)),
    CoreIRAnalysis("RETURNS_TO", input => returnsTo(input).show),
    CoreIRTransform("CONTIFY", checked(contify)),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
