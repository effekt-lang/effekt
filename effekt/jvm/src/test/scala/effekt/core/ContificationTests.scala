package effekt
package core

import java.io.File

import effekt.core.optimizer.{ cleanup, identify, returnsTo, sinking }

class ContificationTests extends CoreTests {

  private def checked(transform: ModuleDecl => ModuleDecl)(input: ModuleDecl): ModuleDecl = {
    val result = transform(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRAnalysis("RETURNS_TO", input => returnsTo(input).show),
    CoreIRTransform("SINK", checked(sinking)),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
