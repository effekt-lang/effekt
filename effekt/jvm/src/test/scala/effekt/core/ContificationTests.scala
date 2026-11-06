package effekt
package core

import java.io.File

import effekt.core.optimizer.{ cleanup, contify, identify, parametrize, place, returnsTo }

class ContificationTests extends CoreTests {

  private def checked(transform: ModuleDecl => ModuleDecl)(input: ModuleDecl): ModuleDecl = {
    val result = transform(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRTransform("PLACE", place),
    CoreIRAnalysis("RETURNS_TO", input => returnsTo(input).show),
    CoreIRTransform("CONTIFY", contify),
    CoreIRTransform("PARAMETRIZE", checked(parametrize)),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
