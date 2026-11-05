package effekt
package core

import java.io.File

import effekt.core.optimizer.ReferencesToParameters

class ReferencesToParametersTests extends CoreTests {

  registerCoreIRTests(
    new File("examples/core/references-to-parameters"),
    CoreIRTransform("REFERENCES_TO_PARAMETERS", ReferencesToParameters.transform)
  )
}
