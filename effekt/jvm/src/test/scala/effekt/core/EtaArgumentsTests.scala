package effekt
package core

import java.io.File

class EtaArgumentsTests extends CoreTests {
  registerCoreIRTests(
    new File("examples/core/eta-arguments"),
    CoreIRTransform("ETA_ARGUMENTS", EtaArguments.reduceArguments.rewrite)
  )
}
