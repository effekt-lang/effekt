package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

abstract class StdlibTests extends EffektTests {
  override lazy val included: List[File] = List(
    examplesDir / "stdlib",
  )

  override lazy val ignored: List[File] = List()
}

class StdlibJavaScriptTests extends StdlibTests {
  def backendName: String = "js"
}
class StdlibChezSchemeMonadicTests extends StdlibTests {
  def backendName: String = "chez-monadic"
}
class StdlibChezSchemeCallCCTests extends StdlibTests {
  def backendName: String = "chez-callcc"
}
class StdlibChezSchemeLiftTests extends StdlibTests {
  def backendName: String = "chez-lift"
}
class StdlibMLTests extends StdlibTests {
  def backendName: String = "ml"

  override lazy val ignored: List[File] = List(
    // For every function using `foreach`:
    // [error] Effect polymorphic recursion is not allowed. The following definition is effect polymorphic since unification variable X occurs in instantiation ...
    examplesDir / "stdlib" / "list" / "join.effekt",
    examplesDir / "stdlib" / "list" / "flatmap.effekt",
  )
}
}