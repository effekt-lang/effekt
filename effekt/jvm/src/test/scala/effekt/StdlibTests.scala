package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

abstract class StdlibTests extends EffektTests {
  override def positives: List[File] = List(
    examplesDir / "stdlib",
  )

  override def ignored: List[File] = List()
}

class StdlibJavaScriptTests extends StdlibTests {
  def backendName: String = "js"
}
abstract class StdlibChezTests extends StdlibTests {
  override def ignored: List[File] = List(
    // Not implemented yet
    examplesDir / "stdlib" / "bytes",
    examplesDir / "stdlib" / "io"
  )
}
class StdlibChezSchemeMonadicTests extends StdlibChezTests {
  def backendName: String = "chez-monadic"
}
class StdlibChezSchemeCallCCTests extends StdlibChezTests {
  def backendName: String = "chez-callcc"
}
class StdlibChezSchemeLiftTests extends StdlibChezTests {
  def backendName: String = "chez-lift"
}
class StdlibMLTests extends StdlibTests {
  def backendName: String = "ml"

  override def ignored: List[File] = List(
    // For every function using `foreach`:
    // [error] Effect polymorphic recursion is not allowed. The following definition is effect polymorphic since unification variable X occurs in instantiation ...
    examplesDir / "stdlib" / "list" / "join.effekt",
    examplesDir / "stdlib" / "list" / "flatmap.effekt",

    // unicode is not supported in the ML backend
    examplesDir / "stdlib" / "string" / "unicode.effekt",

    // Not implemented yet
    examplesDir / "stdlib" / "bytes",
    examplesDir / "stdlib" / "io"
  )
}
class StdlibLLVMTests extends StdlibTests {
  def backendName: String = "llvm"

  override def valgrind = sys.env.get("EFFEKT_VALGRIND").nonEmpty

  override def ignored: List[File] = List(
    // For every function tested using `immutable/result`:
    // [error] Unsupported coercion from Exception47234[E48288] to Exception47234[OutOfBounds47515]
    examplesDir / "stdlib" / "list" / "get.effekt",
    examplesDir / "stdlib" / "list" / "modifyat.effekt",
    // Toplevel let-bindings (for ANSI-color-codes in output) not supported
    examplesDir / "stdlib" / "test" / "test.effekt",

    // Valgrind leak/failure
    examplesDir / "stdlib" / "bytes" / "bytes.effekt",
    examplesDir / "stdlib" / "io" / "files" / "async_file_io.effekt",
    examplesDir / "stdlib" / "io" / "files" / "filesystem.effekt",
    examplesDir / "stdlib" / "io" / "time.effekt",
    examplesDir / "stdlib" / "list" / "flatmap.effekt",
    examplesDir / "stdlib" / "list" / "zip.effekt",
    examplesDir / "stdlib" / "list" / "deleteat.effekt",
    examplesDir / "stdlib" / "list" / "join.effekt",
    examplesDir / "stdlib" / "list" / "updateat.effekt",
    examplesDir / "stdlib" / "list" / "insert.effekt",
    examplesDir / "stdlib" / "list" / "fill.effekt",
    examplesDir / "stdlib" / "list" / "zipwith.effekt",
    examplesDir / "stdlib" / "list" / "collect.effekt",
    examplesDir / "stdlib" / "list" / "build.effekt",
    examplesDir / "stdlib" / "string" / "strings.effekt",
    examplesDir / "stdlib" / "string" / "unicode.effekt",
  )
}
