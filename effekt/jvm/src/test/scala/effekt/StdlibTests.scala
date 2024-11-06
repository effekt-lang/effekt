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

  override def ignored: List[File] = List()
}
abstract class StdlibChezTests extends StdlibTests {
  override def ignored: List[File] = List(
    // Not implemented yet
    examplesDir / "stdlib" / "bytearray",
    examplesDir / "stdlib" / "io",

    // Not implemented: advanced regex features
    examplesDir / "stdlib" / "string" / "regex.effekt"
  )
}
class StdlibChezSchemeMonadicTests extends StdlibChezTests {
  def backendName: String = "chez-monadic"
}
class StdlibChezSchemeCallCCTests extends StdlibChezTests {
  def backendName: String = "chez-callcc"
}
class StdlibLLVMTests extends StdlibTests {
  def backendName: String = "llvm"

  override def valgrind = sys.env.get("EFFEKT_VALGRIND").nonEmpty

  override def ignored: List[File] = List(
    // Toplevel let-bindings (for ANSI-color-codes in output) not supported
    examplesDir / "stdlib" / "test" / "test.effekt",

    // Valgrind leak/failure
    examplesDir / "stdlib" / "bytearray" / "bytearray.effekt",
    examplesDir / "stdlib" / "io" / "filesystem" / "async_file_io.effekt",
    examplesDir / "stdlib" / "io" / "filesystem" / "files.effekt",
    examplesDir / "stdlib" / "io" / "filesystem" / "wordcount.effekt",
    examplesDir / "stdlib" / "io" / "time.effekt",
    examplesDir / "stdlib" / "list" / "flatmap.effekt",
    examplesDir / "stdlib" / "list" / "zip.effekt",
    examplesDir / "stdlib" / "list" / "deleteat.effekt",
    examplesDir / "stdlib" / "list" / "join.effekt",
    examplesDir / "stdlib" / "list" / "modifyat.effekt",
    examplesDir / "stdlib" / "list" / "updateat.effekt",
    examplesDir / "stdlib" / "list" / "insert.effekt",
    examplesDir / "stdlib" / "list" / "fill.effekt",
    examplesDir / "stdlib" / "list" / "zipwith.effekt",
    examplesDir / "stdlib" / "list" / "collect.effekt",
    examplesDir / "stdlib" / "list" / "build.effekt",
    examplesDir / "stdlib" / "string" / "strings.effekt",
    examplesDir / "stdlib" / "string" / "unicode.effekt",

    // Not implemented: advanced regex features
    examplesDir / "stdlib" / "string" / "regex.effekt"
  )
}
