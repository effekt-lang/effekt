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
    examplesDir / "stdlib" / "io",
    examplesDir / "stdlib" / "stream" / "characters.effekt",
    examplesDir / "stdlib" / "stream" / "fuse_newlines.effekt"
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
  override def debug = sys.env.get("EFFEKT_DEBUG").nonEmpty

  override def ignored: List[File] = List(
    // String comparison using `<`, `<=`, `>`, `>=` is not implemented yet on LLVM
    examplesDir / "stdlib" / "string" / "compare.effekt",

    // Wrong codegen for negative types, see #801
    examplesDir / "stdlib" / "json.effekt",
    examplesDir / "stdlib" / "buffer.effekt",
  )
}
