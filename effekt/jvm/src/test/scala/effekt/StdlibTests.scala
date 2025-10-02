package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

abstract class StdlibTests extends EffektTests {
  override def positives: Set[File] = Set(
    examplesDir / "stdlib",
  )

  override def ignored: Set[File] = Set()

  override def withoutOptimizations: Set[File] = Set()
}

class StdlibJavaScriptTests extends StdlibTests {
  def backendName: String = "js"

  override def withoutOptimizations: Set[File] = Set(
    examplesDir / "stdlib" / "acme.effekt",
    examplesDir / "stdlib" / "json.effekt",
    examplesDir / "stdlib" / "exception" / "combinators.effekt",
    examplesDir / "stdlib" / "stream" / "fibonacci.effekt",
    examplesDir / "stdlib" / "list" / "flatmap.effekt",
    examplesDir / "stdlib" / "list" / "sortBy.effekt",
    examplesDir / "stdlib" / "stream" / "zip.effekt",
    examplesDir / "stdlib" / "stream" / "characters.effekt",
    examplesDir / "stdlib" / "list" / "deleteat.effekt",
    examplesDir / "stdlib" / "char" / "ascii_isalphanumeric.effekt",
    examplesDir / "stdlib" / "char" / "ascii_iswhitespace.effekt",
  )

  override def ignored: Set[File] = Set(
    examplesDir / "stdlib" / "network" / "streaming.effekt",
  )
}

abstract class StdlibChezTests extends StdlibTests {
  override def ignored: Set[File] = Set(
    // Not implemented yet
    examplesDir / "stdlib" / "io",
    examplesDir / "stdlib" / "stream" / "characters.effekt",
    examplesDir / "stdlib" / "stream" / "fuse_newlines.effekt",
    examplesDir / "stdlib" / "network" / "streaming.effekt",
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

  override def withoutOptimizations: Set[File] = Set(
    examplesDir / "stdlib" / "acme.effekt",
  )

  override def ignored: Set[File] = Set(
    // String comparison using `<`, `<=`, `>`, `>=` is not implemented yet on LLVM
    examplesDir / "stdlib" / "string" / "compare.effekt",

    // Wrong codegen for negative types, see #801
    examplesDir / "stdlib" / "json.effekt",
    examplesDir / "stdlib" / "buffer.effekt",
  )
}
