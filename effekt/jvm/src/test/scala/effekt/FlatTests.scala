package effekt

import sbt.io.*
import sbt.io.syntax.*

import java.io.File
import scala.language.implicitConversions
import scala.sys.process.Process

// df: My own test class to run my requested tests automatically
class FlatTests extends EffektTests {

  def backendName = "llvm"
  
  override def valgrind = false // works on linux only
  
  override lazy val positives: Set[File] = Set(
    examplesDir / "flat" / "optimized"
  )
  
  override lazy val withoutOptimizations: Set[File] = Set(
    examplesDir / "flat" / "nooptimized",
  )
}
