package effekt

import sbt.io.*
import sbt.io.syntax.*

import java.io.File
import scala.language.implicitConversions
import scala.sys.process.Process

class CTRCTests extends EffektTests {

  def backendName = "llvm"
  
  override def valgrind = false // works on linux only
  
  override lazy val positives: Set[File] = Set(
    examplesDir / "ctrc" / "optimized"
  )
  
  override lazy val withoutOptimizations: Set[File] = Set(
    examplesDir / "ctrc" / "nooptimized",
  )
}
