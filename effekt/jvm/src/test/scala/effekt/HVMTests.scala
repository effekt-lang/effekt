package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class HVMTests extends EffektTests {

  def backendName = "hvm"

  override lazy val positives: List[File] = List(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "benchmarks"
  )

}
