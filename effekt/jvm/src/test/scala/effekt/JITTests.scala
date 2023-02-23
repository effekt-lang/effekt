package effekt

import sbt.io.*
import sbt.io.syntax.*

import java.io.File
import scala.language.implicitConversions
import scala.sys.process.Process

class JITTests extends EffektTests {

  override def backendName: String = "jit"

  override lazy val included: List[File] = List(examplesDir / "jit")

  override lazy val ignored: List[File] = List(
  )
}
