package effekt

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.language.implicitConversions

class PartialTypeSignatureTest extends EffektTests {
  def backendName: String = "pts"

  override lazy val included: List[File] = List(
    examplesDir / "pts"
  )

  override lazy val ignored: List[File] = List(
  )
}
