package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JITTests extends EffektTests {

  def backendName = "jit"

  override def positives: List[File] = List(
    examplesDir / "jit",
  )

  override def ignored: List[File] = List(
    examplesDir / "jit" / "polymorphism_map.effekt",
    examplesDir / "jit" / "polymorphic_failtooption.effekt",
    examplesDir / "jit" / "polymorphism_blockparams.effekt",
  )
}
