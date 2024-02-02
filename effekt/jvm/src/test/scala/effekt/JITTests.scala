package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JITTests extends EffektTests {

  def backendName = "jit"

  override def included: List[File] = List(
    examplesDir / "jit",
  )

  override def ignored: List[File] = List(
  )
}
