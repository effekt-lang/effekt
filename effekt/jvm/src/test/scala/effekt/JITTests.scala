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
    examplesDir / "jit" / "polymorphism_int.effekt",
    examplesDir / "jit" / "polymorphism_map.effekt",
    examplesDir / "jit" / "polymorphic_failtooption.effekt",
    examplesDir / "jit" / "fail.effekt",
    examplesDir / "jit" / "stored.effekt",
    examplesDir / "jit" / "mutual.effekt",
    examplesDir / "jit" / "generator.effekt",
    examplesDir / "jit" / "nested.effekt",
    examplesDir / "jit" / "polymorphism_blockparams.effekt",
    examplesDir / "jit" / "emit.effekt",
    examplesDir / "jit" / "triples.effekt",
    examplesDir / "jit" / "recursive.effekt",
    examplesDir / "jit" / "blockparams.effekt",
    examplesDir / "jit" / "capabilities.effekt",
    examplesDir / "jit" / "gids.effekt",
    examplesDir / "jit" / "polymorphism_data.effekt",
    examplesDir / "jit" / "issue207.effekt",
    examplesDir / "jit" / "failtooption.effekt",
    examplesDir / "jit" / "forking.effekt",
    examplesDir / "jit" / "choice.effekt",
    examplesDir / "jit" / "higher_order_overloading.effekt",
  )
}
