package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import scala.language.implicitConversions


class JavaScriptTests extends EffektTests {

  def backendName = "js"

  override def positives: Set[File] = Set(
    examplesDir / "pos",
    examplesDir / "casestudies",
    examplesDir / "benchmarks",
  )

  override def negatives: Set[File] = Set(
    examplesDir / "neg"
  )

  override lazy val withoutOptimizations: Set[File] = Set(
    // contifying under reset
    examplesDir / "pos" / "issue842.effekt",
    examplesDir / "pos" / "issue861.effekt",

    // contifying a block that is also passed as an argument
    examplesDir / "pos" / "contify_block_used_as_value.effekt",

    // syntax error (multiple declaration)
    examplesDir / "pos" / "parser.effekt",
    examplesDir / "pos" / "probabilistic.effekt",
    
    // local function def needs to be rewritten (issue 1229)
    examplesDir / "benchmarks" / "folklore_to_fact" / "motzkin.effekt",
  )

  override def ignored: Set[File] = super.ignored ++ Set(
    // unsafe cont
    examplesDir / "pos" / "propagators.effekt",
    // stack overflow
    examplesDir / "benchmarks" / "folklore_to_fact" / "cps_tak.effekt",
  )
}
