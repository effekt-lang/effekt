package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.language.implicitConversions

class MLTests extends EffektTests {

  override lazy val included: List[File] = List(examplesDir)

  override lazy val ignored: List[File] = List()

  def runTestFor(input: java.io.File, check: File, expected: String): Unit =
    test(input.getPath + " (ml)") {
      val out = runML(input)
      assertNoDiff(out, expected)
    }

  def runML(input: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "ml"
    ))
    configs.verify()
    compiler.compileFile(input.getPath, configs)
    configs.stringEmitter.result()
  }
}
