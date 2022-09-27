package effekt

import java.io.File
import org.scalatest.funspec.AnyFunSpec
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class MLTests extends EffektTests {

  override lazy val included: List[File] = List(examplesDir / "ml")

  override lazy val ignored: List[File] = List()

  def runTestFor(f: File, expected: String) =
    it(f.getName + " (ml)") {
      val out = runML(f)
      assert(expected == out)
    }

  def runML(f: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "ml"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    removeAnsiColors(configs.stringEmitter.result())
  }
}
