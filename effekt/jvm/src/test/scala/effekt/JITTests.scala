package effekt

import sbt.io.*
import sbt.io.syntax.*

import java.io.File
import scala.language.implicitConversions
import scala.sys.process.Process

class JITTests extends EffektTests {

  override lazy val included: List[File] = List(examplesDir / "pos" / "jit")

  override lazy val ignored: List[File] = List(
    // computes the wrong results
    examplesDir / "pos" / "jit" / "emit.effekt"
  )


  def runTestFor(input: File, check: File, expected: String) =
    test(input.getName + " (jit)") {
      val out = runJIT(input)
      assertNoDiff(out, expected)
    }

  def runJIT(f: File): String = {
    // TODO flaky body
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "jit",
      "--lib", "libraries/jit"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result()
  }
}
