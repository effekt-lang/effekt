package effekt

import sbt.io.*
import sbt.io.syntax.*

import java.io.File
import scala.language.implicitConversions
import scala.sys.process.Process

class JITTests extends EffektTests {

  override lazy val included: List[File] = List(examplesDir / "jit")

  override lazy val ignored: List[File] = List(
    // polymorphic effect operations not supported, yet
    examplesDir / "jit" / "choice.effekt",
    examplesDir / "jit" / "triples.effekt",
  )


  def runTestFor(input: File, check: File, expected: String) =
    test(input.getName + " (jit)") {
      val out = runJIT(input)
      assertNoDiff(out, expected)
    }

  private def createConfig(compiler: Driver = new effekt.Driver {}): EffektConfig = {
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "jit",
      "--lib", "libraries/jit"
    ))
    configs.verify()
    configs
  }

  def runJIT(f: File): String = {
    // TODO flaky body
    val compiler = new effekt.Driver {}
    val configs = createConfig(compiler)
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result()
  }

  def canRun(): Boolean = {
    val arch = Process(Seq(s"uname", "-m")).!!.trim
    val os = Process(Seq(s"uname", "-s")).!!.trim
    val platform = "%s-%s".format(arch, os)
    val configs = createConfig()
    configs.findJITBinary(platform) match {
      case Left(_) => false
      case Right(jitBinary) =>
        canRunExecutable(jitBinary.unixPath, "--check")
    }
  }
}
