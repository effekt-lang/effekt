package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends EffektTests {

  override lazy val included: List[File] = List(examplesDir / "llvm")

  override lazy val ignored: List[File] = List(
    // computes the wrong results
    examplesDir / "llvm" / "nested.effekt",

    // polymorphic effect operations not supported, yet
    examplesDir / "llvm" / "choice.effekt",
    examplesDir / "llvm" / "triples.effekt",
  )

  def runTestFor(input: File, check: File, expected: String): Unit = {
    test(input.getPath + " (llvm)") {
      val out = runLLVM(input)
      assertNoDiff(out, expected)
    }
  }

  def runLLVM(f: File): String = {
    // TODO flaky body
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "llvm",
      "--lib", "libraries/llvm"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result()
  }
}
