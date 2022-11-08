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

    // Primitive missing: Boolean ==
    examplesDir / "jit" / "boolean-algebra.effekt",
    examplesDir / "jit" / "boolean-algebra-with-literals.effekt",

    // mutable state not supported, yet
    examplesDir / "jit" / "gids.effekt",
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
