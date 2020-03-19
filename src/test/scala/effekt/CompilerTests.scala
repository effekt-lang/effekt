package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._
import org.junit.Test
import org.junit.Assert._

import scala.language.implicitConversions

class CompilerTests {

  val srcFolder = new File("examples")
  val posFiles = (srcFolder / "pos") ** "*.effekt"

  @Test def positives(): Unit =
    for (file <- posFiles.get) {
      val path = file.getParentFile
      val baseName = file.getName.stripSuffix(".effekt")
      val checkfile = path / (baseName + ".compile.check")
      val expected = if (checkfile.exists()) { IO.read(checkfile).toString } else { "" }

      val out = compile(file.getPath)

      assertEquals(s"Wrong result: ${file.getPath}\n\n", expected, out)
    }

  def compile(filename: String): String = {
    object compiler extends effekt.Driver
    val configs = compiler.createConfig(Seq("--compile", "--Koutput", "string"))
    configs.verify()
    compiler.compileFile(filename, configs)
    configs.stringEmitter.result()
  }
}
