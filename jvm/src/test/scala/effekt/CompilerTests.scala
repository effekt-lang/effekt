package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._
import org.junit.{ Test, Before }
import org.junit.Assert._

import sys.process._

import scala.language.implicitConversions

class CompilerTests {

  val srcFolder = new File("examples")
  val posFiles = (srcFolder / "pos") ** "*.effekt"
  val outFolder = new File("out")

  @Test def positives(): Unit =
    for (file <- posFiles.get) {
      val path = file.getParentFile
      val baseName = file.getName.stripSuffix(".effekt")
      val compileCheckfile = path / (baseName + ".compile.check")
      val runtimeCheckfile = path / (baseName + ".check")
      val checkfile = if (compileCheckfile.exists()) compileCheckfile else runtimeCheckfile

      val expected = if (checkfile.exists()) { IO.read(checkfile).toString } else { "" }

      val messageOut = compile(file.getPath)

      // currently examples in `pos` are either examples_pos_EX.js or EX.js
      val jsFile = {
        val unqualified = baseName.replace('/', '_') + ".js"
        val qualified   = "examples_pos_" + unqualified

        if ((outFolder / unqualified).exists()) { unqualified } else { qualified }
      }
      val jsScript = s"require('./out/${jsFile}').main().run()"

      val command = Process(Seq("node", "--eval", jsScript))
      val out = messageOut + command.!!

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
