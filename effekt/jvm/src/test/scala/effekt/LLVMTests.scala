package effekt

import java.io.File
import org.scalatest.funspec.AnyFunSpec
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends AnyFunSpec {

  lazy val examplesDir = new File("examples")

  findAllTests(examplesDir / "llvm")

  // Unix jargon for odd Scala names:
  // `.getName` -> "base"
  // `getParentFile` -> "dir"
  def findAllTests(root: File): Unit = describe(root.getName) {
    root.listFiles.foreach {
      case h if h.getName.startsWith(".") => ()
      case d if d.isDirectory             => findAllTests(d)
      case e if e.getName.endsWith(".effekt") =>
        val dirname = e.getParentFile
        val c = dirname / (e.getName.stripSuffix(".effekt") + ".check")
        if (!c.exists())
          sys error s"missing checkfile for: ${e.getPath}"
        it(e.getName) {
          assert(IO.read(c).toString == run(e))
        }
      case _ => ()
    }
  }

  def run(f: File): String = {
    // TODO flaky body
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--backend", "llvm",
      "--lib", "libraries/llvm"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "") // XXX this is a hack
  }
}
