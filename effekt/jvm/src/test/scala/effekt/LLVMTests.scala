package effekt

import java.io.File
import org.scalatest.funspec.AnyFunSpec
import sbt.io._
import sbt.io.syntax._
import scala.language.implicitConversions
import scala.sys.process.Process

class LLVMTests extends AnyFunSpec {
  Process(Seq("rm", "-rf", "out/")).!! // TODO globally configure sbt to clear out before running tests
  go(new File("tests-llvm"))

  def go(root: File): Unit = describe(root.getName) {
    root.listFiles.foreach {
      case h if h.getName.startsWith(".") => ()
      case d if d.isDirectory             => go(d)
      case e if e.getName.endsWith(".effekt") =>
        val dirname = e.getParentFile // TODO ugly
        val c = dirname / (e.getName.stripSuffix(".effekt") + ".check")
        if (!c.exists())
          sys error s"missing checkfile: ${e.getPath}"
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
      "--generator", "llvm",
      "--lib", "libraries/llvm"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "") // XXX this is a hack
  }
}
