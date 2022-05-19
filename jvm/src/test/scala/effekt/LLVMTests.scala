package effekt

import java.io.File
import sbt.io._
import sbt.io.syntax._
//import scala.util.matching._
import org.scalatest.funspec.AnyFunSpec
import scala.language.implicitConversions

class LLVMTests extends AnyFunSpec {
  val root = new File("examples-llvm")
  go(root)

  def go(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case d if d.isDirectory => go(d)
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
      "--lib", "llvm"
    ))
    configs.verify()
    compiler.compileFile(f.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "") // XXX this is a hack
  }
}
