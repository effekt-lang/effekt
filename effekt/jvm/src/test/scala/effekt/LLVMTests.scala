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

  lazy val ignored: List[File] = List(
    // computes the wrong results
    examplesDir / "llvm" / "generator.effekt",
    examplesDir / "llvm" / "capabilities.effekt",
    examplesDir / "llvm" / "nested.effekt",
    examplesDir / "llvm" / "stored.effekt",

    // crashes
    examplesDir / "llvm" / "forking.effekt",

    // polymorphic effect operations not supported, yet
    examplesDir / "llvm" / "choice.effekt",
    examplesDir / "llvm" / "triples.effekt",

    // mutable state not support, yet
    examplesDir / "llvm" / "gids.effekt",

    // strings not supported, yet
    examplesDir / "llvm" / "hello-world.effekt"
  )

  def findAllTests(root: File): Unit = describe(root.getName) {
    // Reminder: mapping of Scala names to Unix jargon:
    // `.getName` -> "base"
    // `.getParentFile` -> "dir"
    root.listFiles.foreach {
      case h if h.getName.startsWith(".") => ()
      case d if d.isDirectory             => findAllTests(d)
      case e if e.getName.endsWith(".effekt") && !ignored.contains(e) =>
        val c = e.getParentFile / (e.getName.stripSuffix(".effekt") + ".check")
        if (!c.exists())
          sys error s"Missing checkfile for: ${e.getPath}"
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
