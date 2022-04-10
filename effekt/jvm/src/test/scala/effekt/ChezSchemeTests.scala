package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class ChezSchemeTests extends AnyFunSpec {

  // The sources of all testfiles are stored here:
  lazy val examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  lazy val ignored: List[File] = List(
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "maps.effekt",

    // bidirectional effects are not yet supported in our Chez backend
    examplesDir / "pos" / "bidirectional",

    // unsafe continuations are not yet supported in our Chez backend
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt",

    // the number representations differ in JS and Chez
    examplesDir / "casestudies" / "ad.md",

    // we do not need to run the negative tests for the other backends
    examplesDir / "neg",

    examplesDir / "pos" / "lambdas" / "simpleclosure.effekt" // doesn't work with lift inference, yet
  )

  runTestsIn(examplesDir)

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory && !ignored.contains(f) =>
        runTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        if (ignored.contains(f)) {
          ignore(f.getName) { () }
        } else {
          it(f.getName + " (callcc)") {

            val out = interpretCS(f, "callcc")
            if (checkfile.exists()) {
              assert(IO.read(checkfile).toString == out)
            }
          }
          it(f.getName + " (lift)") {

            val out = interpretCS(f, "lift")
            if (checkfile.exists()) {
              assert(IO.read(checkfile).toString == out)
            }
          }
          it(f.getName + " (monadic)") {

            val out = interpretCS(f, "monadic")
            if (checkfile.exists()) {
              assert(IO.read(checkfile).toString == out)
            }
          }
        }

      case _ => ()
    }
  }

  def interpretCS(file: File, variant: String): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--generator", s"chez-$variant",
      "--includes", "chez/common",
      "--includes", ".",
      "--lib", s"chez/$variant"
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }
}
