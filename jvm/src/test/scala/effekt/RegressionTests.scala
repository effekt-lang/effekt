package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import scala.util.matching._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class RegressionTests extends AnyFunSpec with TestUtils {
  prepareTestsIn(examplesDir)
  runTestsIn(targetDir)

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory => runTestsIn(f)
      case f if f.getName.endsWith(".effekt") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        it(f.getName) {
          val out = interpret(f)

          if (checkfile.exists()) {
            assert(IO.read(checkfile).toString == out)
          }
        }
      case _ => ()
    }
  }
}

trait TestUtils {

  // The sources of all testfiles are stored here:
  lazy val examplesDir = new File("examples")

  // All testfiles are copied to this directory and .md files are converted to .effekt files
  lazy val targetDir = new File("jvm") / "target" / "test"

  lazy val fenceLine = """^```[^`\n]*$""".r

  /**
   * Generates .effekt files from .md files; copies all other files
   */
  def prepareTestsIn(dir: File): Unit = dir.listFiles.foreach {
    case f if f.isDirectory             => prepareTestsIn(f)
    case f if f.getName.endsWith(".md") => prepareTestsFor(f)
    case f =>
      val targetFile = targetDir / f.relativeTo(examplesDir).get.toString
      IO.copyFile(f, targetFile)
  }

  /**
   * Generates .effekt files from .md files; copies all other files
   */
  def prepareTestsFor(mdFile: File): Unit = {

    var inCode = false

    val lines = IO.readLines(mdFile).flatMap {
      case line if fenceLine.matches(line) =>
        inCode = !inCode
        None
      case line if inCode =>
        Some(line)
      case _ => None
    }

    IO.writeLines(targetFor(mdFile), lines)
  }

  def interpret(file: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq("--Koutput", "string", "--lib", "lib"))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }

  def targetFor(srcFile: File): File = {
    val fileInTarget = targetDir / srcFile.relativeTo(examplesDir).getOrElse {
      sys error s"Cannot compute relative path to examples directory for ${srcFile}"
    }.toString

    if (fileInTarget.name.endsWith(".md")) {
      val baseName = fileInTarget.getPath.stripSuffix(".md")
      new File(baseName + ".effekt")
    } else {
      fileInTarget
    }
  }

  def sourceFor(targetFile: File): File =
    examplesDir / targetFile.relativeTo(targetDir).getOrElse {
      sys error s"Cannot compute relative path to target directory for ${targetFile}"
    }.toString

  /**
   * Generates the check files from the actual outputs.
   *
   * Call from sbt with:
   *    > project effektJVM
   *    > test:console
   *    scala> effekt.TestUtils.generateCheckFiles()
   *
   * Check afterwards with:
   *    git diff
   */
  def generateCheckFilesIn(dir: File): Unit = {
    println(s"Generating check files in folder: ${dir.getPath}")
    dir.listFiles.foreach {
      case f if f.isDirectory => generateCheckFilesIn(f)
      case f if f.getName.endsWith(".effekt") =>
        println(s"Writing checkfile for ${f}")
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".effekt")
        val checkfile = path / (baseName + ".check")

        val out = interpret(f)
        // save checkfile in source folder (e.g. examples/)
        IO.write(sourceFor(checkfile), out)
      case _ => ()
    }
  }

  def generateCheckFiles(): Unit = {
    prepareTestsIn(examplesDir)
    generateCheckFilesIn(targetDir)
  }
}
object TestUtils extends TestUtils
