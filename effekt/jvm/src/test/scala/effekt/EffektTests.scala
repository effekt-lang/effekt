package effekt

import effekt.util.PlainMessaging
import effekt.util.messages.EffektError
import kiama.util.Severities
import kiama.util.Severities.Severity

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.sys.process.*
import scala.language.implicitConversions

trait EffektTests extends munit.FunSuite {

  // The name of the backend as it is passed to the --backend flag.
  def backendName: String

  // Whether to execute using valgrind
  def valgrind = false

  def output: File = new File(".") / "out" / "tests" / getClass.getName.toLowerCase

  // The sources of all testfiles are stored here:
  def examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  def ignored: List[File] = List()

  // Folders to discover and run tests in
  def positives: List[File] = List()

  def negatives: List[File] = List()

  def runTestFor(input: File, expected: String): Unit =
    test(input.getPath + s" (${backendName})") {
      assertNoDiff(run(input), expected)
    }

  // one shared driver for all tests in this test runner
  object driver extends effekt.Driver

  lazy val state: driver.context.State = warmup(file("empty.effekt"))

  def warmup(input: File): driver.context.State =
    val compiler = driver
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--compile",
      "--no-exit-on-error",
      "--backend", backendName,
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(input.getPath, configs)
    compiler.context.backup

  def run(input: File): String =
    val compiler = driver
    var options = Seq(
      "--Koutput", "string",
      "--backend", backendName,
      "--out", output.getPath,
      "--debug",
    )
    if (valgrind) options = options :+ "--valgrind"
    val configs = compiler.createConfig(options)
    configs.verify()

    // reuse state after compiling a trivial file
    driver.context.restore(state)
    compiler.compileFile(input.getPath, configs)
    configs.stringEmitter.result()

  def compile(input: File): List[EffektError] =
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--compile",
      "--no-exit-on-error",
      "--backend", backendName,
      "--out", output.getPath
    ))
    configs.verify()
    compiler.compileFile(input.getPath, configs)
    compiler.messaging.get.toList


  def runTests() =
    Backend.backend(backendName).runner.checkSetup() match {
      case Left(msg) => test(s"${this.getClass.getName}: ${msg}".ignore) { () }
      case Right(value) =>
        negatives.foreach(runNegativeTestsIn)
        positives.foreach(runPositiveTestsIn)
    }

  def runPositiveTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, None) => sys error s"Missing checkfile for ${f.getPath}"
      case (f, Some(expected)) =>
        test(s"${f.getPath} (${backendName})") {
          assertNoDiff(run(f), expected)
        }
    }

  def runNegativeTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, Some(expected)) =>
        test(s"${f.getPath} (${backendName})") {
          assertNoDiff(run(f), expected)
        }

      case (f, None) =>
        test(s"${f.getPath} (${backendName})") {
          validateErrors(f, compile(f))
        }
    }

  def validateErrors(f: File, messages: List[EffektError]): Unit =
    import scala.util.matching.Regex
    val messaging = new PlainMessaging
    val messageWithFormat = messages.map { msg => (msg, messaging.formatContent(msg)) }

    assert(messages.nonEmpty, s"File ${f} is supposed to report at least one error.")

    val rx = """//\s*(ERROR|WARN)\s*(.*)$""".r

    def findError(severity: Severity, lineNo: Int, contents: String): Unit =
      val couldFind = messageWithFormat.exists {
        case (msg, formatted) =>
          def rightSeverity = msg.severity == severity
          def rightLine = msg.startPosition.exists(p => p.line == lineNo)
          def containsText = formatted.contains(contents.strip)
          rightSeverity && rightLine && containsText
      }
      assert(couldFind,
        s"""Could not find an message [${Severities.severityToWord(severity)}] line ${lineNo}: ${contents}
           |
           |Compilation resulted in the following messages
           |----------------------------------------------
           |${ messages.map(m => messaging.formatMessage(m)).distinct.mkString("\n\n") }""".stripMargin)

    // for each line with a comment, check whether a corresponding message exists
    IO.readLines(f).zipWithIndex.foreach {
      case (line, no) =>
        rx.findFirstMatchIn(line).foreach { res =>
          val severity = res.group(1) match {
            case "ERROR" => Severities.Error
            case "WARN" => Severities.Warning
          }
          findError(severity, no + 1, res.group(2))
        }
    }

    assert(messages.nonEmpty)

  def foreachFileIn(file: File)(test: (File, Option[String]) => Unit): Unit =
    file match {
      case f if f.isDirectory && !ignored.contains(f) =>
        f.listFiles.foreach(foreachFileIn(_)(test))
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".effekt.md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        if (ignored.contains(f)) {
          ignored.contains(f)
        } else {
          val checkfile = path / (baseName + ".check")
          val expected = if checkfile.exists() then Some(IO.read(checkfile)) else None

          test(f, expected)
        }
      case _ => ()
    }

  runTests()
}
