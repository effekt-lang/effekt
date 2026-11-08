package effekt

import effekt.util.PlainMessaging
import effekt.util.messages.Diagnostic
import effekt.util.messages.Severity

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

import scala.sys.process.*
import scala.language.implicitConversions

trait EffektTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "m")

  // The name of the backend as it is passed to the --backend flag.
  def backendName: String

  // Whether to execute using valgrind
  def valgrind = false

  // Whether to execute using debug mode
  def debug = false

  def output: File = new File(".") / "out" / "tests" / getClass.getName.toLowerCase

  // The sources of all testfiles are stored here:
  def examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  def ignored: Set[File] = Set()

  // Folders to discover and run tests in
  def positives: Set[File] = Set()

  def negatives: Set[File] = Set()

  // Test files that should be run with optimizations disabled
  def withoutOptimizations: Set[File] = Set()

  // Use `EFFEKT_UPDATE_CHECKFILES=1 sbt testBackendJS` to update check files instead of failing (then review with `git diff`)
  def updateCheckFiles = sys.env.get("EFFEKT_UPDATE_CHECKFILES").nonEmpty

  def snapshotTest(input: File, mode: String, expected: Option[String])(obtained: => String): Unit =
    test(s"${input.getPath} (${mode})") {
      if (updateCheckFiles) {
        CheckFiles.update(input, s"${getClass.getSimpleName} (${mode})", obtained, expected).foreach { conflict => fail(conflict) }
      } else {
        assertNoDiff(obtained, expected.getOrElse { fail(s"Missing checkfile for ${input.getPath}") })
      }
    }

  override def afterAll(): Unit =
    if (updateCheckFiles) CheckFiles.report(getClass.getSimpleName, negatives ++ positives ++ withoutOptimizations)

  def runTestFor(input: File, expected: String): Unit =
    test(input.getPath + s" (${backendName})") {
      assertNoDiff(run(input, true), expected)
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

  def run(input: File, optimizations: Boolean): String =
    val compiler = driver
    var options = Seq(
      "--Koutput", "string",
      "--backend", backendName,
      "--out", output.getPath,
    )
    if (valgrind) options = options :+ "--valgrind"
    if (debug) options = options :+ "--debug"
    if (!optimizations) options = options :+ "--no-optimize"
    val configs = compiler.createConfig(options)
    configs.verify()

    // reuse state after compiling a trivial file
    driver.context.restore(state)
    compiler.compileFile(input.getPath, configs)
    configs.stringEmitter.result()

  def compile(input: File): List[Diagnostic] =
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
        withoutOptimizations.foreach(runWithoutOptimizations)
    }

  def runWithoutOptimizations(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, None) if !updateCheckFiles => sys error s"Missing checkfile for ${f.getPath}"
      case (f, expected) => snapshotTest(f, s"${backendName}-noopt", expected) { run(f, false) }
    }

  def runPositiveTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, None) if !updateCheckFiles => sys error s"Missing checkfile for ${f.getPath}"
      case (f, expected) => snapshotTest(f, backendName, expected) { run(f, true) }
    }

  def runNegativeTestsIn(dir: File): Unit =
    foreachFileIn(dir) {
      case (f, None) if !updateCheckFiles || hasInlineExpectations(f) =>
        test(s"${f.getPath} (${backendName})") {
          validateErrors(f, compile(f))
        }
      case (f, expected) => snapshotTest(f, backendName, expected) { run(f, true) }
    }

  def validateErrors(f: File, messages: List[Diagnostic]): Unit =
    import scala.util.matching.Regex
    val messaging = new PlainMessaging
    val messageWithFormat = messages.map { msg => (msg, messaging.formatContent(msg)) }

    assert(messages.nonEmpty, s"File ${f} is supposed to report at least one error.")

    def findError(severity: Severity, lineNo: Int, contents: String): Unit =
      val couldFind = messageWithFormat.exists {
        case (msg, formatted) =>
          def rightSeverity = msg.severity == severity
          def rightLine = msg.range.exists(r => r.from.line == lineNo)
          def containsText = formatted.contains(contents.strip)
          rightSeverity && rightLine && containsText
      }
      assert(couldFind,
        s"""Could not find an message [${severity.toString.toLowerCase}] line ${lineNo}: ${contents}
           |
           |Compilation resulted in the following messages
           |----------------------------------------------
           |${ messages.map(m => messaging.formatMessage(m)).distinct.mkString("\n\n") }""".stripMargin)

    // for each line with a comment, check whether a corresponding message exists
    IO.readLines(f).zipWithIndex.foreach {
      case (line, no) =>
        inlineExpectation.findFirstMatchIn(line).foreach { res =>
          val severity = res.group(1) match {
            case "ERROR" => Severity.Error
            case "WARN" => Severity.Warning
            case _ => fail("Internal error: Should not happen")
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
        if (!ignored.contains(f)) {
          test(f, expectedResultFor(f))
        }
      case _ => ()
    }

  runTests()
}

def checkFileFor(f: File): File =
  f.getParentFile / (f.getName.stripSuffix(".md").stripSuffix(".effekt") + ".check")

def expectedResultFor(f: File): Option[String] = {
  val checkfile = checkFileFor(f)
  if checkfile.exists() then Some(IO.read(checkfile)) else None
}

/** A line of a negative test stating a diagnostic it expects, for example `// ERROR Expected Int`. */
val inlineExpectation = """//\s*(ERROR|WARN)\s*(.*)$""".r

/** A negative test with inline expectations checks only those, so it has no check file. */
def hasInlineExpectations(f: File): Boolean =
  IO.readLines(f).exists { line => inlineExpectation.findFirstIn(line).nonEmpty }

object CheckFiles {
  private val written = java.util.concurrent.ConcurrentHashMap[File, (String, String)]()
  private case class Change(writer: String, kind: String, checkfile: File)
  private val changes = java.util.concurrent.ConcurrentLinkedQueue[Change]()

  private def same(obtained: String, expected: String): Boolean = munit.diff.Diff(obtained, expected).isEmpty

  /** Writes the check file for `input` unless it already says `obtained`; returns a conflict, if any. */
  def update(input: File, writer: String, obtained: String, expected: Option[String]): Option[String] = {
    val checkfile = checkFileFor(input).getCanonicalFile
    val content = obtained.replaceAll("\u001B\\[[;\\d]*m", "")
    val first = written.putIfAbsent(checkfile, (writer, content))

    if (first != null && !same(content, first._2)) {
      Some(s"${writer} and ${first._1} disagree on ${checkFileFor(input).getPath}; it keeps the output of ${first._1}")
    } else {
      if (!expected.exists { e => same(content, e) }) {
        // keep the file's own ending, so that the diff shows only what the test compares
        val toWrite = expected.fold(content) { e => if (e.endsWith("\n")) content else content.replaceAll("\n+\\z", "") }
        IO.write(checkfile, toWrite)
        changes.add(Change(writer, if (expected.isEmpty) "added" else "updated", checkFileFor(input)))
      }
      None
    }
  }

  /** Prints which check files `suite` changed, and the prphaned check files in `dirs` (without a test). */
  def report(suite: String, dirs: Set[File]): Unit = {
    import scala.jdk.CollectionConverters.*
    changes.asScala
      .filter { c => c.writer.startsWith(suite + " ") }
      .map { c => s"${c.kind}: ${c.checkfile.getPath}" }
      .toList.distinct.sorted.foreach(println)

    def orphans(f: File): List[File] =
      if (f.isDirectory) {
        f.listFiles.toList.flatMap(orphans)
      } else {
        val base = f.getPath.stripSuffix(".check")
        if (f.getName.endsWith(".check") && !File(base + ".effekt").exists && !File(base + ".effekt.md").exists) List(f) else Nil
      }
    dirs.toList.flatMap(orphans).map(_.getPath)
        .distinct.sorted.foreach { f => println(s"orphaned: $f") }
  }
}

