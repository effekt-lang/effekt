package effekt

import java.io.File

import effekt.util.paths.file
import kiama.util.REPLConfig

import org.rogach.scallop.ScallopOption
import org.rogach.scallop.{ fileConverter, fileListConverter, stringConverter, stringListConverter, longConverter }

class EffektConfig(args: Seq[String]) extends REPLConfig(args) {

  val compile: ScallopOption[Boolean] = toggle(
    "compile",
    descrYes = "Compile the Effekt program to the backend specific representation",
    default = Some(false)
  )

  val build: ScallopOption[Boolean] = toggle(
    "build",
    descrYes = "Compile the Effekt program and build a backend specific executable",
    default = Some(false)
  )

  val showIR: ScallopOption[String] = choice(
    choices = List("none", "source", "core", "lifted-core", "machine", "target"),
    name = "showIR",
    descr = "The intermediate presentation that should be printed.",
    default = Some("none"),
    noshort = true
  )

  val showIRs: ScallopOption[Boolean] = toggle(
    "showIRs",
    descrYes = "Write all IRs to files",
    default = Some(false),
  )

  val time: ScallopOption[String] = choice(
    choices = Seq("text", "json"),
    name = "time",
    descr = "Measure the time spent in each compilation phase",
    required = false
  )

  val outputPath: ScallopOption[File] = opt[File](
    "out",
    descr = "Path to write generated files to (defaults to ./out)",
    default = Some(new File("./out")),
    required = false
  )

  val includePath: ScallopOption[List[File]] = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."))),
    noshort = true
  )

  val stdlibPath: ScallopOption[File] = opt[File](
    "lib",
    descr = "Path to the standard library to be used",
    required = false
  )

  val backend: ScallopOption[Backend[_]] = choice(
    choices = List("js", "chez-callcc", "chez-monadic", "chez-lift", "llvm", "ml"),
    name = "backend",
    descr = "The backend that should be used",
    default = Some("js"),
    noshort = true
  ).map(Backend.backend)

  val llvmVersion: ScallopOption[String] = opt[String](
    "llvm-version",
    descr = "the llvm version that should be used to compile the generated programs (only necessary if backend is llvm, defaults to 15)",
    default = Some(sys.env.getOrElse("EFFEKT_LLVM_VERSION", "15")),
    noshort = true
  )

  val preludePath: ScallopOption[List[String]] = opt[List[String]](
    "prelude",
    descr = "Modules to be automatically imported in every file",
    default = None,
    noshort = true
  )

  val exitOnError: ScallopOption[Boolean] = toggle(
    "exit-on-error",
    descrYes = "Exit with non-zero exit code on error",
    default = Some(!repl() && !server()),
    noshort = true
  )

  val optimize: ScallopOption[Boolean] = toggle(
    "optimize",
    descrYes = "Run optimizations (in particular the inliner) when compiling programs",
    default = Some(true),
    short = 'O'
  )

  val maxInlineSize: ScallopOption[Long] = opt(
    "max-inline-size",
    descr = "Maximum size (number of core tree-nodes) of a function considered by the inliner",
    default = Some(50L),
    noshort = true
  )

  /**
   * Tries to find the path to the standard library. Proceeds in the following
   * order:
   * 1) specified as part of the settings arg `lib`?
   * 2) specified in an environment variable `EFFEKT_LIB`
   * 3) relative to the current working directory
   * 4) relative to to the executed JAR file (effekt.jar)
   */
  def findStdLib: util.paths.File = {

    def backendStdLibPath(path: util.paths.File) =
      backend().runner.standardLibraryPath(path)

    // 1) in config?
    if (stdlibPath.isDefined) {
      return stdlibPath()
    }

    // 2) in PATH
    if (System.getenv.containsKey("EFFEKT_LIB")) {
      return System.getenv("EFFEKT_LIB")
    }

    // 3) in PWD
    val pwd = file(".")
    val localLib = backendStdLibPath(pwd)

    if ((localLib / "effekt.effekt").exists) {
      return localLib
    }

    // 4) next to Jar
    val jarPath = try {
      file(getClass.getProtectionDomain.getCodeSource.getLocation.toURI).parent
    } catch {
      case e: Throwable =>
        sys.error("Cannot find path to standard library")
    }

    val jarLib = backendStdLibPath(jarPath / "..")
    if ((jarLib / "effekt.effekt").exists) {
      return jarLib
    }

    sys.error("Cannot find path to standard library")
  }

  lazy val libPath: File = findStdLib.canonicalPath.toFile

  def includes(): List[File] = libPath :: backend().runner.includes(libPath).map(_.toFile) ++ includePath()

  def prelude(): List[String] = preludePath.getOrElse(backend().runner.prelude)

  def requiresCompilation(): Boolean = !server()

  def interpret(): Boolean = !server() && !compile() && !build()

  def repl(): Boolean = filenames().isEmpty && !server() && !compile()

  def timed(): Boolean = time.isSupplied && !server()

  validateFilesIsDirectory(includePath)

  // force some other configs manually to initialize them when compiling with native-image
  server; output; filenames
}
