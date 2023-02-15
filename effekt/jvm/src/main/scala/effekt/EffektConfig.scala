package effekt

import java.io.File

import effekt.util.paths.file
import kiama.util.REPLConfig

import org.rogach.scallop.ScallopOption
import org.rogach.scallop.{ fileConverter, fileListConverter, stringConverter, stringListConverter }

class EffektConfig(args: Seq[String]) extends REPLConfig(args) {

  val compile: ScallopOption[Boolean] = toggle(
    "compile",
    descrYes = "Compile the Effekt program",
    descrNo = "Run the effekt program in the interpreter",
    default = Some(false)
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

  val jitBinaryPath: ScallopOption[File] = opt[File](
    "jit-binary",
    descr = "Path to JIT binary to be used",
    required = false
  )

  val backend: ScallopOption[String] = choice(
    choices = List("js", "chez-callcc", "chez-monadic", "chez-lift", "llvm", "jit", "ml"),
    name = "backend",
    descr = "The backend that should be used",
    default = Some("js"),
    noshort = true
  )

  val llvmVersion: ScallopOption[String] = opt[String](
    "llvm-version",
    descr = "the llvm version that should be used to compile the generated programs (only necessary if backend is llvm, defaults to 12)",
    default = Some(sys.env.getOrElse("EFFEKT_LLVM_VERSION", "12")),
    noshort = true
  )

  val preludePath: ScallopOption[List[String]] = opt[List[String]](
    "prelude",
    descr = "Modules to be automatically imported in every file.",
    default = None,
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

  /**
   * Tries to find the path to the JIT binary. Proceeds in the following order:
   * 1) specified as part of the settings arg `jit-binary`
   * 2) specified in an environment variable `EFFEKT_JIT_BIN`
   * 3) relative to the current working directory
   * 4) relative to the executed JAR file (effekt.jar)
   */
  def findJITBinary(platform: String): effekt.util.paths.File = {
    import effekt.util.paths.file

    val binaryName = s"${platform}/rpyeffect-jit";

    // 1) in config
    if(jitBinaryPath.isDefined) {
      return jitBinaryPath()
    }

    // 2) iin Environment variable EFFEKT_BIN
    if (System.getenv.containsKey("EFFEKT_JIT_BIN")) {
      return System.getenv("EFFEKT_JIT_BIN")
    }

    // 3) in PWD
    val pwd = file(".")
    if((pwd / "bin" / binaryName).exists) {
      return (pwd / "bin" / binaryName)
    }

    // 4) next to JAR
    val jarPath = effekt.util.paths.file(getClass.getProtectionDomain.getCodeSource.getLocation.toURI).parent;
    if((jarPath / binaryName).exists) {
      return (jarPath / binaryName)
    }

    sys error "Cannot find path to the JIT binary"
  }

  lazy val libPath: File = findStdLib.canonicalPath.toFile

  def includes(): List[File] = backendIncludes(libPath).map(_.toFile) ++ includePath()

  def prelude(): List[String] = preludePath.getOrElse(backendPrelude())

  def requiresCompilation(): Boolean = !server()

  def interpret(): Boolean = !server() && !compile()

  private def backendStdLibPath(path: util.paths.File): util.paths.File = backend() match {
    case "js" => path / "libraries" / "js"
    case "chez-monadic" => path / "libraries" / "chez" / "monadic"
    case "chez-callcc" => path / "libraries" / "chez" / "callcc"
    case "chez-lift" => path / "libraries" / "chez" / "lift"
    case "llvm" => path / "libraries" / "llvm"
    case "jit" => path / "libraries" / "jit"
    case "ml" => path / "libraries" / "ml"
    case b => sys error s"Unrecognized backend ${ b }"
  }

  private def backendIncludes(path: util.paths.File): List[util.paths.File] = backend() match {
    case "chez-monadic" | "chez-callcc" | "chez-lift" => List(path, path / ".." / "common")
    case b => List(path)
  }

  private def backendPrelude() = backend() match {
    case "js" | "chez-monadic" | "chez-callcc" | "chez-lift" =>
      List("effekt", "immutable/option", "immutable/list")
    case "ml" =>
      List("effekt", "immutable/option", "immutable/list")
    case b =>
      List("effekt")
  }

  validateFilesIsDirectory(includePath)

  // force some other configs manually to intialize them when compiling with native-image
  server; output; filenames
}
