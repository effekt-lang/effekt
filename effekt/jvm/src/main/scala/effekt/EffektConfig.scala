package effekt

import java.io.File

import effekt.util.paths.file
import kiama.util.REPLConfig

import org.rogach.scallop.ScallopOption
import org.rogach.scallop.{ fileConverter, fileListConverter, stringConverter }

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

  val backend: ScallopOption[String] = choice(
    choices = List("js", "js-lift", "chez-callcc", "chez-monadic", "chez-lift", "llvm"),
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
    stdlibPath.foreach { path =>
      return path
    }

    // 2) in PATH
    if (System.getenv.containsKey("EFFEKT_LIB")) {
      return System.getenv("EFFEKT_LIB")
    }

    // 3) in PWD
    val pwd = file(".")
    val localLib = pwd / "libraries" / "js" / "monadic"
    // TODO also use language config for a better search
    if ((localLib / "effekt.effekt").exists) {
      // here we return the absolute path to avoid problems with LSP
      return ("libraries" / "js" / "monadic").canonicalPath
    }

    // 4) next to Jar
    val jarPath = try {
      file(getClass.getProtectionDomain.getCodeSource.getLocation.toURI).parent
    } catch {
      case e: Throwable =>
        sys.error("Cannot find path to standard library")
    }

    val jarLib = jarPath / ".." / "libraries" / "js" / "monadic"
    if ((jarLib / "effekt.effekt").exists) {
      return jarLib
    }

    sys.error("Cannot find path to standard library")
  }

  lazy val libPath: File = findStdLib.toFile

  def includes() = libPath :: includePath()

  def requiresCompilation(): Boolean = !server()

  def interpret(): Boolean = !server() && !compile()

  validateFilesIsDirectory(includePath)

  // force some other configs manually to intialize them when compiling with native-image
  server; output; filenames
}
