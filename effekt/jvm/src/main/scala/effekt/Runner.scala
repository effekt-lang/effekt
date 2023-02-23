package effekt

import effekt.context.Context
import effekt.util.messages.FatalPhaseError
import effekt.util.paths.{ File, file }
import effekt.util.getOrElseAborting

/**
 * Interface used by [[Driver]] and [[EffektTests]] to run a compiled program.
 *
 * @tparam Executable the executable, as produced by [[BackendCompiler.compile]].
 */
trait Runner[Executable] {

  import scala.sys.process.*

  /**
   * Path to the standard library.
   *
   * @param root is the path of the Effekt compiler installation
   */
  def standardLibraryPath(root: File): File

  /**
   * File extension of generated files (e.g. "js", or "sml")
   */
  def extension: String

  /**
   * Additional includes the specific backend requires.
   *
   * @param stdlibPath is the path to the standard library
   */
  def includes(stdlibPath: File): List[File] = Nil

  /**
   * Modules this backend loads by default
   */
  def prelude: List[String] = List("effekt")

  /**
   * Should check whether everything is installed for this backend
   * to run. Should return Right(()) if everything is ok and
   * Left(explanation) if something is missing.
   */
  def checkSetup(): Either[String, Unit]

  /**
   * Runs the executable (e.g. the main file).
   */
  def eval(executable: Executable)(using Context): Unit

  def canRunExecutable(command: String*): Boolean =
    try {
      Process(command).run(ProcessIO(out => (), in => (), err => ())).exitValue() == 0
    } catch { _ => false }

  /**
   * Helper function to run an executable
   */
  def exec(command: String*)(using C: Context): Unit = try {
    val p = Process(command)
    C.config.output().emit(p.!!)
  } catch {
    case FatalPhaseError(e) => C.report(e)
  }

  /**
   * Try running a handful of names for a system executable; returns the first successful name,
   * if any.
   */
  def discoverExecutable(progs0: List[String], args: Seq[String]): Option[String] = {
    def go(progs: List[String]): Option[String] = progs match {
      case prog :: progs =>
        try { Process(prog +: args).!!; Some(prog) }
        catch case ioe => go(progs)
      case _ => None
    }
    go(progs0)
  }
}

object JSRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "js"

  def standardLibraryPath(root: File): File = root / "libraries" / "js"

  override def prelude: List[String] = List("effekt", "immutable/option", "immutable/list")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("node", "--version") then Right(())
    else Left("Cannot find nodejs. This is required to use the JavaScript backend.")

  def eval(path: String)(using Context): Unit =
    val jsScript = s"require('${path}').main().run()"
    exec("node", "--eval", jsScript)
}

trait ChezRunner extends Runner[String] {
  val extension = "ss"

  override def prelude: List[String] = List("effekt", "immutable/option", "immutable/list")
  override def includes(path: File): List[File] = List(path / ".." / "common")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("scheme", "--help") then Right(())
    else Left("Cannot find scheme. This is required to use the ChezScheme backend.")

  def eval(path: String)(using C: Context): Unit =
    exec("scheme", "--script", path)
}

object ChezMonadicRunner extends ChezRunner {
  def standardLibraryPath(root: File): File = root / "libraries" / "chez" / "monadic"
}

object ChezCallCCRunner extends ChezRunner {
  def standardLibraryPath(root: File): File = root / "libraries" / "chez" / "callcc"
}
object ChezLiftRunner extends ChezRunner {
  def standardLibraryPath(root: File): File = root / "libraries" / "chez" / "lift"
}

object LLVMRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "ll"

  def standardLibraryPath(root: File): File = root / "libraries" / "llvm"

  lazy val gccCmd = discoverExecutable(List("cc", "clang", "gcc"), List("--version"))
  lazy val llcCmd = discoverExecutable(List("llc", "llc-15", "llc-12"), List("--version"))
  lazy val optCmd = discoverExecutable(List("opt", "opt-15", "opt-12"), List("--version"))

  def checkSetup(): Either[String, Unit] =
    gccCmd.getOrElseAborting { return Left("Cannot find gcc. This is required to use the LLVM backend.") }
    llcCmd.getOrElseAborting { return Left("Cannot find llc. This is required to use the LLVM backend.") }
    optCmd.getOrElseAborting { return Left("Cannot find opt. This is required to use the LLVM backend.") }
    Right(())

  /**
   * Compile the LLVM source file (`<...>.ll`) to an executable
   *
   * Requires LLVM and GCC to be installed on the machine.
   * Assumes [[path]] has the format "SOMEPATH.ll".
   */
  def eval(path: String)(using C: Context): Unit =
    val basePath = path.stripSuffix(".ll")
    val optPath = basePath + ".opt.ll"
    val objPath = basePath + ".o"

    def missing(cmd: String) = C.abort(s"Cannot find ${cmd}. This is required to use the LLVM backend.")
    val gcc = gccCmd.getOrElse(missing("gcc"))
    val llc = llcCmd.getOrElse(missing("llc"))
    val opt = optCmd.getOrElse(missing("opt"))

    exec(opt, path, "-S", "-O2", "-o", optPath)
    exec(llc, "--relocation-model=pic", optPath, "-filetype=obj", "-o", objPath)

    val gccMainFile = (C.config.libPath / "main.c").unixPath
    val executableFile = basePath
    exec(gcc, gccMainFile, "-o", executableFile, objPath)

    exec(executableFile)
}

object JITRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "rpyeffect"

  override def standardLibraryPath(root: File): File = root / "libraries" / "jit"

  def platform = {
    val arch = Process(Seq(s"uname", "-m")).!!.trim
    val os = Process(Seq(s"uname", "-s")).!!.trim
    "%s-%s".format(arch, os)
  }

  /**
   * Tries to find the path to the JIT binary. Proceeds in the following order:
   * 1) specified in an environment variable `EFFEKT_JIT_BIN`
   * 2) relative to the current working directory
   * 3) relative to the executed JAR file (effekt.jar)
   *
   * If successful, returns `Right(file)`, otherwise `Left(errorMessage)`
   */
  def findJITBinary(platform: String): Either[String, effekt.util.paths.File] = {
    // TODO better error handling than Left(String)
    import effekt.util.paths.file
    val supportedJITPlatforms = List("x86_64-Linux", "arm64-Darwin")

    if (!supportedJITPlatforms.contains(platform)) {
      return Left(s"Unsupported platform ${platform}. Currently supported platforms: ${supportedJITPlatforms.mkString(", ")}")
    }

    val binaryName = s"${platform}/rpyeffect-jit";

    // 1) iin Environment variable EFFEKT_BIN
    if (System.getenv.containsKey("EFFEKT_JIT_BIN")) {
      return Right(System.getenv("EFFEKT_JIT_BIN"))
    }

    // 2) in PWD
    val pwd = file(".")
    if ((pwd / "bin" / binaryName).exists) {
      return Right(pwd / "bin" / binaryName)
    }

    // 3) next to JAR
    val jarPath = effekt.util.paths.file(getClass.getProtectionDomain.getCodeSource.getLocation.toURI).parent;
    if ((jarPath / binaryName).exists) {
      return Right(jarPath / binaryName)
    }

     Left("Cannot find path to the JIT binary")
  }

  override def checkSetup(): Either[String, Unit] = {
    findJITBinary(platform).flatMap{ jitBinaryPath =>
        if canRunExecutable(jitBinaryPath.unixPath, "--check") then Right(())
        else Left("Cannot run jit executable file at %s.".format(jitBinaryPath.unixPath))
    }
  }

  override def eval(executable: String)(using C: Context): Unit = {
    findJITBinary(platform) match {
      case Left(err) => sys.error(err)
      case Right(jitBinary) =>
        exec(jitBinary.unixPath, executable)
    }
  }
}

object MLRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "sml"

  def standardLibraryPath(root: File): File = root / "libraries" / "ml"

  override def prelude: List[String] = List("effekt", "immutable/option", "immutable/list")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("mlton") then Right(())
    else Left("Cannot find mlton. This is required to use the ML backend.")

  /**
   * Compile the LLVM source file (`<...>.ll`) to an executable
   *
   * Requires LLVM and GCC to be installed on the machine.
   * Assumes [[path]] has the format "SOMEPATH.ll".
   */
  def eval(path: String)(using C: Context): Unit =
    val executable = (C.config.outputPath() / "mlton-main").canonicalPath
    exec("mlton", "-output", executable, path)
    exec(executable)
}
