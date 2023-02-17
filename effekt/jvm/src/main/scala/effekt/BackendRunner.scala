package effekt

import effekt.context.Context
import effekt.util.paths.File

case class Backend[E](
  /**
   * The name of this backend (should be identical to the --backend flag)
   */
  name: String,

  /**
   * The compiler for this backend
   */
  compiler: BackendCompiler[E],

  /**
   * The runner for this backend
   */
  runner: BackendRunner[E])

object Backend {
  val js = Backend("js", JSCompiler, JSRunner)
}

/**
 * Interface used by [[Driver]] and [[EffektTests]] to run a compiled program.
 *
 * @tparam Executable the executable, as produced by [[BackendCompiler.compile]].
 */
trait BackendRunner[Executable] {

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
}

object JSRunner extends BackendRunner[String] {
  import scala.sys.process.Process

  val extension = "js"

  def standardLibraryPath(root: File): File = root / "libraries" / "js"

  override def prelude: List[String] = List("effekt", "immutable/option", "immutable/list")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("node", "--version") then Right(())
    else Left("Cannot find nodejs. This is required to use the JavaScript backend.")

  def eval(path: String)(using C: Context): Unit =
    val jsScript = s"require('${path}').main().run()"
    val command = Process(Seq("node", "--eval", jsScript))
    C.config.output().emit(command.!!)
}




//  private def backendStdLibPath(path: util.paths.File): util.paths.File = backend() match {
//      case "js" => path / "libraries" / "js"
//      case "chez-monadic" => path / "libraries" / "chez" / "monadic"
//      case "chez-callcc" => path / "libraries" / "chez" / "callcc"
//      case "chez-lift" => path / "libraries" / "chez" / "lift"
//      case "llvm" => path / "libraries" / "llvm"
//      case "ml" => path / "libraries" / "ml"
//      case b => sys error s"Unrecognized backend ${ b }"
//    }
//
//    private def backendIncludes(path: util.paths.File): List[util.paths.File] = backend() match {
//      case "chez-monadic" | "chez-callcc" | "chez-lift" => List(path, path / ".." / "common")
//      case b => List(path)
//    }
//
//    private def backendPrelude() = backend() match {
//      case "js" | "chez-monadic" | "chez-callcc" | "chez-lift" =>
//        List("effekt", "immutable/option", "immutable/list")
//      case "ml" =>
//        List("effekt", "immutable/option", "immutable/list")
//      case b =>
//        List("effekt")
//    }

