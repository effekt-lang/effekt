package effekt

import effekt.context.Context
import effekt.util.messages.FatalPhaseError
import effekt.util.paths.{File, file}
import effekt.util.getOrElseAborting
import kiama.util.IO

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
   * Builds a given executable and returns the resulting path to the executable.
   */
  def build(executable: Executable)(using Context): String

  /**
   * Runs the executable (e.g. the main file) by calling the build function.
   */
  def eval(executable: Executable)(using C: Context): Unit = {
    val execFile = build(executable)

    val exitCode = Process(execFile).run(new ProcessLogger {

      override def out(s: => String): Unit = {
        C.config.output().emitln(s)
      }

      override def err(s: => String): Unit = System.err.println(s)

      override def buffer[T](f: => T): T = f

    }, connectInput = true).exitValue()

    if (exitCode != 0) {
      C.error(s"Process exited with non-zero exit code ${exitCode}.")
    }
  }

  def canRunExecutable(command: String*): Boolean =
    try {
      Process(command).run(ProcessIO(out => (), in => (), err => ())).exitValue() == 0
    } catch { case _ => false }

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

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "generic", path / ".." / "js")

  override def prelude: List[String] = List("effekt", "option", "list", "result", "show", "exception", "array", "string", "ref")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("node", "--version") then Right(())
    else Left("Cannot find nodejs. This is required to use the JavaScript backend.")

  /**
   * Creates an executable `.js` file besides the given `.js` file ([[path]])
   * and then returns the absolute path of the created executable.
   */
  def build(path: String)(using C: Context): String =
    val out = C.config.outputPath().getAbsolutePath
    val jsFilePath = (out / path).unixPath
    // create "executable" using shebang besides the .js file
    val jsScriptFilePath = jsFilePath.stripSuffix(s".$extension")
    val jsScript = s"require('${jsFilePath}').main()"
    val shebang = "#!/usr/bin/env node"
    IO.createFile(jsScriptFilePath, s"$shebang\n$jsScript", true)
    jsScriptFilePath
}

trait ChezRunner extends Runner[String] {
  val extension = "ss"

   def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def prelude: List[String] = List("effekt", "option", "list", "result", "show", "exception", "array", "string", "ref")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("scheme", "--help") then Right(())
    else Left("Cannot find scheme. This is required to use the ChezScheme backend.")

  /**
   * Creates an executable bash script besides the given `.ss` file ([[path]])
   * and returns the resulting absolute path.
   */
  def build(path: String)(using C: Context): String =
    val out = C.config.outputPath().getAbsolutePath
    val schemeFilePath = (out / path).unixPath
    val bashScriptPath = schemeFilePath.stripSuffix(s".$extension")
    val bashScript = s"#!/bin/bash\nscheme --script $schemeFilePath"
    IO.createFile(bashScriptPath, bashScript, true)
    bashScriptPath
}

object ChezMonadicRunner extends ChezRunner {
  override def includes(path: File): List[File] = List(
    path / ".." / "generic",
    path / ".." / "chez" / "common",
    path / ".." / "chez" / "monadic")
}

object ChezCallCCRunner extends ChezRunner {
  override def includes(path: File): List[File] = List(
    path / ".." / "generic",
    path / ".." / "chez" / "common",
    path / ".." / "chez" / "callcc")
}
object ChezLiftRunner extends ChezRunner {
  override def includes(path: File): List[File] = List(
    path / ".." / "generic",
    path / ".." / "chez" / "common",
    path / ".." / "chez" / "lift")
}

object LLVMRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "ll"

  def standardLibraryPath(root: File): File = root / "libraries" / "llvm"

  lazy val gccCmd = discoverExecutable(List("cc", "clang", "gcc"), List("--version"))
  lazy val llcCmd = discoverExecutable(List("llc", "llc-15", "llc-16"), List("--version"))
  lazy val optCmd = discoverExecutable(List("opt", "opt-15", "opt-16"), List("--version"))

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
  override def build(path: String)(using C: Context): String =
    val out = C.config.outputPath()
    val basePath = (out / path.stripSuffix(".ll")).unixPath
    val llPath  = basePath + ".ll"
    val optPath = basePath + ".opt.ll"
    val objPath = basePath + ".o"

    def missing(cmd: String) = C.abort(s"Cannot find ${cmd}. This is required to use the LLVM backend.")
    val gcc = gccCmd.getOrElse(missing("gcc"))
    val llc = llcCmd.getOrElse(missing("llc"))
    val opt = optCmd.getOrElse(missing("opt"))

    exec(opt, llPath, "-S", "-O2", "-o", optPath)
    exec(llc, "--relocation-model=pic", optPath, "-filetype=obj", "-o", objPath)

    val gccMainFile = (C.config.libPath / "main.c").unixPath
    val executableFile = basePath
    exec(gcc, gccMainFile, "-o", executableFile, objPath)
    executableFile
}


object MLRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "sml"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def prelude: List[String] = List("effekt", "option", "list", "result", "show", "exception", "array", "string", "ref")

  override def includes(path: File): List[File] = List(
    path / ".." / "monomorphic",
    path / ".." / "ml")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("mlton") then Right(())
    else Left("Cannot find mlton. This is required to use the ML backend.")

  /**
   * Compile the MLton source file (`<...>.sml`) to an executable.
   *
   * Requires the MLton compiler to be installed on the machine.
   * Assumes [[path]] has the format "SOMEPATH.sml".
   */
  override def build(path: String)(using C: Context): String =
    val out = C.config.outputPath()
    val buildFile = (out / "main.mlb").canonicalPath
    val executable = (out / "mlton-main").canonicalPath
    exec("mlton",
      "-default-type", "int64", // to avoid integer overflows
      "-output", executable, buildFile)
    executable
}
