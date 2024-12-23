package effekt

import effekt.context.Context
import effekt.util.messages.FatalPhaseError
import effekt.util.paths.{File, file}
import effekt.util.{getOrElseAborting, escape, OS, os}
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
   * Creates a OS-specific script file that will execute the command when executed,
   * forwarding command line arguments.
   * `$SCRIPT_DIR` refers to the directory the script is in.
   *
   * @return the actual name of the generated script (might be `!= name`)
   */
  def createScript(name: String, command: String*): String = os match {
    case OS.POSIX =>
      val computeScriptDir =
        """# Determine the directory of the script
          |SCRIPT_DIR=$(dirname "$(realpath "$0")")
          |""".stripMargin
      IO.createFile(name, s"#!/bin/sh\n${computeScriptDir}\n${command.mkString(" ")} \"$$@\"", true)
      name
    case OS.Windows =>
      val computeScriptDir =
        """setlocal enabledelayedexpansion
          |
          |:: Get the directory of the batch file
          |set "SCRIPT_DIR=%~dp0"
          |
          |:: Remove trailing backslash
          |set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
          |""".stripMargin
      val batName = name + ".bat"
      // replace UNIX-style variables in command: $SCRIPT_DIR  -->  %SCRIPT_DIR%
      val cmd = command.mkString(" ").replaceAll("\\$([A-Za-z_][A-Za-z0-9_]*)", "%$1%")
      IO.createFile(batName, s"@echo off\r\n${computeScriptDir}\r\n${cmd} %*")
      batName
  }

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
    val valgrindArgs = Seq("--leak-check=full", "--quiet", "--log-file=valgrind.log", "--error-exitcode=1")
    val process = if (C.config.valgrind())
      Process("valgrind", valgrindArgs ++ (execFile +: Context.config.runArgs()))
    else
      Process(execFile, Context.config.runArgs())

    val exitCode = process.run(new ProcessLogger {

      override def out(s: => String): Unit = {
        C.config.output().emitln(s)
      }

      override def err(s: => String): Unit = System.err.println(s)

      override def buffer[T](f: => T): T = f

    }, connectInput = true).exitValue()

    if (exitCode != 0) {
      C.error(s"Process exited with non-zero exit code ${exitCode}.")
      if (C.config.valgrind()) C.error(s"Valgrind log:\n" ++ scala.io.Source.fromFile("valgrind.log").mkString)
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

object JSNodeRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "js"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "js")

  override def prelude: List[String] = List("effekt", "option", "list", "result", "exception", "array", "string", "ref")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("node", "--version") then Right(())
    else Left("Cannot find nodejs. This is required to use the JavaScript backend.")

  /**
   * Creates an executable `.js` file besides the given `.js` file ([[path]])
   * and then returns the absolute path of the created executable.
   */
  def build(path: String)(using C: Context): String =
    val out = C.config.outputPath().getAbsolutePath
    val jsFilePath = (out / path).canonicalPath.escape
    val jsFileName = path.unixPath.split("/").last
    // create "executable" using shebang besides the .js file
    val jsScript = s"require('./${jsFileName}').main()"

    os match {
      case OS.POSIX =>
        val shebang = "#!/usr/bin/env node"
        val jsScriptFilePath = jsFilePath.stripSuffix(s".$extension")
        IO.createFile(jsScriptFilePath, s"$shebang\n$jsScript", true)
        jsScriptFilePath

      case OS.Windows =>
        val jsMainFilePath = jsFilePath.stripSuffix(s".$extension") + "__main.js"
        val jsMainFileName = jsFileName.stripSuffix(s".$extension") + "__main.js"
        val exePath = jsFilePath.stripSuffix(s".$extension")
        IO.createFile(jsMainFilePath, jsScript)
        createScript(exePath, "node", "$SCRIPT_DIR/" + jsMainFileName)
    }
}
object JSWebRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "js"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "js")

  override def prelude: List[String] = List("effekt", "option", "list", "result", "exception", "array", "string", "ref")

  def checkSetup(): Either[String, Unit] =
    Left("Running js-web code directly is not supported. Use `--compile` to generate a js file / `--build` to generate a html file.")

  /**
   * Creates an openable `.html` file besides the given `.js` file ([[path]])
   * and then errors out, printing it's path.
   */
  def build(path: String)(using C: Context): String =
    import java.nio.file.Path
    val out = C.config.outputPath().getAbsolutePath
    val jsFilePath = (out / path).unixPath
    val jsFileName = path.unixPath.split("/").last
    val htmlFilePath = jsFilePath.stripSuffix(s".$extension") + ".html"
    val mainName = "$" + jsFileName.stripSuffix(".js") + ".main"
    val htmlContent =
      s"""<!DOCTYPE html>
         |<html>
         |  <body>
         |    <script type="text/javascript" src="${jsFileName}"></script>
         |    <script type="text/javascript">
         |      window.onload=${mainName};
         |    </script>
         |  </body>
         |</html>
         |""".stripMargin
    IO.createFile(htmlFilePath, htmlContent, false)
    C.abort(s"Open file://${htmlFilePath} in your browser or include ${jsFilePath}.")
}

trait ChezRunner extends Runner[String] {
  val extension = "ss"

   def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def prelude: List[String] = List("effekt", "option", "list", "result", "exception", "array", "string", "ref")

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("scheme", "--help") then Right(())
    else Left("Cannot find scheme. This is required to use the ChezScheme backend.")

  /**
   * Creates an executable bash script besides the given `.ss` file ([[path]])
   * and returns the resulting absolute path.
   */
  def build(path: String)(using C: Context): String =
    val out = C.config.outputPath().getAbsolutePath
    val schemeFilePath = (out / path).canonicalPath.escape
    val exeScriptPath = schemeFilePath.stripSuffix(s".$extension")
    val schemeFileName = ("./" + (path.unixPath.split('/').last)).escape
    createScript(exeScriptPath, "scheme", "--script", "$SCRIPT_DIR/" + schemeFileName)
}

object ChezMonadicRunner extends ChezRunner {
  override def includes(path: File): List[File] = List(
    path / ".." / "chez" / "common",
    path / ".." / "chez" / "monadic")
}

object ChezCallCCRunner extends ChezRunner {
  override def includes(path: File): List[File] = List(
    path / ".." / "chez" / "common",
    path / ".." / "chez" / "callcc")
}

object LLVMRunner extends Runner[String] {
  import scala.sys.process.Process

  val extension = "ll"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "llvm")

  override def prelude: List[String] = List("effekt", "option", "list", "result", "exception", "string") // "array", "ref")


  lazy val gccCmd = discoverExecutable(List("cc", "clang", "gcc"), List("--version"))
  lazy val llcCmd = discoverExecutable(List("llc", "llc-15", "llc-16"), List("--version"))
  lazy val optCmd = discoverExecutable(List("opt", "opt-15", "opt-16"), List("--version"))

  def checkSetup(): Either[String, Unit] =
    gccCmd.getOrElseAborting { return Left("Cannot find gcc. This is required to use the LLVM backend.") }
    llcCmd.getOrElseAborting { return Left("Cannot find llc. This is required to use the LLVM backend.") }
    optCmd.getOrElseAborting { return Left("Cannot find opt. This is required to use the LLVM backend.") }
    Right(())

  def libuvArgs(using C: Context): Seq[String] =
    val OS = System.getProperty("os.name").toLowerCase
    val libraries = C.config.gccLibraries.toOption.map(file).orElse {
      OS match {
        case os if os.contains("mac")  => Some(file("/opt/homebrew/lib"))
        case os if os.contains("win") => None
        case os if os.contains("linux") => Some(file("/usr/local/lib"))
        case os => None
      }
    }
    val includes = C.config.gccIncludes.toOption.map(file).orElse {
      OS match {
        case os if os.contains("mac")  => Some(file("/opt/homebrew/include"))
        case os if os.contains("win") => None
        case os if os.contains("linux") => Some(file("/usr/local/include"))
        case os => None
      }
    }
    (libraries, includes) match {
      case (Some(lib), Some(include)) => Seq(s"-L${lib.unixPath}", "-luv", s"-I${include.unixPath}")
      case _ =>
        C.warning(s"Cannot find libuv on ${OS}; please use --gcc-libraries and --gcc-includes to configure the paths for the libuv dylib and header files, respectively.")
        Seq()
    }

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
    val linkedLibraries = Seq(
      "-lm", // Math library
    ) ++ libuvArgs

    def missing(cmd: String) = C.abort(s"Cannot find ${cmd}. This is required to use the LLVM backend.")
    val gcc = gccCmd.getOrElse(missing("gcc"))
    val llc = llcCmd.getOrElse(missing("llc"))
    val opt = optCmd.getOrElse(missing("opt"))

    exec(opt, llPath, "-S", "-O2", "-o", optPath)
    exec(llc, "--relocation-model=pic", optPath, "-filetype=obj", "-o", objPath)

    val gccMainFile = (C.config.libPath / ".." / "llvm" / "main.c").unixPath
    val executableFile = basePath
    var gccArgs = Seq(gcc, gccMainFile, "-o", executableFile, objPath) ++ linkedLibraries

    if (C.config.debug()) gccArgs ++= Seq("-g", "-Wall", "-Wextra", "-Werror")
    if (C.config.valgrind()) gccArgs ++= Seq("-O0", "-g")
    else if (C.config.debug()) gccArgs ++= Seq("-fsanitize=address,undefined", "-fstack-protector-all")

    exec(gccArgs: _*)

    executableFile
}
