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
   * Modules this backend loads by default.
   *
   * Invariants:
   * - All imports of prelude modules have to be in the prelude as well.
   * - The order matters and should correspond to the topological ordering with respect to the imports, that is,
   * if module A depends on module B, then B should come before A.
   * - Furthermore, each module mentioned here must import the `effekt` module as its first import.
   */
  def prelude: List[String] = List("effekt", "option", "list", "result", "exception", "array", "char", "string", "ref")

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
  def build(executable: Executable)(using Context): Option[String]

  /**
   * Runs the executable (e.g. the main file) by calling the build function.
   */
  def eval(executable: Executable)(using C: Context): Unit = build(executable).foreach { execFile =>
    val valgrindArgs = Seq("--leak-check=full", "--undef-value-errors=no", "--quiet", "--log-file=valgrind.log", "--error-exitcode=1")
    val process = if (C.config.valgrind())
      Process("valgrind", valgrindArgs ++ (execFile +: Context.config.runArgs()))
    else
      Process(execFile, Context.config.runArgs())

    // Check if the output is a stdout one. TODO use a different method for tests and remove the default case
    val exitCode = C.config.output() match {
      case _: kiama.util.OutputEmitter =>
        process.!< // we are not capturing the output, so forward directly
      case outs =>
        process.run(new ProcessLogger {

          override def out(s: => String): Unit = {
            outs.emitln(s)
          }

          override def err(s: => String): Unit = System.err.println(s)

          override def buffer[T](f: => T): T = f

        }, connectInput = true).exitValue()
    }

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

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("node", "--version") then Right(())
    else Left("Cannot find nodejs. This is required to use the JavaScript backend.")

  /**
   * Creates an executable `.js` file besides the given `.js` file ([[path]])
   * and then returns the absolute path of the created executable.
   */
  def build(path: String)(using C: Context): Option[String] =
    val out = C.config.outputPath().getAbsolutePath
    val jsFilePath = (out / path).canonicalPath.escape
    val jsFileName = path.unixPath.split("/").last
    // create "executable" using shebang besides the .js file
    val jsScript = s"require('./${jsFileName}').main()"

    // Create a package.json that declares that the generated JavaScript files are CommonJS modules (rather than ES modules).
    // This is needed in case the user has set the global default to ES modules, for instance with a default ~/package.json file.
    // See https://github.com/effekt-lang/effekt/issues/834 for more details
    val packageJson = """{ "type": "commonjs" }"""
    val packageJsonFilePath = (out / "package.json").canonicalPath.escape
    IO.createFile(packageJsonFilePath, packageJson)

    os match {
      case OS.POSIX =>
        val shebang = "#!/usr/bin/env node"
        val jsScriptFilePath = jsFilePath.stripSuffix(s".$extension")
        IO.createFile(jsScriptFilePath, s"$shebang\n$jsScript", true)
        Some(jsScriptFilePath)

      case OS.Windows =>
        val jsMainFilePath = jsFilePath.stripSuffix(s".$extension") + "__main.js"
        val jsMainFileName = jsFileName.stripSuffix(s".$extension") + "__main.js"
        val exePath = jsFilePath.stripSuffix(s".$extension")
        IO.createFile(jsMainFilePath, jsScript)
        Some(createScript(exePath, "node", "$SCRIPT_DIR/" + jsMainFileName))
    }
}
object JSWebRunner extends Runner[String] {

  val extension = "js"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "js")

  def checkSetup(): Either[String, Unit] =
    Left("Running js-web code directly is not supported. Use `--compile` to generate a js file / `--build` to generate a html file.")

  /**
   * Creates an openable `.html` file besides the given `.js` file ([[path]])
   * and then errors out, printing it's path.
   */
  def build(path: String)(using C: Context): Option[String] =
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
         |    <div id="app"></div>
         |  </body>
         |</html>
         |""".stripMargin
    IO.createFile(htmlFilePath, htmlContent, false)

    C.info(s"Open file://${htmlFilePath} in your browser or include ${jsFilePath}.")
    None
}

trait ChezRunner extends Runner[String] {
  val extension = "ss"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  def checkSetup(): Either[String, Unit] =
    if canRunExecutable("scheme", "--help") then Right(())
    else Left("Cannot find scheme. This is required to use the ChezScheme backend.")

  /**
   * Creates an executable bash script besides the given `.ss` file ([[path]])
   * and returns the resulting absolute path.
   */
  def build(path: String)(using C: Context): Option[String] =
    val out = C.config.outputPath().getAbsolutePath
    val schemeFilePath = (out / path).canonicalPath.escape
    val exeScriptPath = schemeFilePath.stripSuffix(s".$extension")
    val schemeFileName = ("./" + (path.unixPath.split('/').last)).escape
    Some(createScript(exeScriptPath, "scheme", "--script", "$SCRIPT_DIR/" + schemeFileName))
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

  val extension = "ll"

  def standardLibraryPath(root: File): File = root / "libraries" / "common"

  override def includes(path: File): List[File] = List(path / ".." / "llvm")

  lazy val clangCmd = discoverExecutable(List("clang-18", "clang"), List("--version"))
  lazy val llcCmd = discoverExecutable(List("llc-18", "llc"), List("--version"))
  lazy val optCmd = discoverExecutable(List("opt-18", "opt"), List("--version"))

  def checkSetup(): Either[String, Unit] =
    clangCmd.getOrElseAborting { return Left("Cannot find clang. This is required to use the LLVM backend.") }
    llcCmd.getOrElseAborting { return Left("Cannot find llc. This is required to use the LLVM backend.") }
    optCmd.getOrElseAborting { return Left("Cannot find opt. This is required to use the LLVM backend.") }
    Right(())

  def libuvArgs(using C: Context): Seq[String] =
    val OS = System.getProperty("os.name").toLowerCase
    val libraries = C.config.clangLibraries.toOption.map(file).orElse {
      OS match {
        case os if os.contains("mac")  => Some(file("/opt/homebrew/lib"))
        case os if os.contains("win") => None
        case os if os.contains("linux") => Some(file("/usr/local/lib"))
        case os => None
      }
    }
    val includes = C.config.clangIncludes.toOption.map(file).orElse {
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
        C.warning(s"Cannot find libuv on ${OS}; please use --clang-libraries and --clang-includes to configure the paths for the libuv dylib and header files, respectively.")
        Seq()
    }

  def useLTO(using C: Context): Boolean =
    !C.config.debug() && !C.config.valgrind() && C.config.optimize()

  /**
   * Compile the LLVM source file (`<...>.ll`) to an executable
   *
   * Requires LLVM and GCC to be installed on the machine.
   * Assumes [[path]] has the format "SOMEPATH.ll".
   */
  override def build(path: String)(using C: Context): Option[String] =

    val out = C.config.outputPath()
    val basePath = (out / path.stripSuffix(".ll")).unixPath
    val llPath  = basePath + ".ll"
    val bcPath  = basePath + ".bc"
    val linkedLibraries = Seq(
      "-lm", // Math library
    ) ++ libuvArgs

    def missing(cmd: String) = C.abort(s"Cannot find ${cmd}. This is required to use the LLVM backend.")
    val clang = clangCmd.getOrElse(missing("clang"))
    val llc = llcCmd.getOrElse(missing("llc"))
    val opt = optCmd.getOrElse(missing("opt"))

    val clangMainFile = (C.config.libPath / ".." / "llvm" / "main.c").unixPath
    val executableFile = basePath

    if (useLTO) {
      // Convert to bitcode with aggressive optimizations
      exec(opt, llPath, "-O3", "-o", bcPath)

      var clangArgs = Seq(clang, clangMainFile, bcPath, "-o", executableFile) ++ linkedLibraries

      clangArgs ++= Seq(
        "-O3",
        "-flto=full",
        "-Wno-override-module"
      )

      if (C.config.native()) {
        clangArgs :+= "-march=native"
      }

      exec(clangArgs: _*)
    } else {
      exec(opt, llPath, "-O2", "-o", bcPath)

      var clangArgs = Seq(clang, clangMainFile, "-o", executableFile, bcPath, "-Wno-override-module") ++ linkedLibraries

      if (C.config.debug()) clangArgs ++= Seq("-g", "-Wall", "-Wextra", "-Werror")
      if (C.config.valgrind()) clangArgs ++= Seq("-O0", "-g")
      else if (C.config.debug()) clangArgs ++= Seq("-fsanitize=address,undefined", "-fstack-protector-all")

      exec(clangArgs: _*)
    }

    Some(executableFile)
}
