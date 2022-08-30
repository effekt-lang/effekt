package effekt

// Adapted from
//   https://github.com/inkytonik/kiama/blob/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }

import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ IO, Source }

import effekt.util.messages.{ BufferedMessaging, EffektError, EffektMessaging, FatalPhaseError }
import effekt.util.paths.file
import effekt.util.{ AnsiColoredMessaging, MarkdownSource }

import scala.sys.process.Process

/**
 * effekt.Compiler <----- compiles code with  ------ Driver ------ implements UI with -----> kiama.util.Compiler
 */
trait Driver extends kiama.util.Compiler[Tree, ModuleDecl, EffektConfig, EffektError] { outer =>

  val name = "effekt"

  object messaging extends AnsiColoredMessaging

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) with IOModuleDB { val messaging = outer.messaging }

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit =
    if (config.filenames().isEmpty && !config.server() && !config.compile()) {
      new Repl(this).run(config)
    } else {
      super.run(config)
    }

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = try {
    val src = if (source.name.endsWith(".md")) { MarkdownSource(source) } else { source }

    // remember that we have seen this source, this is used by LSP (kiama.util.Server)
    sources(source.name) = src

    implicit val C = context
    C.setup(config)

    val Compiled(main, outputFiles) = C.compileWhole(src).getOrElse { return }

    outputFiles.foreach {
      case (filename, doc) =>
        saveOutput(filename, doc.layout)
    }

    if (config.interpret()) {
      // type check single file -- `mod` is necessary for positions in error reporting.
      val mod = C.runFrontend(src).getOrElse { return }
      C.at(mod.decl) { C.checkMain(mod); eval(main) }
    }
  } catch {
    case FatalPhaseError(msg) => context.report(msg)
  } finally {
    // This reports error messages
    afterCompilation(source, config)(context)
  }

  // TODO check whether we still need requiresCompilation
  private def saveOutput(path: String, doc: String)(implicit C: Context): Unit =
    if (C.config.requiresCompilation()) {
      C.config.outputPath().mkdirs
      IO.createFile(path, doc)
    }

  /**
   * Overridden in [[Server]] to also publish core and js compilation results to VSCode
   */
  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.messaging.buffer, config)
  }

  def eval(path: String)(implicit C: Context): Unit =
    C.config.backend() match {
      case gen if gen.startsWith("js")   => evalJS(path)
      case gen if gen.startsWith("chez") => evalCS(path)
      case gen if gen.startsWith("llvm") => evalLLVM(path)
    }

  def evalJS(path: String)(implicit C: Context): Unit =
    try {
      val jsScript = s"require('${path}').main().run()"
      val command = Process(Seq("node", "--eval", jsScript))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) => C.report(e)
    }

  /**
   * Precondition: the module has been compiled to a file that can be loaded.
   */
  def evalCS(path: String)(implicit C: Context): Unit =
    try {
      val command = Process(Seq("scheme", "--script", path))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) => C.report(e)
    }

  /**
   * Compile the LLVM source file (`<...>.ll`) to an executable
   *
   * Requires LLVM and GCC to be installed on the machine.
   * Assumes [[llvmPath]] has the format "SOMEPATH.ll".
   */
  def evalLLVM(llvmPath: String)(implicit C: Context): Unit = try {

    val LLVM_VERSION = C.config.llvmVersion()

    val basePath = llvmPath.stripSuffix(".ll")
    val optPath = basePath + "_opt.ll"
    val objPath = basePath + ".o"

    val out = C.config.output()

    // TODO figure out whether "opt" or "opt-VERSION" exist as file
    //   if not, raise error.
    // Same for llc
    val optCommand = Process(Seq(s"opt", llvmPath, "-S", "-O2", "-o", optPath))
    out.emit(optCommand.!!)

    val llcCommand = Process(Seq(s"llc", "--relocation-model=pic", optPath, "-filetype=obj", "-o", objPath))
    out.emit(llcCommand.!!)

    val gccMainFile = (C.config.libPath / "main.c").unixPath
    val executableFile = basePath //TODO-LLVM C.codeGenerator.path(mod)
    val gccCommand = Process(Seq("gcc", gccMainFile, "-o", executableFile, objPath))
    out.emit(gccCommand.!!)

    val command = Process(Seq(executableFile))
    out.emit(command.!!)
  } catch {
    case FatalPhaseError(e) => C.report(e)
  }

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.messaging.buffer, C.config)

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`.
   * Not used anymore
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = ???

  /**
   * Originally called by kiama, not used anymore.
   */
  override final def parse(source: Source): ParseResult[ModuleDecl] = ???

  /**
   * Originally called by kiama, not used anymore.
   */
  override final def format(m: ModuleDecl): Document = ???
}
