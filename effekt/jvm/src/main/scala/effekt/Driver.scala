package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }
import effekt.util.{ ColoredMessaging, MarkdownSource }

import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ IO, Source }

import effekt.util.messages.FatalPhaseError

import effekt.util.paths._
//TODO-LLVM import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.sys.process.Process

import effekt.generator

/**
 * effekt.Compiler <----- compiles code with  ------ Driver ------ implements UI with -----> kiama.util.Compiler
 */
trait Driver extends kiama.util.Compiler[Tree, ModuleDecl, EffektConfig] { outer =>

  val name = "effekt"

  override val messaging = new ColoredMessaging(positions)

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) with IOModuleDB {
    /**
     * Output: JavaScript -> File
     */
    override def saveOutput(doc: String, path: String)(implicit C: Context): Unit =
      if (C.config.requiresCompilation()) {
        C.config.outputPath().mkdirs
        IO.createFile(path, doc)
      }
  }

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
  override def compileSource(source: Source, config: EffektConfig): Unit = {
    val src = if (source.name.endsWith(".md")) { MarkdownSource(source) } else { source }

    sources(source.name) = src

    implicit val C = context
    C.setup(config)

    for {
      doc <- C.generate(src)
      if config.interpret()
      mod <- C.frontend(src)
    } eval(mod)

    afterCompilation(source, config)
  }

  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.buffer.get, config)
  }

  def eval(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    C.config.generator() match {
      case gen if gen.startsWith("js")   => evalJS(mod)
      case gen if gen.startsWith("chez") => evalCS(mod)
      case gen if gen.startsWith("llvm") => { evalLLVM__TEMPORARY_HACK(mod); evalLLVM(C.codeGenerator.path(mod)) }
    }
  }

  def evalJS(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      val _ = C.checkMain(mod)
      val jsFile = C.codeGenerator.path(mod)
      val jsScript = s"require('${jsFile}').main().run()"
      val command = Process(Seq("node", "--eval", jsScript))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) =>
        C.error(e)
    }
  }

  def evalCS(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      val _ = C.checkMain(mod)
      val csFile = C.codeGenerator.path(mod)
      val command = Process(Seq("scheme", "--script", csFile))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) =>
        C.error(e)
    }
  }

  def evalLLVM__TEMPORARY_HACK(mod: Module)(implicit C: Context): Unit = {
    val path = C.codeGenerator.path(mod)
    C.saveOutput((C.generate(mod.source).get).layout, path + ".ll")
  }
  def evalLLVM(path: String)(implicit C: Context): Unit =
    try {
      val LLVM_VERSION = sys.env.get("EFFEKT_LLVM_VERSION").getOrElse("12") // TODO Make global config?

      val xPath = (suffix: String) => path + suffix
      val llvmPath = xPath(".ll")
      val optPath = xPath("_opt.ll")
      val objPath = xPath(".o")

      val optCommand = Process(Seq(s"opt-${LLVM_VERSION}", llvmPath, "-S", "-O2", "-o", optPath))
      C.config.output().emit(optCommand.!!)

      val llcCommand = Process(Seq(s"llc-${LLVM_VERSION}", "--relocation-model=pic", optPath, "-filetype=obj", "-o", objPath))
      C.config.output().emit(llcCommand.!!)

      val gccMainFile = (C.config.libPath / "main.c").unixPath
      val executableFile = path //TODO-LLVM C.codeGenerator.path(mod)
      val gccCommand = Process(Seq("gcc", gccMainFile, "-o", executableFile, objPath))
      C.config.output().emit(gccCommand.!!)

      val command = Process(Seq(executableFile))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) => C.error(e)
    }

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.buffer.get, C.config)

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
