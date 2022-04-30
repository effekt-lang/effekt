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

import scala.sys.process.Process

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
      doc <- C.compileSeparate(src)
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
    }
  }

  def evalJS(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      C.checkMain(mod)
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
      C.checkMain(mod)
      val csFile = C.codeGenerator.path(mod)
      val command = Process(Seq("scheme", "--script", csFile))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) =>
        C.error(e)
    }
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
