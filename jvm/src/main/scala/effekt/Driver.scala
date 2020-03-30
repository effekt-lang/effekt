package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ CompilerWithConfig, IO, Source }

import scala.sys.process.Process

trait Driver extends Compiler with CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { outer =>

  val name = "effekt"

  // Compiler context
  // ================
  // We always only have one global instance of CompilerContext
  object context extends Context(outer) with IOModuleDB

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit =
    if (config.filenames().isEmpty && !config.server()) {
      new Repl(this).run(config)
    } else {
      super.run(config)
    }

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = {
    sources(source.name) = source

    implicit val C = context

    for {
      ast <- parser(source)
      _ = context.setup(ast, config)
      mod <- pipeline(source, ast)
      if config.interpret()
    } eval(mod)

    // report messages
    clearSyntacticMessages(source, config)
    clearSemanticMessages(source, config)
    report(source, C.buffer.get, config)
  }

  /**
   * Output: JavaScript -> File
   */
  override def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit =
    if (C.config.requiresCompilation()) {
      C.config.outputPath().mkdirs
      val jsFile = jsPath(unit)
      IO.createFile(jsFile, js.layout)
    }

  def eval(mod: Module)(implicit C: Context): Unit = {

    val main = mod.terms.getOrElse("main", {
      C.error("No main function defined")
      return
    })

    val mainParams = C.blockTypeOf(main).params
    if ((mainParams.size != 1) || (mainParams.head != Nil)) {
      C.error("Main does not take arguments")
      return
    }

    val jsFile = jsPath(mod)
    val jsScript = s"require('${jsFile}').main().run()"
    val command = Process(Seq("node", "--eval", jsScript))
    C.config.output().emit(command.!!)
  }

  def jsPath(mod: Module)(implicit C: Context): String = {
    val outDir = C.config.outputPath().toPath
    outDir.resolve(mod.outputName).toFile.getCanonicalPath
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
  override def parse(source: Source): ParseResult[ModuleDecl] = ???

  def format(m: ModuleDecl): Document = ???
}
