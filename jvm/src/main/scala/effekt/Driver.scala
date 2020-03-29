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

  override def parse(source: Source): ParseResult[ModuleDecl] =
    parser.parseAll(parser.program, source)

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {

    implicit val C = context

    context.setup(ast, config)

    for {
      mod <- pipeline(source, ast)
      if !config.server() && !config.compile()
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

  def format(m: ModuleDecl): Document = ???
}
