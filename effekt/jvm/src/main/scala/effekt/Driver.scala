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
import effekt.util.{ AnsiColoredMessaging, MarkdownSource, getOrElseAborting }

import scala.sys.process.Process

import java.io.IOException

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

    C.backend match {

      case Backend(name, compiler, runner) =>
        def compile() = compiler.compile(src) map {
          case (outputFiles, exec) =>
            outputFiles.foreach {
              case (filename, doc) =>
                saveOutput(filename, doc.layout)
            }
            exec
        }

        // we are in one of three exclusive modes: LSPServer, Compile, Run
        if (config.server()) { compiler.runFrontend(src) }
        else if (config.interpret()) { compile() foreach runner.eval }
        else if (config.compile()) { compile() }
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
