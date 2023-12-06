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
trait Driver extends kiama.util.Compiler[EffektConfig, EffektError] { outer =>

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
    // This is overridden by kiama.Server to launch the LSP server.
    // TODO: remove dynamic dispatch here and consider replacing inheritance by composition.
    } else if (config.server()) {
      super.run(config)
    } else for (filename <- config.filenames()) {
      compileFile(filename, config)
    }

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = try {
    val src = if (source.name.endsWith(".effekt.md")) { MarkdownSource(source) } else { source }

    // remember that we have seen this source, this is used by LSP (kiama.util.Server)
    sources(source.name) = src

    implicit val C = context
    C.setup(config)

    def saveOutput(path: String, doc: String): Unit =
      if (C.config.requiresCompilation()) {
        val out = C.config.outputPath()
        out.mkdirs
        IO.createFile((out / path).unixPath, doc)
      }

    C.backend match {

      case Backend(name, compiler, runner) =>
        def compile() = compiler.compile(src) map {
          case (outputFiles, exec) =>
            outputFiles.foreach {
              case (filename, doc) =>
                saveOutput(filename, doc)
            }
            exec
        }

        // we are in one of three exclusive modes: LSPServer, Compile, Run
        if (config.server()) { compiler.runFrontend(src) }
        else if (config.interpret()) { compile() foreach runner.eval }
        else if (config.build()) { compile() foreach runner.build }  
        else if (config.compile()) { compile() }
    }
  } catch {
    case FatalPhaseError(msg) => context.report(msg)
  } finally {
    // This reports error messages
    afterCompilation(source, config)(context)
  }

  /**
   * Overridden in [[Server]] to also publish core and js compilation results to VSCode
   */
  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.messaging.buffer, config)
  }
}
