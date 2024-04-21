package effekt

import effekt.context.{ Context }
import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.util.messages.{ BufferedMessaging, CompilerPanic, EffektError, EffektMessaging, FatalPhaseError }
import effekt.util.paths.file
import effekt.util.{ AnsiColoredMessaging, MarkdownSource, getOrElseAborting }
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ IO, Source }

import scala.sys.process.Process

/**
 * effekt.Compiler <----- compiles code with  ------ Driver ------ implements UI with -----> kiama.util.Compiler
 */
trait Driver extends kiama.util.Compiler[EffektConfig, EffektError] { outer =>

  object messaging extends AnsiColoredMessaging

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) { val messaging = outer.messaging }

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit = ()

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = ()

  /**
   * Outputs the timing information captured in [[effekt.util.Timers]] by [[effekt.context.Context]]. Either a JSON file
   * is written to disk or a plain text message is written to stdout.
   */
  def outputTimes(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  def showIR(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  def writeIRs(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  /**
   * Overridden in [[Server]] to also publish core and js compilation results to VSCode
   */
  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()
}
