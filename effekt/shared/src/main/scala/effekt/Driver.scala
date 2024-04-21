package effekt

import effekt.context.{ Context }
import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import kiama.util.{ Source }
import effekt.util.{ AnsiColoredMessaging }
import effekt.util.messages.EffektError


import scala.sys.process.Process

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

  override def compileSource(source: Source, config: EffektConfig): Unit = ()

  def outputTimes(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  def showIR(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  def writeIRs(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()

  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = ()
}
