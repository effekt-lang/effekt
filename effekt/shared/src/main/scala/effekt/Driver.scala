package effekt

import effekt.context.{ Context }
import kiama.util.{ Source }
import effekt.util.messages.EffektError



trait Driver extends kiama.util.Compiler[EffektConfig, EffektError] { outer =>


  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) { val messaging = ??? }

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
