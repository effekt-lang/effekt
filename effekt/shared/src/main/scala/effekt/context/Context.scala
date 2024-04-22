package effekt
package context

import effekt.typer.TyperOps
import effekt.util.messages.{ ErrorReporter, EffektMessages }
import effekt.symbols.Module

import kiama.util.Positions

trait ContextOps
    extends ErrorReporter
    with AnnotationsDB { self: Context =>

  def in[T](block: => T): T = block
}

abstract class Context(val positions: Positions) extends TyperOps {

  // bring the context itself in scope
  implicit val context: Context = this

  // the currently processed module
  var module: Module = _


  var _config: EffektConfig = _
  def config = _config


  /**
   * Clear current context to start processing a fresh unit
   */
  def setup(cfg: EffektConfig): Unit = ()

  override def in[T](block: => T): T = ???

  // temporarily switches the message buffer to collect messages
  def withMessages[T](block: => T): (EffektMessages, T) = ???
}

/**
 * Helper method to find the currently implicit context
 */
def Context(using C: Context): C.type = C
