package effekt
package context

import effekt.typer.TyperOps
import effekt.util.messages.{ ErrorReporter }
import effekt.symbols.Module


trait ContextOps
    extends ErrorReporter
    with AnnotationsDB { self: Context =>

  def in[T](block: => T): T = block
}

sealed trait Positions

abstract class Context(val positions: Positions) extends TyperOps {

  // bring the context itself in scope
  implicit val context: Context = this

  // the currently processed module
  var module: Module = _

  override def in[T](block: => T): T = ???

}

/**
 * Helper method to find the currently implicit context
 */
def Context(using C: Context): C.type = C
