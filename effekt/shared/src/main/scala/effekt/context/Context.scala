package effekt
package context

import effekt.namer.NamerOps
import effekt.typer.TyperOps
import effekt.source.Tree
import effekt.util.messages.{ ErrorReporter, EffektMessages }
import effekt.symbols.Module

import kiama.util.Positions

/**
 * Phases like Typer can add operations to the context by extending this trait
 *
 * For example, see TyperOps
 */
trait ContextOps
    extends ErrorReporter
    with AnnotationsDB { self: Context =>

  /**
   * Used throughout the compiler to create a new "scope"
   *
   * Each XOps slice can define what a new "scope" means and
   * backup and restore state accordingly. Overriding definitions
   * should call `super.in(block)`.
   */
  def in[T](block: => T): T = block
}

/**
 * The compiler context consists of
 * - configuration (immutable)
 * - symbols (mutable database)
 * - types (mutable database)
 * - error reporting (mutable focus)
 */
abstract class Context(val positions: Positions) extends TyperOps {

  // bring the context itself in scope
  implicit val context: Context = this

  // the currently processed module
  var module: Module = _

  // the currently processed node
  var focus: Tree = _

  var _config: EffektConfig = _
  def config = _config


  /**
   * Clear current context to start processing a fresh unit
   */
  def setup(cfg: EffektConfig): Unit = ()

  def using[T](module: Module = module, focus: Tree = focus)(block: => T): T = ???

  override def in[T](block: => T): T = ???

  // temporarily switches the message buffer to collect messages
  def withMessages[T](block: => T): (EffektMessages, T) = ???
}

/**
 * Helper method to find the currently implicit context
 */
def Context(using C: Context): C.type = C
