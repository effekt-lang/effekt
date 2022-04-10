package effekt
package context

import effekt.namer.NamerOps
import effekt.typer.TyperOps
import effekt.core.TransformerOps
import effekt.regions.{ RegionCheckerOps, RegionReporter }
import effekt.source.{ CapabilityPassingOps, Tree }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import effekt.symbols.Module
import kiama.util.Messaging.Messages
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
abstract class Context(val positions: Positions)
    extends Compiler
    with NamerOps
    with ModuleDB
    with TyperOps
    with CapabilityPassingOps
    with RegionCheckerOps
    with RegionReporter
    with TransformerOps {

  // bring the context itself in scope
  implicit val context: Context = this

  // the currently processed module
  var module: Module = _

  // the currently processed node
  var focus: Tree = _

  var _buffer: MessageBuffer = new MessageBuffer
  def buffer = _buffer
  var _config: EffektConfig = _
  def config = _config

  /**
   * Clear current context to start processing a fresh unit
   */
  def setup(cfg: EffektConfig): Unit = {
    buffer.clear()
    _config = cfg
  }

  def using[T](module: Module = module, focus: Tree = focus)(block: => T): T = this in {
    this.module = module
    this.focus = focus
    block
  }

  /**
   * This is useful to write code like: reporter in { ... implicitly uses reporter ... }
   */
  override def in[T](block: => T): T = {
    val focusBefore = focus
    val moduleBefore = module
    val result = super.in(block)

    // we purposefully do not include the reset into `finally` to preserve the
    // state at the error position
    focus = focusBefore
    module = moduleBefore
    result
  }

  // temporarily switches the message buffer to collect messages
  def withMessages[T](block: => T): (Messages, T) = {
    val bufferBefore = _buffer

    _buffer = new MessageBuffer
    val res = block
    val msgs = _buffer.get
    _buffer = bufferBefore
    (msgs, res)
  }

}
