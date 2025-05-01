package effekt
package context

import effekt.namer.NamerOps
import effekt.typer.TyperOps
import effekt.core.TransformerOps
import effekt.source.Tree
import effekt.util.messages.{ ErrorReporter, EffektMessages }
import effekt.util.Timers
import effekt.symbols.Module

import kiama.util.Positions

/**
 * Phases like Typer can add operations to the context by extending this trait
 *
 * For example, see TyperOps
 */
trait ContextOps
    extends ErrorReporter
    with TreeAnnotations
    with SourceAnnotations 
    with SymbolAnnotations { self: Context =>

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
    extends NamerOps
    with TyperOps
    with ModuleDB
    with TransformerOps
    with Timers {

  // bring the context itself in scope
  implicit val context: Context = this

  // the currently processed module
  var module: Module = _

  // the currently processed node
  var focus: Tree = _

  var _config: EffektConfig = _
  def config = _config

  // cache used by tasks to save their results (in addition to information in the AnnotationsDB)
  var cache: util.Task.Cache = util.Task.emptyCache

  // We assume the backend never changes
  lazy val backend = config.backend()
  lazy val compiler = backend.compiler
  lazy val runner = backend.runner

  /**
   * Clear current context to start processing a fresh unit
   */
  def setup(cfg: EffektConfig): Unit = {
    messaging.clear()
    // No timings are captured in server mode to keep the memory footprint small. Since the server is run continuously,
    // the memory claimed by the timing information would increase continuously.
    clearTimers(cfg.timed())
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
  def withMessages[T](block: => T): (EffektMessages, T) = {
    val bufferBefore = messaging.buffer

    messaging.clear()

    val res = block
    val msgs = messaging.buffer
    messaging.buffer = bufferBefore
    (msgs, res)
  }

  /**
   * The compiler state
   */
  case class State(annotations: DB, cache: util.Task.Cache)

  /**
   * Export the compiler state
   */
  def backup: State = State(this.db, this.cache)

  /**
   * Restores the compiler state from a backup
   */
  def restore(s: State) = {
    db = s.annotations
    cache = s.cache
  }
}

/**
 * Helper method to find the currently implicit context
 */
def Context(using C: Context): C.type = C
