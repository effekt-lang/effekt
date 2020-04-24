package effekt
package context

import effekt.namer.{ NamerOps, NamerState }
import effekt.typer.{ TyperOps, TyperState }
import effekt.source.{ ModuleDecl, Tree }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

/**
 * The compiler context consists of
 * - configuration (immutable)
 * - symbols (mutable database)
 * - types (mutable database)
 * - error reporting (mutable focus)
 */
abstract class Context(val compiler: Compiler)
    // Namer
    extends SymbolsDB
    with NamerOps
    with ModuleDB
    // Typer
    with TypesDB
    with TyperOps
    // Util
    with ErrorReporter { self =>

  // bring the context itself in scope
  implicit val context: Context = self

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

  /**
   * The state of the namer phase
   */
  private var _namerState: NamerState = _
  def namerState: NamerState = _namerState
  def namerState_=(st: NamerState): Unit = _namerState = st

  /**
   * The state of the typer phase
   */
  private var _typerState: TyperState = _
  def typerState: TyperState = _typerState
  def typerState_=(st: TyperState): Unit = _typerState = st

  def using[T](module: Module = module, focus: Tree = focus)(block: => T): T = this in {
    self.module = module
    self.focus = focus
    block
  }

  /**
   * This is useful to write code like: reporter in { ... implicitly uses reporter ... }
   */
  def in[T](block: => T): T = {
    val namerBefore = namerState
    val typerBefore = typerState
    val focusBefore = focus
    val moduleBefore = module
    val result = block
    // we purposefully do not include the reset into `finally` to preserve the
    // state at the error position
    namerState = namerBefore
    typerState = typerBefore
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
