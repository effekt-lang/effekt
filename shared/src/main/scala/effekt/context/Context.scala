package effekt
package context

import effekt.namer.{ NamerOps, NamerState }
import effekt.typer.{ TyperOps, TyperState }
import effekt.source.{ ModuleDecl, Tree }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * The compiler context consists of
 * - configuration (immutable)
 * - symbols (mutable database)
 * - types (mutable database)
 * - error reporting (mutable focus)
 */
abstract class Context(val positions: Positions)
    // Compiler phases
    extends Compiler
    // Namer
    with NamerOps
    with ModuleDB
    // Typer
    with TyperOps
    with AnnotationsDB
    // Util
    with ErrorReporter {

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
    this.module = module
    this.focus = focus
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

    // TyperState has two kinds of components:
    // - reader-like (like effects that are in scope)
    // - state-like (like annotations and unification constraints)
    //
    // The dynamic scoping of `in` should only affect the "reader" components of `typerState`, but
    // not the "state" components. For those, we manually perform backup and restore in typer.
    typerState = if (typerBefore != null) {
      val annos = typerState.annotations
      // keep the annotations
      typerBefore.copy(annotations = annos)
    } else { typerBefore }

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
