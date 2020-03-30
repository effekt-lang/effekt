package effekt
package context

import effekt.namer.{ NamerOps, NamerState }
import effekt.typer.{ TyperOps, TyperState }
import effekt.source.{ ModuleDecl, Tree }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }

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
    with ErrorReporter { context =>

  var focus: Tree = _

  var config: EffektConfig = _

  val buffer: MessageBuffer = new MessageBuffer

  def setup(ast: ModuleDecl, cfg: EffektConfig): Unit = {
    config = cfg
    focus = ast
    buffer.clear()
  }

  /**
   * The state of the namer phase
   */
  var _namerState: NamerState = _
  def namerState: NamerState = _namerState
  def namerState_=(st: NamerState): Unit = _namerState = st

  /**
   * The state of the typer phase
   */
  var _typerState: TyperState = _
  def typerState: TyperState = _typerState
  def typerState_=(st: TyperState): Unit = _typerState = st

  /**
   * This is useful to write code like: reporter in { ... implicitly uses reporter ... }
   */
  def in[T](block: => T): T = {
    val namerBefore = namerState
    val typerBefore = typerState
    val focusBefore = focus
    val result = block
    // we purposefully do not include the reset into `finally` to preserve the
    // state at the error position
    namerState = namerBefore
    typerState = typerBefore
    focus = focusBefore
    result
  }
}
