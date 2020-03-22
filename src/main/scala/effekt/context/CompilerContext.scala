package effekt
package context

import effekt.namer.{ Namer, NamerState, NamerOps }
import effekt.typer.{ Typer, TyperState, TyperOps }
import effekt.source.{ ModuleDecl, Tree }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }

trait Phase {
  def name: String

  def Compiler(given ctx: CompilerContext): CompilerContext = ctx
}
object NoPhase extends Phase {
  def name = "no-phase"
}

/**
 * The compiler context consists of
 * - configuration (immutable)
 * - symbols (mutable database)
 * - types (mutable database)
 * - error reporting (mutable focus)
 */
abstract class CompilerContext
    // Namer
    extends SymbolsDB
    with NamerOps
    with ModuleDB
    // Typer
    with TypesDB
    with TyperOps
    // Util
    with Assertions
    with ErrorReporter { context =>

  var focus: Tree = _

  var config: EffektConfig = _

  val buffer: MessageBuffer = new MessageBuffer

  def setup(ast: ModuleDecl, cfg: EffektConfig) = {
    config = cfg
    focus  = ast
    buffer.clear()
  }

  /**
   * The different phases of the frontend
   */
  object namer extends Namer
  object typer extends Typer


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
  def in[T](block: (given this.type) => T): T = {
    val namerBefore = namerState
    val typerBefore = typerState
    val result = block(given this)
    namerState = namerBefore
    typerState = typerBefore
    result
  }

  object phases {

    var _current: Phase = NoPhase

    def init(p: Phase): Unit = {
      _current = p
    }
  }
}