package effekt

import effekt.{ CompilationUnit, EffektConfig, ModuleDB }
import effekt.source.Tree
import effekt.symbols.{ Assertions, SymbolsDB, TypesDB }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.Source

trait Phase {
  def name: String

  type State <: Product
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
class CompilerContext(
  var focus: Tree,
  val config: EffektConfig,
  val process: Source => Either[CompilationUnit, Messages]
) extends TypesDB with SymbolsDB with ModuleDB with ErrorReporter with Assertions { context =>

  val buffer: MessageBuffer = new MessageBuffer

  /**
   * This is useful to write code like: reporter in { ... implicitly uses reporter ... }
   */
  def in[T](block: (given this.type) => T): T = {
    val before = phases._state
    val result = block(given this)
    phases._state = before
    result
  }

  object phases {

    var _current: Phase = NoPhase
    var _state: Product = _

    def init(p: Phase)(state: p.State): Unit = {
      _current = p
      _state = state
    }

    def get(p: Phase): p.State =
      if (_current == p) { _state.asInstanceOf[p.State] }
      else sys.error(s"Trying to access state of a different phase (${_current})")

    def put(p: Phase)(state: p.State): CompilerContext = {
      _state = state
      context
    }

    def update(p: Phase)(f: p.State => p.State): CompilerContext =
      put(p)(f(get(p)))
  }
}