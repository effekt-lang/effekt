package effekt
package typer

import effekt.context.Context
import effekt.symbols.*
import effekt.util.messages.ErrorReporter


sealed trait Polarity { def flip: Polarity }

/**
 * The state of the unification scope, used for backtracking on overload resolution
 *
 * See [[Unification.backup]] and [[Unification.restore]]
 */
case class UnificationState(
  scope: Scope,
  constraints: Constraints
)

sealed trait Scope

class Unification(using C: ErrorReporter) extends TypeInstantiator { self =>

  // State of the unification engine
  // -------------------------------
  private var scope: Scope = ???
  protected var constraints = new Constraints

  def requireSubtype(t1: ValueType, t2: ValueType, errorContext: ErrorContext): Unit = ()

}



case class Instantiation(values: Map[TypeVar, ValueType], captures: Map[Capture, Captures])

trait TypeInstantiator { self: Unification =>

}
