package effekt
package typer

import effekt.context.Context
import effekt.source.MatchPattern
import effekt.symbols.*
import effekt.symbols.builtins.{ TBottom, TTop }
import effekt.util.messages.ErrorReporter


sealed trait Polarity { def flip: Polarity }
case object Covariant extends Polarity { def flip = Contravariant}
case object Contravariant extends Polarity { def flip = Covariant }
case object Invariant extends Polarity { def flip = Invariant }

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


/**
 * A unification scope -- every fresh unification variable is associated with a scope.
 *
 * Structural comparison of types are outsourced into [[TypeComparer]], [[TypeUnifier]], and
 * [[TypeMerger]]. This way, the dependencies are a bit clearer and testability is improved.
 *
 * TODO
 *   - [ ] All incoming types need to be "normalized": substituted and dealiased.
 */
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
