package effekt
package typer

import effekt.symbols.ErrorMessageInterpolator

sealed trait ErrorContext { def polarity: Polarity }
sealed trait PositiveContext extends ErrorContext { def polarity = Covariant }
sealed trait NegativeContext extends ErrorContext { def polarity = Contravariant }
sealed trait InvariantContext extends ErrorContext { def polarity = Invariant }

object ErrorContext {

  /**
   * A generic context, checking a type against an expected type at position [[checkedTree]].
   */
  case class Expected(checkedTree: source.Tree) extends PositiveContext

  /**
   * A pattern matching context, checking a scrutinee against the return type of the match pattern [[pattern]]
   */
  case class PatternMatch(pattern: source.MatchPattern) extends PositiveContext

  /**
   * A context matching a declared type [[declared]] against the type defined at the use site [[defined]].
   */
  case class Declaration(param: source.Param, declared: symbols.Type, defined: symbols.Type) extends NegativeContext

  case class MergeLowerBounds() extends PositiveContext
  case class MergeUpperBounds() extends NegativeContext
  case class MergeInvariant() extends InvariantContext

  case class TypeConstructor(outer: ErrorContext) extends InvariantContext
  case class TypeConstructorArgument(outer: ErrorContext) extends InvariantContext
  case class BoxedTypeBlock(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class BoxedTypeCapture(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class FunctionArgument(left: symbols.FunctionType, right: symbols.FunctionType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity.flip }
  case class FunctionReturn(outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class FunctionEffects(outer: ErrorContext) extends InvariantContext

  def explainMismatch(tpe1: symbols.Type, tpe2: symbols.Type, ctx: ErrorContext): String = pp"Expected $tpe2 but got $tpe1"
  def explainInContext(msg: String, ctx: ErrorContext): String = msg
}
