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


  case class CaptureFlow(from: symbols.Captures, to: symbols.Captures, checkedTree: source.Tree) extends PositiveContext

  case class MergeLowerBounds() extends PositiveContext
  case class MergeUpperBounds() extends NegativeContext
  case class MergeInvariant(outer: ErrorContext) extends InvariantContext

  case class TypeConstructor(outer: ErrorContext) extends InvariantContext
  case class TypeConstructorArgument(outer: ErrorContext) extends InvariantContext
  case class BoxedTypeBlock(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class BoxedTypeCapture(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class FunctionArgument(left: symbols.FunctionType, right: symbols.FunctionType, outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity.flip }
  case class FunctionReturn(outer: ErrorContext) extends ErrorContext { def polarity = outer.polarity }
  case class FunctionEffects(outer: ErrorContext) extends InvariantContext

  // TODO defer rendering of error messages to Context
  def explainMismatch(tpe1: symbols.Type, tpe2: symbols.Type, outerCtx: ErrorContext): String =

    def go(ctx: ErrorContext): String = ctx match {

      case Expected(tree) => pp"Expected $tpe2 but got $tpe1"
      case PatternMatch(pattern) => pp"Pattern expected $tpe2 but scrutinee has type $tpe1"
      case Declaration(param, declared, defined) => pp"Type $defined does not match the declared type $declared."

      // This case should not occur.
      case CaptureFlow(from, to, tree) => sys error "Internal error"

      case MergeLowerBounds() | MergeUpperBounds() => pp"Trying to merge the two types $tpe1 and $tpe2"
      case MergeInvariant(outer) => go(outer)

      case BoxedTypeBlock(left, right, outer) => go(outer)
      case BoxedTypeCapture(left, right, outer) => go(outer)
      case FunctionArgument(left, right, outer) =>
        go(outer) + pp"\n  comparing the argument types of\n    ${left} (given)\n    ${right} (expected)"
      case FunctionReturn(outer) => go(outer) + pp"\n  when comparing the return type of the function."
      case FunctionEffects(outer) => go(outer)
      case TypeConstructor(outer) => go(outer)
      case TypeConstructorArgument(outer) => go(outer)
    }
    go(outerCtx)

  def explainInContext(msg: String, ctx: ErrorContext): String =
    msg
}
