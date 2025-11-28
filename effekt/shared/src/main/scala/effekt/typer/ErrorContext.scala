package effekt
package typer

import effekt.symbols.ErrorMessageInterpolator

sealed trait ErrorContext { def polarity: Polarity; def coercible: Option[source.Tree] }
sealed trait PositiveContext extends ErrorContext { def polarity = Covariant }
sealed trait NegativeContext extends ErrorContext, NotCoercible { def polarity = Contravariant }
sealed trait InvariantContext extends ErrorContext, NotCoercible { def polarity = Invariant }
trait NotCoercible { def coercible: Option[source.Tree] = None }

object ErrorContext {

  /**
   * A generic context, checking a type against an expected type at position [[checkedTree]].
   *
   * TODO we use this for value types AND blocktypes (which cannot be coerced)
   */
  case class Expected(got: symbols.Type, exp: symbols.Type, checkedTree: source.Tree, coercible: Option[source.Tree]) extends PositiveContext

  /**
   * A pattern matching context, checking a scrutinee against the return type of the match pattern [[pattern]]
   *
   * TODO we need to have the scrutinee since this is the one that we would add a coercion on.
   */
  case class PatternMatch(pattern: source.MatchPattern) extends PositiveContext, NotCoercible

  // TODO this should have a tree where the coercion _could_ be inserted.
  //  it also raises the question: what if we have several "unit"s? Which one do we coerce into?
  // This would be easier, if we _only_ add coercions for effekt.Unit and effekt.Nothing
  case class MergeTypes(left: symbols.Type, right: symbols.Type) extends PositiveContext, NotCoercible

  /**
   * covariant, but doesn't admit coercions since it is not used on types, but captures.
   */
  case class CaptureFlow(from: symbols.Captures, to: symbols.Captures, checkedTree: source.Tree) extends PositiveContext, NotCoercible

  case class MergeCaptures() extends PositiveContext, NotCoercible

  /**
   * A context matching a declared type [[declared]] against the type defined at the use site [[defined]].
   *
   * Doesn't admit coercions.
   */
  case class Declaration(param: source.Param, declared: symbols.Type, defined: symbols.Type) extends NegativeContext


  case class MergeInvariant(outer: ErrorContext) extends InvariantContext
  case class TypeConstructor(outer: ErrorContext) extends InvariantContext
  case class TypeConstructorArgument(outer: ErrorContext) extends InvariantContext
  case class BoxedTypeBlock(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends InvariantContext
  case class BoxedTypeCapture(left: symbols.BoxedType, right: symbols.BoxedType, outer: ErrorContext) extends InvariantContext
  case class FunctionArgument(left: symbols.FunctionType, right: symbols.FunctionType, outer: ErrorContext) extends InvariantContext
  case class FunctionReturn(outer: ErrorContext) extends InvariantContext
  case class FunctionEffects(outer: ErrorContext) extends InvariantContext

  // TODO defer rendering of error messages to Context
  def explainMismatch(tpe1: symbols.Type, tpe2: symbols.Type, outerCtx: ErrorContext): String =

    def go(ctx: ErrorContext): String = ctx match {

      case Expected(got, exp, tree, coercible) =>
        val expRendered = pp"$exp"
        val gotRendered = pp"$got"
        val msg = if ((expRendered.size + gotRendered.size) < 25) {
          s"Expected $expRendered but got $gotRendered."
        } else {
          s"Expected type\n  $expRendered\nbut got type\n  $gotRendered"
        }
        if (tpe1 != got || tpe2 != exp)
          pp"$msg\n\nType mismatch between $tpe2 and $tpe1."
        else
          msg
      case PatternMatch(pattern) => pp"Pattern matches against type $tpe2 but scrutinee has type $tpe1."
      case Declaration(param, declared, defined) => pp"Type $defined does not match the declared type $declared."

      // TODO
      //  (1) carry through position information about left and right trees.
      //  (2) refactor so that explainMismatch is part of Messages and can add additional
      //      messages (with info about the trees and their types).
      case MergeTypes(left, right) =>
        val msg = pp"Different arms of a conditional/match have incompatible types.\n\nOne arm has type\n  $left\nwhile another one has type\n  $right"

        if (tpe2 != left || tpe1 != right)
          pp"$msg\n\nType mismatch between $tpe2 and $tpe1."
        else
          msg

      case MergeCaptures() => sys error "Should not occur"
      case CaptureFlow(from, to, tree) => sys error "Should not occur"

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
