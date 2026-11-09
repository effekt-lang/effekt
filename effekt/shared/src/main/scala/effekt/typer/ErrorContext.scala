package effekt
package typer

import effekt.util.messages.{ErrorMessageReifier, Message, Reason, StructuredMessage, Subdiagnostics, TypeMismatch, note}

sealed trait ErrorContext { def polarity: Polarity; def coercible: Option[Coercion => Unit] }
sealed trait PositiveContext extends ErrorContext { def polarity = Covariant }
sealed trait NegativeContext extends ErrorContext, NotCoercible { def polarity = Contravariant }
sealed trait InvariantContext extends ErrorContext, NotCoercible { def polarity = Invariant }
trait NotCoercible { def coercible: Option[Coercion => Unit] = None }

object ErrorContext {

  /**
   * A generic context, checking a type against an expected type at position [[checkedTree]].
   *
   * TODO we use this for value types AND blocktypes (which cannot be coerced)
   */
  case class Expected(got: symbols.Type, exp: symbols.Type, checkedTree: source.Tree, coercible: Option[Coercion => Unit]) extends PositiveContext

  /**
   * A pattern matching context, checking a scrutinee of type [[scrutinee]] against the type [[matched]] of the match pattern [[pattern]]
   *
   * TODO we need to have the scrutinee since this is the one that we would add a coercion on.
   */
  case class PatternMatch(scrutinee: symbols.ValueType, matched: symbols.ValueType, pattern: source.MatchPattern) extends PositiveContext, NotCoercible

  // TODO this should have a tree where the coercion _could_ be inserted.
  case class MergeTypes(left: symbols.Type, right: symbols.Type, leftArm: Arm, rightArm: Arm) extends PositiveContext, NotCoercible

  enum Arm {
    case IfBranch(body: source.Stmt)
    case WhileBranch(body: Option[source.Stmt])
    case MatchClause(body: source.Stmt)

    def tree: Option[source.Stmt] = this match {
      case IfBranch(body)    => Some(body)
      case WhileBranch(body) => body
      case MatchClause(body) => Some(body)
    }

    def kind: String = this match {
      case IfBranch(_) | WhileBranch(_) => "branch"
      case MatchClause(_)               => "clause"
    }
  }

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

  def root(ctx: ErrorContext): ErrorContext = ctx match {
    case MergeInvariant(outer)          => root(outer)
    case TypeConstructor(outer)         => root(outer)
    case TypeConstructorArgument(outer) => root(outer)
    case BoxedTypeBlock(_, _, outer)    => root(outer)
    case BoxedTypeCapture(_, _, outer)  => root(outer)
    case FunctionArgument(_, _, outer)  => root(outer)
    case FunctionReturn(outer)          => root(outer)
    case FunctionEffects(outer)         => root(outer)
    case other                          => other
  }

  def claim(ctx: ErrorContext, issue: => StructuredMessage): Reason = root(ctx) match {
    case Expected(got, exp, _, _)            => TypeMismatch(exp, got)
    case Declaration(_, declared, defined)   => Message(pretty"Type ${defined} does not match the declared type ${declared}.")
    case PatternMatch(scrutinee, matched, _) => Message(pretty"Pattern matches against type ${matched} but scrutinee has type ${scrutinee}.")
    case MergeTypes(_, _, arm, _)            => arm match {
      case Arm.IfBranch(_)    => Message(pretty"The branches of this `if` have incompatible types.")
      case Arm.WhileBranch(_) => Message(pretty"The branches of this `while` have incompatible types.")
      case Arm.MatchClause(_) => Message(pretty"The clauses of this `match` have incompatible types.")
    }
    case _                                   => Message(issue)
  }

  def mismatch(left: symbols.Type, right: symbols.Type, ctx: ErrorContext): StructuredMessage = root(ctx) match {
    case _: MergeTypes => pretty"${left} and ${right} differ"
    case _             => pretty"Expected ${right} but got ${left}"
  }

  /** Is the root claim of [[ctx]] already about [[left]] and [[right]] (up to boxing?) */
  def isClaimed(left: symbols.Type, right: symbols.Type, ctx: ErrorContext): Boolean = {
    def unboxed(t: symbols.Type): symbols.Type = t match {
      case symbols.BoxedType(tpe, _) => tpe
      case other                     => other
    }
    def same(t1: symbols.Type, t2: symbols.Type) = unboxed(t1) == unboxed(t2)

    root(ctx) match {
      case Expected(got, exp, _, _)            => same(left, got) && same(right, exp)
      case Declaration(_, declared, defined)   => same(left, defined) && same(right, declared)
      case PatternMatch(scrutinee, matched, _) => same(left, scrutinee) && same(right, matched)
      case MergeTypes(l, r, _, _)              => (same(left, l) && same(right, r)) || (same(left, r) && same(right, l))
      case _                                   => false
    }
  }

  /** Why a comparison failed, as subdiagnostics. */
  def explain(issue: Option[StructuredMessage], ctx: ErrorContext)(using Subdiagnostics): Unit = root(ctx) match {
    case _: CaptureFlow | _: MergeCaptures => ()
    case MergeTypes(left, right, leftArm, rightArm) =>
      note(pretty"This ${leftArm.kind} has type ${left}.", leftArm.tree.map(_.span))
      note(pretty"This ${rightArm.kind} has type ${right}.", rightArm.tree.map(_.span))
      issue.foreach { i => note(pretty"${i}.") }
    case _ =>
      issue.foreach { i => note(pretty"${i}.") }
  }
}
