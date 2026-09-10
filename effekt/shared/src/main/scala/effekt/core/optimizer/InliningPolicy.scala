package effekt
package core
package optimizer

import scala.annotation.tailrec

import effekt.core.optimizer.Normalizer.Context

sealed trait InliningPolicy {
  def apply(site: CallSite)(using Context): Boolean
}

case class CallSite(
  callee: BlockLit,
  boundBy: Option[BlockVar],
  vargs: List[Expr],
  bargs: List[Block]
)

/** Inline a callee used exactly once, one with a known block argument, or one small enough. */
class Unique(threshold: Int) extends InliningPolicy {
  def apply(site: CallSite)(using Context): Boolean = site.boundBy match {
    // an anonymous callee cannot be recursive (has no name)
    case None => site.bargs.exists { b => b.isInstanceOf[BlockLit] }
    case Some(callee) =>
      !Normalizer.isRecursive(callee.id) &&
        (Normalizer.isOnce(callee.id) || site.callee.body.size <= threshold)
  }
}

/**
 * [[Unique]], with the used-once rule bounded.
 *
 * @param threshold the max size budget a call site starts with (`--max-inline-size`)
 * @param onceLimit size budget for [[usedOnce]]; `None` means unbounded (`--max-once-inline-size -1`)
 * @param carryingLimit size budget for [[carriesAnOperationToItsHandler]] (`--max-carrying-inline-size`)
 */
class Default(threshold: Int, onceLimit: Option[Int], carryingLimit: Int) extends InliningPolicy {

  def apply(site: CallSite)(using Context): Boolean = site.boundBy match {
    case None => hasKnownBlockArg(site)
    case Some(callee) =>
      // 1) don't inline recursive identifiers
      !Normalizer.isRecursive(callee.id) &&
        // 2) if the callee is only used once, let [[usedOnce]] try first
        (usedOnce(callee, site) ||
        // 3) or if the call carries an operation to the handler that serves it
        carriesAnOperationToItsHandler(site) ||
        // 4) otherwise, inline if [[affordable]]
        affordable(site))
  }

  /** Whether inlining this call would carry a `Reset` or `Region` into the extent of a prompt we are in. */
  private def movesAScopeIntoAPrompt(site: CallSite)(using C: Context): Boolean =
    C.prompts.nonEmpty && installsScope(site.callee.body) 

  /** A callee used exactly once is inlined if it's at most [[onceLimit]], and only if it doesn't move scopes. */
  private def usedOnce(callee: BlockVar, site: CallSite)(using C: Context): Boolean =
    Normalizer.isOnce(callee.id) &&
      onceLimit.forall { limit => site.callee.body.size <= limit } &&
        !movesAScopeIntoAPrompt(site)

  private def carriesAnOperationToItsHandler(site: CallSite)(using C: Context): Boolean =
    // 1) cheapest first: if there are block args
    site.bargs.nonEmpty &&
      // 2) and callee's body is within the [[carryingLimit]]
      site.callee.body.size <= carryingLimit &&
      // 3) and there's a block arg which shifts into an enclosing prompt
      site.bargs.exists { barg =>
        Normalizer.knownAndUsedOnce(barg).exists {
          case Block.New(impl) => shiftsToAnEnclosingPrompt(impl)
          case _ => false
        }
      } &&
      // 4) and finally, it does not move a scope into a prompt
      !movesAScopeIntoAPrompt(site) // somewhat costly, so we're trying this only at the end

  private def shiftsToAnEnclosingPrompt(impl: Implementation)(using C: Context): Boolean =
    object query extends Tree.Query[Unit, Boolean] {
      def empty = false
      def combine = _ || _
      override def stmt(using Unit) = {
        case Stmt.Shift(prompt, _, _) => C.prompts.contains(prompt.id)
      }
    }
    query.query(impl)(using ())

  /** Does the body fit the budget this call site can afford? */
  private def affordable(site: CallSite)(using Context): Boolean =
    // tries the normal threshold first to avoid calculating the size
    site.callee.body.size <= threshold ||
      site.callee.body.size <= threshold + discount(site)

  /** How much of the callee provably becomes dead once the arguments known *here* are substituted. */
  private def discount(site: CallSite)(using Context): Int = {
    // [[Normalizer.active]] is what `normalize` itself consults to fold an `If` or a `Match`, so
    // this predicts a reduction exactly when normalization would perform one.
    val known: Map[Id, Expr] = (site.callee.vparams zip site.vargs).flatMap {
      case (param, arg) => Normalizer.active(arg) match {
        case value @ (_: Expr.Literal | _: Expr.Make) => Some(param.id -> value)
        case _ => None
      }
    }.toMap

    if known.isEmpty then 0 else deadBranches(known, site.callee.body)
  }

  /**
   * The size of the branches that knowing each of [[known]] would delete.
   *
   * Conservative: a scrutinee inside an RHS we do not enter is not counted, and
   * under-counting only loses an inlining where over-counting would let the size grow.
   */
  private def deadBranches(known: Map[Id, Expr], body: Stmt): Int =
    @tailrec // this style is needed for perf. reasons :(
    def go(stmt: Stmt, acc: Int): Int = stmt match {
      // a binder cannot select a branch, so pass through it
      case Stmt.Val(_, _, rest) => go(rest, acc)
      case Stmt.Let(_, _, rest) => go(rest, acc)
      case Stmt.ImpureApp(_, _, _, _, _, rest) => go(rest, acc)
      case Stmt.Def(_, _, rest) => go(rest, acc)
      case Stmt.Alloc(_, _, _, rest) => go(rest, acc)
      case Stmt.Var(_, _, _, rest) => go(rest, acc)
      case Stmt.Get(_, _, _, _, rest) => go(rest, acc)
      case Stmt.Put(_, _, _, rest) => go(rest, acc)

      case Stmt.If(ValueVar(x, _), thn, els) if known.contains(x) => known(x) match {
        case Expr.Literal(true, _) => go(thn, acc + els.size)
        case Expr.Literal(false, _) => go(els, acc + thn.size)
        case _ => acc
      }

      case Stmt.Match(ValueVar(x, _), _, clauses, default) if known.contains(x) => known(x) match {
        case Expr.Make(_, tag, _, _) =>
          val dead = clauses.collect { case (t, clause) if t != tag => clause.size }.sum
          clauses.collectFirst { case (t, clause) if t == tag => clause } match {
            case Some(clause) => go(clause.body, acc + dead + default.map { d => d.size }.getOrElse(0))
            case None => default match {
              case Some(d) => go(d, acc + dead)
              case None => acc + dead
            }
          }
        case _ => acc
      }

      // past anything else we cannot see what dies, so nothing does
      case _ => acc
    }
    go(body, 0)


  /** A block argument known can be called directly instead of becoming a closure. */
  private def hasKnownBlockArg(site: CallSite): Boolean =
    site.bargs.exists {
      case _: BlockLit | _: Block.New => true
      case _: BlockVar | _: Block.Unbox => false
    }

  /** Whether inlining [[body]] would *move* a scope rather than copy computation. */
  private def installsScope(body: Stmt): Boolean =
    object query extends Tree.Query[Unit, Boolean] {
      def empty = false
      def combine = _ || _

      override def stmt(using Unit) = {
        case _: Stmt.Reset => true
        case _: Stmt.Region => true
      }
    }
    query.query(body)(using ())
}
