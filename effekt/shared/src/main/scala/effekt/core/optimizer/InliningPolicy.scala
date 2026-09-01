package effekt
package core
package optimizer

import effekt.core.optimizer.Normalizer.Context

sealed trait InliningPolicy {
  def apply(site: CallSite)(using Context): Boolean
}

case class CallSite(
  callee: BlockLit,
  boundBy: Option[BlockVar],
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
 */
class Default(threshold: Int, onceLimit: Option[Int]) extends InliningPolicy {

  def apply(site: CallSite)(using Context): Boolean = site.boundBy match {
    case None => hasKnownBlockArg(site)
    case Some(callee) =>
      // 1) don't inline recursive identifiers
      !Normalizer.isRecursive(callee.id) &&
        // 2) if the callee is only used once, let [[usedOnce]] try first
        (usedOnce(callee, site) ||
          // 3) otherwise, inline if below [[threshold]]
          site.callee.body.size <= threshold)
  }

  /** A callee used exactly once is inlined if it's at most [[onceLimit]], and only if it doesn't move scopes. */
  private def usedOnce(callee: BlockVar, site: CallSite)(using C: Context): Boolean =
    Normalizer.isOnce(callee.id) &&
      onceLimit.forall { limit => site.callee.body.size <= limit } &&
        (C.prompts == 0 || !installsScope(site.callee.body)) // don't move resets/regions

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
