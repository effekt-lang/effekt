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
