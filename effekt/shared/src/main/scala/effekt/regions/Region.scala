package effekt
package regions

import effekt.context.Context
import effekt.source.Tree
import effekt.symbols.Symbol

sealed trait Region {

}
object Region {
  // user facing, we use lists
  def apply(regs: List[Symbol]): RegionSet = new RegionSet(regs.toSet)

  val empty: RegionSet = new RegionSet(Set.empty)
  def apply(s: Symbol): RegionSet = new RegionSet(Set(s))

}

/**
 * A region variable introduced by unification
 *
 * source is the tree at which position this region variable has been introduced
 * we use it mainly for error reporting
 */
class RegionVar(val id: Int, val source: Tree) extends Region {

}

/**
 * Conceptually: A set of region variables
 *
 * { x, y, z, ?r1, ?r2 } = {x, y, z} u ?r1 u ?r2
 */
class RegionSet(val regions: Set[Symbol]) extends Region {

}
