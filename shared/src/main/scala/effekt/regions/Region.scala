package effekt
package regions

import effekt.context.Context
import effekt.source.Tree
import effekt.symbols.Symbol

sealed trait Region {

  /**
   * Is this region a concrete region set? That is, will `asRegionSet`
   * be successful without errors?
   */
  def isInstantiated: Boolean

  /**
   * View this region as a concrete region set
   */
  def asRegionSet(implicit C: Context): RegionSet

  /**
   * Variant of asRegionSet that does not raise errors
   */
  def getRegionSet: Option[RegionSet]

  /**
   * View this region as a region variable
   */
  def asRegionVar(implicit C: Context): RegionVar

  /**
   * Runs the given block if this region is instantiated
   */
  def withRegion[T](block: RegionSet => T): Option[T] =
    getRegionSet.map(block)

}
object Region {
  // user facing, we use lists
  def apply(regs: List[Symbol]): RegionSet = new RegionSet(regs.toSet)
  def unapply(regs: RegionSet): Option[List[Symbol]] = Some(regs.regions.toList)

  val empty: RegionSet = Region(Nil)
  def apply(s: Symbol): RegionSet = Region(List(s))

  private var lastId = 0

  def fresh(source: Tree): RegionVar = {
    lastId += 1
    new RegionVar(lastId, source)
  }
}

/**
 * A region variable introduced by unification
 *
 * source is the tree at which position this region variable has been introduced
 * we use it mainly for error reporting
 */
class RegionVar(val id: Int, val source: Tree) extends Region {

  // sys.error(s"Created fresh variable ${id}")

  private var _region: Option[Region] = None

  // do not treat region variables that point to other variables as instantiated
  def isInstantiated: Boolean = _region match {
    case Some(r: RegionSet) => true
    case _                  => false
  }

  /**
   * Once we know the actual region this variable represents, we
   * can instantiate it (this way we do not need to implement substitution).
   */
  def instantiate(r: Region): Unit = _region = Some(r)

  override def toString = _region match {
    case None    => s"?r${id}"
    case Some(r) => r.toString // s"?r${id} -> ${r.toString}"
  }

  override def equals(a: Any): Boolean = a match {
    case r: RegionVar => id == r.id
    case _            => false
  }
  override def asRegionSet(implicit C: Context): RegionSet =
    _region.map { r => r.asRegionSet } getOrElse {
      C.panic(s"Cannot find region for unification variable ${this}")
    }
  override def asRegionVar(implicit C: Context): RegionVar = this

  def getRegionSet: Option[RegionSet] = _region.flatMap {
    case r: RegionSet => Some(r)
    case r: RegionVar => r.getRegionSet
  }
}

/**
 * Conceptually: A set of region variables
 *
 * { x, y, z, ?r1, ?r2 } = {x, y, z} u ?r1 u ?r2
 */
class RegionSet(val regions: Set[Symbol]) extends Region {

  def isInstantiated = true

  def contains(r: Symbol): Boolean = regions.contains(r)

  def ++(other: RegionSet): RegionSet = new RegionSet(regions ++ other.regions)
  def --(other: RegionSet): RegionSet = new RegionSet(regions -- other.regions)

  override def equals(other: Any): Boolean = other match {
    case r: RegionSet => r.regions == regions
    case _            => false
  }

  def subsetOf(other: RegionSet): Boolean = regions.subsetOf(other.regions)

  def substitute(x: Symbol, r: RegionSet): RegionSet =
    if (contains(x)) { new RegionSet((regions - x) ++ r.regions) } else this

  def isEmpty: Boolean = regions.isEmpty
  def nonEmpty: Boolean = regions.nonEmpty
  def intersect(other: RegionSet): RegionSet = new RegionSet(regions.intersect(other.regions))

  override def toString = s"{${regions.map(_.toString).mkString(", ")}}"

  override def asRegionSet(implicit C: Context): RegionSet = this
  override def asRegionVar(implicit C: Context): RegionVar =
    C.panic(s"Required a region variable, but got: ${this}")

  override def getRegionSet: Option[RegionSet] = Some(this)
}
object RegionSet {

  /**
   * Extracts the underlying region set for instantiated region variables
   */
  def unapply(r: Region): Option[RegionSet] = r.getRegionSet
}
