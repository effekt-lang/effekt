package effekt
package regions

/**
 * Equality constraint on regions -- introduced by Typer and collected
 * as part of Substitution
 */
case class RegionEq(r1: Region, r2: Region) {
  override def toString = s"$r1 =!= $r2"

  // constraints are symmetric
  override def equals(other: Any): Boolean = other match {
    case RegionEq(r3, r4) => (r1 == r3 && r2 == r4) || (r1 == r4 && r2 == r3)
    case _                => false
  }

  override def hashCode(): Int = r1.hashCode() + r2.hashCode()
}
