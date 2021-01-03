package effekt
package regions

/**
 * Equality constraint on regions -- introduced by Typer and collected
 * as part of Substitution
 */
case class RegionEq(exp: Region, got: Region, tree: source.Tree) {
  override def toString = s"$exp =!= $got"
}
