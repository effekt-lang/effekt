package effekt
package symbols

import org.bitbucket.inkytonik.kiama.util.Counter

/**
 * A symbol uniquely represents a code entity
 *
 * We use the unique id of a symbol to represent it. That is, symbols
 * are considert equal, if and only if their id is the same.
 *
 * This code is taken from the Kiama Named implementation and moved to the toplevel.
 * This way ALL symbols are Named and the dependencies are reduced
 *
 * TODO should we add an (optional) pointer to the original source tree (that itself contains the position)?
 *      this way it should be easy to have jumpToDefinition.
 */
trait Symbol {

  /**
   * The name of this symbol
   */
  val name: Name

  /**
   * The unique id of this symbol
   */
  lazy val id: Int = Symbol.fresh.next()

  /**
   * Does this symbol denote a builtin?
   */
  def builtin = false

  /**
   * Is this symbol synthesized? (e.g. a constructor or field access)
   */
  def synthetic = false

  override def hashCode: Int = id
  override def equals(other: Any): Boolean = other match {
    case other: Symbol => this.id == other.id
    case _             => false
  }

  override def toString: String = name.toString
}
object Symbol {
  val fresh = new Counter(0)
}
