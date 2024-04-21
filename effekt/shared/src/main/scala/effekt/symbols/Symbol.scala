package effekt
package symbols

import kiama.util.Counter

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
   * Is this symbol synthesized? (e.g. a constructor or field access)
   */
  def synthetic = false

  override def hashCode: Int = id
  override def equals(other: Any): Boolean = other match {
    case other: Symbol => this.id == other.id
    case _             => false
  }

  def show: String = name.toString + id

  override def toString: String = name.toString
}
object Symbol {
  val fresh = new Counter(0)
}
