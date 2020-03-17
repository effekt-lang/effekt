package effekt.symbols

import org.bitbucket.inkytonik.kiama.util.Counter

sealed trait Name {
  def name: String
  def qualified: String
  override def toString: String = name
}
case class LocalName(name: String) extends Name { def qualified = name }
case class QualifiedName(path: String, name: String) extends Name {
  def qualified: String = moduleName(path) + "." + name
}

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
   * The name of the symbol as it appears in the source program
   */
  val name: Name

  /**
   * The unique name of this symbol
   */
  lazy val uniqueName: String = name.toString + Symbol.fresh.next()

  /**
   * Does this symbol denote a builtin?
   */
  def builtin = false

  override def hashCode: Int = uniqueName.hashCode
  override def equals(other: Any): Boolean = other match {
    case other: Symbol => this.uniqueName == other.uniqueName
    case _ => false
  }

  override def toString = name.toString

  // we need `val decl: Tree` for reverse lookup in server-mode
}
object Symbol {
  val fresh = new Counter(0)
}
