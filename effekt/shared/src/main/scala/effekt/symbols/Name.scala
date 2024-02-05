package effekt.symbols

import effekt.source.Id
import effekt.context.Context

sealed trait Name {
  /**
   * The local part of the name
   */
  def name: String

  /**
   * Renames the local part of the name
   */
  def rename(f: String => String): Name

  override def toString = name
}

case object NoName extends Name {
  def name = "<anon>"
  def rename(f: String => String): NoName.type = this
}

/**
 * Names of local variables, local definitions, and members like fields
 */
case class LocalName(name: String) extends Name {
  def rename(f: String => String): LocalName = LocalName(f(name))
}

/**
 * Names of definitions that are globally reachable via a stable path
 */
case class QualifiedName(prefix: List[String], name: String) extends Name {
  val localName = name
  val qualifiedName = (prefix :+ name).mkString("::")
  def rename(f: String => String): QualifiedName = QualifiedName(prefix, f(name))
}

// Pseudo-constructors to safely convert ids and strings into names.
object Name {

  def local(id: Id): LocalName = local(id.name)

  def local(id: String): LocalName = LocalName(id)

}
