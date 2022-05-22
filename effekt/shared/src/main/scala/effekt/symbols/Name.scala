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
  val qualifiedName = (prefix :+ name).mkString(".")
  def rename(f: String => String): QualifiedName = QualifiedName(prefix, f(name))
}

// Pseudo-constructors to safely convert ids and strings into names.
object Name {

  def local(id: Id): LocalName = local(id.name)

  def local(id: String): LocalName = LocalName(id)

  /**
   * Creates a name for the ID inside the module of the current context.
   *
   * @param id The id to be converted into a name.
   * @param C The implicit context used to find the current module.
   * @return A qualified name inside the module.
   */
  def qualified(id: Id)(implicit C: Context): Name = Name.qualified(id.name, C.module)

  /**
   * Creates a qualified name for a symbol inside the given module.
   *
   * @param name the local name of the symbol.
   * @param module the module containing the symbol.
   * @return A name with the module path as a parent.
   *
   * @example The name `"baz"` inside the module with the path `"foo/bar"` is parsed as qualified name `"foo.bar.baz"`
   */
  def qualified(name: String, module: Module): QualifiedName = QualifiedName(module.path.split("/").toList, name)

}
