package effekt.symbols

import effekt.source.Id
import effekt.context.Context

sealed trait Name {
  /**
   * The local part of the name relative to its potential parent.
   *
   * @example The local part of the name `"foo.baz.bar"` is `"bar"`.
   */
  val localName: String

  /**
   * Computes the qualified version of this name.
   * @return a String containing the local part of this name prefixed by their parent name. Components are separated by the point character (`.`).
   *
   * @note this should only be used for reporting errors, not for code generation.
   */
  def qualifiedName: String

  /**
   * @return The parent of this name or [[None]] if this name represents a top-level name.
   */
  def parentOption: Option[Name]

  /**
   * @return The qualified name of the parent or `None` if this name doesn't have a parent.
   */
  def qualifiedParentOption: Option[String] = parentOption.map((parent) => parent.qualifiedName)

  /**
   * Creates a new name with the same parent as this name and the changed local name.
   * @param f rename function.
   * @return A Name with local name equal to `f(this.localName)`.
   */
  def rename(f: String => String): Name

  /**
   * Creates a new [[NestedName]] with this name as the parent.
   * @param name the local name of the nested name.
   * @return A [[NestedName]] with this Name as parent.
   */
  def nested(name: String) = this match {
    case EmptyName => Name(name)
    case _         => NestedName(this, name)
  }

  override def toString = localName
}

case object EmptyName extends Name {
  def parentOption: Option[Name] = None

  val localName: String = ""
  def qualifiedName: String = ""

  def rename(f: String => String): Name = this
}

/**
 * A Name without a parent, e.g. the name of a global symbol.
 * @param localName the local name which is also the qualified name.
 * The local name should not contain point characters (`'.'`). This could lead to unexpected behavior.
 *
 * @note Don't use the constructor of this type directly. Instead use [[Name(qualifiedName)]] to safely convert strings to names.
 */
case class ToplevelName(localName: String) extends Name {
  def parentOption: Option[Name] = None

  def qualifiedName: String = localName

  def rename(f: String => String): Name = ToplevelName(f(localName))
}

/**
 * A Name which is a child of a other name, e.g. the name of a nested module.
 *
 * @param parent The parent of this name.
 * @param localName The local name relative to the parent.
 * The local name should not contain point characters (`'.'`). This could lead to unexpected behavior.
 *
 * @note Creation of [[NestedName]] via the [[Name.nested()]] method is prefered.
 */
case class NestedName(parent: Name, localName: String) extends Name {
  def parentOption: Some[Name] = Some(parent)

  def qualifiedName: String = s"${parent.qualifiedName}.$localName"

  def rename(f: String => String): Name = NestedName(parent, f(localName))
}

// Pseudo-constructors to safely convert ids and strings into names.
object Name {
  /**
   * Creates a name for the ID inside the module of the current context.
   *
   * @param id The id to be converted into a name.
   * @param C The implicit context used to find the current module.
   * @return A qualified name inside the module.
   */
  def apply(id: Id)(implicit C: Context): Name = Name.apply(id.name, C.module)

  /**
   * Creates a qualified name for a symbol inside the given module.
   *
   * @param name the local name of the symbol.
   * @param module the module containing the symbol.
   * @return A name with the module path as a parent.
   *
   * @example The name `"baz"` inside the module with the path `"foo/bar"` is parsed as qualified name `"foo.bar.baz"`
   */
  def apply(name: String, module: Module): Name = module.name.nested(name)

  /**
   * Parses a [[Name]] from a String which might be a qualified name.
   * @param name A non-empty String which might be a qualified or local name.
   * @return A Name where [[Name.qualifiedName]] is equal to the input name.
   *
   * @example `Name("foo") == ToplevelName("foo")`
   * @example `Name("foo.bar.baz") == NestedName(NestedName(ToplevelName("foo"), "bar"), "baz")`
   */
  def apply(name: String): Name = {
    if (name.isEmpty)
      return EmptyName

    val segments = name.split('.')
    val top: Name = ToplevelName(segments.head)
    segments.drop(1).foldLeft(top)((parent, segment) => parent.nested(segment))
  }

  /**
   * Convert a module path into a valid name.
   *
   * @param path the module path where each segment is separated by a slash character (`'/'`).
   * @return a qualified name with the same name components as the input path.
   *
   * @example `module("foo/bar/baz") == Name("foo.bar.baz")`
   * @note this method might be removed in the future because module paths don't exists in the new module-system.
   */
  def module(path: String): Name = Name(path.replace("/", "."))
}
