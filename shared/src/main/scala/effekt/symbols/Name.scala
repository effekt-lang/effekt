package effekt.symbols

import effekt.source.Id
import effekt.context.Context

trait Name {
  def name: String

  // this should only be used for reporting errors, not for code generation
  def qualified: String
  override def toString = name
  def rename(f: String => String): Name
}

case class LegacyName(name: String, module: LegacyModule) extends Name {
  def qualified: String = s"${module.name}.${name}"
  def rename(f: String => String): Name = copy(name = f(name))
}

case object NoName extends Name {
  def name = "<NoName>"
  def qualified: String = name
  def rename(f: String => String): Name = this
}

object Name {
  def apply(id: Id)(implicit C: Context): Name = LegacyName(id.name, C.module)
  def apply(name: String, module: LegacyModule) = LegacyName(name, module)
}

/**
  * Represents the name of a Symbol.
  *
  * @param module the name of the module in which this symbol was defined or None if the symbol was defined in the toplevel scope. 
  * @param name the local name of the symbol.
  */
case class SymbolName(module: Option[ModuleName], name: String) extends Name {
  def qualified: String = module match {
    case Some(m) => s"${m.qualified}.${name}"
    case None => name
  }

  def rename(f: String => String): Name = copy(name = f(name))
}


/**
  * Represents a module name which is either a toplevel name or a nested name.
  */
sealed abstract class ModuleName extends Name {
  /**
    * Creates a name for a submodule which is nested inside this module.
    *
    * @param localName local name of the submodule. Might be a nested name.
    */
  def submodule(localName: String): ModuleName.Nested = {
    assert(localName.nonEmpty, "Submodule name cannot be empty.")
    return localName.split(".").foldLeft(this)((module, name) => Nested(module, name))
  }
  /**
   * Creates a symbol name relative to this module name.
   *
   * @param relativeName the name of the symbol relative to this module name.
   */
  def symbol(relativeName: String): SymbolName = {
    assert(relativeName.nonEmpty, "Symbol name cannot be empty.")
    val segments = relativeName.split(".")
    val localName = segments.last
    val module = segments.dropRight(1).foldLeft(this)((module, name) => Nested(module, name))

    return SymbolName(module, localName)
  }

  // Compare module names by their qualified name.
  override def equals(obj: Object): Boolean = obj match {
    case m: ModuleName => m.qualified == this.qualified
    case _ => false
  }

  // Use hash of qualified name
  override def hashCode(): Int = qualified.hashCode()

  /**
    * @note Do not use the constructor of this type directly. Instead call `Name.module()` to safely convert strings into valid module names.
    */
  case class Toplevel(name: String) extends ModuleName {

    def qualified: String = name

    def rename(f: String => String): Name = copy(name = f(name))
  }

  /**
    * @note Do not use the constructor of this type directly. Instead call the `submodule(name)` function of an existing ModuleName.
    */
  case class Nested private (parent: ModuleName, name: Name) extends ModuleName {
    def qualified: String = parent.qualified + "." + name

    def rename(f: String => String): Name = copy(name = f(name))
  }
}