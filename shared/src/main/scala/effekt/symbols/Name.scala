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

  /**
    * Name of the effekt stdlib module.
    */
  val effektModule: ModuleName = Name.module("effekt")

  /**
    * Converts the given String into a qualified module name.
    *
    * @param qualifiedName a String containing the qualified name of the module.
    * @return A module name.
    */
  def module(qualifiedName: String): ModuleName = {
    assert(
      qualifiedName.nonEmpty,
      "Cannot create module name from empty string."
    )

    val segments = qualifiedName.split(".")
    return module(segments)
  }

  /**
    * Converts a String into a qualified name of a symbol.
    *
    * @param qualifiedName a String containing the qualified name of the symbol.
    * @return
    */
  def symbol(qualifiedName: String): SymbolName = {
    assert(qualifiedName.nonEmpty, "Cannot create symbol name from empty string.")

    val segments = qualifiedName.split(".")
    val moduleSegments = qualifiedName.dropRight(1)
    val moduleName =
      if (moduleSegments.nonEmpty) Some(module(moduleSegments)) else None

    return SymbolName(moduleName, segments.last)
  }

  // Helper function to parse a module name from a non-empty array of strings.
  private def module(segments: Array[String]): ModuleName = {
    val top: ModuleName = ToplevelName(segments.head)

    return segments
      .drop(1)
      .foldLeft(top)((moduleName, next) => moduleName.submodule(next))
  }
}

/**
  * Represents the name of a Symbol.
  *
  * @param module the name of the module in which this symbol was defined or None if the symbol was defined in the toplevel scope.
  * @param name the local name of the symbol.
  */
case class SymbolName(module: Option[ModuleName], name: String) extends Name {
  def qualified: String =
    module match {
      case Some(m) => s"${m.qualified}.${name}"
      case None    => name
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
  def submodule(localName: String): ModuleName = {
    assert(localName.nonEmpty, "Submodule name cannot be empty.")
    return localName
      .split(".")
      .foldLeft(this)((module, name) => NestedName(module, name))
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
    val module = segments
      .dropRight(1)
      .foldLeft(this)((module, name) => NestedName(module, name))

    return SymbolName(Some(module), localName)
  }

  // Compare module names by their qualified name.
  override def equals(obj: Any): Boolean =
    obj match {
      case m: ModuleName => m.qualified == this.qualified
      case _             => false
    }

  // Use hash of qualified name
  override def hashCode(): Int = qualified.hashCode()
}

/**
  * @note Do not use the constructor of this type directly. Instead call `Name.module()` to safely convert strings into valid module names.
  */
case class ToplevelName(name: String) extends ModuleName {

  def qualified: String = name

  def rename(f: String => String): Name = copy(name = f(name))
}

/**
  * @note Do not use the constructor of this type directly. Instead call the `submodule(name)` function of an existing ModuleName.
  */
case class NestedName(parent: ModuleName, name: String) extends ModuleName {
  def qualified: String = parent.qualified + "." + name

  def rename(f: String => String): Name = copy(name = f(name))
}
