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

case class QualifiedName(name: String, module: LegacyModule) extends Name {
  def qualified: String = s"${module.name}.${name}"
  def rename(f: String => String): Name = copy(name = f(name))
}

case object NoName extends Name {
  def name = "<NoName>"
  def qualified: String = name
  def rename(f: String => String): Name = this
}

object Name {
  def apply(id: Id)(implicit C: Context): Name = QualifiedName(id.name, C.module)
  def apply(name: String, module: LegacyModule) = QualifiedName(name, module)
}
