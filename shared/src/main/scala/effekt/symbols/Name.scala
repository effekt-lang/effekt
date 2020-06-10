package effekt.symbols

import effekt.source.Id
import effekt.context.Context

trait Name {
  def name: String
  def qualified(implicit C: Context): String
  override def toString = name
  def rename(f: String => String): Name
}

case class QualifiedName(name: String, module: Module) extends Name {
  def qualified(implicit C: Context): String = s"${module.name}.${name}"
  def rename(f: String => String): Name = copy(name = f(name))
}

case object NoName extends Name {
  def name = "<NoName>"
  def qualified(implicit C: Context): String = name
  def rename(f: String => String): Name = this
}

object Name {
  def apply(id: Id)(implicit C: Context): Name = QualifiedName(id.name, C.module)
  def apply(name: String, module: Module) = QualifiedName(name, module)
}
