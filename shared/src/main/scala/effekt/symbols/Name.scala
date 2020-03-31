package effekt.symbols

import effekt.source.Id
import effekt.context.Context

case class Name(name: String, module: Module) {
  override def toString = name

  def qualified: String = s"${module.name}.${name}"
}

object Name {
  def apply(id: Id)(implicit C: Context): Name = Name(id.name, C.module)
}
