package effekt.symbols

sealed trait Name {
  def name: String
  def qualified: String

  override def toString: String = name
}
case class LocalName(name: String) extends Name { def qualified = name }
case class QualifiedName(path: String, name: String) extends Name {
  def qualified: String = moduleName(path) + "." + name
}
object Name {
  // constructs a name form a path
  def apply(path: String): Name = path.split('/') match {
    case Array(name) => LocalName(name)
    case segs        => QualifiedName(segs.init.mkString("/"), segs.last)
  }
}
