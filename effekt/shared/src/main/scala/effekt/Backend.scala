package effekt

case class Backend[E](name: String, compiler: Compiler[E])

object Backend {

  def backend(name: String): Backend[_] = ???
}
