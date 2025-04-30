package effekt
package symbols

object builtins {

  private def name(s: String) = QualifiedName(List("effekt"), s)

  val UnitSymbol = Symbol(name("Unit"))
  val IntSymbol = Symbol(name("Int"))
  val DoubleSymbol = Symbol(name("Double"))
  val StringSymbol = Symbol(name("String"))
  val CharSymbol = Symbol(name("Char"))
  val ByteSymbol = Symbol(name("Byte"))
  val BoolSymbol = Symbol(name("Bool"))
  val TopSymbol = Symbol(name("Any"))
  val BottomSymbol = Symbol(name("Nothing"))
  val IOSymbol = Symbol(name("IO"))
  val AsyncSymbol = Symbol(name("Async"))
  val GlobalSymbol = Symbol(name("Global"))
  val RegionSymbol = Symbol(name("Region"))

  val globalCapture = Symbol(name("global"))
  val ioCapture = Symbol(name("io"))
  val asyncCapture = Symbol(name("async"))

  object TState {
    val interface = Symbol(name("Ref"))
    val get = Symbol(name("get"))
    val put = Symbol(name("put"))
  }


  val rootTypes: Map[String, Symbol] = Map(
    "Unit" -> UnitSymbol,
    "Bool" -> BoolSymbol,
    "Int" -> IntSymbol,
    "Double" -> DoubleSymbol,
    "String" -> StringSymbol,
    "Char" -> CharSymbol,
    "Byte" -> ByteSymbol,
    "Any" -> TopSymbol,
    "Nothing" -> BottomSymbol,
    "IO" -> IOSymbol,
    "Region" -> RegionSymbol
  )

  val rootCaptures: Map[String, Symbol] = Map(
    "io" -> globalCapture,
    "async" -> asyncCapture,
    "global" -> globalCapture
  )
}
