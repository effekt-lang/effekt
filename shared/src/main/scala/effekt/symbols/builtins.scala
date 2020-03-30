package effekt.symbols

/**
 * The symbols, which are built into Effekt
 */
object builtins {

  private def name(s: String) = QualifiedName("effekt", s)

  val TInt = BuiltinType(name("Int"), Nil)
  val TBoolean = BuiltinType(name("Boolean"), Nil)
  val TUnit = BuiltinType(name("Unit"), Nil)
  val TString = BuiltinType(name("String"), Nil)
  val TDouble = BuiltinType(name("Double"), Nil)

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Int" -> TInt,
    "Boolean" -> TBoolean,
    "Unit" -> TUnit,
    "String" -> TString,
    "Double" -> TDouble
  )
}
