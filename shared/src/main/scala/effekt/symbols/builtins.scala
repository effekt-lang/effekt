package effekt.symbols

import effekt.source.ModuleDecl
import org.bitbucket.inkytonik.kiama.util.StringSource

/**
 * The symbols, which are built into Effekt
 */
object builtins {

  // a dummy module for built in types. Can be dropped, once they are
  // defined in the prelude
  lazy val prelude = SourceModule(ModuleDecl("effekt", Nil, Nil), StringSource("", "effekt.effekt"))

  private def name(s: String) = Name(s, prelude)

  val TInt = BuiltinType(name("Int"), Nil)
  val TBoolean = BuiltinType(name("Boolean"), Nil)
  val TUnit = BuiltinType(name("Unit"), Nil)
  val TString = BuiltinType(name("String"), Nil)
  val TDouble = BuiltinType(name("Double"), Nil)

  val THole = BuiltinType(name("Unknown"), Nil)

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Int" -> TInt,
    "Boolean" -> TBoolean,
    "Unit" -> TUnit,
    "String" -> TString,
    "Double" -> TDouble
  )
}
