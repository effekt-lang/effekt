package effekt.symbols

import effekt.source.ModuleDecl
import org.bitbucket.inkytonik.kiama.util.StringSource

/**
 * The symbols, which are built into Effekt
 */
object builtins {

  // a dummy module for built in types. Can be dropped, once they are
  // defined in the prelude
  lazy val prelude = Module(ModuleDecl("effekt", Nil, Nil), StringSource("", "effekt.effekt"))

  private def name(s: String) = Name.qualified(s, prelude)

  val TInt = BuiltinType(name("Int"), Nil)
  val TBoolean = BuiltinType(name("Boolean"), Nil)
  val TUnit = BuiltinType(name("Unit"), Nil)
  val TString = BuiltinType(name("String"), Nil)
  val TDouble = BuiltinType(name("Double"), Nil)

  val THole = BuiltinType(name("Unknown"), Nil)

  // Builtin Local State
  val TState = {
    val S = TypeVar(LocalName("S"))
    val interface = Interface(LocalName("$State"), List(S), Nil)
    val get = Operation(LocalName("get"), Nil, Nil, S, interface)
    val put = Operation(LocalName("put"), Nil, List(ValueParam(LocalName("s"), S)), TUnit, interface)
    interface.ops = List(get, put)
    interface
  }

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Int" -> TInt,
    "Boolean" -> TBoolean,
    "Unit" -> TUnit,
    "String" -> TString,
    "Double" -> TDouble
  )
}
