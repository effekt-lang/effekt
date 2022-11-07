package effekt
package symbols

import effekt.source.ModuleDecl
import effekt.context.Context
import effekt.symbols.ErrorMessageInterpolator
import effekt.util.messages.ErrorMessageReifier

import kiama.util.StringSource

/**
 * The symbols, which are built into Effekt
 */
object builtins {

  // a dummy module for built in types. Can be dropped, once they are
  // defined in the prelude
  lazy val prelude = Module(ModuleDecl("effekt", Nil, Nil), StringSource("", "effekt.effekt"))

  private def name(s: String) = Name.qualified(s, prelude)

  val UnitSymbol = BuiltinType(name("Unit"), Nil)
  val TUnit = ValueTypeApp(UnitSymbol, Nil)

  val BooleanSymbol = BuiltinType(name("Boolean"), Nil)
  val TBoolean = ValueTypeApp(BooleanSymbol, Nil)

  val IntSymbol = BuiltinType(name("Int"), Nil)
  val TInt = ValueTypeApp(IntSymbol, Nil)

  val DoubleSymbol = BuiltinType(name("Double"), Nil)
  val TDouble = ValueTypeApp(DoubleSymbol, Nil)

  val StringSymbol = BuiltinType(name("String"), Nil)
  val TString = ValueTypeApp(StringSymbol, Nil)

  val TopSymbol = BuiltinType(name("⊤"), Nil)
  val TTop = ValueTypeApp(TopSymbol, Nil)

  val BottomSymbol = BuiltinType(name("⊥"), Nil)
  val TBottom = ValueTypeApp(BottomSymbol, Nil)

  val IOSymbol = Interface(Name.local("IO"), Nil, Nil)
  val IOCapability = BlockParam(name("io"), InterfaceType(IOSymbol, Nil))

  val ControlSymbol = Interface(Name.local("Control"), Nil, Nil)
  val ControlCapability = BlockParam(name("control"), InterfaceType(ControlSymbol, Nil))

  object TState {
    val S = TypeParam(Name.local("S"))
    val interface = Interface(Name.local("$State"), List(S), Nil)
    val get = Operation(name("get"), Nil, Nil, ValueTypeRef(S), Effects.Pure, interface)
    val put = Operation(name("put"), Nil, List(ValueParam(Name.local("s"), Some(ValueTypeRef(S)))), TUnit, Effects.Pure, interface)
    interface.ops = List(get, put)

    def apply(stateType: ValueType) = InterfaceType(interface, List(stateType))

    def extractType(state: BlockType)(using C: Context): ValueType =
      state match {
        case InterfaceType(i, List(tpe)) if i == interface => tpe
        case tpe => C.panic(pretty"Expected builtin state, but got $tpe")
      }
  }

  val RegionSymbol = Interface(Name.local("Region"), Nil, Nil)
  val TRegion = InterfaceType(RegionSymbol, Nil)

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Unit" -> UnitSymbol,
    "Boolean" -> BooleanSymbol,
    "Int" -> IntSymbol,
    "Double" -> DoubleSymbol,
    "String" -> StringSymbol,
    "IO" -> IOSymbol,
    "Region" -> RegionSymbol
  )

  lazy val globalRegion = BlockParam(name("global"), TRegion)

  // it is a set, because terms can be overloaded...
  val rootTerms: Map[String, TermSymbol] = Map(
    "global" -> globalRegion
  )

  val rootCaptures: Map[String, Capture] = Map(
    "io" -> IOCapability.capture,
    "control" -> ControlCapability.capture,
    "global" -> globalRegion.capture
  )
}
