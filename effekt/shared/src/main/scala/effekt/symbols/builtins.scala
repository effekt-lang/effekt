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

  private def name(s: String) = QualifiedName(List("effekt"), s)

  val UnitSymbol = ExternType(name("Unit"), Nil)
  val TUnit = ValueTypeApp(UnitSymbol, Nil)

  val BooleanSymbol = ExternType(name("Bool"), Nil)
  val TBoolean = ValueTypeApp(BooleanSymbol, Nil)

  val IntSymbol = ExternType(name("Int"), Nil)
  val TInt = ValueTypeApp(IntSymbol, Nil)

  val DoubleSymbol = ExternType(name("Double"), Nil)
  val TDouble = ValueTypeApp(DoubleSymbol, Nil)

  val StringSymbol = ExternType(name("String"), Nil)
  val TString = ValueTypeApp(StringSymbol, Nil)

  val CharSymbol = ExternType(name("Char"), Nil)
  val TChar = ValueTypeApp(CharSymbol, Nil)

  val ByteSymbol = ExternType(name("Byte"), Nil)
  val TByte = ValueTypeApp(ByteSymbol, Nil)

  val TopSymbol = ExternType(name("Any"), Nil)
  val TTop = ValueTypeApp(TopSymbol, Nil)

  // should this be a datatype, not an extern type?
  val BottomSymbol = ExternType(name("Nothing"), Nil)
  val TBottom = ValueTypeApp(BottomSymbol, Nil)

  val IOSymbol = Interface(Name.local("IO"), Nil, Nil)
  val IOCapability = ExternResource(name("io"), InterfaceType(IOSymbol, Nil))

  val ControlSymbol = Interface(Name.local("Control"), Nil, Nil)
  val ControlCapability = ExternResource(name("control"), InterfaceType(ControlSymbol, Nil))

  object TState {
    val S: TypeParam = TypeParam(Name.local("S"))
    val interface: Interface = Interface(Name.local("Ref"), List(S), Nil)
    val get = Operation(name("get"), List(S), Nil, Nil, ValueTypeRef(S), Effects.Pure, interface)
    val put = Operation(name("put"), List(S), List(ValueParam(Name.local("s"), Some(ValueTypeRef(S)))), Nil, TUnit, Effects.Pure, interface)
    interface.operations = List(get, put)

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
    "Bool" -> BooleanSymbol,
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

  lazy val globalRegion = ExternResource(name("global"), TRegion)

  val rootTerms: Map[String, TermSymbol] = Map(
    "global" -> globalRegion
  )

  val rootCaptures: Map[String, Capture] = Map(
    "io" -> IOCapability.capture,
    "control" -> ControlCapability.capture,
    "global" -> globalRegion.capture
  )

  // captures which are allowed on the toplevel
  val toplevelCaptures: CaptureSet = CaptureSet() // CaptureSet(IOCapability.capture, globalRegion.capture)

  lazy val rootBindings: Bindings =
    Bindings(rootTerms.map { case (k, v) => (k, Set(v)) }, rootTypes, rootCaptures,
      Map("effekt" -> Bindings(rootTerms.map { case (k, v) => (k, Set(v)) }, rootTypes, rootCaptures, Map.empty)))

}
