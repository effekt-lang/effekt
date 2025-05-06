package effekt
package symbols

import effekt.source.{Many, ModuleDecl, Span}
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
  val builtinSource = StringSource("", "effekt.effekt")
  lazy val prelude = Module(ModuleDecl("effekt", Nil, Nil, source.Span(builtinSource, 0, 0)), builtinSource)

  private def name(s: String) = QualifiedName(List("effekt"), s)

  val UnitSymbol = ExternType(name("Unit"), Many.empty(Span.builtin))
  val TUnit = ValueTypeApp(UnitSymbol, Many.empty(Span.builtin))

  val BooleanSymbol = ExternType(name("Bool"), Many.empty(Span.builtin))
  val TBoolean = ValueTypeApp(BooleanSymbol, Many.empty(Span.builtin))

  val IntSymbol = ExternType(name("Int"), Many.empty(Span.builtin))
  val TInt = ValueTypeApp(IntSymbol, Many.empty(Span.builtin))

  val DoubleSymbol = ExternType(name("Double"), Many.empty(Span.builtin))
  val TDouble = ValueTypeApp(DoubleSymbol, Many.empty(Span.builtin))

  val StringSymbol = ExternType(name("String"), Many.empty(Span.builtin))
  val TString = ValueTypeApp(StringSymbol, Many.empty(Span.builtin))

  val CharSymbol = ExternType(name("Char"), Many.empty(Span.builtin))
  val TChar = ValueTypeApp(CharSymbol, Many.empty(Span.builtin))

  val ByteSymbol = ExternType(name("Byte"), Many.empty(Span.builtin))
  val TByte = ValueTypeApp(ByteSymbol, Many.empty(Span.builtin))

  val TopSymbol = ExternType(name("Any"), Many.empty(Span.builtin))
  val TTop = ValueTypeApp(TopSymbol, Many.empty(Span.builtin))

  // should this be a datatype, not an extern type?
  val BottomSymbol = ExternType(name("Nothing"), Many.empty(Span.builtin))
  val TBottom = ValueTypeApp(BottomSymbol, Many.empty(Span.builtin))

  val IOSymbol = Interface(Name.local("IO"), Nil, Nil)
  val IOCapability = ExternResource(name("io"), InterfaceType(IOSymbol, Nil))

  val AsyncSymbol = Interface(Name.local("Async"), Nil, Nil)
  val AsyncCapability = ExternResource(name("async"), InterfaceType(AsyncSymbol, Nil))

  val GlobalSymbol = Interface(Name.local("Global"), Nil, Nil)
  val GlobalCapability = ExternResource(name("global"), InterfaceType(GlobalSymbol, Nil))

  object TState {
    val S: TypeParam = TypeParam(Name.local("S"))
    val interface: Interface = Interface(Name.local("Ref"), List(S), Nil)
    val get = Operation(name("get"), Many(List(S), ???), Many.empty(???), Many.empty(???), ValueTypeRef(S), Effects.Pure, interface)
    val put = Operation(name("put"), Many(List(S), ???), Many(List(ValueParam(Name.local("s"), Some(ValueTypeRef(S)))), ???), Many.empty(???), TUnit, Effects.Pure, interface)
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

  val rootCaptures: Map[String, Capture] = Map(
    "io" -> IOCapability.capture,
    "async" -> AsyncCapability.capture,
    "global" -> GlobalCapability.capture
  )

  // captures which are allowed on the toplevel
  val toplevelCaptures: CaptureSet = CaptureSet() // CaptureSet(IOCapability.capture, GlobalCapability.capture)

  lazy val rootBindings: Bindings =
    Bindings(Map.empty, rootTypes, rootCaptures, Map("effekt" -> Bindings(Map.empty, rootTypes, rootCaptures, Map.empty)))

}
