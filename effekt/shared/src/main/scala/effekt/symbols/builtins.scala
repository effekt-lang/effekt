package effekt
package symbols

import effekt.source.{Many, ModuleDecl, NoSource, Span}
import effekt.context.Context
import effekt.core.Type.{PromptSymbol, ResumeSymbol}
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
  lazy val prelude = Module(ModuleDecl("effekt", Nil, Nil, None, source.Span(builtinSource, 0, 0)), builtinSource)

  private def name(s: String) = QualifiedName(List("effekt"), s)

  val UnitSymbol = ExternType(name("Unit"), Nil, NoSource)
  val TUnit = ValueTypeApp(UnitSymbol, Nil)

  val BooleanSymbol = ExternType(name("Bool"), Nil, NoSource)
  val TBoolean = ValueTypeApp(BooleanSymbol, Nil)

  val IntSymbol = ExternType(name("Int"), Nil, NoSource)
  val TInt = ValueTypeApp(IntSymbol, Nil)

  val DoubleSymbol = ExternType(name("Double"), Nil, NoSource)
  val TDouble = ValueTypeApp(DoubleSymbol, Nil)

  val StringSymbol = ExternType(name("String"), Nil, NoSource)
  val TString = ValueTypeApp(StringSymbol, Nil)

  val CharSymbol = ExternType(name("Char"), Nil, NoSource)
  val TChar = ValueTypeApp(CharSymbol, Nil)

  val ByteSymbol = ExternType(name("Byte"), Nil, NoSource)
  val TByte = ValueTypeApp(ByteSymbol, Nil)

  // should this be a datatype, not an extern type?
  val BottomSymbol = DataType(name("Nothing"), Nil, Nil, NoSource)
  val TBottom = ValueTypeApp(BottomSymbol, Nil)

  val IOSymbol = Interface(Name.local("IO"), Nil, Nil, decl = NoSource)
  val IOCapability = ExternResource(name("io"), InterfaceType(IOSymbol, Nil), decl = NoSource)

  val AsyncSymbol = Interface(Name.local("Async"), Nil, Nil, decl = NoSource)
  val AsyncCapability = ExternResource(name("async"), InterfaceType(AsyncSymbol, Nil), decl = NoSource)

  val GlobalSymbol = Interface(Name.local("Global"), Nil, Nil, decl = NoSource)
  val GlobalCapability = ExternResource(name("global"), InterfaceType(GlobalSymbol, Nil), decl = NoSource)

  object TState {
    val S: TypeParam = TypeParam(Name.local("S"))
    val interface: Interface = Interface(Name.local("Ref"), List(S), Nil, decl = NoSource)
    val get = Operation(name("get"), List(S), Nil, Nil, ValueTypeRef(S), Effects.Pure, interface, decl = NoSource)
    val put = Operation(name("put"), List(S), List(ValueParam(Name.local("s"), Some(ValueTypeRef(S)), decl = NoSource)), Nil, TUnit, Effects.Pure, interface, decl = NoSource)
    interface.operations = List(get, put)

    def apply(stateType: ValueType) = InterfaceType(interface, List(stateType))

    def extractType(state: BlockType)(using C: Context): ValueType =
      state match {
        case InterfaceType(i, List(tpe)) if i == interface => tpe
        case tpe => C.panic(pretty"Expected builtin state, but got $tpe")
      }
  }

  val RegionSymbol = Interface(Name.local("Region"), Nil, Nil, decl = NoSource)
  val TRegion = InterfaceType(RegionSymbol, Nil)

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Unit" -> UnitSymbol,
    "Bool" -> BooleanSymbol,
    "Int" -> IntSymbol,
    "Double" -> DoubleSymbol,
    "String" -> StringSymbol,
    "Char" -> CharSymbol,
    "Byte" -> ByteSymbol,
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

  // All built-in symbols that can occur in core programs
  val coreBuiltins: Map[String, symbols.Symbol] = {
    symbols.builtins.rootTypes
      ++ symbols.builtins.rootCaptures
      + ("Resume" -> ResumeSymbol)
      + ("Prompt" -> PromptSymbol)
      + ("Ref" -> effekt.symbols.builtins.TState.interface)
  }

  def isCoreBuiltin(s: symbols.Symbol): Boolean =
    coreBuiltins.contains(s.name.name) && coreBuiltins(s.name.name) == s

  def coreBuiltinSymbolToString(s: symbols.Symbol): Option[String] =
    if isCoreBuiltin(s) then Some(s.name.name) else None

  def coreBuiltinSymbolFromString(s: String): Option[symbols.Symbol] = coreBuiltins.get(s)
}
