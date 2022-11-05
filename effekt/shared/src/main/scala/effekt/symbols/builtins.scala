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

  val TUnit = BuiltinType(name("Unit"), Nil)
  val TBoolean = BuiltinType(name("Boolean"), Nil)
  val TInt = BuiltinType(name("Int"), Nil)
  val TDouble = BuiltinType(name("Double"), Nil)
  val TString = BuiltinType(name("String"), Nil)

  val TTop = BuiltinType(name("⊤"), Nil)
  val TBottom = BuiltinType(name("⊥"), Nil)

  val IOEffect = Interface(Name.local("IO"), Nil, Nil)
  val IOCapability = BlockParam(name("io"), IOEffect)

  val ControlEffect = Interface(Name.local("Control"), Nil, Nil)
  val ControlCapability = BlockParam(name("control"), ControlEffect)

  object TState {
    val S = TypeVar(Name.local("S"))
    val interface = Interface(Name.local("$State"), List(S), Nil)
    val get = Operation(name("get"), Nil, Nil, S, Effects.Pure, interface)
    val put = Operation(name("put"), Nil, List(ValueParam(Name.local("s"), Some(S))), TUnit, Effects.Pure, interface)
    interface.ops = List(get, put)

    def extractType(state: BlockType)(using C: Context): ValueType =
      state match {
        case BlockTypeApp(i, List(tpe)) => tpe
        case tpe => C.panic(pretty"Expected builtin state, but got $tpe")
      }
  }

  val TRegion = Interface(Name.local("Region"), Nil, Nil)

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Unit" -> TUnit,
    "Boolean" -> TBoolean,
    "Int" -> TInt,
    "Double" -> TDouble,
    "String" -> TString,
    "IO" -> IOEffect,
    "Region" -> TRegion
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
