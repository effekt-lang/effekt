package effekt

import java.io.File
import effekt.core.PolymorphismBoxing
import effekt.context.Context
import effekt.source.{IdDef, Import, ModuleDecl}
import kiama.{parsing, util}
import effekt.symbols.{Module, Name, TypeConstructor, TypeSymbol, ValueType}
import effekt.source
import effekt.util.messages
import effekt.util.messages.DebugMessaging
import kiama.parsing.{Failure, NoSuccess, Success}
import kiama.util.Severities

abstract class AbstractPolymorphismBoxingTests extends munit.FunSuite {

  def boxDef(tpe: ValueType.ValueTypeApp): List[symbols.Symbol] = {
    val tpeCns: symbols.TypeConstructor.Record =
      symbols.TypeConstructor.Record(symbols.LocalName("Boxed" ++ tpe.constructor.name.name), List(), null)
    tpeCns.constructor =
      symbols.Constructor(symbols.LocalName("MkBoxed" ++ tpe.constructor.name.name), List(), null, tpeCns)
    tpeCns.constructor.fields = List(
      symbols.Field(symbols.LocalName("unbox" ++ tpe.constructor.name.name),
        symbols.ValueParam(symbols.LocalName("value"), Some(tpe)), tpeCns.constructor))
    List(tpeCns, tpeCns.constructor, tpeCns.constructor.fields.head)
  }

  val boxtpes = symbols.builtins.rootTypes.values.flatMap {
    case t: TypeConstructor.ExternType => boxDef(ValueType.ValueTypeApp(t, List()))
    case _ => Nil
  }.map { c => (c.name.name, c) }.toMap

  /** Mock context for Polymorphism boxing.
   * Only implements what is actually used by [[core.PolymorphismBoxing]]
   */
  object context extends Context(new util.Positions()) {
    this.module = new Module(ModuleDecl("test", List(Import("effekt")), List()), util.StringSource("", "test")) {
      override def findPrelude: Module = new Module(ModuleDecl("effekt", List(), List()), util.StringSource("", "effekt")) {
        override def types: Map[String, TypeSymbol] = boxtpes.collect[String, symbols.TypeSymbol]{
          case (k,t: symbols.TypeSymbol) => (k,t)
        }
      }
    }

    object messaging extends DebugMessaging

    def contentsOf(path: String): Option[String] = None

    def findSource(path: String): Option[kiama.util.Source] = None
  }

  val names = new core.Names(boxtpes ++
    symbols.builtins.rootTypes ++ Map(
    // TODO maybe add used names
  ))

  def assertTransformsTo(input: String, expected: String): Unit = {
    val pInput = core.CoreParsers.module(input, names) match {
      case Success(result, next) => result
      case nosuccess: NoSuccess => fail(nosuccess.toMessage)
    }
    val pExpected = core.CoreParsers.module(expected, names) match {
      case Success(result, next) => result
      case nosuccess: NoSuccess => fail(nosuccess.toMessage)
    }
    given core.PolymorphismBoxing.PContext = new PolymorphismBoxing.PContext(List())(using context)
    val got = PolymorphismBoxing.transform(pInput)
    assertEquals(got, pExpected)
  }
}
class PolymorphismBoxingTests extends AbstractPolymorphismBoxingTests {

  test("simple non-polymorphic code should stay the same"){
    val code =
      """module main
        |
        |extern {} def bar() {f@f: MyInterface}: Foo at {f} = "my decl"
        |extern {} def bar() {g: MyInterface}: Foo at {g} = "my decl"
        |
        |def prim = { () => return 42 }
        |def union = { () => if (true) (f: () => Int @ {f})() else (g: () => Unit @ {g})() }
        |def main = { () => return () }
        |
        |export prim
        |""".stripMargin
    assertTransformsTo(code, code)
  }

}