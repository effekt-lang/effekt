package effekt.core

import effekt.{core, source, symbols}
import effekt.context.Context
import effekt.core.{Block, Definition, DirectApp, PolymorphismBoxing, Pure, Run, Stmt}
import effekt.source.{IdDef, Include}
import effekt.symbols.{Module, Name, TypeConstructor, TypeSymbol, ValueSymbol, ValueType}
import effekt.util.messages
import effekt.util.messages.DebugMessaging
import kiama.parsing.{Failure, NoSuccess, Success}

abstract class AbstractPolymorphismBoxingTests extends CorePhaseTests(PolymorphismBoxing) {

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

  /** Make sure that the stdlib module is found, with the appropriate `Boxed`... definitions */
  override val theSourceModule = new Module(source.ModuleDecl("test", List(Include("effekt")), List()), kiama.util.StringSource("", "test")) {
    override def findPrelude: Module = new Module(effekt.source.ModuleDecl("effekt", List(), List()), kiama.util.StringSource("", "effekt")) {
      override def types: Map[String, TypeSymbol] = boxtpes.collect[String, symbols.TypeSymbol]{
        case (k,t: symbols.TypeSymbol) => (k,t)
      }
    }
  }

  override protected val defaultNames = boxtpes ++
    symbols.builtins.rootTypes ++ Map(
    // TODO maybe add used names
  )
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

  test("if only data types are used, code should stay the same"){
    val code =
      """module main
        |
        |type Foo { X() B() }
        |
        |def baz = { ['b](){ f@f: MyInterface } => (f: () => Unit @ {})() }
        |def bar = { ['b](){ f@f: MyInterface } => return box {f} (f : MyInterface @ {f}) }
        |def id = { ['A](a: 'A) => return a: 'A }
        |
        |def returnA = { () => return (id: ['A]('A) => 'A @ {})[Foo]((X: () => Foo @ {})()) }
        |def pReturnA = { ['B](b: 'B) => return (id: ['A]('A) => 'A @ {})['A](b: 'B) }
        |
        |""".stripMargin
    assertTransformsTo(code,code)
  }

  test("simple PureApp with int gets wrapped"){
    val from =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) => return (id: ['A]('A) => 'A @ {})[Int](x: Int) }
        |""".stripMargin
    val to =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) => return (id: ['A]('A) => 'A @ {})[BoxedInt](make BoxedInt MkBoxedInt(x: Int)).unboxInt: Int }
        |""".stripMargin
    assertTransformsTo(from, to)
  }

  test("simple App with int gets wrapped"){
    val from =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) => (id: ['A]('A) => 'A @ {})[Int](x: Int) }
        |""".stripMargin
    val to =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) => val tmp = (id: ['A]('A) => 'A @ {})[BoxedInt](make BoxedInt MkBoxedInt(x: Int)) ; return tmp:BoxedInt.unboxInt: Int }
        |""".stripMargin
    assertTransformsTo(from, to)
  }

  test("DirectApp with [Int] gets wrapped correctly"){
    val from =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) =>
        |    {
        |        let res = !(id: ['A]('A) => 'A @ {})[Int](x: Int)
        |        return res: Int
        |    }
        |}
        |""".stripMargin
    val to =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) =>
        |    {
        |        let res = run {
        |            let boxedRes = !(id: ['A]('A) => 'A @ {})[BoxedInt](make BoxedInt MkBoxedInt(x: Int))
        |            return boxedRes:BoxedInt.unboxInt: Int
        |        }
        |        return res: Int
        |    }
        |}
        |""".stripMargin
    assertTransformsTo(from,to)
  }

  test("block parameters get wrapped \"inversely\""){
    val from =
      """module main
        |def test = { () =>
        |    (hof: ['A](){ b : ('A) => 'A } => 'A @ {} )[Int](){ (x: Int) => return x: Int }
        |}
        |""".stripMargin
    val to =
      """module main
        |def test = { () =>
        |    val r = (hof: ['A](){ b : ('A) => 'A } => 'A @ {} )[BoxedInt](){ (boxedX: BoxedInt) =>
        |      {
        |         def originalFn = { (x: Int) => return x: Int }
        |         val result = (originalFn: (Int) => Int @ {})(boxedX: BoxedInt.unboxInt: Int);
        |         return make BoxedInt MkBoxedInt(result: Int)
        |      }
        |    };
        |    return r:BoxedInt.unboxInt: Int
        |}
        |""".stripMargin
    assertTransformsTo(from, to)
  }

  test("higher higher order functions get wrapped correctly"){
    val from =
      """module main
        |
        |def hhof_caller = { () =>
        |    (hhof: ['A](){ b: (){ hhofarg: ('A) => 'A } => 'A } => 'A @ {})[Int](){
        |        (){ hhofarg: (Int) => Int } => (hhofarg: (Int) => Int @ {})(5)
        |    }
        |}
        |""".stripMargin
    val to =
      """module main
        |
        |def hhof_caller = { () =>
        |    val result = (hhof: ['A](){ b: (){ hhofarg: ('A) => 'A } => 'A } => 'A @ {})[BoxedInt](){
        |        (){ hhofargB: ('A) => 'A } =>
        |          {
        |              def originalFn = { (){ hhofarg: (Int) => Int } => (hhofarg: (Int) => Int @ {})(5) }
        |              val res = (originalFn: (){ hhofarg: (Int) => Int } => Int @ {})(){
        |                  (hhofargarg: Int) =>
        |                      {
        |                        def tmp = hhofargB: ('A) => 'A @ {}
        |                        val rres = (tmp: ('A) => 'A @ {})(make BoxedInt MkBoxedInt(hhofargarg: Int));
        |                        return rres:BoxedInt.unboxInt: Int
        |                      }
        |              };
        |              return make BoxedInt MkBoxedInt(res:Int)
        |          }
        |    };
        |    return result:BoxedInt.unboxInt: Int
        |}
        |""".stripMargin
    assertTransformsTo(from, to)
  }
}
