package effekt
package core

import effekt.{core, source, symbols}
import effekt.context.Context
import effekt.core.{Block, Definition, DirectApp, PolymorphismBoxing, Pure, Run, Stmt}
import effekt.source.{IdDef, Include}
import effekt.util.messages
import effekt.util.messages.DebugMessaging
import kiama.parsing.{Failure, NoSuccess, Success}

abstract class AbstractPolymorphismBoxingTests extends CorePhaseTests(PolymorphismBoxing) {

  override protected val defaultNames: Map[String, _root_.effekt.symbols.Symbol] = super.defaultNames ++ Map(
    "BoxedInt" -> Id("BoxedInt"),
    "BoxedString" -> Id("BoxedString"),
    "MkBoxedString" -> Id("MkBoxedString"),
    "boxInt" -> Id("boxInt"),
    "unboxInt" -> Id("unboxInt"),
    "unboxString" -> Id("unboxInt"),
  )
  val boxDecls = List(
    Declaration.Data(defaultNames("BoxedString"), List(),
      List(Constructor(defaultNames("MkBoxedString"),
        List(Field(defaultNames("unboxString"), ValueType.Data(defaultNames("String"), Nil))))))
  )
  val boxExterns = List(
    Extern.Def(defaultNames("boxInt"), Nil, Nil, List(ValueParam(Id("i"), ValueType.Data(defaultNames("Int"), Nil))), Nil,
      ValueType.Data(defaultNames("BoxedInt"), Nil), Set.empty,
      ExternBody.StringExternBody(source.FeatureFlag.Default, Template(List("<box int>"), Nil))),
    Extern.Def(defaultNames("unboxInt"), Nil, Nil, List(ValueParam(Id("i"), ValueType.Data(defaultNames("BoxedInt"), Nil))), Nil,
      ValueType.Data(defaultNames("Int"), Nil), Set.empty,
      ExternBody.StringExternBody(source.FeatureFlag.Default, Template(List("<unbox int>"), Nil)))
  )

  override def transform(input: ModuleDecl): ModuleDecl = input match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      super.transform(ModuleDecl(path, includes, boxDecls ++ declarations, boxExterns ++ externs, definitions, exports)) match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations.filterNot(boxDecls.contains), externs.filterNot(boxExterns.contains), definitions, exports)
      }
  }

}
class PolymorphismBoxingTests extends AbstractPolymorphismBoxingTests {
  test("simple non-polymorphic code should stay the same"){
    val code =
      """module main
        |
        |extern {} def bar() {f@f: MyInterface}: Foo at {f} = default "my decl"
        |extern {} def bar() {g: MyInterface}: Foo at {g} = default "my decl"
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
        |def idInt = { (x: Int) => return (unboxInt: (BoxedInt) => Int @ {})((id: ['A]('A) => 'A @ {})[BoxedInt]((boxInt: (Int) => BoxedInt @ {})(x: Int))) }
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
        |def idInt = { (x: Int) => val tmp = (id: ['A]('A) => 'A @ {})[BoxedInt]((boxInt: (Int) => BoxedInt @ {})(x: Int)) ; return (unboxInt: (BoxedInt) => Int @ {})(tmp:BoxedInt) }
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
        |            let boxedRes = !(id: ['A]('A) => 'A @ {})[BoxedInt]((boxInt: (Int) => BoxedInt @ {})(x: Int))
        |            return (unboxInt: (BoxedInt) => Int @ {})(boxedRes:BoxedInt)
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
        |         val result = (originalFn: (Int) => Int @ {})((unboxInt: (BoxedInt) => Int @ {})(boxedX: BoxedInt));
        |         return (boxInt: (Int) => BoxedInt @ {})(result: Int)
        |      }
        |    };
        |    return (unboxInt: (BoxedInt) => Int @ {})(r:BoxedInt)
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
        |                        val rres:BoxedInt = (tmp: ('A) => 'A @ {})((boxInt: (Int) => BoxedInt @ {})(hhofargarg: Int));
        |                        return (unboxInt: (BoxedInt) => Int @ {})(rres:BoxedInt)
        |                      }
        |              };
        |              return (boxInt: (Int) => BoxedInt @ {})(res:Int)
        |          }
        |    };
        |    return (unboxInt: (BoxedInt) => Int @ {})(result:BoxedInt)
        |}
        |""".stripMargin
    assertTransformsTo(from, to)
  }
}
