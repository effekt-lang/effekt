package effekt
package core

import effekt.{core, source, symbols}
import effekt.context.Context
import effekt.core.{Block, PolymorphismBoxing, Expr, Stmt}
import effekt.source.{IdDef, Include, Span}
import effekt.util.messages
import effekt.util.messages.DebugMessaging
import kiama.parsing.{Failure, NoSuccess, Success}

abstract class AbstractPolymorphismBoxingTests extends CorePhaseTests(PolymorphismBoxing) {

  override protected val defaultNames: Map[String, _root_.effekt.symbols.Symbol] = super.defaultNames ++ Map(
    "BoxedInt" -> PolymorphismBoxing.TBoxedInt.name,
    "BoxedString" -> Id("BoxedString"),
    "MkBoxedString" -> Id("MkBoxedString"),
    "coercePosInt" -> PolymorphismBoxing.TCoercePosInt,
    "coerceIntPos" -> PolymorphismBoxing.TCoerceIntPos,
  )

  override def transform(input: ModuleDecl): ModuleDecl = input match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      super.transform(ModuleDecl(path, includes, declarations, externs, definitions, exports)) match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations, externs, definitions, exports)
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
        |def idInt = { (x: Int) => return (coercePosInt: (BoxedInt) => Int @ {})((id: ['A]('A) => 'A @ {})[BoxedInt]((coerceIntPos: (Int) => BoxedInt @ {})(x: Int))) }
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
        |def idInt = { (x: Int) => val tmp = (id: ['A]('A) => 'A @ {})[BoxedInt]((coerceIntPos: (Int) => BoxedInt @ {})(x: Int)) ; return (coercePosInt: (BoxedInt) => Int @ {})(tmp:BoxedInt) }
        |""".stripMargin
    assertTransformsTo(from, to)
  }

  test("ImpureApp with [Int] gets wrapped correctly"){
    val from =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) =>
        |    {
        |        let ! res = (id: ['A]('A) => 'A @ {})[Int](x: Int)
        |        return res: Int
        |    }
        |}
        |""".stripMargin
    val to =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |def idInt = { (x: Int) =>
        |  let ! boxed = (id: ['A]('A) => 'A @ {})[BoxedInt]((coerceIntPos: (Int) => BoxedInt @ {})(x: Int))
        |  let unboxed = (coercePosInt: (BoxedInt) => Int @ {})(boxed:BoxedInt)
        |  return unboxed: Int
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
        |         val result = ({ (a: Int) => return a: Int })((coercePosInt: (BoxedInt) => Int @ {})(boxedX: BoxedInt));
        |         return (coerceIntPos: (Int) => BoxedInt @ {})(result: Int)
        |      }
        |    };
        |    return (coercePosInt: (BoxedInt) => Int @ {})(r:BoxedInt)
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
        |              val res = ({ (){ hhofarg: (Int) => Int } => (hhofarg: (Int) => Int @ {})(5) })(){
        |                  (hhofargarg: Int) =>
        |                      {
        |                        val rres:BoxedInt = (hhofargB: ('A) => 'A @ {})((coerceIntPos: (Int) => BoxedInt @ {})(hhofargarg: Int));
        |                        return (coercePosInt: (BoxedInt) => Int @ {})(rres:BoxedInt)
        |                      }
        |              };
        |              return (coerceIntPos: (Int) => BoxedInt @ {})(res:Int)
        |          }
        |    };
        |    return (coercePosInt: (BoxedInt) => Int @ {})(result:BoxedInt)
        |}
        |""".stripMargin
    assertTransformsTo(from, to)
  }
}
