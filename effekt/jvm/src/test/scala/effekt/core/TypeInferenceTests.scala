package effekt
package core

import effekt.core.Type.*

class TypeInferenceTests extends CoreTests {

  val SomeC: Id    = Id("Some")
  val NoneC: Id    = Id("None")
  val OptionId: Id = Id("Option")
  val f: Id        = Id("f")
  val g: Id        = Id("g")
  val A: Id        = Id("A")
  val B: Id        = Id("B")
  val C: Id        = Id("C")

  def OptionT(tpe: core.ValueType): core.ValueType.Data =
    core.ValueType.Data(OptionId, List(tpe))

  test("infer Make type") {
    val intOptionTy = OptionT(TInt)

    // Some[Int](42)
    val option = Make(intOptionTy, SomeC, List(), List(Literal(42, TInt)))

    // Option[Int]
    val expected = intOptionTy

    assertEquals(option.tpe, expected)
  }

  test("type equality") {
    assertEquals(Type.equals(TInt, TInt), true)
    assertEquals(Type.equals(TInt, TBoolean), false)
    assertEquals(Type.equals(TInt, TBottom), false)
    assertEquals(Type.equals(TBottom, TInt), false)
    assertEquals(Type.equals(TBottom, TBottom), true)

    assertEquals(Type.equals(OptionT(TInt), OptionT(TInt)), true)
    assertEquals(Type.equals(OptionT(TInt), OptionT(TBoolean)), false)

    assertEquals(Type.equals(Set(f, g), Set(g, f)), true)

    // () => A
    def thunk(id: Id) = BlockType.Function(Nil, Nil, Nil, Nil, ValueType.Var(id))

    // [A](A) {f: () => A } => () => A at {f}
    val polyA = BlockType.Function(A :: Nil, f :: Nil,
      ValueType.Var(A) :: Nil,
      thunk(A) :: Nil,
      ValueType.Boxed(thunk(A), Set(f)))

    val polyB1 = BlockType.Function(B :: Nil, f :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: Nil,
      ValueType.Boxed(thunk(B), Set(f)))

    val polyB2 = BlockType.Function(B :: Nil, g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: Nil,
      ValueType.Boxed(thunk(B), Set(g)))

    assertEquals(Type.equals(polyA, polyB1), true)
    assertEquals(Type.equals(polyB1, polyB2), true)
    assertEquals(Type.equals(polyA, polyB2), true)

    val polyB3 = BlockType.Function(B :: Nil, f :: g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: thunk(B) :: Nil,
      ValueType.Boxed(thunk(B), Set(g)))

    val polyB4 = BlockType.Function(B :: Nil, f :: g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: thunk(B) :: Nil,
      ValueType.Boxed(thunk(B), Set(f, g)))

    assertEquals(Type.equals(polyA, polyB3), false)

    assertEquals(Type.equals(polyB3, polyB4), false)
  }
}
