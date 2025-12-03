package effekt
package core

import effekt.core.Type.*
import effekt.util.messages.{ DebugMessaging, ErrorReporter }

class TypeInferenceTests extends CoreTests {

  val SomeC: Id    = Id("Some")
  val NoneC: Id    = Id("None")
  val OptionId: Id = Id("Option")
  val f: Id        = Id("f")
  val value: Id    = Id("value")
  val g: Id        = Id("g")
  val x: Id        = Id("x")
  val y: Id        = Id("y")
  val A: Id        = Id("A")
  val B: Id        = Id("B")
  val C: Id        = Id("C")
  val infixAdd      = Id("infixAdd")

  def OptionT(tpe: core.ValueType): core.ValueType.Data =
    core.ValueType.Data(OptionId, List(tpe))

  test("infer Make type") {

    // Some[Int](42)
    val option = Make(OptionT(TInt), SomeC, List(), List(Literal(42, TInt)))

    // Option[Int]
    val expected = OptionT(TInt)

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

  test("compatibility") {
    assertEquals(Free.valuesCompatible(Map.empty, Map.empty), true)
    assertEquals(Free.valuesCompatible(Map(f -> TInt), Map.empty), true)
    assertEquals(Free.valuesCompatible(Map(f -> TInt), Map(f -> TInt)), true)
    assertEquals(Free.valuesCompatible(Map(f -> TInt), Map(f -> TInt, g -> TByte)), true)

    assertEquals(Free.valuesCompatible(Map(f -> TInt), Map(f -> TBoolean, g -> TByte)), false)
  }

  test("typechecking") {

    given DeclarationContext(List(
      Declaration.Data(OptionId, A :: Nil,
        Constructor(SomeC, Nil, Field(value, ValueType.Var(A)) :: Nil) ::
        Constructor(NoneC, Nil, Nil) :: Nil)
    ), Nil)

    val input1 = Stmt.Val(x,
      Stmt.Return(Expr.Literal(42, TInt)),
      Stmt.Return(Expr.ValueVar(x, TInt)))

    assertEquals(typecheck(input1),
      Result(TInt, Set.empty, Free.empty))

    val input2 = Stmt.Val(x,
      Stmt.Return(Expr.Literal(true, TBoolean)),
      Stmt.Return(Expr.ValueVar(x, TInt)))

    intercept[TypeError] { typecheck(input2) }

    // we can even type check open terms:
    assertEquals(typecheck(Stmt.Return(Expr.ValueVar(x, TInt))),
      Result(TInt, Set.empty, Free(Map(x -> TInt), Map.empty)))

    val add: Block.BlockVar = Block.BlockVar(infixAdd, BlockType.Function(Nil, Nil, List(TInt, TInt), Nil, TInt), Set.empty)

    val input3 = Stmt.Val(x,
      Stmt.Return(Expr.Literal(42, TInt)),
      Stmt.Return(Expr.PureApp(add, Nil, Expr.ValueVar(x, TInt) :: Expr.ValueVar(x, TInt) :: Nil)))

    assertEquals(typecheck(input3),
      Result(TInt, Set.empty, Free.block(add.id, add.annotatedTpe, add.annotatedCapt)))

    // [A](Option[A], A): A
    val orElse: Block.BlockVar = Block.BlockVar(f, BlockType.Function(A :: Nil, Nil, List(OptionT(ValueType.Var(A)), ValueType.Var(A)), Nil, ValueType.Var(A)), Set.empty)

    shouldTypeCheckAs(TInt, Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(x, OptionT(TInt)) :: Expr.ValueVar(y, TInt) :: Nil))

    // swapped arguments
    intercept[TypeError] {
      typecheck(Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(y, TInt) :: Expr.ValueVar(x, OptionT(TInt)) :: Nil))
    }
    // too few arguments
    intercept[TypeError] {
      typecheck(Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(x, OptionT(TInt)) :: Nil))
    }
    // incompatible free variables in arguments
    intercept[TypeError] {
      typecheck(Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(x, OptionT(TInt)) :: Expr.ValueVar(x, TInt) :: Nil))
    }

    shouldTypeCheckAs(OptionT(TInt), Make(OptionT(TInt), SomeC, List(), List(Literal(42, TInt))))
    shouldTypeCheckAs(OptionT(TInt), Make(OptionT(TInt), NoneC, List(), List()))
  }

  inline def shouldTypeCheckAs(expected: ValueType, expr: Expr)(using DeclarationContext): Unit =
    val Result(tpe, _, _) = typecheck(expr)
    assertEquals(Type.equals(tpe, expected), true)
}
