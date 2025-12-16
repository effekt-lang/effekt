package effekt
package core

import effekt.core.Type.*
import effekt.util.messages.{ DebugMessaging, ErrorReporter }

class TypeInferenceTests extends CoreTests {

  val SomeC: Id    = Id("Some")
  val NoneC: Id    = Id("None")
  val OptionId: Id = Id("Option")
  val f: Id        = Id("f")
  val r: Id        = Id("r")
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

    // TODO since right now we ignore captures, these are equal (but should not!)
    assertEquals(Type.equals(polyB3, polyB4), true)
  }

  test("compatibility") {
    Free.valuesCompatible(Map.empty, Map.empty)
    Free.valuesCompatible(Map(f -> TInt), Map.empty)
    Free.valuesCompatible(Map(f -> TInt), Map(f -> TInt))
    Free.valuesCompatible(Map(f -> TInt), Map(f -> TInt, g -> TByte))

    intercept[TypeError] {
      Free.valuesCompatible(Map(f -> TInt), Map(f -> TBoolean, g -> TByte))
    }
  }

  test("typechecking") {

    given DeclarationContext(List(
      Declaration.Data(OptionId, A :: Nil,
        Constructor(SomeC, Nil, Field(value, ValueType.Var(A)) :: Nil) ::
        Constructor(NoneC, Nil, Nil) :: Nil)
    ), Nil)

    val ex1 = Stmt.Val(x,
      Stmt.Return(Expr.Literal(42, TInt)),
      Stmt.Return(Expr.ValueVar(x, TInt)))

    assertEquals(ex1.tpe, TInt)
    assertEquals(ex1.capt, Set.empty)
    assertEquals(ex1.free, Free.empty)

    intercept[TypeError] {
      Stmt.Val(x,
        Stmt.Return(Expr.Literal(true, TBoolean)),
        Stmt.Return(Expr.ValueVar(x, TInt)))
    }

    // we can even type check open terms:
    assertEquals(typecheck(Stmt.Return(Expr.ValueVar(x, TInt))),
      Typing(TInt, Set.empty, Free(Map(x -> TInt), Map.empty, Constraints.empty)))

    val add: Block.BlockVar = Block.BlockVar(infixAdd, BlockType.Function(Nil, Nil, List(TInt, TInt), Nil, TInt), Set.empty)

    val ex3 = Stmt.Val(x,
      Stmt.Return(Expr.Literal(42, TInt)),
      Stmt.Return(Expr.PureApp(add, Nil, Expr.ValueVar(x, TInt) :: Expr.ValueVar(x, TInt) :: Nil)))

    assertEquals(typecheck(ex3),
      Typing(TInt, Set.empty, Free.block(add.id, add.annotatedTpe, add.annotatedCapt)), Constraints.empty)

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

    val ex4: Expr.Make = Make(OptionT(TInt), SomeC, List(), List(Literal(42, TInt)))
    assertSameType(ex4.tpe, OptionT(TInt))
    assertEquals(ex4.free, Free.defer(ex4))

    val ex5: Expr.Make = Make(OptionT(TInt), NoneC, List(), List())
    assertSameType(ex5.tpe, OptionT(TInt))
    assertEquals(ex5.free, Free.defer(ex5))
  }

  val ex6 = Stmt.Region(Block.BlockLit(Nil, r :: Nil, Nil, BlockParam(r, TRegion, Set(r)) :: Nil,
    Stmt.Alloc(x, Expr.Literal(42, TInt), r,
      Stmt.Get(y, TInt, x, Set(r),
        Stmt.Return(Expr.ValueVar(y, TInt))))))

  println(ex6.tpe)

  inline def assertSameType(got: ValueType, expected: ValueType)(using DeclarationContext): Unit =
    assertEquals(Type.equals(got, expected), true)

  inline def shouldTypeCheckAs(expected: ValueType, expr: Expr)(using DeclarationContext): Unit =
    assertSameType(expr.tpe, expected)
}
