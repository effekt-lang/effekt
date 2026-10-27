package effekt
package core

import effekt.core.Type.*
import effekt.util.messages.{ DebugMessaging, ErrorReporter }
import effekt.util.DB

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
  val infixPlus      = Id("infixPlus")

  def OptionT(tpe: core.ValueType): core.ValueType.Data =
    core.ValueType.Data(OptionId, List(tpe))

  test("infer Make type") {

    // Some[Int](42)
    val option = Make(OptionT(TInt), SomeC, List(), List(Literal(42, TInt)))

    // Option[Int]
    val expected = OptionT(TInt)

    assertEquals(option.tpe, expected)
  }

  test("shift and resume with multiple values") {
    val p = Id("p")
    val k = Id("k")
    val prompt: Block.BlockVar = Block.BlockVar(p, BlockType.Prompt(List(TInt)), Set(p))
    val resumption = BlockType.Resume(List(TInt, TBoolean), List(TInt))

    // shift(p) { {k: (Int, Bool) ~> Int} => resume(k) { return 1, true } }
    val resumed = Stmt.Return(List(Literal(1, TInt), Literal(true, TBoolean)))
    val resume = Stmt.Resume(Block.BlockVar(k, resumption, Set(k)), resumed)
    val shift = Stmt.Shift(prompt, BlockParam(k, resumption, Set(k)), resume)

    assertEquals(resume.tpe, List(TInt))
    assertEquals(shift.tpe, List(TInt, TBoolean))
    assertEquals(typecheck(shift).tpe, List(TInt, TBoolean))

    // resuming with the wrong number of values
    intercept[TypeError] {
      typecheck(Stmt.Resume(Block.BlockVar(k, resumption, Set(k)), Stmt.Return(List(Literal(1, TInt)))))
    }
  }

  test("reset and shift with multiple answers") {
    val p = Id("p")
    val k = Id("k")
    val answer = List(TInt, TBoolean)
    val promptParam = BlockParam(p, BlockType.Prompt(answer), Set(p))
    val prompt: Block.BlockVar = Block.BlockVar(p, BlockType.Prompt(answer), Set(p))
    val resumption = BlockType.Resume(List(TInt), answer)

    // reset { {p: Prompt[(Int, Bool)]} => val x = shift(p) { {k} => resume(k) { return 1 } }; return x, true }
    val shift = Stmt.Shift(prompt, BlockParam(k, resumption, Set(k)),
      Stmt.Resume(Block.BlockVar(k, resumption, Set(k)), Stmt.Return(List(Literal(1, TInt)))))
    val body = Stmt.Val(List(x), shift, Stmt.Return(List(ValueVar(x, TInt), Literal(true, TBoolean))))
    val reset = Stmt.Reset(Block.BlockLit(Nil, List(p), Nil, List(promptParam), body))

    assertEquals(shift.tpe, List(TInt))
    assertEquals(reset.tpe, answer)
    assertEquals(typecheck(reset).tpe, answer)

    // the shift body must compute the answer of the prompt
    intercept[TypeError] {
      val wrong = BlockType.Resume(List(TInt), List(TInt))
      typecheck(Stmt.Shift(prompt, BlockParam(k, wrong, Set(k)),
        Stmt.Resume(Block.BlockVar(k, wrong, Set(k)), Stmt.Return(List(Literal(1, TInt))))))
    }
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
    def thunk(id: Id) = BlockType.Function(Nil, Nil, Nil, Nil, List(ValueType.Var(id)))

    // [A](A) {f: () => A } => () => A at {f}
    val polyA = BlockType.Function(A :: Nil, f :: Nil,
      ValueType.Var(A) :: Nil,
      thunk(A) :: Nil,
      List(ValueType.Boxed(thunk(A), Set(f))))

    val polyB1 = BlockType.Function(B :: Nil, f :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: Nil,
      List(ValueType.Boxed(thunk(B), Set(f))))

    val polyB2 = BlockType.Function(B :: Nil, g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: Nil,
      List(ValueType.Boxed(thunk(B), Set(g))))

    assertEquals(Type.equals(polyA, polyB1), true)
    assertEquals(Type.equals(polyB1, polyB2), true)
    assertEquals(Type.equals(polyA, polyB2), true)

    val polyB3 = BlockType.Function(B :: Nil, f :: g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: thunk(B) :: Nil,
      List(ValueType.Boxed(thunk(B), Set(g))))

    val polyB4 = BlockType.Function(B :: Nil, f :: g :: Nil,
      ValueType.Var(B) :: Nil,
      thunk(B) :: thunk(B) :: Nil,
      List(ValueType.Boxed(thunk(B), Set(f, g))))

    assertEquals(Type.equals(polyA, polyB3), false)

    // TODO since right now we ignore captures, these are equal (but should not!)
    assertEquals(Type.equals(polyB3, polyB4), true)
  }

  test("compatibility") {
    Free.mergeValues(DB.empty, DB.empty)
    Free.mergeValues(DB(f -> TInt), DB.empty)
    Free.mergeValues(DB(f -> TInt), DB(f -> TInt))
    Free.mergeValues(DB(f -> TInt), DB(f -> TInt, g -> TByte))

    intercept[TypeError] {
      Free.mergeValues(DB(f -> TInt), DB(f -> TBoolean, g -> TByte))
    }
  }

  test("typechecking") {

    given DeclarationContext(List(
      Declaration.Data(OptionId, A :: Nil,
        Constructor(SomeC, Nil, Field(value, ValueType.Var(A)) :: Nil) ::
        Constructor(NoneC, Nil, Nil) :: Nil)
    ), Nil)

    val ex1 = Stmt.Val(List(x),
      Stmt.Return(List(Expr.Literal(42, TInt))),
      Stmt.Return(List(Expr.ValueVar(x, TInt))))

    assertEquals(ex1.tpe, List(TInt))
    assertEquals(ex1.capt, Set.empty)
    assertEquals(ex1.free, Free.value(x, TInt).withoutValue(x, TInt))

    // we can even type check open terms:
    val ex2 = Stmt.Return(List(Expr.ValueVar(x, TInt)))
    assertEquals(typecheck(ex2),
      Typing(List(TInt), Set.empty, Constraints.empty))

    assertEquals(ex2.free, Free.value(x, TInt))

    val add: Block.BlockVar = Block.BlockVar(infixPlus, BlockType.Function(Nil, Nil, List(TInt, TInt), Nil, List(TInt)), Set.empty)

    val ex3 = Stmt.Val(List(x),
      Stmt.Return(List(Expr.Literal(42, TInt))),
      Stmt.Return(List(Expr.PureApp(add, Nil, Expr.ValueVar(x, TInt) :: Expr.ValueVar(x, TInt) :: Nil))))

    val result3 = typecheck(ex3)
    assertEquals(result3.tpe, List(TInt))
    assertEquals(result3.capt, Set.empty)
    assert(ex3.free.freeIds == Set(add.id))

    // [A](Option[A], A): A
    val orElse: Block.BlockVar = Block.BlockVar(f, BlockType.Function(A :: Nil, Nil, List(OptionT(ValueType.Var(A)), ValueType.Var(A)), Nil, List(ValueType.Var(A))), Set.empty)

    shouldTypeCheckAs(TInt, Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(x, OptionT(TInt)) :: Expr.ValueVar(y, TInt) :: Nil))

    // swapped arguments
    intercept[TypeError] {
      val res = typecheck(Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(y, TInt) :: Expr.ValueVar(x, OptionT(TInt)) :: Nil))
      res.check()
    }
    // too few arguments
    intercept[TypeError] {
      val res = typecheck(Expr.PureApp(orElse, TInt :: Nil, Expr.ValueVar(x, OptionT(TInt)) :: Nil))
      res.check()
    }

    val ex4: Expr.Make = Make(OptionT(TInt), SomeC, List(), List(Literal(42, TInt)))
    assertSameType(ex4.tpe, OptionT(TInt))
    assertEquals(ex4.typing.constraints, Vector(ex4))

    val ex5: Expr.Make = Make(OptionT(TInt), NoneC, List(), List())
    assertSameType(ex5.tpe, OptionT(TInt))
    assertEquals(ex5.typing.constraints, Vector(ex5))
  }

  val ex6 = Stmt.Region(Block.BlockLit(Nil, r :: Nil, Nil, BlockParam(r, TRegion, Set(r)) :: Nil,
    Stmt.Alloc(x, Expr.Literal(42, TInt), r,
      Stmt.Get(y, TInt, x, Set(r),
        Stmt.Return(List(Expr.ValueVar(y, TInt)))))))

  println(ex6.tpe)

  inline def assertSameType(got: ValueType, expected: ValueType)(using DeclarationContext): Unit =
    assertEquals(Type.equals(got, expected), true)

  inline def shouldTypeCheckAs(expected: ValueType, expr: Expr)(using DeclarationContext): Unit =
    expr.typing.check()
    assertSameType(expr.tpe, expected)
}
