package effekt.generator.js

class ControlFlowSimplificationTests extends munit.FunSuite {

  private def name(value: String): JSName = JSName(value)

  test("remove a terminal break to its enclosing labeled block") {
    val exit = name("exit")
    val binding = Stmt.Const(
      Pattern.Variable(name("value")),
      Expr.RawLiteral("0"))
    val input = List(Stmt.Block(Some(exit), List(
      binding,
      Stmt.Break(Some(exit)))))
    val expected = List(Stmt.Block(Some(exit), List(binding)))

    assertEquals(ControlFlowSimplification.transform(input), expected)
  }

  test("keep nonterminal and differently targeted breaks") {
    val outer = name("outer")
    val inner = name("inner")
    val nonterminal = Stmt.Break(Some(outer))
    val following = Stmt.ExprStmt(Expr.RawLiteral("following"))
    val input = List(Stmt.Block(Some(outer), List(
      Stmt.Block(Some(inner), List(nonterminal)),
      following)))

    assertEquals(ControlFlowSimplification.transform(input), input)
  }

  test("remove a labeled join when every branch breaks to it") {
    val join = name("join")
    val value = Expr.Variable(name("value"))
    val assign = (raw: String) => Stmt.Assign(value, Expr.RawLiteral(raw))
    val input = List(Stmt.Block(Some(join), List(
      Stmt.If(
        Expr.RawLiteral("condition"),
        Stmt.Block(None, List(assign("1"), Stmt.Break(Some(join)))),
        Stmt.Block(None, List(assign("2"), Stmt.Break(Some(join))))))))
    val expected = List(Stmt.If(
      Expr.RawLiteral("condition"),
      Stmt.Block(None, List(assign("1"))),
      Stmt.Block(None, List(assign("2")))))

    assertEquals(ControlFlowSimplification.transform(input), expected)
  }

  test("remove a break from an ending path independently of sibling paths") {
    val join = name("join")
    val input = List(Stmt.Block(Some(join), List(
      Stmt.If(
        Expr.RawLiteral("condition"),
        Stmt.Block(None, List(Stmt.Break(Some(join)))),
        Stmt.Block(None, Nil)))))
    val expected = List(Stmt.Block(Some(join), List(
      Stmt.If(
        Expr.RawLiteral("condition"),
        Stmt.Block(None, Nil),
        Stmt.Block(None, Nil)))))

    assertEquals(ControlFlowSimplification.transform(input), expected)
  }

  test("simplify ending paths through nested labels") {
    val outer = name("outer")
    val inner = name("inner")
    val branch = Stmt.If(
      Expr.RawLiteral("first"),
      Stmt.Block(None, List(Stmt.Break(Some(outer)))),
      Stmt.If(
        Expr.RawLiteral("second"),
        Stmt.Block(None, List(Stmt.Break(Some(inner)))),
        Stmt.Block(None, List(Stmt.Break(Some(inner))))))
    val simplified = Stmt.If(
        Expr.RawLiteral("first"),
        Stmt.Block(None, Nil),
        Stmt.If(
          Expr.RawLiteral("second"),
          Stmt.Block(None, Nil),
          Stmt.Block(None, Nil)))
    val input = List(Stmt.Block(Some(outer), List(
      Stmt.Block(Some(inner), List(branch)))))
    val expected = List(Stmt.Block(Some(outer), List(simplified)))

    assertEquals(ControlFlowSimplification.transform(input), expected)
  }
}
