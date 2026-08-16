package effekt.generator.js

class FunctionFloatingTests extends munit.FunSuite {

  private def name(value: String): JSName = JSName(value)
  private def variable(value: JSName): Expr = Expr.Variable(value)

  private val truth: Expr = Expr.RawLiteral("true")
  private val zero: Expr = Expr.RawLiteral("0")

  private def call(callee: JSName, arguments: JSName*): Stmt =
    Stmt.ExprStmt(Expr.Call(variable(callee), arguments.toList.map(variable)))

  test("float a function out of a loop to its captured binding") {
    val run = name("run")
    val loop = name("loop")
    val helper = name("helper")
    val captured = name("captured")
    val value = name("value")

    val helperDef = Stmt.Function(helper, List(value), List(
      Stmt.Return(Expr.ArrayLiteral(List(variable(captured), variable(value))))
    ))
    val loopDef = Stmt.Function(loop, Nil, List(
      Stmt.While(Some(loop), truth, List(helperDef, call(helper, captured)))
    ))
    val capturedDef = Stmt.Const(Pattern.Variable(captured), zero)

    val input = List(Stmt.Function(run, Nil, List(capturedDef, loopDef)))
    val expected = List(Stmt.Function(run, Nil, List(
      helperDef,
      Stmt.Function(loop, Nil, List(
        Stmt.While(Some(loop), truth, List(call(helper, captured)))
      )),
      capturedDef
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("float a function that is not inside a loop") {
    val outer = name("outer")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val input = List(Stmt.Function(outer, Nil, List(helperDef, call(helper))))
    val expected = List(
      helperDef,
      Stmt.Function(outer, Nil, List(call(helper)))
    )

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a captured parameter stops floating at its function") {
    val loop = name("loop")
    val helper = name("helper")
    val limit = name("limit")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(limit))))
    val input = List(Stmt.Function(loop, List(limit), List(
      Stmt.While(Some(loop), truth, List(helperDef, call(helper)))
    )))
    val expected = List(Stmt.Function(loop, List(limit), List(
      helperDef,
      Stmt.While(Some(loop), truth, List(call(helper)))
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a loop-local capture keeps the declaration in the loop") {
    val loop = name("loop")
    val helper = name("helper")
    val current = name("current")

    val currentDef = Stmt.Const(Pattern.Variable(current), zero)
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(current))))
    val input = List(Stmt.While(Some(loop), truth, List(
      currentDef,
      helperDef,
      call(helper)
    )))
    val expected = List(Stmt.While(Some(loop), truth, List(
      helperDef,
      currentDef,
      call(helper)
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("dependencies keep floating functions together") {
    val run = name("run")
    val loop = name("loop")
    val first = name("first")
    val second = name("second")
    val captured = name("captured")

    val firstDef = Stmt.Function(first, Nil, List(Stmt.Return(variable(captured))))
    val secondDef = Stmt.Function(second, Nil, List(Stmt.Return(variable(first))))
    val capturedDef = Stmt.Const(Pattern.Variable(captured), zero)
    val input = List(Stmt.Function(run, Nil, List(
      capturedDef,
      Stmt.While(Some(loop), truth, List(firstDef, secondDef, call(second)))
    )))
    val expected = List(Stmt.Function(run, Nil, List(
      firstDef,
      secondDef,
      capturedDef,
      Stmt.While(Some(loop), truth, List(call(second)))
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a helper can float together with its enclosing function") {
    val loop = name("loop")
    val enclosing = name("enclosing")
    val helper = name("helper")
    val inner = name("inner")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(enclosing))))
    val enclosingDef = Stmt.Function(enclosing, Nil, List(
      Stmt.While(Some(inner), truth, List(helperDef, call(helper)))
    ))
    val input = List(Stmt.While(Some(loop), truth, List(enclosingDef, call(enclosing))))
    val expected = List(
      helperDef,
      Stmt.Function(enclosing, Nil, List(
        Stmt.While(Some(inner), truth, List(call(helper)))
      )),
      Stmt.While(Some(loop), truth, List(call(enclosing)))
    )

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("switch-local bindings keep declarations in their clause") {
    val loop = name("loop")
    val helper = name("helper")
    val problemSize = name("problemSize")
    val scrutinee = name("scrutinee")

    val problemSizeDef = Stmt.Const(Pattern.Variable(problemSize), zero)
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(problemSize))))
    val input = List(Stmt.While(Some(loop), truth, List(
      Stmt.Switch(variable(scrutinee), List(
        zero -> List(problemSizeDef, helperDef, call(helper))
      ), None)
    )))
    val expected = List(Stmt.While(Some(loop), truth, List(
      Stmt.Switch(variable(scrutinee), List(
        zero -> List(Stmt.Block(None, List(helperDef, problemSizeDef, call(helper))))
      ), None)
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a module-closed function floats across an if") {
    val outer = name("outer")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val input = List(Stmt.Function(outer, Nil, List(
      Stmt.If(truth, Stmt.Block(None, List(helperDef, call(helper))), Stmt.Block(None, Nil))
    )))
    val expected = List(
      helperDef,
      Stmt.Function(outer, Nil, List(
        Stmt.If(truth, Stmt.Block(None, List(call(helper))), Stmt.Block(None, Nil))
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a module-closed function floats across a loop and its enclosing if") {
    val outer = name("outer")
    val loop = name("loop")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val loopDef = Stmt.While(Some(loop), truth, List(helperDef, call(helper)))
    val input = List(Stmt.Function(outer, Nil, List(
      Stmt.If(truth, Stmt.Block(None, List(loopDef)), Stmt.Block(None, Nil))
    )))
    val expected = List(
      helperDef,
      Stmt.Function(outer, Nil, List(
        Stmt.If(truth, Stmt.Block(None, List(
          Stmt.While(Some(loop), truth, List(call(helper)))
        )), Stmt.Block(None, Nil))
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("functions referenced outside an if cross it with their dependencies") {
    val outer = name("outer")
    val base = name("base")
    val helper = name("helper")
    val captured = name("captured")

    val baseDef = Stmt.Function(base, Nil, List(Stmt.Return(variable(captured))))
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(base))))
    val input = List(Stmt.Function(outer, List(captured), List(
      Stmt.If(truth, Stmt.Block(None, List(baseDef, helperDef)), Stmt.Block(None, Nil)),
      call(helper)
    )))
    val expected = List(Stmt.Function(outer, List(captured), List(
      baseDef,
      helperDef,
      Stmt.If(truth, Stmt.Block(None, Nil), Stmt.Block(None, Nil)),
      call(helper)
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a module-closed function floats across a switch clause") {
    val outer = name("outer")
    val helper = name("helper")
    val scrutinee = name("scrutinee")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val input = List(Stmt.Function(outer, Nil, List(
      Stmt.Switch(variable(scrutinee), List(
        zero -> List(helperDef, call(helper))
      ), None)
    )))
    val expected = List(
      helperDef,
      Stmt.Function(outer, Nil, List(
        Stmt.Switch(variable(scrutinee), List(
          zero -> List(call(helper))
        ), None)
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("functions referenced outside a switch clause cross it with their dependencies") {
    val outer = name("outer")
    val base = name("base")
    val helper = name("helper")
    val captured = name("captured")
    val scrutinee = name("scrutinee")

    val baseDef = Stmt.Function(base, Nil, List(Stmt.Return(variable(captured))))
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(base))))
    val input = List(Stmt.Function(outer, List(captured), List(
      Stmt.Switch(variable(scrutinee), List(zero -> List(baseDef, helperDef)), None),
      call(helper)
    )))
    val expected = List(Stmt.Function(outer, List(captured), List(
      baseDef,
      helperDef,
      Stmt.Switch(variable(scrutinee), List(zero -> Nil), None),
      call(helper)
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("labeled blocks remain blocks") {
    val loop = name("loop")
    val join = name("join")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val joinPoint = Stmt.Block(Some(join), List(Stmt.Break(Some(join))))
    val input = List(Stmt.While(Some(loop), truth, List(helperDef, joinPoint)))
    val expected = List(
      helperDef,
      Stmt.While(Some(loop), truth, List(joinPoint))
    )

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a module-closed function floats out of a lambda") {
    val holder = name("holder")
    val loop = name("loop")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val input = List(Stmt.Const(Pattern.Variable(holder), Expr.Lambda(Nil,
      Stmt.While(Some(loop), truth, List(helperDef, call(helper)))
    )))
    val expected = List(
      helperDef,
      Stmt.Const(Pattern.Variable(holder), Expr.Lambda(Nil,
        Stmt.While(Some(loop), truth, List(call(helper)))
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("module-closed dependencies float together across different boundaries") {
    val outer = name("outer")
    val base = name("base")
    val helper = name("helper")

    val baseDef = Stmt.Function(base, Nil, List(Stmt.Return(zero)))
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(base))))
    val input = List(Stmt.Function(outer, Nil, List(
      baseDef,
      Stmt.If(truth, Stmt.Block(None, List(helperDef, call(helper))), Stmt.Block(None, Nil))
    )))
    val expected = List(
      baseDef,
      helperDef,
      Stmt.Function(outer, Nil, List(
        Stmt.If(truth, Stmt.Block(None, List(call(helper))), Stmt.Block(None, Nil))
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a dependency on a captured function prevents module lifting") {
    val outer = name("outer")
    val base = name("base")
    val helper = name("helper")
    val captured = name("captured")

    val baseDef = Stmt.Function(base, Nil, List(Stmt.Return(variable(captured))))
    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(base))))
    val branch = Stmt.If(
      truth,
      Stmt.Block(None, List(helperDef, call(helper))),
      Stmt.Block(None, Nil))
    val input = List(Stmt.Function(outer, List(captured), List(baseDef, branch)))

    assertEquals(FunctionFloating.transform(input), input)
  }

  test("a module-closed function floats out of a class method") {
    val container = name("Container")
    val method = name("method")
    val helper = name("helper")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(zero)))
    val input = List(Stmt.Class(container, List(
      Stmt.Function(method, Nil, List(helperDef, call(helper)))
    )))
    val expected = List(
      helperDef,
      Stmt.Class(container, List(
        Stmt.Function(method, Nil, List(call(helper)))
      )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

  test("a function capturing a lambda parameter stays in the lambda") {
    val holder = name("holder")
    val loop = name("loop")
    val helper = name("helper")
    val captured = name("captured")

    val helperDef = Stmt.Function(helper, Nil, List(Stmt.Return(variable(captured))))
    val input = List(Stmt.Const(Pattern.Variable(holder), Expr.Lambda(List(captured),
      Stmt.While(Some(loop), truth, List(helperDef, call(helper)))
    )))
    val expected = List(Stmt.Const(Pattern.Variable(holder), Expr.Lambda(List(captured),
      Stmt.Block(None, List(
        helperDef,
        Stmt.While(Some(loop), truth, List(call(helper)))
      ))
    )))

    assertEquals(FunctionFloating.transform(input), expected)
  }

}
