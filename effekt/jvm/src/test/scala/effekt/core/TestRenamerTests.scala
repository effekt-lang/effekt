package effekt.core

/**
 * This is testing the test/core.TestRenamer, not the main/core.Renamer :)
 * These tests are important, because we use TestRenamer for deciding test-friendly alpha-equivalence in CoreTests.
 */
class TestRenamerTests extends CoreTests {

  def assertRenamedTo(input: String,
                      renamed: String,
                      clue: => Any = "Not renamed to given value",
                      names: Names = Names(defaultNames))(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val pExpected = parse(renamed, "expected", names)
    val renamer = new TestRenamer(names)
    val obtained = renamer(pInput)
    val prettyInput = effekt.core.PrettyPrinter.format(pInput).layout
    assertEquals(prettyInput, renamed)
    assertEquals(effekt.util.PrettyPrinter.format(pInput).layout, effekt.util.PrettyPrinter.format(pExpected).layout)
    assertAlphaEquivalent(obtained, pExpected, clue)
  }

  test("No bound local variables"){
    val input =
      """module main
        |
        |def foo$1 = { () =>
        |  return (bar$2: (Int) => Int @ {})(baz$3:Int)
        |}
        |""".stripMargin

    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def foo$1() = {
        |  return (bar$2: (Int) => Int @ {})(baz$3: Int)
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("val binding"){
    val input =
      """module main
        |
        |def foo$1 = { () =>
        |  val x$2 = (foo$1:(Int)=>Int@{})(4) ;
        |  return x$2:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def foo$1() = {
        |  val x$2: Int = {
        |    foo$1: (Int) => Int @ {}(4)
        |  };
        |  return x$2: Int
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("var binding"){
    val input =
      """module main
        |
        |def foo$1 = { () =>
        |  var x$2 @ global = (foo$1:(Int)=>Int@{})(4) ;
        |  return x$2:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def foo$1() = {
        |  var x$2 @ global = (foo$1: (Int) => Int @ {})(4);
        |  return x$2: Int
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("function (value) parameters"){
    val input =
      """module main
        |
        |def foo$1 = { (x$2:Int) =>
        |  return x$2:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def foo$1(x$2: Int) = {
        |  return x$2: Int
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("match clauses"){
    val input =
      """module main
        |
        |type Data$1 { X$2(a$3:Int, b$3:Int) }
        |def foo$4 = { () =>
        |  12 match {
        |    X$2 : {(aa$5:Int, bb$6:Int) => return aa$5:Int }
        |  }
        |}
        |""".stripMargin
    val expected =
        """module main
          |
          |
          |
          |type Data$1 {
          |  X$2(a$3: Int, b$3: Int)
          |}
          |
          |
          |
          |def foo$4() = {
          |  12 match {
          |    X$2 : { (aa$5: Int, bb$6: Int) =>
          |      return aa$5: Int
          |    }
          |  }
          |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("type parameters"){
    val input =
      """module main
        |
        |def foo$1 = { ['A$2](a$3: A$2) =>
        |  return a$3: Identity$4[A$2]
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def foo$1['A$2](a$3: A$2) = {
        |  return a$3: Identity$4[A$2]
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("pseudo recursive"){
    val input =
      """ module main
        |
        | def bar$1 = { () => return 1 }
        | def main$2 = { () =>
        |   def foo$3 = { () => (bar$1 : () => Unit @ {})() }
        |   def bar$4 = { () => return 2 }
        |   (foo$3 : () => Unit @ {})()
        | }
        |""".stripMargin

    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def bar$1() = {
        |  return 1
        |}
        |def main$2() = {
        |  def foo$3() = {
        |    bar$1: () => Unit @ {}()
        |  }
        |  def bar$4() = {
        |    return 2
        |  }
        |  foo$3: () => Unit @ {}()
        |}""".stripMargin

    assertRenamedTo(input, expected)
  }

  test("shadowing let bindings"){
    val code =
      """ module main
        |
        | def main$1 = { () =>
        |   let x$2 = 1
        |   let x$3 = 2
        |   return x$3:Int
        | }
        |""".stripMargin

    val expected =
      """module main
        |
        |
        |
        |
        |
        |
        |
        |def main$1() = {
        |  let x$2 = 1
        |  let x$3 = 2
        |  return x$3: Int
        |}""".stripMargin

    assertRenamedTo(code, expected)
  }
}
