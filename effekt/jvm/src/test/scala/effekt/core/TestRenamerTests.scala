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
    val renamer = new TestRenamer(names, "_renamed_") // use "renamed" as prefix so we can refer to it
    val obtained = renamer(pInput)
    val obtainedPrinted = effekt.core.ReparsablePrettyPrinter.format(obtained).layout
    val expectedPrinted = effekt.core.ReparsablePrettyPrinter.format(pExpected).layout
    assertEquals(obtainedPrinted, expectedPrinted)
    shouldBeEqual(obtained, pExpected, clue)
  }

  test("No bound local variables"){
    val input =
      """module main
        |
        |def foo = { () =>
        |  return (bar: (Int) => Int @ {})(baz:Int)
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def foo_renamed_0() = {
        |  return (bar: (Int) => Int @ {})(baz: Int)
        |}
        |""".stripMargin

    assertRenamedTo(input, expected)
  }

  test("val binding"){
    val input =
      """module main
        |
        |def foo = { () =>
        |  val x = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def foo_renamed_0() = {
        |  val x_renamed_1: Int = {
        |    foo_renamed_0: (Int) => Int @ {}(4)
        |  };
        |  return x_renamed_1: Int
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("var binding"){
    val input =
      """module main
        |
        |def foo = { () =>
        |  var x @ global = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def foo_renamed_0() = {
        |  var x_renamed_1 @ global = (foo_renamed_0: (Int) => Int @ {})(4);
        |  return x_renamed_1: Int
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("function (value) parameters"){
    val input =
      """module main
        |
        |def f(x: Int) = {
        |  return x: Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def f_renamed_0(x_renamed_1: Int) = {
        |  return x_renamed_1: Int
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("match clauses"){
    val input =
      """module main
        |
        |type Data { X(a:Int, b:Int) }
        |def foo = { () =>
        |  12 match [Int] {
        |    X : {(aa:Int, bb:Int) => return aa:Int }
        |  }
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |type Data_renamed_1 {
        |  X_renamed_0(a: Int, b: Int)
        |}
        |
        |def foo_renamed_2() = {
        |  12 match[Int] {
        |    X : { (aa_renamed_3: Int, bb_renamed_4: Int) =>
        |      return aa_renamed_3: Int
        |    }
        |  }
        |}""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("type parameters"){
    val input =
      """module main
        |
        |def foo = { ['A](a: Identity[A]) =>
        |  return a:Identity[A]
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def foo_renamed_0['A_renamed_1](a_renamed_2: Identity[A_renamed_1]) = {
        |  return a_renamed_2: Identity[A_renamed_1]
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("pseudo recursive"){
    val input =
      """ module main
        |
        | def bar = { () => return 1 }
        | def main = { () =>
        |   def foo = { () => (bar : () => Unit @ {})() }
        |   def bar = { () => return 2 }
        |   (foo : () => Unit @ {})()
        | }
        |""".stripMargin

    val expected =
      """module main
        |
        |def bar_renamed_0() = {
        |  return 1
        |}
        |def main_renamed_1() = {
        |  def foo_renamed_2() = {
        |    bar_renamed_0: () => Unit @ {}()
        |  }
        |  def bar_renamed_3() = {
        |    return 2
        |  }
        |  foo_renamed_2: () => Unit @ {}()
        |}
        |""".stripMargin

    assertRenamedTo(input, expected)
  }
  test("shadowing let bindings"){
    val input =
      """ module main
        |
        | def main = { () =>
        |   let x = 1
        |   let x = 2
        |   return x:Int
        | }
        |""".stripMargin

    val expected =
      """module main
        |
        |def main_renamed_0() = {
        |  let x_renamed_1 = 1
        |  let x_renamed_2 = 2
        |  return x_renamed_2: Int
        |}
        |""".stripMargin

    assertRenamedTo(input, expected)
  }
}
