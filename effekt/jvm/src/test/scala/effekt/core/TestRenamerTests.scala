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
    val renamer = new TestRenamer(names, "renamed") // use "renamed" as prefix so we can refer to it
    val obtained = renamer(pInput)
    val obtainedPrinted = effekt.core.PrettyPrinter.format(obtained).layout
    val expectedPrinted = effekt.core.PrettyPrinter.format(pExpected).layout
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
        |def renamed0() = {
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
        |def renamed0() = {
        |  val renamed1: Int = {
        |    renamed0: (Int) => Int @ {}(4)
        |  };
        |  return renamed1: Int
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
        |def renamed0() = {
        |  var renamed1 @ global = (renamed0: (Int) => Int @ {})(4);
        |  return renamed1: Int
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("function (value) parameters"){
    val input =
      """module main
        |
        |def renamed0(renamed1: Int) = {
        |  return renamed1: Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def renamed0(renamed1: Int) = {
        |  return renamed1: Int
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
        |  12 match {
        |    X : {(aa:Int, bb:Int) => return aa:Int }
        |  }
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |type renamed3 {
        |  renamed4(a: Int, b: Int)
        |}
        |
        |def renamed2() = {
        |  12 match {
        |    X : { (renamed5: Int, renamed6: Int) =>
        |      return renamed5: Int
        |    }
        |  }
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("type parameters"){
    val input =
      """module main
        |
        |def foo = { ['A](a: A) =>
        |  return a:Identity[A]
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def renamed0['renamed1](renamed2: renamed1) = {
        |  return renamed2: Identity[renamed1]
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
        |def renamed0() = {
        |  return 1
        |}
        |def renamed1() = {
        |  def renamed2() = {
        |    renamed0: () => Unit @ {}()
        |  }
        |  def renamed3() = {
        |    return 2
        |  }
        |  renamed2: () => Unit @ {}()
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
        |def renamed0() = {
        |  let renamed1 = 1
        |  let renamed2 = 2
        |  return renamed2: Int
        |}
        |""".stripMargin

    assertRenamedTo(input, expected)
  }
}
