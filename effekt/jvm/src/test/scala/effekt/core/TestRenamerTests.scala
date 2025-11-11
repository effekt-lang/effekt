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
    shouldBeEqual(obtained, pExpected, clue)
  }

  test("No bound local variables"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  return (bar: (Int) => Int @ {})(baz:Int)
        |}
        |""".stripMargin
    assertRenamedTo(code, code)
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
        |def foo = { () =>
        |  val $1 = (foo:(Int)=>Int@{})(4);
        |  return $1:Int
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
        |def foo = { () =>
        |  var $1 @ global = (foo:(Int)=>Int@{})(4);
        |  return $1:Int
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }

  test("function (value) parameters"){
    val input =
      """module main
        |
        |def foo = { (x:Int) =>
        |  return x:Int
        |}
        |""".stripMargin
    val expected =
      """module main
        |
        |def foo = { ($1:Int) =>
        |  return $1:Int
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
          |type Data { X(a:Int, b:Int) }
          |def foo = { () =>
          |  12 match {
          |    X : {($1:Int, $2:Int) => return $1:Int }
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
        |def foo = { ['$1]($2: $1) =>
        |  return $2:Identity[$1]
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
      """ module main
        |
        | def bar = { () => return 1 }
        | def main = { () =>
        |   def $1 = { () => (bar : () => Unit @ {})() }
        |   def $2 = { () => return 2 }
        |   ($1 : () => Unit @ {})()
        | }
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
      """ module main
        |
        | def main = { () =>
        |   let $1 = 1
        |   let $2 = 2
        |   return $2:Int
        | }
        |""".stripMargin

    assertRenamedTo(input, expected)
  }
}
