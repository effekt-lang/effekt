package effekt.core

class RenamerTests extends CoreTests {

  def assertRenamedTo(input: String,
                      renamed: String,
                      clue: => Any = "Not renamed to given value",
                      names: Names = defaultNames)(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val pExpected = parse(renamed, "expected", names)
    val renamer = new Renamer(names)
    val obtained = renamer(pInput)
    assertEquals(obtained, pExpected, clue)
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
        |  val l1 = (foo:(Int)=>Int@{})(4);
        |  return l1:Int
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
        |  var l1 @ global = (foo:(Int)=>Int@{})(4);
        |  return l1:Int
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
        |def foo = { (l1:Int) =>
        |  return l1:Int
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
          |    X : {(l2:Int, l1:Int) => return l2:Int }
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
        |def foo = { ['l2](l1: l2) =>
        |  return l1:Identity[l2]
        |}
        |""".stripMargin
    assertRenamedTo(input, expected)
  }
}
