package effekt.core
import effekt.symbols

class RenamerTests extends CoreTests {

  def assertRenamedTo(input: String,
                      renamed: String,
                      clue: => Any = "Not renamed to given value",
                      names: Names = Names(defaultNames))(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val pExpected = parse(renamed, "expected", names)
    val renamer = new Renamer(names, "renamed") // use "renamed" as prefix so we can refer to it
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
        |  val renamed1 = (foo:(Int)=>Int@{})(4);
        |  return renamed1:Int
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
        |  var renamed1 @ global = (foo:(Int)=>Int@{})(4);
        |  return renamed1:Int
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
        |def foo = { (renamed1:Int) =>
        |  return renamed1:Int
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
          |    X : {(renamed1:Int, renamed2:Int) => return renamed1:Int }
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
        |def foo = { ['renamed1](renamed2: renamed1) =>
        |  return renamed2:Identity[renamed1]
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
        |   def renamed1 = { () => (bar : () => Unit @ {})() }
        |   def renamed2 = { () => return 2 }
        |   (renamed1 : () => Unit @ {})()
        | }
        |""".stripMargin

    assertRenamedTo(input, expected)
  }
  // TODO this needs to be fixed
  //  test("shadowing let bindings"){
  //    val input =
  //      """ module main
  //        |
  //        | def main = { () =>
  //        |   let x = 1
  //        |   let x = 2
  //        |   return x:Int
  //        | }
  //        |""".stripMargin
  //
  //    val expected =
  //      """ module main
  //        |
  //        | def main = { () =>
  //        |   let renamed1 = 1
  //        |   let renamed2 = 2
  //        |   return renamed2:Int
  //        | }
  //        |""".stripMargin
  //
  //    assertRenamedTo(input, expected)
  //  }
}
