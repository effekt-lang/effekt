package effekt.core

/**
 * This is testing the main/core.Renamer using the test/core.TestRenamer.
 */
class RenamerTests extends CoreTests {

  /**
   * Check that the renamed input preserves alpha-equivalence using [[assertAlphaEquivalent]]
   */
  def assertRenamingPreservesAlpha(input: String,
                                   clue: => Any = "Not renamed to given value",
                                   names: Names = Names(defaultNames))(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val renamer = new Renamer(names, "renamed")
    val obtained = renamer(pInput)
    assertAlphaEquivalent(obtained, pInput, clue)
  }

  test("No bound local variables"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  return (bar: (Int) => Int @ {})(baz:Int)
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("val binding"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  val x = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("var binding"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  var x @ global = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("function (value) parameters"){
    val code =
      """module main
        |
        |def foo = { (x:Int) =>
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("match clauses"){
    val code =
      """module main
        |
        |type Data { X(a:Int, b:Int) }
        |def foo = { () =>
        |  12 match {
        |    X : {(aa:Int, bb:Int) => return aa:Int }
        |  }
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("type parameters"){
    val code =
      """module main
        |
        |def foo = { ['A](a: A) =>
        |  return a:Identity[A]
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }

  test("pseudo recursive"){
    val code =
      """ module main
        |
        | def bar = { () => return 1 }
        | def main = { () =>
        |   def foo = { () => (bar : () => Unit @ {})() }
        |   def bar = { () => return 2 }
        |   (foo : () => Unit @ {})()
        | }
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }
  test("shadowing let bindings"){
    val code =
      """ module main
        |
        | def main = { () =>
        |   let x = 1
        |   let x = 2
        |   return x:Int
        | }
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }
}
