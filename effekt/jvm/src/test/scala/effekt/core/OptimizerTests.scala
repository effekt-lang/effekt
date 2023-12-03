package effekt
package core
import effekt.symbols

class OptimizerTests extends CoreTests {

  val mainSymbol = Id("main")

  def assertTransformsTo(
    input: String,
    transformed: String,
    names: Names = Names(defaultNames + ("main" -> mainSymbol))
  )(using munit.Location) = {
    val moduleHeader =
      """module test
        |
        |""".stripMargin
    val pInput = parse(moduleHeader + input, "input", names)
    val pExpected = parse(moduleHeader + transformed, "expected", names)

    // the parser is not assigning symbols correctly, so we need to run renamer first
    val renamed = Renamer(names).rewrite(pInput)

    val obtained = RemoveUnusedDefinitions(Set(mainSymbol), renamed).run()
    assertAlphaEquivalent(obtained, pExpected, "Not transformed to")
  }

  test("toplevel"){
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => return 42 }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    assertTransformsTo(input, expected)
  }

  test("transitive (length 3)"){
    val input =
      """ def foo = { () => return 42 }
        | def bar = { () => (foo : () => Unit @ {})() }
        | def baz = { () => (bar : () => Unit @ {})() }
        | def bam = { () => (baz : () => Unit @ {})() }
        | def main = { () => (bam : () => Unit @ {})() }
        |""".stripMargin

    assertTransformsTo(input, input)
  }

  test("recursive (unused)"){
    val input =
      """ def foo = { () => (bar : () => Unit @ {})() }
        | def bar = { () => (foo : () => Unit @ {})() }
        | def main = { () => return 42 }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    assertTransformsTo(input, expected)
  }

  test("recursive used"){
    val input =
      """ def foo = { () => (bar : () => Unit @ {})() }
        | def bar = { () => (foo : () => Unit @ {})() }
        | def main = { () => (foo : () => Unit @ {})() }
        |""".stripMargin

    assertTransformsTo(input, input)
  }

  test("nested all removed"){
    val input =
      """ def main = { () =>
        |   def foo = { () => return 1 }
        |   return 2
        | }
        |""".stripMargin

    val expected =
      """ def main = { () => return 2 }
        |""".stripMargin

    assertTransformsTo(input, expected)
  }

  test("pseudo recursive"){
    val input =
      """ def bar = { () => return 1 }
        | def main = { () =>
        |   def foo = { () => (bar : () => Unit @ {})() }
        |   def bar = { () => return 2 }
        |   (foo : () => Unit @ {})()
        | }
        |""".stripMargin


    val expected =
      """ def bar = { () => return 1 }
        | def main = { () =>
        |   def foo = { () => (bar : () => Unit @ {})() }
        |   (foo : () => Unit @ {})()
        | }
        |""".stripMargin

    assertTransformsTo(input, expected)
  }

}
