package effekt
package core
import effekt.symbols

class OptimizerTests extends CoreTests {

  val mainSymbol = Id("main")

  def assertTransformsTo(
    input: String,
    transformed: String,
    names: Names = Names(defaultNames + ("main" -> mainSymbol))
  )(transform: ModuleDecl => ModuleDecl)(using munit.Location) = {
    val moduleHeader =
      """module test
        |
        |""".stripMargin
    val pInput = parse(moduleHeader + input, "input", names)
    val pExpected = parse(moduleHeader + transformed, "expected", names)

    // the parser is not assigning symbols correctly, so we need to run renamer first
    val renamed = Renamer(names).rewrite(pInput)

    val obtained = transform(renamed)
    assertAlphaEquivalent(obtained, pExpected, "Not transformed to")
  }

  def removeUnused(input: String, expected: String)(using munit.Location) =
    assertTransformsTo(input, expected) { tree =>
      RemoveUnusedDefinitions(Set(mainSymbol), tree).run()
    }

  def inlineOnce(input: String, expected: String)(using munit.Location) =
    assertTransformsTo(input, expected) { tree =>
      val (result, count) = InlineUnique.once(Set(mainSymbol), tree)
      result
    }

  test("toplevel"){
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => return 42 }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    removeUnused(input, expected)
  }

  test("transitive (length 3)"){
    val input =
      """ def foo = { () => return 42 }
        | def bar = { () => (foo : () => Unit @ {})() }
        | def baz = { () => (bar : () => Unit @ {})() }
        | def bam = { () => (baz : () => Unit @ {})() }
        | def main = { () => (bam : () => Unit @ {})() }
        |""".stripMargin

    removeUnused(input, input)
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

    removeUnused(input, expected)
  }

  test("recursive used"){
    val input =
      """ def foo = { () => (bar : () => Unit @ {})() }
        | def bar = { () => (foo : () => Unit @ {})() }
        | def main = { () => (foo : () => Unit @ {})() }
        |""".stripMargin

    removeUnused(input, input)
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

    removeUnused(input, expected)
  }
  // let y = !(println: (String) => Unit @ {io})("hello")
  test("drop pure let expressions"){
    val input =
      """ def main = { () =>
        |   let x = (add : (Int, Int) => Int @ {})(1, 2)
        |   let y = !(println: (String) => Unit @ {io})("hello")
        |   let z = 7
        |   return z:Int
        | }
        |""".stripMargin

    val expected =
      """ def main = { () =>
        |   let y = !(println: (String) => Unit @ {io})("hello")
        |   let z = 7
        |   return z:Int
        | }
        |""".stripMargin

    removeUnused(input, expected)
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

    removeUnused(input, expected)
  }

  test("inline toplevel"){
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => (foo : () => Unit @ {})() }
        |""".stripMargin

    val expected =
      """ def foo = { () => return 42 }
        | def main = { () => return 42 }
        |""".stripMargin

    inlineOnce(input, expected)
  }

  test("inline with argument"){
    val input =
      """ def foo = { (n: Int) => return n:Int }
        | def main = { () => (foo : (Int) => Unit @ {})(42) }
        |""".stripMargin

    val expected =
      """ def foo = { (n: Int) => return n:Int }
        | def main = { () => return 42 }
        |""".stripMargin

    inlineOnce(input, expected)
  }

  test("inline higher order function"){
    val input =
      """ def foo = { (n: Int) => return n:Int }
        | def hof = { (){f : (Int) => Int} =>
        |   (f : (Int) => Int @ {f})(1)
        | }
        | def main = { () =>
        |   (hof : (){f : (Int) => Int} => Int @ {})(){ (foo : (Int) => Unit @ {}) }
        | }
        |""".stripMargin

    val expected =
      """ def foo = { (n: Int) => return n:Int }
        | def hof = { (){f : (Int) => Int} =>
        |   (f : (Int) => Int @ {f})(1)
        | }
        | def main = { () =>
        |   def local(n: Int) = return n:Int
        |   (local : (Int) => Int @ {})(1)
        | }
        |""".stripMargin

    inlineOnce(input, expected)
  }

}
