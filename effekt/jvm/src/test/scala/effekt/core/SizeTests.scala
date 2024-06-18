package effekt.core

class SizeTests extends CoreTests {

  def assertSized(size: Int)(input: String)(using munit.Location): Unit = {
    val tree = parse(input, "input", Names(defaultNames))
    assertEquals(tree.size, size, "Wrong number of nodes")
  }

  test("Small program"){
    assertSized(7) {
      """module main
        |
        |def foo = { () =>
        |  return (bar: (Int) => Int @ {})(baz:Int)
        |}
        |""".stripMargin
    }
  }

  test("Nested definitions") {
    assertSized(18) {
      """ module main
        |
        | def bar = { () => return 1 }
        | def main = { () =>
        |   def renamed1 = { () => (bar : () => Unit @ {})() }
        |   def renamed2 = { () => return 2 }
        |   (renamed1 : () => Unit @ {})()
        | }
        |""".stripMargin
    }
  }
}
