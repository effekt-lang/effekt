package effekt
package core

import effekt.core.optimizer.ReferencesToParameters

class ReferencesToParametersTests extends CoreTests {

  def assertTransformsTo(input: String, expected: String)(using munit.Location) = {
    val header = "module test\n\n"
    val names = Names(defaultNames)
    val parsedInput = parse(header + input, "input", names)
    val parsedExpected = parse(header + expected, "expected", names)
    val renamed = TestRenamer(names)(parsedInput)
    assertAlphaEquivalent(ReferencesToParameters.transform(renamed), parsedExpected, "Not transformed to")
  }

  test("straight-line code") {
    val input =
      """def main = { () =>
        |  var x @ c = 0;
        |  get a: Int = !x @ c;
        |  put x @ c = 1;
        |  get b: Int = !x @ c;
        |  return b: Int
        |}
        |""".stripMargin
    val expected =
      """def main = { () =>
        |  let x0 = 0
        |  let a = x0: Int
        |  let x1 = 1
        |  let b = x1: Int
        |  return b: Int
        |}
        |""".stripMargin
    assertTransformsTo(input, expected)
  }

  test("tail-called loop gets a parameter but no result") {
    val input =
      """def main = { (n: Bool) =>
        |  var x @ c = 0;
        |  def loop(b: Bool) = {
        |    if (b: Bool) {
        |      put x @ c = 1;
        |      (loop : (Bool) => Int @ {c})(false)
        |    } else {
        |      get r: Int = !x @ c;
        |      return r: Int
        |    }
        |  }
        |  (loop : (Bool) => Int @ {c})(n: Bool)
        |}
        |""".stripMargin
    val expected =
      """def main = { (n: Bool) =>
        |  let x0 = 0
        |  def loop(b: Bool, x1: Int) = {
        |    if (b: Bool) {
        |      let x2 = 1
        |      (loop : (Bool, Int) => Int @ {})(false, x2: Int)
        |    } else {
        |      let r = x1: Int
        |      return r: Int
        |    }
        |  }
        |  (loop : (Bool, Int) => Int @ {})(n: Bool, x0: Int)
        |}
        |""".stripMargin
    assertTransformsTo(input, expected)
  }

  test("loop called in non-tail position returns the state") {
    val input =
      """def main = { (n: Bool) =>
        |  var x @ c = 0;
        |  def loop(b: Bool) = {
        |    if (b: Bool) {
        |      put x @ c = 1;
        |      (loop : (Bool) => Unit @ {c})(false)
        |    } else {
        |      return ()
        |    }
        |  }
        |  val u = (loop : (Bool) => Unit @ {c})(n: Bool);
        |  get r: Int = !x @ c;
        |  return r: Int
        |}
        |""".stripMargin
    val expected =
      """def main = { (n: Bool) =>
        |  let x0 = 0
        |  def loop(b: Bool, x1: Int) = {
        |    if (b: Bool) {
        |      let x2 = 1
        |      (loop : (Bool, Int) => (Unit, Int) @ {})(false, x2: Int)
        |    } else {
        |      return (), x1: Int
        |    }
        |  }
        |  val u, x3 = (loop : (Bool, Int) => (Unit, Int) @ {})(n: Bool, x0: Int);
        |  let r = x3: Int
        |  return r: Int
        |}
        |""".stripMargin
    assertTransformsTo(input, expected)
  }

  test("state that is dead after a non-tail call is not returned") {
    val input =
      """def main = { (n: Bool) =>
        |  var x @ c = 0;
        |  def loop(b: Bool) = {
        |    if (b: Bool) {
        |      put x @ c = 1;
        |      (loop : (Bool) => Unit @ {c})(false)
        |    } else {
        |      return ()
        |    }
        |  }
        |  val u = (loop : (Bool) => Unit @ {c})(n: Bool);
        |  return 42
        |}
        |""".stripMargin
    val expected =
      """def main = { (n: Bool) =>
        |  let x0 = 0
        |  def loop(b: Bool, x1: Int) = {
        |    if (b: Bool) {
        |      let x2 = 1
        |      (loop : (Bool, Int) => Unit @ {})(false, x2: Int)
        |    } else {
        |      return ()
        |    }
        |  }
        |  val u = (loop : (Bool, Int) => Unit @ {})(n: Bool, x0: Int);
        |  return 42
        |}
        |""".stripMargin
    assertTransformsTo(input, expected)
  }

  test("reference escaping into a block argument is kept") {
    val input =
      """def main = { () =>
        |  var x @ c = 0;
        |  (foo : () {f: () => Unit} => Unit @ {})() { () =>
        |    put x @ c = 1;
        |    return ()
        |  }
        |}
        |""".stripMargin
    assertTransformsTo(input, input)
  }
}
