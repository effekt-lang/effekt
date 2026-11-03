package effekt
package cps

import effekt.core.{ CoreTests, Names }
import kiama.parsing.{ NoSuccess, Success }
import munit.Location

/**
 * Tests the translation from core to CPS.
 *
 * Both, the translated program and the expected CPS program, are renamed
 * and compared by their pretty printed form.
 */
class TransformerTests extends CoreTests {

  def assertTransformsTo(input: String, expected: String)(using Location): Unit = {
    val names = Names(defaultNames)
    val parsed = parse("module test\n\n" + input, "input", names)
    // establish unique names (as in the compiler pipeline)
    val obtained = transform(new effekt.core.Renamer().apply(parsed))

    val expectedTree = Parser.module(expected, names) match {
      case Success(result, next) if next.atEnd => result
      case Success(result, next) =>
        fail(s"Parsing expected had trailing garbage: ${next.source.toString.substring(next.offset)}")
      case err: NoSuccess =>
        val pos = err.next.position
        fail(s"Parsing expected failed\n[${pos.line}:${pos.column}] ${err.message}")
    }

    def show(m: ModuleDecl) = PrettyPrinter.format(new TestRenamer(normalizeGlobals = true)(m)).layout
    assertNoDiff(show(obtained), show(expectedTree))
  }

  test("returning calls the continuation") {
    assertTransformsTo(
      """def f(x: Int) = {
        |  val y: Int = return x: Int;
        |  return y: Int
        |}
        |""".stripMargin,
      """def f(x, ks, k) {
        |  k(x, ks)
        |}
        |""".stripMargin)
  }

  test("control-pure calls bind their result") {
    assertTransformsTo(
      """def g(x: Int) = return x: Int
        |def f(x: Int) = {
        |  val y: Int = (g: (Int) => Int @ {})(x: Int);
        |  return y: Int
        |}
        |""".stripMargin,
      """def g(x, ks, k) {
        |  k(x, ks)
        |}
        |def f(x, ks, k) {
        |  let result = g!(x, ks, return);
        |  k(result, ks)
        |}
        |""".stripMargin)
  }

  test("calls in tail position pass their continuation") {
    assertTransformsTo(
      """def g(x: Int) = return x: Int
        |def f(x: Int) = (g: (Int) => Int @ {})(x: Int)
        |""".stripMargin,
      """def g(x, ks, k) {
        |  k(x, ks)
        |}
        |def f(x, ks, k) {
        |  g!(x) @ ks, k
        |}
        |""".stripMargin)
  }

  test("effectful calls pass their continuation") {
    assertTransformsTo(
      """def f(){h: (Int) => Int} = {
        |  val y: Int = (h: (Int) => Int @ {h})(1);
        |  return y: Int
        |}
        |def g(){h: (Int) => Int} = (h: (Int) => Int @ {h})(2)
        |""".stripMargin,
      """def f(h, ks, k) {
        |  def k_1(y, ks_1) {
        |    k(y, ks_1)
        |  }
        |  h!(1) @ ks, k_1
        |}
        |def g(h, ks, k) {
        |  h!(2) @ ks, k
        |}
        |""".stripMargin)
  }

  test("method calls continue with the returned meta-continuation") {
    assertTransformsTo(
      """interface Reader { read: () => Int }
        |def f(){reader: Reader} = {
        |  val y: Int = (reader: Reader @ {reader}).read: () => Int();
        |  return y: Int
        |}
        |""".stripMargin,
      """def f(reader, ks, k) {
        |  let result | ks_1 = reader.read!(ks, return);
        |  k(result, ks_1)
        |}
        |""".stripMargin)
  }

  test("conditionals share their continuation as a joinpoint") {
    assertTransformsTo(
      """def f(b: Bool) = {
        |  val y: Int = if (b: Bool) return 1 else return 2;
        |  return y: Int
        |}
        |""".stripMargin,
      """def f(b, ks, k) {
        |  def k_1(y, ks_1) {
        |    k(y, ks_1)
        |  }
        |  if (b) { k_1(1, ks) } else { k_1(2, ks) }
        |}
        |""".stripMargin)
  }

  test("resets return multiple values") {
    assertTransformsTo(
      """def f() = {
        |  val a, b = reset { (){p: Prompt[Int, Int]} => return 1, 2 };
        |  return b: Int, a: Int
        |}
        |""".stripMargin,
      """def f(ks, k) {
        |  def k_1(a, b, ks_1) {
        |    k(b, a, ks_1)
        |  }
        |  reset[2](p, ks_2, k_2) { k_2(1, 2, ks_2) } @ ks, k_1
        |}
        |""".stripMargin)
  }
}
