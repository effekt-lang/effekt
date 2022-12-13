package effekt

import kiama.util.StringConsole

class ReplTests extends munit.FunSuite {

  val inputs = List("1 + 1")

  def runRepl(input: String): String = {
    val compiler = new effekt.Driver {}

    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--lib", "libraries/js"))

    configs.verify()

    val repl = Repl(compiler)

    repl.processconsole(StringConsole(input), "", configs)

    configs.stringEmitter.result()
  }

  test("Evaluating small examples") {
    assertNoDiff(runRepl("1 + 1\n"), "2\n")
    assertNoDiff(runRepl(":t 1 + 1\n"), "Int\n")
    assertNoDiff(runRepl(":t 1 + 1\n1 + 1\n"), "Int\n2\n")
  }

  test("Function definitions") {
    val in =
      """|def f() = 1
         |def g() = 2
         |
         |f() + g()
         |""".stripMargin

    val out =
      """|f: () => Int
         |g: () => Int
         |3
         |""".stripMargin

    assertNoDiff(runRepl(in), out)
  }

  test("Regression (#168)") {
    // defining the same function twice lead to a cache inconsistency.
    val in =
      """|def f() = 1
         |def f() = 1
         |
         |f() + f()
         |""".stripMargin

    val out =
      """|f: () => Int
         |f: () => Int
         |2
         |""".stripMargin

    assertNoDiff(runRepl(in), out)
  }

  test("Regression: toplevel block definitions") {
    val in =
      """|interface Foo {}
         |def f = new Foo {}
         |""".stripMargin

    val out =
      """|f: Foo
         |""".stripMargin

    assertNoDiff(runRepl(in), out)
  }

  test("Imports") {

    val expected =
      """|Successfully imported examples/pos/builtins
         |
         |Imported Types
         |==============
         |type Color {
         |  def Red(): Color / {}
         |  def Green(): Color / {}
         |  def Blue(): Color / {}
         |}
         |
         |Imported Functions
         |==================
         |def show(c: Color): String / {}
         |def main(): Unit / {}
         |""".stripMargin

    assertNoDiff(runRepl("import examples/pos/builtins"), expected)
  }


}
