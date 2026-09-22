package effekt
package core

class DelimiterTests extends CoreTests {

  val mainSymbol = Id("main")

  def module(input: String): ModuleDecl =
    val names = Names(defaultNames + ("main" -> mainSymbol))
    TestRenamer(names)(parse("module test\n\n" + input, "input", names))

  def disciplined(input: String)(using munit.Location) = module(input).typecheck()
  def undisciplined(input: String)(using munit.Location) =
    val error = intercept[AssertionError] { module(input).typecheck() }
    assert(error.getMessage.contains("A handler must not observe"), error.getMessage)

  test("a handler must not observe a variable its own try binds") {
    undisciplined(
      """ def main = { () => reset { (){p: Prompt[Int]} => var r @ c = 1; shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => get y : Int = ! r @ c; return y:Int } } }
        |""".stripMargin)
  }

  test("what handler resumes with may observe it") {
    disciplined(
      """ def main = { () => reset { (){p: Prompt[Int]} => var r @ c = 1; shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { get y : Int = ! r @ c; return y:Int } } } }
        |""".stripMargin)
  }

  test("a handler must not shift to its own prompt outside a resumption") {
    undisciplined(
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => shift (p : Prompt[Int] @ {p}) { {j: Resume[Int, Int]} => return 1 } } } }
        |""".stripMargin)
  }

  test("a handler must not shift to a prompt its own try installs") {
    undisciplined(
      """ def main = { () => reset { (){p: Prompt[Int]} => reset { (){q: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 1 } } } } }
        |""".stripMargin)
  }

  test("a handler must not call a block that closed over a prompt its own try installs") {
    undisciplined(
      """ def main = { () => reset { (){p: Prompt[Int]} => reset { (){q @ qc: Prompt[Int]} => def f = { () => shift (q : Prompt[Int] @ {qc}) { {j: Resume[Int, Int]} => return 1 } } shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => (f : () => Int @ {qc})() } } } }
        |""".stripMargin)
  }

  test("a handler may shift to a prompt installed outside its try") {
    disciplined(
      """ def main = { () => reset { (){q: Prompt[Int]} => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 1 } } } } }
        |""".stripMargin)
  }
}
