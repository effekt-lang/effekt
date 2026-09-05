package effekt
package core

import effekt.core.optimizer.*

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
    val renamer = TestRenamer(names)
    val renamed = renamer(pInput)

    val obtained = transform(renamed)
    assertAlphaEquivalent(obtained, pExpected, "Not transformed to")
  }

  def removeUnused(input: String, expected: String)(using munit.Location) =
    assertTransformsTo(input, expected) { tree =>
      Deadcode.remove(Set(mainSymbol), tree)
    }

  def removeTailResumptions(input: String, expected: String)(using munit.Location) =
    assertTransformsTo(input, expected) { tree => RemoveTailResumptions(tree) }

  def normalizeWith(policy: InliningPolicy)(input: String, expected: String)(using munit.Location) =
    assertTransformsTo(input, expected) { tree =>
      val anfed = BindSubexpressions.transform(tree)
      val normalized = Normalizer.normalize(Set(mainSymbol), anfed, policy)
      Deadcode.remove(mainSymbol, normalized)
    }

  def normalize(input: String, expected: String)(using munit.Location) =
    normalizeWith(Unique(threshold = 50))(input, expected)

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
        |   let ! y = (println: (String) => Unit @ {io})("hello")
        |   let z = 7
        |   return z:Int
        | }
        |""".stripMargin

    val expected =
      """ def main = { () =>
        |   let ! y = (println: (String) => Unit @ {io})("hello")
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
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    normalize(input, expected)
  }

  test("inline with argument"){
    val input =
      """ def foo = { (n: Int) => return n:Int }
        | def main = { () => (foo : (Int) => Int @ {})(42) }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    normalize(input, expected)
  }

  test("inline higher order function"){
    val input =
      """ def foo = { (n: Int) => return n:Int }
        | def hof = { (){f : (Int) => Int} =>
        |   (f : (Int) => Int @ {f})(1)
        | }
        | def main = { () =>
        |   (hof : (){f : (Int) => Int} => Int @ {})(){ (foo : (Int) => Int @ {}) }
        | }
        |""".stripMargin

    val expected =
      """ def main = { () => return 1 }
        |""".stripMargin

    normalize(input, expected)
  }

  test("fully inline higher order function"){
    val input =
      """ def foo = { (n: Int) => return n:Int }
        | def hof = { (){f : (Int) => Int} =>
        |   (f : (Int) => Int @ {f})(1)
        | }
        | def main = { () =>
        |   (hof : (){f : (Int) => Int} => Int @ {})(){ (foo : (Int) => Int @ {}) }
        | }
        |""".stripMargin

    val expected =
      """ def main = { () => return 1 }
        |""".stripMargin

    normalize(input, expected)
  }


  test("used once is inlined even when the threshold forbids it") {
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    val expected =
      """ def main = { () => return 42 }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = None))(input, expected)
  }

  test("used once is not inlined once it exceeds the once-limit") {
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0)))(input, input)
  }

  test("an object argument is known, so the callee is inlined") {
    val input =
      """ interface Foo { op: () => Int }
        | def main = { () => ({ (){f: Foo} => (f : Foo @ {f}).op : () => Int () })(){ new Foo { def op() = return 42 } } }
        |""".stripMargin

    val expected =
      """ interface Foo { op: () => Int }
        | def main = { () => def f = new Foo { def op() = return 42 } (f : Foo @ {}).op : () => Int () }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0)))(input, expected)
  }

  test("a block variable argument is not known, so the callee is kept") {
    val input =
      """ interface Foo { op: () => Int }
        | def main = { (){g: Foo} => ({ (){f: Foo} => (f : Foo @ {f}).op : () => Int () })(){ (g : Foo @ {g}) } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0)))(input, input)
  }

  test("a used-once block that installs a scope is inlined where no prompt encloses it") {
    val input =
      """ def foo = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = None))(input, expected)
  }

  test("the same block is kept when the call site is already under a prompt") {
    val input =
      """ def foo = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        | def main = { () => reset { (){q: Prompt[Int]} => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => resume (j : Resume[Int, Int] @ {j}) { (foo : () => Int @ {})() } } } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = None))(input, input)
  }

  test("a known argument discounts the call, so an over-budget callee is inlined") {
    val input =
      """ def foo = { (b: Bool) => if (b: Bool) { return 1 } else { return 2 } }
        | def main = { () => (foo : (Bool) => Int @ {})(true) }
        |""".stripMargin

    val expected =
      """ def main = { () => return 1 }
        |""".stripMargin

    normalizeWith(Default(threshold = 4, onceLimit = Some(0)))(input, expected)
  }

  test("an unknown argument earns no discount, so the same callee is kept") {
    val input =
      """ def foo = { (b: Bool) => if (b: Bool) { return 1 } else { return 2 } }
        | def main = { (x: Bool) => (foo : (Bool) => Int @ {})(x: Bool) }
        |""".stripMargin

    normalizeWith(Default(threshold = 4, onceLimit = Some(0)))(input, input)
  }

  test("an aborting shift in tail position of its prompt becomes what it aborts with") {
    val input =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => if (b: Bool) { return 1 } else { shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 } } } }
        |""".stripMargin

    val expected =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => if (b: Bool) { return 1 } else { return 2 } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a binder does not end tail position, so the abort behind one is still removed") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => let y = 7 shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => let y = 7 return 2 } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("an aborting shift consumed by a val is not in tail position") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 }; return x:Int } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a nested prompt stands between the abort and the prompt it names") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => reset { (){q: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 } } } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }
}
