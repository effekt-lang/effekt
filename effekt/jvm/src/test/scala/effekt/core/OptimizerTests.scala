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
    assertTransformsTo(input, expected) { tree => StaticResumptions(tree) }


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

    normalizeWith(Default(threshold = 0, onceLimit = None, carryingLimit = 0))(input, expected)
  }

  test("used once is not inlined once it exceeds the once-limit") {
    val input =
      """ def foo = { () => return 42 }
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0), carryingLimit = 0))(input, input)
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

    normalizeWith(Default(threshold = 0, onceLimit = Some(0), carryingLimit = 0))(input, expected)
  }

  test("a block variable argument is not known, so the callee is kept") {
    val input =
      """ interface Foo { op: () => Int }
        | def main = { (){g: Foo} => ({ (){f: Foo} => (f : Foo @ {f}).op : () => Int () })(){ (g : Foo @ {g}) } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0), carryingLimit = 0))(input, input)
  }

  test("a used-once block that installs a scope is inlined where no prompt encloses it") {
    val input =
      """ def foo = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        | def main = { () => (foo : () => Int @ {})() }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = None, carryingLimit = 0))(input, expected)
  }

  test("the same block is kept when the call site is already under a prompt") {
    val input =
      """ def foo = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } }
        | def main = { () => reset { (){q: Prompt[Int]} => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => resume (j : Resume[Int, Int] @ {j}) { (foo : () => Int @ {})() } } } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = None, carryingLimit = 0))(input, input)
  }

  test("a known argument discounts the call, so an over-budget callee is inlined") {
    val input =
      """ def foo = { (b: Bool) => if (b: Bool) { return 1 } else { return 2 } }
        | def main = { () => (foo : (Bool) => Int @ {})(true) }
        |""".stripMargin

    val expected =
      """ def main = { () => return 1 }
        |""".stripMargin

    normalizeWith(Default(threshold = 4, onceLimit = Some(0), carryingLimit = 0))(input, expected)
  }

  test("an unknown argument earns no discount, so the same callee is kept") {
    val input =
      """ def foo = { (b: Bool) => if (b: Bool) { return 1 } else { return 2 } }
        | def main = { (x: Bool) => (foo : (Bool) => Int @ {})(x: Bool) }
        |""".stripMargin

    normalizeWith(Default(threshold = 4, onceLimit = Some(0), carryingLimit = 0))(input, input)
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

  test("an aborting shift consumed by a val moves its frame into a join point it never calls") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 }; return x:Int } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => val x = (s : () => Int @ {sc})(); return x:Int } } return 2 } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a nested prompt stands between the abort and the prompt it names") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => reset { (){q: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 2 } } } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a resumption that resumes with something naming it again resumes through the join point twice") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => resume (k : Resume[Int, Int] @ {k}) { resume (k : Resume[Int, Int] @ {k}) { return 1 } } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => (s : () => Int @ {sc})() } } (j : (){s: () => Int} => Int @ {})(){ () => (j : (){s: () => Int} => Int @ {})(){ () => return 1 } } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a resumption from inside an abort to an outer prompt goes through the join point too") {
    val input =
      """ def main = { () => reset { (){o: Prompt[Int]} => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => shift (o : Prompt[Int] @ {o}) { {i: Resume[Nothing, Int]} => resume (k : Resume[Int, Int] @ {k}) { return 1 } } } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){o: Prompt[Int]} => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => (s : () => Int @ {sc})() } } shift (o : Prompt[Int] @ {o}) { {i: Resume[Nothing, Int]} => (j : (){s: () => Int} => Int @ {})(){ () => return 1 } } } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a tail resumption crosses a variable its body cannot observe") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => var r @ c = 1; resume (k : Resume[Int, Int] @ {k}) { return 2 } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => var r @ c = 1; return 2 } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a tail resumption whose body observes the handler's variable keeps it below the fresh delimiter") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => var r @ c = 1; resume (k : Resume[Int, Int] @ {k}) { get y : Int = ! r @ c; return y:Int } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => (s : () => Int @ {sc})() } } var r @ c = 1; (j : (){s: () => Int} => Int @ {})(){ () => get y : Int = ! r @ c; return y:Int } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a resumption reached through a block that is only ever tail-called") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => def w = { () => resume (k : Resume[Int, Int] @ {k}) { return 1 } } (w : () => Int @ {})() } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def w = { () => return 1 } (w : () => Int @ {})() } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("an abort behind a joinpoint in a tail-called worker is still in tail position") {
    val input =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => def go = { (i: Int) => def k = { (x: Int) => (go : (Int) => Int @ {})(x: Int) } if (b: Bool) { shift (p : Prompt[Int] @ {p}) { {r: Resume[Int, Int]} => return 2 } } else { (k : (Int) => Int @ {})(i: Int) } } (go : (Int) => Int @ {})(1) } }
        |""".stripMargin

    val expected =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => def go = { (i: Int) => def k = { (x: Int) => (go : (Int) => Int @ {})(x: Int) } if (b: Bool) { return 2 } else { (k : (Int) => Int @ {})(i: Int) } } (go : (Int) => Int @ {})(1) } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a shift at its delimiter is erased although one of its exits returns") {
    val input =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => if (b: Bool) { resume (k : Resume[Int, Int] @ {k}) { return 1 } } else { return 2 } } } }
        |""".stripMargin

    val expected =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => if (b: Bool) { return 1 } else { return 2 } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("the same shift away from its delimiter resumes through a join point carrying its frame") {
    val input =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => if (b: Bool) { resume (k : Resume[Int, Int] @ {k}) { return 1 } } else { return 2 } }; return x:Int } }
        |""".stripMargin

    val expected =
      """ def main = { (b: Bool) => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => val x = (s : () => Int @ {sc})(); return x:Int } } if (b: Bool) { (j : (){s: () => Int} => Int @ {})(){ () => return 1 } } else { return 2 } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a resumption handed to a tail-called block is not a tail resumption") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => def w = { (){r: Resume[Int, Int]} => return 1 } (w : (){r: Resume[Int, Int]} => Int @ {})(){ (k : Resume[Int, Int] @ {k}) } } } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a resumption under a nested prompt that the resumed statement shifts to keeps the prompts in order") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => reset { (){q: Prompt[Int]} => resume (k : Resume[Int, Int] @ {k}) { shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 0 } } } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => (s : () => Int @ {sc})() } } reset { (){r: Prompt[Int]} => (j : (){s: () => Int} => Int @ {})(){ () => shift (r : Prompt[Int] @ {r}) { {i: Resume[Int, Int]} => return 0 } } } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a resumption under a nested prompt nobody resumes into is erased at the delimiter") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => reset { (){q: Prompt[Int]} => def f = { () => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 0 } } val a = (f : () => Int @ {q})(); resume (k : Resume[Int, Int] @ {k}) { return a:Int } } } } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => reset { (){q: Prompt[Int]} => def f = { () => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 0 } } val a = (f : () => Int @ {q})(); return a:Int } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("the same nested prompt away from the delimiter: the frame joins, the inner abort becomes the answer") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => reset { (){q: Prompt[Int]} => def f = { () => shift (q : Prompt[Int] @ {q}) { {j: Resume[Int, Int]} => return 0 } } val a = (f : () => Int @ {q})(); resume (k : Resume[Int, Int] @ {k}) { return a:Int } } }; return x:Int } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => val x = (s : () => Int @ {sc})(); return x:Int } } reset { (){r: Prompt[Int]} => def f = { () => shift (r : Prompt[Int] @ {r}) { {i: Resume[Int, Int]} => return 0 } } val a = (f : () => Int @ {r})(); (j : (){s: () => Int} => Int @ {})(){ () => return a:Int } } } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a block that closed over the prompt must not be called in the rebuilt frames either") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => def f = { () => shift (p : Prompt[Int] @ {p}) { {i: Resume[Int, Int]} => return 0 } } val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => val a = resume (k : Resume[Int, Int] @ {k}) { return 1 }; return 7 }; val z = (f : () => Int @ {p})(); return z:Int } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a var between the delimiter and the shift cannot be copied, so a resumption that is not tail is kept") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => var r @ c = 1; val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => val a = resume (k : Resume[Int, Int] @ {k}) { return 1 }; return a:Int }; return x:Int } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a block that closed over the prompt must not run inside the join point, so the shift is kept") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => def f = { () => shift (p : Prompt[Int] @ {p}) { {i: Resume[Int, Int]} => return 0 } } val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => val a = resume (k : Resume[Int, Int] @ {k}) { (f : () => Int @ {p})() }; return a:Int }; return x:Int } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("the frames in a join point shift to their own fresh prompt") {
    val input =
      """ def main = { () => reset { (){p: Prompt[Int]} => val x = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => val a = resume (k : Resume[Int, Int] @ {k}) { return 1 }; return a:Int }; val y = shift (p : Prompt[Int] @ {p}) { {k2: Resume[Int, Int]} => val b = resume (k2 : Resume[Int, Int] @ {k2}) { return 2 }; return b:Int }; return y:Int } }
        |""".stripMargin

    val expected =
      """ def main = { () => reset { (){p: Prompt[Int]} => def j = { (){s @ sc: () => Int} => reset { (){q: Prompt[Int]} => val x = (s : () => Int @ {sc})(); def j2 = { (){s2 @ sc2: () => Int} => reset { (){q2: Prompt[Int]} => val y = (s2 : () => Int @ {sc2})(); return y:Int } } val b = (j2 : (){s2: () => Int} => Int @ {})(){ () => return 2 }; return b:Int } } val a = (j : (){s: () => Int} => Int @ {})(){ () => return 1 }; return a:Int } }
        |""".stripMargin

    removeTailResumptions(input, expected)
  }

  test("a block that closed over the prompt through its capture must not run inside the join point") {
    val input =
      """ def main = { () => reset { (){p @ pc: Prompt[Int]} => def f = { () => shift (p : Prompt[Int] @ {pc}) { {i: Resume[Int, Int]} => return 0 } } val x = shift (p : Prompt[Int] @ {pc}) { {k: Resume[Int, Int]} => val a = resume (k : Resume[Int, Int] @ {k}) { (f : () => Int @ {pc})() }; return 7 }; return x:Int } }
        |""".stripMargin

    removeTailResumptions(input, input)
  }

  test("a call carrying a capability to a prompt we are inside of is inlined past the normal budget") {
    val input =
      """ interface Exc { raise: () => Int }
        | def foo = { (){e: Exc} => (e : Exc @ {e}).raise : () => Int () }
        | def main = { () => reset { (){p: Prompt[Int]} => def e = new Exc { def raise() = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 1 } } (foo : (){e : Exc} => Int @ {})(){ (e : Exc @ {e}) } } }
        |""".stripMargin

    // `foo` is inlined although it is over the "normal" budget (but inside the carryingLimit)
    val expected =
      """ interface Exc { raise: () => Int }
        | def main = { () => reset { (){p: Prompt[Int]} => def e = new Exc { def raise() = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 1 } } (e : Exc @ {e}).raise : () => Int () } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0), carryingLimit = 100))(input, expected)
  }

  test("a call carrying a capability to a prompt we are inside of is kept when the carrying budget does not reach it") {
    val input =
      """ interface Exc { raise: () => Int }
        | def foo = { (){e: Exc} => (e : Exc @ {e}).raise : () => Int () }
        | def main = { () => reset { (){p: Prompt[Int]} => def e = new Exc { def raise() = shift (p : Prompt[Int] @ {p}) { {k: Resume[Int, Int]} => return 1 } } (foo : (){e : Exc} => Int @ {})(){ (e : Exc @ {e}) } } }
        |""".stripMargin

    normalizeWith(Default(threshold = 0, onceLimit = Some(0), carryingLimit = 0))(input, input)
  }
}
