package effekt.generator.js

import effekt.context.Context
import effekt.core.{DeclarationContext, Names, TestContext}
import effekt.cps
import kiama.parsing.{NoSuccess, Success}

class DefunctionalizationTests extends munit.FunSuite {

  private def parse(source: String): cps.ModuleDecl =
    cps.Parser.module(source, Names(Map.empty)) match {
      case Success(module, next) if next.atEnd => module
      case Success(_, next) =>
        fail(s"Trailing input at ${next.position}")
      case error: NoSuccess =>
        fail(s"Parse error at ${error.next.position}: ${error.message}")
    }

  private def representations(source: String): DefinitionPlanning.Plan = {
    val module = parse(source)
    assertEquals(module.escapes, cps.escapeAnalysis.escapes(module))
    TransformerCps.computePlan(module)
  }

  private def analyze(source: String): Defunctionalization.Plan =
    representations(source).defunctionalization

  private def translate(source: String): String = {
    val module = parse(source)
    val main = module.definitions.collectFirst {
      case cps.ToplevelDefinition.Def(id, _, _) if id.name.name == "main" => id
    }.getOrElse(fail("missing main definition"))
    given Context = new TestContext
    given DeclarationContext = new DeclarationContext(module.declarations, Nil)
    PrettyPrinter.format(TransformerCps.toJS(module, Nil, Set(main)).stmts).layout
  }

  private def dispatch(
    plan: Defunctionalization.Plan,
    entry: String
  ): Option[Defunctionalization.ContinuationDispatch] =
    plan.dispatches.values.find(_.entry.name.name == entry)

  private def definition(
    plan: DefinitionPlanning.Plan,
    name: String
  ): (effekt.core.Id, DefinitionPlanning.Kind) =
    plan.kinds.find(_._1.name.name == name).getOrElse(fail(s"missing definition $name"))

  private def continuationCase(
    plan: DefinitionPlanning.Plan,
    name: String
  ): Defunctionalization.ContinuationCase =
    plan.defunctionalization.cases.values
      .find(_.definition.name.name == name)
      .getOrElse(fail(
        s"missing continuation case $name; found " +
          plan.defunctionalization.cases.keys.map(_.name.name).mkString(", ")))

  test("a closed recursive continuation parameter becomes a dispatch") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def fibonacci(i, c) {
          if (true) {
            c(i)
          } else {
            def next(x) { fibonacci(x, c) }
            fibonacci(i, next)
          }
        }
        fibonacci(seed, done)
      }
    """)

    val result = dispatch(plan, "fibonacci").getOrElse(fail("missing dispatch"))
    assertEquals(result.boundary, false)
    assertEquals(result.cases.map(_.definition.name.name), Vector("done", "next"))
    assertEquals(result.cases.map(_.tag), Vector(0, 1))
  }

  test("representation planning removes demands caused only by a continuation closure") {
    val plan = representations("""
      def main(n, k) {
        def loop(i, next) {
          if (true) {
            next(i)
          } else {
            def after(value) { loop(value, next) }
            loop(i, after)
          }
        }
        loop(n, k)
      }
    """)

    // Syntactically, `after` closes over `loop`. Once `after` is represented
    // by a case at `loop`'s dispatcher, that occurrence is a jump to a
    // lexically available label rather than a stored function value.
    assert(definition(plan, "loop")._2.isSecondClass)
    assertEquals(
      continuationCase(plan, "after").captures.map(_.name.name),
      Vector("next"))
  }

  test("a preceding definition can become a label spanning the remainder") {
    val plan = representations("""
      def main(n, k) {
        def helper(value) { k(value) }
        def loop(i, next) {
          if (true) {
            next(i)
          } else {
            def after(value) { helper(value) }
            loop(i, after)
          }
        }
        loop(n, k)
      }
    """)

    // Initially `helper` appears to escape through `after`. Once `after` is a
    // continuation case, the binding of `helper` can be lowered to a label
    // whose scope contains the whole remainder, including `loop`'s dispatcher.
    assert(definition(plan, "helper")._2.isSecondClass)
    assertEquals(
      continuationCase(plan, "after").captures.map(_.name.name),
      Vector.empty)
  }

  test("dynamic values remain in the residual frame") {
    val plan = representations("""
      def main(n, k) {
        def loop(i, next) {
          if (true) {
            next(i)
          } else {
            def after(value) { loop(i, next) }
            loop(i, after)
          }
        }
        loop(n, k)
      }
    """)

    // `loop` denotes the same static label at the dispatcher, whereas `i` and
    // `next` vary from iteration to iteration and therefore remain fields.
    assertEquals(
      continuationCase(plan, "after").captures.map(_.name.name),
      Vector("i", "next"))
  }

  test("a stable definition crossing a function boundary remains first-class") {
    val plan = representations("""
      def main(seed, k) {
        def outer(n, c) {
          def inner(m, next) {
            if (true) {
              next(m)
            } else {
              def after(value) { outer(value, next) }
              inner(m, after)
            }
          }
          run ignored = println(inner);
          inner(n, c)
        }
        outer(seed, k)
      }
    """)

    // `outer` has one stable identity throughout `inner`, so its continuation
    // cases can reference it directly instead of storing it in every frame.
    // It cannot become a label, however, because `inner` is an independently
    // escaping JavaScript function. The omitted field therefore induces the
    // first-class representation constraint recorded by the joint plan.
    assert(!definition(plan, "inner")._2.isSecondClass)
    assert(!definition(plan, "outer")._2.isSecondClass)
    assert(plan.defunctionalization.firstClassRequirements.exists(
      _.name.name == "outer"))
    assertEquals(
      continuationCase(plan, "after").captures.map(_.name.name),
      Vector("next"))
  }

  test("case relocation contributes residual recursion") {
    val source = """
      def main(seed, k) {
        def generate(n, c) {
          def recur(i, next) {
            if (true) {
              next(i)
            } else {
              def after(value) { recur(value, next) }
              recur(i, after)
            }
          }
          recur(n, c)
        }
        def first(value) { generate(value, k) }
        generate(seed, first)
      }
    """
    val module = parse(source)
    val generate = module.uses.keys
      .find(_.name.name == "generate")
      .getOrElse(fail("missing definition generate"))
    assert(!module.uses.getOrElse(generate, Set.empty).contains(generate))

    val plan = TransformerCps.computePlan(module)
    val kind = definition(plan, "generate")._2
    assert(kind.isSecondClass)
    assert(kind.isRecursive)

    // `first` is moved into `recur`'s dispatcher, which itself lies in
    // `generate`'s body. Its second call to `generate` is therefore a
    // residual back-edge and must be emitted as a loop continuation.
    val javascript = translate(source)
    assert(javascript.contains("generate_0: while (true)"))
    assert(javascript.contains("continue generate_0"))
  }

  test("an unknown incoming continuation is represented by a boundary case") {
    val source = """
      def main(i, c) {
        def fibonacci(n, k) {
          if (true) {
            k(n)
          } else {
            def next(x) { fibonacci(x, k) }
            fibonacci(n, next)
          }
        }
        fibonacci(i, c)
      }
    """
    val plan = analyze(source)

    val result = dispatch(plan, "fibonacci").getOrElse(fail("missing dispatch"))
    assertEquals(result.boundary, true)
    assertEquals(result.cases.map(_.definition.name.name), Vector("next"))
    assertEquals(result.cases.map(_.tag), Vector(0))

    val javascript = translate(source)
    assert(javascript.contains("__tag: -1"))
    assert(javascript.contains("case -1:"))
    assert(javascript.contains("typeof"))
  }

  test("a boundary accepts both raw continuations and existing frames") {
    val source = """
      def main(i, c) {
        def done(x) { c(x) }
        def loop(n, k) {
          if (true) {
            k(n)
          } else {
            def next(x) { k(x) }
            loop(n, next)
          }
        }
        if (true) { loop(i, c) } else { loop(i, done) }
      }
    """

    assert(dispatch(analyze(source), "loop").nonEmpty)
    assert(translate(source).contains("typeof"))
  }

  test("a local labeled entry wraps its boundary after assigning parameters") {
    val javascript = translate("""
      def main(i, c) {
        def loop(n, k) {
          if (true) {
            k(n)
          } else {
            def next(x) { k(x) }
            loop(n, next)
          }
        }
        loop(i, c)
      }
    """)

    val boundary = javascript.indexOf("__tag: -1")
    val entryExit = javascript.lastIndexOf("break ", boundary)
    val body = javascript.indexOf(": while", boundary)
    assert(entryExit >= 0 && entryExit < boundary && boundary < body)
  }

  test("nested recursive loops share one overlapping continuation domain") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def outer(i, outerK) {
          if (true) {
            outerK(i)
          } else {
            def inner(j, innerK) {
              if (true) {
                outer(j, innerK)
              } else {
                def next(x) { innerK(x) }
                inner(j, next)
              }
            }
            inner(i, outerK)
          }
        }
        outer(seed, done)
      }
    """)

    val outer = dispatch(plan, "outer").getOrElse(fail("missing outer dispatch"))
    assertEquals(dispatch(plan, "inner"), None)
    assertEquals(outer.cases.map(_.definition.name.name).toSet, Set("done", "next"))
  }

  test("case calls close otherwise disjoint continuation domains") {
    val source = """
      def main(seed, k) {
        def doneA(x) { k(x) }
        def first(n, a) {
          def doneB(x) { a(x) }
          def second(m, b) {
            if (true) {
              def nextA(x) { b(x) }
              first(m, nextA)
            } else {
              def nextB(x) { b(x) }
              second(m, nextB)
            }
          }
          if (true) { a(n) } else { second(n, doneB) }
        }
        first(seed, doneA)
      }
    """
    val plan = analyze(source)

    val first = dispatch(plan, "first").getOrElse(fail("missing first dispatch"))
    assertEquals(first.cases.map(_.definition.name.name).toSet,
      Set("doneA", "doneB", "nextA", "nextB"))
    assert(translate(source).contains("switch"))
  }

  test("second-class definitions inside a case preserve label visibility") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def loop(n, c) {
          if (true) {
            def frame(x) {
              def helper(y) { c(y) }
              helper(x)
            }
            loop(n, frame)
          } else {
            c(n)
          }
        }
        loop(seed, done)
      }
    """)

    assert(dispatch(plan, "loop").nonEmpty)
  }

  test("effect bodies are not JavaScript function boundaries") {
    val source = """
      def main(seed, meta, outside) {
        def done(x) { outside(x) }
        def loop(n, c) {
          if (true) {
            c(n)
          } else {
            def frame(x) { loop(x, c) }
            loop(n, frame)
          }
        }
        reset(p, ks, k) {
          loop(seed, done)
        } @ meta, outside
      }
    """

    assert(dispatch(analyze(source), "loop").nonEmpty)

    val javascript = translate(source)
    assert(javascript.contains("const ["))
    assert(javascript.contains("] = RESET("))
    assert(javascript.contains("switch (cont_"))
    assert(!javascript.contains("RESET(("))
  }

  test("an application in a nested JavaScript function remains functional") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def loop(n, c) {
          if (true) {
            def frame(x) {
              def helper(y) { c(y) }
              run ignored = println(helper);
              helper(x)
            }
            loop(n, frame)
          } else {
            c(n)
          }
        }
        loop(seed, done)
      }
    """)

    assertEquals(dispatch(plan, "loop"), None)
  }

  test("an escaping continuation definition remains functional") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def loop(n, c) {
          if (true) {
            def frame(x) { c(x) }
            run ignored = println(frame);
            loop(n, frame)
          } else {
            c(n)
          }
        }
        loop(seed, done)
      }
    """)

    assertEquals(dispatch(plan, "loop"), None)
  }

  test("all applications in one dispatch have the same arity") {
    val plan = analyze("""
      def main(seed, k) {
        def done(x) { k(x) }
        def loop(n, c) {
          if (true) {
            c(n)
          } else {
            if (false) {
              c(n, n)
            } else {
              def frame(x) { loop(x, c) }
              loop(n, frame)
            }
          }
        }
        loop(seed, done)
      }
    """)

    assertEquals(dispatch(plan, "loop"), None)
  }
}
