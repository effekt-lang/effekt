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

  private def analyze(source: String): Defunctionalization.Plan = {
    val module = parse(source)
    val uses = module.uses.toMap
    val escaping = module.escapes
    assertEquals(escaping, cps.escapeAnalysis.escapes(module))
    Defunctionalization.analyze(
      module,
      id => uses.get(id).exists(_.contains(id)),
      id => uses.contains(id) && !escaping.contains(id))
  }

  private def translate(source: String): String = {
    val module = parse(source)
    given Context = new TestContext
    given DeclarationContext = new DeclarationContext(module.declarations, Nil)
    PrettyPrinter.format(TransformerCps.toJS(module, Nil).stmts).layout
  }

  private def dispatch(
    plan: Defunctionalization.Plan,
    entry: String
  ): Option[Defunctionalization.ContinuationDispatch] =
    plan.dispatches.values.find(_.entry.name.name == entry)

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
