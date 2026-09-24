package effekt
package cps

import effekt.core.{ Id, Names }
import kiama.parsing.{ NoSuccess, Success }
import munit.Location

class FlowAnalysisTests extends munit.FunSuite {

  private val names = Map(
    "main" -> Id("main", -1),
    "loop" -> Id("loop", -20),
    "add" -> Id("add", -2),
    "sub" -> Id("sub", -3),
    "eq" -> Id("eq", -4)
  )

  private def parse(input: String)(using Location): ModuleDecl =
    Parser.module(input, Names(names)) match {
      case Success(result, next) if next.atEnd => result
      case Success(_, next) => fail(s"trailing input: ${next.source.toString.substring(next.offset)}")
      case failure: NoSuccess => fail(s"parse failed: ${failure.message}")
    }

  private val loopProgram = """
    def loop(i, acc, ks, k) {
      run done = eq(i, 0);
      if (done) {
        k(acc, ks)
      } else {
        run i2 = sub(i, 1);
        run acc2 = add(acc, i);
        loop(i2, acc2, ks, k)
      }
    }
    def main(ks, k) {
      loop(10, 0, ks, k)
    }
  """

  private def statements(module: ModuleDecl): List[Stmt] = {
    val result = scala.collection.mutable.ListBuffer.empty[Stmt]

    def visit(statement: Stmt): Unit = {
      result += statement
      statement match {
        case Stmt.Def(_, _, body, rest) => visit(body); visit(rest)
        case Stmt.New(_, _, operations, rest) =>
          operations.foreach(operation => visit(operation.body)); visit(rest)
        case Stmt.Let(_, _, rest) => visit(rest)
        case Stmt.Call(_, _, ReturnPoint.Bind(_, _, _, rest)) => visit(rest)
        case Stmt.Call(_, _, ReturnPoint.Direct(_, rest)) => visit(rest)
        case Stmt.Call(_, _, ReturnPoint.Tail(_, _)) => ()
        case Stmt.Run(_, _, _, _, rest) => visit(rest)
        case Stmt.If(_, thn, els) => visit(thn); visit(els)
        case Stmt.Match(_, clauses, default) =>
          clauses.foreach { case (_, clause) => visit(clause.body) }
          default.foreach(visit)
        case Stmt.Region(_, _, rest) => visit(rest)
        case Stmt.Alloc(_, _, _, rest) => visit(rest)
        case Stmt.Var(_, _, _, rest) => visit(rest)
        case Stmt.Dealloc(_, rest) => visit(rest)
        case Stmt.Get(_, _, rest) => visit(rest)
        case Stmt.Put(_, _, rest) => visit(rest)
        case Stmt.Reset(_, _, _, body, _, _) => visit(body)
        case Stmt.Shift(_, _, _, _, body, _, _) => visit(body)
        case Stmt.Resume(_, _, _, body, _, _) => visit(body)
        case Stmt.Call(_, _, ReturnPoint.Jump) | _: Stmt.Return | _: Stmt.Hole => ()
      }
    }

    module.definitions.foreach {
      case ToplevelDefinition.Def(_, _, body) => visit(body)
      case ToplevelDefinition.Val(_, _, _, binding) => visit(binding)
    }
    result.toList
  }

  private def named(id: Id, name: String): Boolean = id.name.name == name

  test("value flow resolves the recursive call to loop") {
    val module = parse(loopProgram)
    val pointsTo = new FlowAnalysis(module)

    val call = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if id == names("loop") => jump
    }.getOrElse(fail("no recursive call found"))
    assertEquals(pointsTo.targetsAt(call), Set(names("loop")))
  }

  test("closures retain their lexical environment") {
    val module = parse("""
      def main(ks, k) {
        def target(x, ks1, k1) {
          k1(x, ks1)
        }
        def create(f, x, ks2, k2) {
          def wrapper(y, ks3, k3) {
            f(y, ks3, k3)
          }
          wrapper(x, ks2, k2)
        }
        create(target, 0, ks, k)
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val indirect = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if named(id, "f") => jump
    }.getOrElse(fail("no captured call found"))

    assert(pointsTo.targetsAt(indirect).exists(named(_, "target")))
  }

  test("states are reconsidered when a value read from the store grows") {
    val module = parse("""
      def main(flag, ks, k) {
        def target(x, ks1, k1) {
          k1(x, ks1)
        }
        def apply(f, x, ks2, k2) {
          f(x, ks2, k2)
        }
        def later(ks3, k3) {
          apply(target, 0, ks3, k3)
        }
        if (flag) {
          apply(0, 0, ks, k)
        } else {
          later(ks, k)
        }
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val indirect = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if named(id, "f") => jump
    }.getOrElse(fail("no indirect call found"))

    assert(pointsTo.targetsAt(indirect).exists(named(_, "target")))
  }

  test("operations retain their implementing closures") {
    val module = parse("""
      def main(ks, k) {
        new object : Handler {
          def operation(ks1, k1) = {
            k1(1, ks1)
          }
        }
        object.operation(ks, k)
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val invocation = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Method(_, _), _, ReturnPoint.Jump) => jump
    }.getOrElse(fail("no invocation found"))

    assert(pointsTo.targetsAt(invocation).exists(named(_, "operation")))
    assert(pointsTo.closedAt(invocation))
  }

  test("operations with the same property retain distinct implementations") {
    val module = parse("""
      def main(flag, ks, k) {
        def firstTarget(ks1, k1) {
          k1(1, ks1)
        }
        def secondTarget(ks2, k2) {
          k2(2, ks2)
        }
        new first : Handler {
          def operation(ks3, k3) = {
            firstTarget(ks3, k3)
          }
        }
        new second : Handler {
          def operation(ks4, k4) = {
            secondTarget(ks4, k4)
          }
        }
        if (flag) {
          first.operation(ks, k)
        } else {
          second.operation(ks, k)
        }
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val applications = statements(module).collect {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if
          named(id, "firstTarget") || named(id, "secondTarget") => jump
    }

    assertEquals(applications.size, 2)
    applications.foreach { application =>
      val Stmt.Call(Callee.Function(target), _, ReturnPoint.Jump) = application: @unchecked
      assert(pointsTo.targetsAt(application).contains(target))
    }
  }

  test("closures flow through constructor fields and matches") {
    val module = parse("""
      type BoxData { Box(value: Int) }

      def main(ks, k) {
        def target(x, ks1, k1) {
          k1(x, ks1)
        }
        let box = make Box(target);
        box match {
          case Box(f) => f(0, ks, k)
        }
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val application = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if named(id, "f") => jump
    }.getOrElse(fail("no application of matched field found"))

    assert(pointsTo.targetsAt(application).exists(named(_, "target")))
  }

  test("a compositional call reifies and enters its remainder") {
    val module = parse("""
      def main(ks, k) {
        def returned(x, ks1, k1) {
          k1(x, ks1)
        }
        def choose(ks2, k2) {
          k2(returned, ks2)
        }
        let result | returnedKs = choose!(ks, return);
        result(0, returnedKs, k)
      }
    """)
    val pointsTo = new FlowAnalysis(module, reifyCalls = true)
    val application = statements(module).collectFirst {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if named(id, "result") => jump
    }.getOrElse(fail("no application of returned value found"))

    assert(pointsTo.targetsAt(application).exists(named(_, "returned")))
    assert(pointsTo.closedAt(application))
  }

  test("opaque control makes crossing continuations escape") {
    val module = parse("""
      def main(ks, k) {
        def continuation(value, returnedKs) {
          k(value, returnedKs)
        }
        reset(prompt, innerKs, innerK) {
          innerK(0, innerKs)
        } @ ks, continuation
      }
    """)
    val pointsTo = new FlowAnalysis(module)

    assert(pointsTo.escapedFunctions.exists(named(_, "continuation")))
  }

  test("structurally equal statements remain distinct program points") {
    val module = parse("""
      def main(flag, ks, k) {
        def target(value, ks1, k1) {
          k1(value, ks1)
        }
        if (flag) {
          target(0, ks, k)
        } else {
          target(0, ks, k)
        }
      }
    """)
    val pointsTo = new FlowAnalysis(module)
    val calls = statements(module).collect {
      case jump @ Stmt.Call(Callee.Function(id), _, ReturnPoint.Jump) if named(id, "target") => jump
    }

    assertEquals(calls.size, 2)
    calls.foreach { call =>
      assert(pointsTo.targetsAt(call).exists(named(_, "target")))
    }
  }

  test("pattern parameters have monovariant binding addresses") {
    val module = parse("""
      type BoxData { Box(value: Int) }

      def main(flag, ks, k) {
        let first = make Box(1);
        let second = make Box(2);
        def visit(box, ks1, k1) {
          box match {
            case Box(value) =>
              def capture(ignored, ks2, k2) {
                k2(value, ks2)
              }
              capture(value, ks1, k1)
          }
        }
        if (flag) {
          visit(first, ks, k)
        } else {
          visit(second, ks, k)
        }
      }
    """)
    val analysis = new FlowAnalysis(module)
    val capture = statements(module).collectFirst {
      case Stmt.Def(id, _, _, _) if named(id, "capture") => id
    }.getOrElse(fail("no capture definition found"))
    val closures = analysis.valueAt(analysis.Address.Binding(capture)).values.collect {
      case closure: analysis.Value.Closure => closure
    }

    assertEquals(closures.size, 1)
  }

  test("compositional recursion changes its continuation but not its meta-continuation") {
    val module = parse("""
      type ListData { Nil() Cons(head: Int, tail: ListData) }

      def main(n, ks, k) {
        def produce(i, ks1, k1) {
          run done = eq(i, 0);
          if (done) {
            k1(make Nil(), ks1)
          } else {
            run previous = sub(i, 1);
            let tail = produce!(previous, ks1, return);
            k1(make Cons(i, tail), ks1)
          }
        }
        produce(n, ks, k)
      }
    """)
    val static = GuardedEquality.analyzeRecursion(module).staticParameters
    val produce = static.keys.find(named(_, "produce")).getOrElse(fail("no produce"))

    assertEquals(static(produce), Vector(false, true, false))
  }

  test("destructuring a recursive parameter changes it") {
    val module = parse("""
      type TreeData { Leaf(value: Int) Node(left: TreeData, right: TreeData) }

      def main(tree, ks, k) {
        def lookup(current, ks1, k1) {
          current match {
            case Leaf(value) => k1(value, ks1)
            case Node(left, right) => lookup(left, ks1, k1)
          }
        }
        lookup(tree, ks, k)
      }
    """)
    val static = GuardedEquality.analyzeRecursion(module).staticParameters
    val lookup = static.keys.find(named(_, "lookup")).getOrElse(fail("no lookup"))

    assertEquals(static(lookup), Vector(false, true, true))
  }

}
