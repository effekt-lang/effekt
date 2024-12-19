package effekt
package core

import effekt.core.Interpreter.{ InterpreterError, State }
import effekt.source.FeatureFlag
import effekt.symbols.QualifiedName
import effekt.symbols.given
import kiama.util.FileSource

class InterpreterTests extends munit.FunSuite {

  import effekt.context.{ Context, IOModuleDB }
  import effekt.util.AnsiColoredMessaging
  import kiama.util.{ Positions, StringSource }

  //  object driver extends effekt.Driver
  //
  //  def run(content: String): String =
  //    var options = Seq(
  //      "--Koutput", "string",
  //      "--backend", "js",
  //    )
  //    val configs = driver.createConfig(options)
  //    configs.verify()
  //
  //    val compiler = new TestFrontend
  //    compiler.compile(StringSource(content, "input.effekt"))(using context).map {
  //      case (_, decl) => decl
  //    }
  //    configs.stringEmitter.result()
  val positions = new Positions
  object ansiMessaging extends AnsiColoredMessaging
  object context extends Context(positions) with IOModuleDB {
    val messaging = ansiMessaging
    object testFrontend extends TestFrontend
    override lazy val compiler = testFrontend.asInstanceOf
  }

  def run(content: String) =
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)
    context.testFrontend.compile(StringSource(content, "input.effekt"))(using context).map {
      case (_, decl) => decl
    }

  def runFile(path: String) =
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)
    context.testFrontend.compile(FileSource(path))(using context).map {
      case (_, decl) => decl
    }


  val recursion =
      """def countdown(n: Int): Int =
        |  if (n == 0) 42
        |  else countdown(n - 1)
        |
        |def fib(n: Int): Int =
        |  if (n == 0) 1
        |  else if (n == 1) 1
        |  else fib(n - 2) + fib(n - 1)
        |
        |def main() = {
        |  println(fib(10))
        |}
        |""".stripMargin

  val dynamicDispatch = """def size[T](l: List[T]): Int =
      |  l match {
      |    case Nil() => 0
      |    case Cons(hd, tl) => 1 + size(tl)
      |  }
      |
      |def map[A, B](l: List[A]) { f: A => B }: List[B] =
      |  l match {
      |    case Nil() => Nil()
      |    case Cons(hd, tl) => Cons(f(hd), map(tl){f})
      |  }
      |
      |def main() = {
      |  println(size([1, 2, 3].map { x => x + 1 }))
      |}
      |""".stripMargin

  val eraseUnused =
    """def replicate(v: Int, n: Int, a: List[Int]): List[Int] =
      |  if (n == 0) {
      |    a
      |  } else {
      |    replicate(v, n - 1, Cons(v, a))
      |  }
      |
      |def useless(i: Int, n: Int, _: List[Int]): Int =
      |  if (i < n) {
      |    useless(i + 1, n, replicate(0, i, Nil()))
      |  } else {
      |    i
      |  }
      |
      |def run(n: Int) =
      |  useless(0, n, Nil())
      |
      |def main() = {
      |  println(run(10))
      |}
      |""".stripMargin

  val simpleObject =
    """interface Counter {
      |  def inc(): Unit
      |  def get(): Int
      |}
      |
      |def main() = {
      |  def c = new Counter {
      |    def inc() = println("tick")
      |    def get() = 0
      |  };
      |  c.inc()
      |  c.inc()
      |  c.inc()
      |}
      |
      |""".stripMargin

  val factorialAccumulator =
    """import examples/benchmarks/runner
      |
      |def factorial(a: Int, i: Int): Int =
      |  if (i == 0) {
      |    a
      |  } else {
      |    factorial((i * a).mod(1000000007), i - 1)
      |  }
      |
      |def run(n: Int): Int =
      |  factorial(1, n)
      |
      |def main() = benchmark(5){run}
      |
      |""".stripMargin

  val sort =
    """import list
      |
      |def main() = {
      |  // synchronized with doctest in `sortBy`
      |  println([1, 3, -1, 5].sortBy { (a, b) => a <= b })
      |}
      |""".stripMargin

  val mutableState =
    """def main() = {
      |  var x = 42;
      |  x = x + 1;
      |  println(x.show)
      |  [1, 2, 3].map { x => x + 1 }.foreach { x => println(x) };
      |
      |  region r {
      |    var x in r = 42;
      |    x = x + 1
      |    println(x)
      |  }
      |}
      |""".stripMargin

  val simpleException =
    """effect raise(): Unit
      |
      |def main() = {
      |  try {
      |    println("before");
      |    do raise()
      |    println("after")
      |  } with raise { println("caught") }
      |}
      |
      |""".stripMargin

  val triples =
    """import examples/benchmarks/runner
      |
      |record Triple(a: Int, b: Int, c: Int)
      |
      |interface Flip {
      |  def flip(): Bool
      |}
      |
      |interface Fail {
      |  def fail(): Nothing
      |}
      |
      |def choice(n: Int): Int / {Flip, Fail} = {
      |  if (n < 1) {
      |    do fail() match {}
      |  } else if (do flip()) {
      |    n
      |  } else {
      |    choice(n - 1)
      |  }
      |}
      |
      |def triple(n: Int, s: Int): Triple / {Flip, Fail} = {
      |  val i = choice(n)
      |  val j = choice(i - 1)
      |  val k = choice(j - 1)
      |  if (i + j + k == s) {
      |    Triple(i, j, k)
      |  } else {
      |    do fail() match {}
      |  }
      |}
      |
      |def hash(triple: Triple): Int = triple match {
      |  case Triple(a, b, c) => mod(((53 * a) + 2809 * b + 148877 * c), 1000000007)
      |}
      |
      |def run(n: Int) =
      |  try {
      |    hash(triple(n, n))
      |  } with Flip {
      |    def flip() = mod(resume(true) + resume(false), 1000000007)
      |  } with Fail {
      |    def fail() = 0
      |  }
      |
      |def main() = benchmark(10){run}
      |
      |
      |""".stripMargin

  // doesn't work: product_early (since it SO due to run run run)

  def runTest(file: String): Unit =

    val Some(main, mod, decl) = runFile(file): @unchecked

    val gced = Deadcode.remove(main, decl)

    val inlined = Inline.full(Set(main), gced, 40)

    try {
      object data extends Counting {
        override def step(state: Interpreter.State) = state match {
          case State.Done(result) => ???
          case State.Step(stmt, env, stack) =>
            //println(Interpreter.show(stack))
        }
      }
      Interpreter(data).run(main, inlined)

      data.report()

    } catch {
      case err: InterpreterError =>
        err match {
          case InterpreterError.NotFound(id) => println(s"Not found: ${util.show(id)}")
          case InterpreterError.NotAnExternFunction(id) => err.printStackTrace()
          case InterpreterError.MissingBuiltin(name) => println(s"Missing ${name}")
          case InterpreterError.RuntimeTypeError(msg) => err.printStackTrace()
          case InterpreterError.NonExhaustive(missingCase) => err.printStackTrace()
          case InterpreterError.Hole() => err.printStackTrace()
          case InterpreterError.NoMain() => err.printStackTrace()
        }
    }

  // TODO allocate arrays and ref on a custom heap that could be inspected and visualized

  runTest("examples/benchmarks/are_we_fast_yet/bounce.effekt")
  runTest("examples/benchmarks/are_we_fast_yet/list_tail.effekt")
  runTest("examples/benchmarks/are_we_fast_yet/mandelbrot.effekt")
  runTest("examples/benchmarks/are_we_fast_yet/nbody.effekt")

  // global is missing
  //runTest("examples/benchmarks/are_we_fast_yet/permute.effekt")
  //runTest("examples/benchmarks/are_we_fast_yet/storage.effekt")

  runTest("examples/benchmarks/are_we_fast_yet/queens.effekt")
  runTest("examples/benchmarks/are_we_fast_yet/sieve.effekt")
  runTest("examples/benchmarks/are_we_fast_yet/towers.effekt")
}

class TestFrontend extends Compiler[(Id, symbols.Module, core.ModuleDecl)] {


  import effekt.PhaseResult.CoreTransformed
  import effekt.context.Context
  import kiama.output.PrettyPrinterTypes.Document
  import kiama.util.Source


  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".class"

  override def supportedFeatureFlags: List[String] = List("jvm")

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = None

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = None

  override def compile(source: Source)(using C: Context): Option[(Map[String, String], (Id, symbols.Module, core.ModuleDecl))] =
    Optimized.run(source).map { res => (Map.empty, res) }


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => CPS => JS
  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      (mainSymbol, mod, core)
  }
}
