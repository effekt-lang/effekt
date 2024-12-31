package effekt
package core
package interpreter

import effekt.symbols.QualifiedName
import effekt.symbols.given

class InterpreterTests extends munit.FunSuite {

  import effekt.context.{ Context, IOModuleDB }
  import effekt.util.PlainMessaging
  import effekt.generator.ir.IR
  import kiama.util.{ Positions, StringSource, FileSource }


  val positions = new Positions
  object plainMessaging extends PlainMessaging
  object context extends Context(positions) with IOModuleDB {
    val messaging = plainMessaging
    object frontend extends IR
    override lazy val compiler = frontend.asInstanceOf
  }

  def compileString(content: String): (Id, symbols.Module, ModuleDecl) =
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)
    context.frontend.compile(StringSource(content, "input.effekt"))(using context).map {
      case (_, decl) => decl
    }.getOrElse {
      val errors = plainMessaging.formatMessages(context.messaging.buffer)
      sys error errors
    }

  def compileFile(path: String): (Id, symbols.Module, ModuleDecl) =
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)

    context.frontend.compile(FileSource(path))(using context).map {
      case (_, decl) => decl
    }.getOrElse {
      val errors = plainMessaging.formatMessages(context.messaging.buffer)
      sys error s"Cannot compile ${path}\n\n${errors}"
    }

  class OutputCapturingRuntime extends Runtime {

    import java.io.{ByteArrayOutputStream, PrintStream}

    private val outputStream = new ByteArrayOutputStream()
    val out = new PrintStream(outputStream)

    def output(): String = {
      out.flush()
      outputStream.toString
    }
  }

  def runString(contents: String): String =
    val (main, mod, decl) = compileString(contents)
    object runtime extends OutputCapturingRuntime
    Interpreter(NoInstrumentation, runtime).run(main, decl)
    runtime.output()

  def runFile(file: String): String =
    val (main, mod, decl) = compileFile(file)
    object runtime extends OutputCapturingRuntime
    Interpreter(NoInstrumentation, runtime).run(main, decl)
    runtime.output()

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
        |  println(fib(10).show)
        |}
        |""".stripMargin

  assertEquals(runString(recursion), "89\n")

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

  assertEquals(runString(dynamicDispatch), "3\n")

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

  // TODO allocate arrays and ref on a custom heap that could be inspected and visualized
  //
  //  runTest("examples/benchmarks/are_we_fast_yet/bounce.effekt")
  //  runTest("examples/benchmarks/are_we_fast_yet/list_tail.effekt")
  //  runTest("examples/benchmarks/are_we_fast_yet/mandelbrot.effekt")
  //  runTest("examples/benchmarks/are_we_fast_yet/nbody.effekt")
  //
  //  // global is missing
  //  //runTest("examples/benchmarks/are_we_fast_yet/permute.effekt")
  //  //runTest("examples/benchmarks/are_we_fast_yet/storage.effekt")
  //
  //  runTest("examples/benchmarks/are_we_fast_yet/queens.effekt")
  //  runTest("examples/benchmarks/are_we_fast_yet/sieve.effekt")
  //  runTest("examples/benchmarks/are_we_fast_yet/towers.effekt")
  //
  //  runTest("examples/benchmarks/duality_of_compilation/fibonacci_recursive.effekt")
}
