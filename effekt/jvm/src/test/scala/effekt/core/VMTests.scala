package effekt
package core

import core.vm.*

class VMTests extends munit.FunSuite {

  import effekt.context.{ Context, IOModuleDB }
  import effekt.util.PlainMessaging
  import effekt.generator.vm.VM
  import kiama.util.{ Positions, StringSource, FileSource }


  val positions = new Positions
  object plainMessaging extends PlainMessaging
  object context extends Context(positions) with IOModuleDB {
    val messaging = plainMessaging
    object frontend extends generator.vm.VM
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
    """def fib(n: Int): Int =
      |  if (n == 0) 1
      |  else if (n == 1) 1
      |  else fib(n - 2) + fib(n - 1)
      |
      |def main() = {
      |  println(fib(10).show)
      |}
      |""".stripMargin


  test ("simple recursion") {
    assertEquals(runString(recursion), "89\n")
  }

  val dynamicDispatch =
    """def size[T](l: List[T]): Int =
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

  test ("dynamic dispatch") {
    assertEquals(runString(dynamicDispatch), "3\n")
  }

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

  test ("simple object") {
    assertEquals(runString(simpleObject), "tick\ntick\ntick\n")
  }

  val mutableState =
    """def main() = {
      |  var x = 42;
      |  x = x + 1;
      |  println(x.show)
      |
      |  region r {
      |    var x in r = 10;
      |    x = x + 1
      |    println(x.show)
      |  }
      |}
      |""".stripMargin

  test ("mutable state") {
    assertEquals(runString(mutableState), "43\n11\n")
  }

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

  test ("simple exception") {
    assertEquals(runString(simpleException), "before\ncaught\n")
  }

  val sorting =
    """import list
      |
      |def main() = {
      |  // synchronized with doctest in `sortBy`
      |  println([1, 3, -1, 5].sortBy { (a, b) => a <= b })
      |}
      |""".stripMargin

  test ("sorting (integration)") {
    assertEquals(runString(sorting), "Cons(-1, Cons(1, Cons(3, Cons(5, Nil()))))\n")
  }


  import java.io.File
  import sbt.io.*
  import sbt.io.syntax.*

  def examplesDir = new File("examples")

  val testFiles: Seq[File] = Seq(
    examplesDir / "benchmarks" / "are_we_fast_yet" / "bounce.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "list_tail.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "mandelbrot.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "nbody.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "queens.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "sieve.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "towers.effekt",

    examplesDir / "benchmarks" / "duality_of_compilation" / "erase_unused.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "factorial_accumulator.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "fibonacci_recursive.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "iterate_increment.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "lookup_tree.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "match_options.effekt",
    examplesDir / "benchmarks" / "duality_of_compilation" / "sum_range.effekt",

    examplesDir / "benchmarks" / "effect_handlers_bench" / "countdown.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "iterator.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "nqueens.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "parsing_dollars.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "product_early.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "resume_nontail.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "tree_explore.effekt",
    examplesDir / "benchmarks" / "effect_handlers_bench" / "triples.effekt",
  )

  val notWorking: Seq[File] = Seq(
    // global is missing
    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt",
    examplesDir / "benchmarks" / "are_we_fast_yet" / "storage.effekt",
  )

  def runTest(f: File): Unit =
    val path = f.getPath
    test(path) {
      try {
        val result = runFile(path)
        val expected = expectedResultFor(f).getOrElse { s"Missing checkfile for ${path}"}
        assertNoDiff(result, expected)
      } catch {
        case i: VMError => fail(i.getMessage, i)
      }
    }

  testFiles.foreach(runTest)


}
