package effekt
package core

import core.vm.*
import effekt.util.MarkdownSource

class VMTests extends munit.FunSuite {

  import effekt.context.{ Context, IOModuleDB }
  import effekt.util.PlainMessaging
  import effekt.generator.vm.VM
  import kiama.util.{StringSource, FileSource }


  object plainMessaging extends PlainMessaging
  object context extends Context with IOModuleDB {
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

    val source = if path.endsWith(".effekt.md") then
      MarkdownSource(FileSource(path))
    else
      FileSource(path)

    context.frontend.compile(source)(using context).map {
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

  def runCounting(main: Id, decl: ModuleDecl): (String, Summary) =
    object runtime extends OutputCapturingRuntime
    object counting extends Counting
    Interpreter(counting, runtime).run(main, decl)
    (runtime.output(), Summary(
      staticDispatches = counting.staticDispatches,
      dynamicDispatches = counting.dynamicDispatches,
      patternMatches = counting.patternMatches,
      branches = counting.branches,
      pushedFrames = counting.pushedFrames,
      poppedFrames = counting.poppedFrames,
      allocations = counting.allocations,
      closures = counting.closures,
      variableReads = counting.variableReads,
      variableWrites = counting.variableWrites,
      resets = counting.resets,
      shifts = counting.shifts,
      resumes = counting.resumes
    ))

  def runString(contents: String): (String, Summary) =
    val (main, mod, decl) = compileString(contents)
    runCounting(main, decl)

  def runFile(file: String): (String, Summary) =
    val (main, mod, decl) = compileFile(file)
    runCounting(main, decl)


  case class Summary(
    staticDispatches: Int,
    dynamicDispatches: Int,
    patternMatches: Int,
    branches: Int,
    pushedFrames: Int,
    poppedFrames: Int,
    allocations: Int,
    closures: Int,
    variableReads: Int,
    variableWrites: Int,
    resets: Int,
    shifts: Int,
    resumes: Int
  )

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
    assertEquals(runString(recursion)._1, "89\n")
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
    assertEquals(runString(dynamicDispatch)._1, "3\n")
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
    assertEquals(runString(simpleObject)._1, "tick\ntick\ntick\n")
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
    assertEquals(runString(mutableState)._1, "43\n11\n")
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
    assertEquals(runString(simpleException)._1, "before\ncaught\n")
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
    assertEquals(runString(sorting)._1, "Cons(-1, Cons(1, Cons(3, Cons(5, Nil()))))\n")
  }

  val toplevelVal =
    """
      |val top: Int = 43
      |
      |def main() = {
      |  val x = top + top
      |  println(x.show)
      |}
      |""".stripMargin

  test ("toplevel val") {
    assertEquals(runString(toplevelVal)._1, "86\n")
  }


  import java.io.File
  import sbt.io.*
  import sbt.io.syntax.*

  def examplesDir = new File("examples")

  val are_we_fast_yet: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "are_we_fast_yet" / "bounce.effekt" -> Some(Summary(
      staticDispatches = 20202,
      dynamicDispatches = 5000,
      patternMatches = 0,
      branches = 31628,
      pushedFrames = 5052,
      poppedFrames = 5052,
      allocations = 0,
      closures = 100,
      variableReads = 7132,
      variableWrites = 3157,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "list_tail.effekt" -> Some(Summary(
      staticDispatches = 23713,
      dynamicDispatches = 0,
      patternMatches = 41728,
      branches = 2843,
      pushedFrames = 4961,
      poppedFrames = 4961,
      allocations = 34,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "mandelbrot.effekt" -> Some(Summary(
      staticDispatches = 18,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 26,
      pushedFrames = 3,
      poppedFrames = 3,
      allocations = 0,
      closures = 0,
      variableReads = 70,
      variableWrites = 32,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "nbody.effekt" -> Some(Summary(
      staticDispatches = 56,
      dynamicDispatches = 0,
      patternMatches = 455,
      branches = 56,
      pushedFrames = 15,
      poppedFrames = 15,
      allocations = 31,
      closures = 0,
      variableReads = 34,
      variableWrites = 30,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "queens.effekt" -> Some(Summary(
      staticDispatches = 2022,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 3347,
      pushedFrames = 1106,
      poppedFrames = 1090,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 113,
      shifts = 8,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "sieve.effekt" -> Some(Summary(
      staticDispatches = 21738,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 26736,
      pushedFrames = 671,
      poppedFrames = 671,
      allocations = 0,
      closures = 0,
      variableReads = 34546,
      variableWrites = 11738,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "towers.effekt" -> Some(Summary(
      staticDispatches = 16401,
      dynamicDispatches = 0,
      patternMatches = 16396,
      branches = 16287,
      pushedFrames = 8193,
      poppedFrames = 8193,
      allocations = 8206,
      closures = 0,
      variableReads = 41027,
      variableWrites = 8205,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt" -> Some(Summary(
      staticDispatches = 17326,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 17326,
      pushedFrames = 8661,
      poppedFrames = 8661,
      allocations = 0,
      closures = 0,
      variableReads = 23776,
      variableWrites = 5039,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "storage.effekt" -> Some(Summary(
      staticDispatches = 5463,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 5463,
      pushedFrames = 5462,
      poppedFrames = 5462,
      allocations = 5461,
      closures = 0,
      variableReads = 8192,
      variableWrites = 4096,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val duality_of_compilation: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "duality_of_compilation" / "erase_unused.effekt" -> Some(Summary(
      staticDispatches = 21,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 21,
      pushedFrames = 6,
      poppedFrames = 6,
      allocations = 16,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "factorial_accumulator.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "fibonacci_recursive.effekt" -> Some(Summary(
      staticDispatches = 15,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 27,
      pushedFrames = 15,
      poppedFrames = 15,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "iterate_increment.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "lookup_tree.effekt" -> Some(Summary(
      staticDispatches = 12,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 6,
      pushedFrames = 7,
      poppedFrames = 7,
      allocations = 6,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "match_options.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 6,
      pushedFrames = 6,
      poppedFrames = 6,
      allocations = 6,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "sum_range.effekt" -> Some(Summary(
      staticDispatches = 12,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 6,
      pushedFrames = 12,
      poppedFrames = 12,
      allocations = 6,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val effect_handlers_bench: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "effect_handlers_bench" / "countdown.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 6,
      variableWrites = 5,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "iterator.effekt" -> Some(Summary(
      staticDispatches = 7,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 7,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 7,
      variableWrites = 6,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "nqueens.effekt" -> Some(Summary(
      staticDispatches = 626,
      dynamicDispatches = 0,
      patternMatches = 400,
      branches = 1282,
      pushedFrames = 491,
      poppedFrames = 715,
      allocations = 54,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1,
      shifts = 211,
      resumes = 220
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "parsing_dollars.effekt" -> Some(Summary(
      staticDispatches = 67,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 210,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 222,
      variableWrites = 88,
      resets = 1,
      shifts = 1,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "product_early.effekt" -> Some(Summary(
      staticDispatches = 6013,
      dynamicDispatches = 0,
      patternMatches = 5005,
      branches = 6013,
      pushedFrames = 6008,
      poppedFrames = 1008,
      allocations = 1002,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 5,
      shifts = 5,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "resume_nontail.effekt" -> Some(Summary(
      staticDispatches = 7001,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 12001,
      pushedFrames = 11001,
      poppedFrames = 11001,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1000,
      shifts = 5000,
      resumes = 5000
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "tree_explore.effekt" -> Some(Summary(
      staticDispatches = 2697,
      dynamicDispatches = 0,
      patternMatches = 2380,
      branches = 3167,
      pushedFrames = 2697,
      poppedFrames = 4297,
      allocations = 1446,
      closures = 0,
      variableReads = 941,
      variableWrites = 630,
      resets = 10,
      shifts = 310,
      resumes = 620
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "triples.effekt" -> Some(Summary(
      staticDispatches = 231,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 701,
      pushedFrames = 582,
      poppedFrames = 876,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1,
      shifts = 347,
      resumes = 350
    )),
  )

  val casestudies: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "casestudies" / "ad.effekt.md" -> Some(Summary(
      staticDispatches = 19,
      dynamicDispatches = 335,
      patternMatches = 706,
      branches = 247,
      pushedFrames = 372,
      poppedFrames = 372,
      allocations = 174,
      closures = 39,
      variableReads = 0,
      variableWrites = 0,
      resets = 8,
      shifts = 29,
      resumes = 29
    )),

    examplesDir / "casestudies" / "buildsystem.effekt.md" -> Some(Summary(
      staticDispatches = 43,
      dynamicDispatches = 57,
      patternMatches = 31,
      branches = 40,
      pushedFrames = 24,
      poppedFrames = 21,
      allocations = 15,
      closures = 36,
      variableReads = 7,
      variableWrites = 3,
      resets = 9,
      shifts = 7,
      resumes = 4
    )),

    examplesDir / "casestudies" / "scheduler.effekt.md" -> Some(Summary(
      staticDispatches = 60,
      dynamicDispatches = 7,
      patternMatches = 80,
      branches = 41,
      pushedFrames = 57,
      poppedFrames = 58,
      allocations = 58,
      closures = 7,
      variableReads = 29,
      variableWrites = 18,
      resets = 1,
      shifts = 7,
      resumes = 7
    )),

    examplesDir / "casestudies" / "lexer.effekt.md" -> Some(Summary(
      staticDispatches = 302,
      dynamicDispatches = 18,
      patternMatches = 205,
      branches = 405,
      pushedFrames = 187,
      poppedFrames = 187,
      allocations = 109,
      closures = 27,
      variableReads = 164,
      variableWrites = 51,
      resets = 31,
      shifts = 11,
      resumes = 11
    )),

    examplesDir / "casestudies" / "parser.effekt.md" -> Some(Summary(
      staticDispatches = 11375,
      dynamicDispatches = 783,
      patternMatches = 9427,
      branches = 14892,
      pushedFrames = 7172,
      poppedFrames = 7160,
      allocations = 3848,
      closures = 521,
      variableReads = 6742,
      variableWrites = 1901,
      resets = 778,
      shifts = 229,
      resumes = 213
    )),

    examplesDir / "casestudies" / "anf.effekt.md" -> Some(Summary(
      staticDispatches = 6140,
      dynamicDispatches = 443,
      patternMatches = 5098,
      branches = 8110,
      pushedFrames = 4086,
      poppedFrames = 4074,
      allocations = 2143,
      closures = 358,
      variableReads = 4080,
      variableWrites = 1343,
      resets = 458,
      shifts = 322,
      resumes = 306
    )),

    examplesDir / "casestudies" / "inference.effekt.md" -> Some(Summary(
      staticDispatches = 1482048,
      dynamicDispatches = 3224955,
      patternMatches = 1497935,
      branches = 304502,
      pushedFrames = 2643292,
      poppedFrames = 2066027,
      allocations = 4654126,
      closures = 867068,
      variableReads = 2955965,
      variableWrites = 1480868,
      resets = 288904,
      shifts = 298107,
      resumes = 9263
    )),

    examplesDir / "pos" / "raytracer.effekt" -> Some(Summary(
      staticDispatches = 85740,
      dynamicDispatches = 0,
      patternMatches = 763548,
      branches = 65951,
      pushedFrames = 43131,
      poppedFrames = 43131,
      allocations = 70805,
      closures = 0,
      variableReads = 77886,
      variableWrites = 26904,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val other: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "other" / "emit.effekt" -> Some(Summary(
      staticDispatches = 11,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 11,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 61,
      variableWrites = 30,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "church_exponentiation.effekt" -> Some(Summary(
      staticDispatches = 7,
      dynamicDispatches = 797188,
      patternMatches = 0,
      branches = 5,
      pushedFrames = 531467,
      poppedFrames = 531467,
      allocations = 0,
      closures = 26,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "variadic_combinators.effekt" -> Some(Summary(
      staticDispatches = 27057,
      dynamicDispatches = 9009,
      patternMatches = 30052,
      branches = 3003,
      pushedFrames = 12024,
      poppedFrames = 12024,
      allocations = 24060,
      closures = 12030,
      variableReads = 24048,
      variableWrites = 18036,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "stdlib" / "stream" / "fix.effekt" -> Some(Summary(
      staticDispatches = 38,
      dynamicDispatches = 52,
      patternMatches = 37,
      branches = 0,
      pushedFrames = 17,
      poppedFrames = 17,
      allocations = 37,
      closures = 37,
      variableReads = 10,
      variableWrites = 9,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "pos" / "diy_binder.effekt" -> Some(Summary(
      staticDispatches = 28,
      dynamicDispatches = 0,
      patternMatches = 28,
      branches = 26,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 30,
      closures = 0,
      variableReads = 28,
      variableWrites = 29,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val testFiles: Seq[(File, Option[Summary])] =
    are_we_fast_yet ++
    duality_of_compilation ++
    effect_handlers_bench ++
    casestudies ++
    other

  def runTest(f: File, expectedSummary: Option[Summary]): Unit =
    val path = f.getPath
    test(path) {
      try {
        val (result, summary) = runFile(path)
        val expected = expectedResultFor(f).getOrElse { s"Missing checkfile for ${path}"}
        assertNoDiff(result, expected)
        expectedSummary.foreach { expected => assertEquals(summary, expected) }
      } catch {
        case i: VMError => fail(i.getMessage, i)
      }
    }

  testFiles.foreach(runTest)
}
