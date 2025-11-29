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
      staticDispatches = 20203,
      dynamicDispatches = 5000,
      patternMatches = 0,
      branches = 31628,
      pushedFrames = 5053,
      poppedFrames = 5053,
      allocations = 0,
      closures = 100,
      variableReads = 7132,
      variableWrites = 3157,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "list_tail.effekt" -> Some(Summary(
      staticDispatches = 23714,
      dynamicDispatches = 0,
      patternMatches = 41728,
      branches = 2843,
      pushedFrames = 4962,
      poppedFrames = 4962,
      allocations = 34,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "mandelbrot.effekt" -> Some(Summary(
      staticDispatches = 19,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 26,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 0,
      closures = 0,
      variableReads = 70,
      variableWrites = 32,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "nbody.effekt" -> Some(Summary(
      staticDispatches = 57,
      dynamicDispatches = 0,
      patternMatches = 455,
      branches = 56,
      pushedFrames = 16,
      poppedFrames = 16,
      allocations = 31,
      closures = 0,
      variableReads = 34,
      variableWrites = 30,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "queens.effekt" -> Some(Summary(
      staticDispatches = 2023,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 3347,
      pushedFrames = 1107,
      poppedFrames = 1091,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 113,
      shifts = 8,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "sieve.effekt" -> Some(Summary(
      staticDispatches = 21739,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 26736,
      pushedFrames = 672,
      poppedFrames = 672,
      allocations = 0,
      closures = 0,
      variableReads = 34546,
      variableWrites = 11738,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "towers.effekt" -> Some(Summary(
      staticDispatches = 16402,
      dynamicDispatches = 0,
      patternMatches = 16396,
      branches = 16287,
      pushedFrames = 8194,
      poppedFrames = 8194,
      allocations = 8206,
      closures = 0,
      variableReads = 41027,
      variableWrites = 8205,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "permute.effekt" -> Some(Summary(
      staticDispatches = 17327,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 17326,
      pushedFrames = 8662,
      poppedFrames = 8662,
      allocations = 0,
      closures = 0,
      variableReads = 23776,
      variableWrites = 5039,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "are_we_fast_yet" / "storage.effekt" -> Some(Summary(
      staticDispatches = 5464,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 5463,
      pushedFrames = 5463,
      poppedFrames = 5463,
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
      staticDispatches = 22,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 21,
      pushedFrames = 7,
      poppedFrames = 7,
      allocations = 16,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "factorial_accumulator.effekt" -> Some(Summary(
      staticDispatches = 7,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "fibonacci_recursive.effekt" -> Some(Summary(
      staticDispatches = 16,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 27,
      pushedFrames = 16,
      poppedFrames = 16,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "iterate_increment.effekt" -> Some(Summary(
      staticDispatches = 7,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "lookup_tree.effekt" -> Some(Summary(
      staticDispatches = 13,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 6,
      pushedFrames = 8,
      poppedFrames = 8,
      allocations = 6,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "duality_of_compilation" / "match_options.effekt" -> Some(Summary(
      staticDispatches = 7,
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

    examplesDir / "benchmarks" / "duality_of_compilation" / "sum_range.effekt" -> Some(Summary(
      staticDispatches = 13,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 6,
      pushedFrames = 13,
      poppedFrames = 13,
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
      staticDispatches = 7,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 6,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 6,
      variableWrites = 5,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "iterator.effekt" -> Some(Summary(
      staticDispatches = 8,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 7,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 7,
      variableWrites = 6,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "nqueens.effekt" -> Some(Summary(
      staticDispatches = 627,
      dynamicDispatches = 0,
      patternMatches = 400,
      branches = 1282,
      pushedFrames = 492,
      poppedFrames = 716,
      allocations = 54,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1,
      shifts = 211,
      resumes = 220
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "parsing_dollars.effekt" -> Some(Summary(
      staticDispatches = 68,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 210,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 222,
      variableWrites = 88,
      resets = 2,
      shifts = 68,
      resumes = 66
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "product_early.effekt" -> Some(Summary(
      staticDispatches = 6014,
      dynamicDispatches = 0,
      patternMatches = 5005,
      branches = 6013,
      pushedFrames = 6009,
      poppedFrames = 1009,
      allocations = 1002,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 5,
      shifts = 5,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "resume_nontail.effekt" -> Some(Summary(
      staticDispatches = 7002,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 12001,
      pushedFrames = 11002,
      poppedFrames = 11002,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1000,
      shifts = 5000,
      resumes = 5000
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "tree_explore.effekt" -> Some(Summary(
      staticDispatches = 2698,
      dynamicDispatches = 0,
      patternMatches = 2380,
      branches = 3167,
      pushedFrames = 2698,
      poppedFrames = 4298,
      allocations = 1446,
      closures = 0,
      variableReads = 941,
      variableWrites = 630,
      resets = 10,
      shifts = 310,
      resumes = 620
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "triples.effekt" -> Some(Summary(
      staticDispatches = 232,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 701,
      pushedFrames = 583,
      poppedFrames = 877,
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
      staticDispatches = 48,
      dynamicDispatches = 335,
      patternMatches = 706,
      branches = 247,
      pushedFrames = 401,
      poppedFrames = 401,
      allocations = 174,
      closures = 39,
      variableReads = 0,
      variableWrites = 0,
      resets = 8,
      shifts = 29,
      resumes = 29
    )),

    examplesDir / "casestudies" / "buildsystem.effekt.md" -> Some(Summary(
      staticDispatches = 36,
      dynamicDispatches = 51,
      patternMatches = 31,
      branches = 40,
      pushedFrames = 28,
      poppedFrames = 25,
      allocations = 15,
      closures = 34,
      variableReads = 7,
      variableWrites = 3,
      resets = 9,
      shifts = 7,
      resumes = 4
    )),

    examplesDir / "casestudies" / "scheduler.effekt.md" -> Some(Summary(
      staticDispatches = 66,
      dynamicDispatches = 7,
      patternMatches = 80,
      branches = 41,
      pushedFrames = 63,
      poppedFrames = 64,
      allocations = 58,
      closures = 7,
      variableReads = 29,
      variableWrites = 18,
      resets = 1,
      shifts = 7,
      resumes = 7
    )),

    examplesDir / "casestudies" / "lexer.effekt.md" -> Some(Summary(
      staticDispatches = 370,
      dynamicDispatches = 15,
      patternMatches = 235,
      branches = 405,
      pushedFrames = 253,
      poppedFrames = 253,
      allocations = 109,
      closures = 23,
      variableReads = 164,
      variableWrites = 51,
      resets = 28,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "casestudies" / "parser.effekt.md" -> Some(Summary(
      staticDispatches = 11467,
      dynamicDispatches = 783,
      patternMatches = 9468,
      branches = 14892,
      pushedFrames = 7264,
      poppedFrames = 7252,
      allocations = 3848,
      closures = 521,
      variableReads = 6742,
      variableWrites = 1901,
      resets = 776,
      shifts = 542,
      resumes = 526
    )),

    examplesDir / "casestudies" / "anf.effekt.md" -> Some(Summary(
      staticDispatches = 6189,
      dynamicDispatches = 443,
      patternMatches = 5109,
      branches = 8110,
      pushedFrames = 4135,
      poppedFrames = 4123,
      allocations = 2143,
      closures = 358,
      variableReads = 4080,
      variableWrites = 1343,
      resets = 451,
      shifts = 488,
      resumes = 472
    )),

    examplesDir / "casestudies" / "inference.effekt.md" -> Some(Summary(
      staticDispatches = 1482252,
      dynamicDispatches = 3224986,
      patternMatches = 1497945,
      branches = 304507,
      pushedFrames = 2643313,
      poppedFrames = 2066048,
      allocations = 4654131,
      closures = 867079,
      variableReads = 2955965,
      variableWrites = 1480868,
      resets = 288867,
      shifts = 298106,
      resumes = 9257
    )),

    examplesDir / "pos" / "raytracer.effekt" -> Some(Summary(
      staticDispatches = 94956,
      dynamicDispatches = 0,
      patternMatches = 763548,
      branches = 65951,
      pushedFrames = 52347,
      poppedFrames = 52347,
      allocations = 70805,
      closures = 0,
      variableReads = 77886,
      variableWrites = 26904,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val folklore_to_fact: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "folklore_to_fact" / "ack.effekt" -> Some(Summary(
      staticDispatches = 66,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 100,
      pushedFrames = 31,
      poppedFrames = 31,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "ack_goto.effekt" -> Some(Summary(
      staticDispatches = 66,
      dynamicDispatches = 30,
      patternMatches = 0,
      branches = 100,
      pushedFrames = 31,
      poppedFrames = 31,
      allocations = 0,
      closures = 30,
      variableReads = 0,
      variableWrites = 0,
      resets = 30,
      shifts = 30,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "cps_tak.effekt" -> Some(Summary(
      staticDispatches = 470,
      dynamicDispatches = 352,
      patternMatches = 0,
      branches = 469,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 352,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "deriv.effekt" -> Some(Summary(
      staticDispatches = 133,
      dynamicDispatches = 0,
      patternMatches = 235,
      branches = 43,
      pushedFrames = 73,
      poppedFrames = 73,
      allocations = 203,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "divrec.effekt" -> Some(Summary(
      staticDispatches = 24,
      dynamicDispatches = 0,
      patternMatches = 17,
      branches = 11,
      pushedFrames = 24,
      poppedFrames = 24,
      allocations = 17,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "even_odd.effekt" -> Some(Summary(
      staticDispatches = 13,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 27,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "even_odd_goto.effekt" -> Some(Summary(
      staticDispatches = 13,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 27,
      pushedFrames = 14,
      poppedFrames = 14,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 11,
      shifts = 11,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "life.effekt" -> Some(Summary(
      staticDispatches = 950576,
      dynamicDispatches = 0,
      patternMatches = 2554143,
      branches = 929615,
      pushedFrames = 51608,
      poppedFrames = 51608,
      allocations = 139869,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "merge.effekt" -> Some(Summary(
      staticDispatches = 67,
      dynamicDispatches = 20,
      patternMatches = 62,
      branches = 43,
      pushedFrames = 45,
      poppedFrames = 45,
      allocations = 63,
      closures = 2,
      variableReads = 22,
      variableWrites = 20,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
//    examplesDir / "benchmarks" / "folklore_to_fact" / "minimax.effekt" -> Some(Summary(
//      staticDispatches = 114835627,
//      dynamicDispatches = 0,
//      patternMatches = 161274589,
//      branches = 87719807,
//      pushedFrames = 41437511,
//      poppedFrames = 41437511,
//      allocations = 67287094,
//      closures = 0,
//      variableReads = 0,
//      variableWrites = 0,
//      resets = 0,
//      shifts = 0,
//      resumes = 0
//    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "motzkin.effekt" -> Some(Summary(
      staticDispatches = 7015,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 7014,
      pushedFrames = 5334,
      poppedFrames = 5334,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "motzkin_goto.effekt" -> Some(Summary(
      staticDispatches = 7015,
      dynamicDispatches = 4348,
      patternMatches = 0,
      branches = 7014,
      pushedFrames = 5334,
      poppedFrames = 5334,
      allocations = 0,
      closures = 4348,
      variableReads = 0,
      variableWrites = 0,
      resets = 5333,
      shifts = 5333,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "nqueens.effekt" -> Some(Summary(
      staticDispatches = 771,
      dynamicDispatches = 0,
      patternMatches = 734,
      branches = 1092,
      pushedFrames = 341,
      poppedFrames = 341,
      allocations = 192,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "perm.effekt" -> Some(Summary(
      staticDispatches = 29232,
      dynamicDispatches = 0,
      patternMatches = 25494,
      branches = 23450,
      pushedFrames = 12247,
      poppedFrames = 12247,
      allocations = 10180,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "primes.effekt" -> Some(Summary(
      staticDispatches = 37,
      dynamicDispatches = 0,
      patternMatches = 26,
      branches = 22,
      pushedFrames = 32,
      poppedFrames = 32,
      allocations = 26,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "sudan.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 8,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "sudan_goto.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 3,
      patternMatches = 0,
      branches = 8,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 0,
      closures = 3,
      variableReads = 0,
      variableWrites = 0,
      resets = 3,
      shifts = 3,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "tail_fib.effekt" -> Some(Summary(
      staticDispatches = 12,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 11,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "tak.effekt" -> Some(Summary(
      staticDispatches = 470,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 469,
      pushedFrames = 353,
      poppedFrames = 353,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "tak_goto.effekt" -> Some(Summary(
      staticDispatches = 470,
      dynamicDispatches = 352,
      patternMatches = 0,
      branches = 469,
      pushedFrames = 353,
      poppedFrames = 353,
      allocations = 0,
      closures = 352,
      variableReads = 0,
      variableWrites = 0,
      resets = 352,
      shifts = 352,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "takl.effekt" -> Some(Summary(
      staticDispatches = 4956,
      dynamicDispatches = 0,
      patternMatches = 16827,
      branches = 506,
      pushedFrames = 869,
      poppedFrames = 869,
      allocations = 37,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val nofib: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "nofib" / "boyer.effekt" -> Some(Summary(
      staticDispatches = 14378304,
      dynamicDispatches = 717624,
      patternMatches = 23083633,
      branches = 4865861,
      pushedFrames = 12561051,
      poppedFrames = 12561051,
      allocations = 38231904,
      closures = 3041208,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "constraints.effekt" -> Some(Summary(
      staticDispatches = 894048,
      dynamicDispatches = 60355,
      patternMatches = 1388933,
      branches = 432810,
      pushedFrames = 797580,
      poppedFrames = 797580,
      allocations = 1003044,
      closures = 10,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    //    examplesDir / "benchmarks" / "nofib" / "cryptarithm1.effekt" -> Some(Summary(
    //      staticDispatches = 44977545,
    //      dynamicDispatches = 0,
    //      patternMatches = 74007933,
    //      branches = 3628825,
    //      pushedFrames = 40939622,
    //      poppedFrames = 40939622,
    //      allocations = 56068493,
    //      closures = 0,
    //      variableReads = 0,
    //      variableWrites = 0,
    //      resets = 0,
    //      shifts = 0,
    //      resumes = 0
    //    )),
    examplesDir / "benchmarks" / "nofib" / "fish.effekt" -> Some(Summary(
      staticDispatches = 989578,
      dynamicDispatches = 4840,
      patternMatches = 1657577,
      branches = 21,
      pushedFrames = 113428,
      poppedFrames = 113428,
      allocations = 1269842,
      closures = 4840,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "gcd.effekt" -> Some(Summary(
      staticDispatches = 20659,
      dynamicDispatches = 0,
      patternMatches = 39341,
      branches = 17169,
      pushedFrames = 8576,
      poppedFrames = 8576,
      allocations = 37661,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "integer.effekt" -> Some(Summary(
      staticDispatches = 4027,
      dynamicDispatches = 792,
      patternMatches = 2928,
      branches = 1079,
      pushedFrames = 3081,
      poppedFrames = 3081,
      allocations = 4532,
      closures = 22,
      variableReads = 924,
      variableWrites = 792,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "lcss.effekt" -> Some(Summary(
      staticDispatches = 100606,
      dynamicDispatches = 215,
      patternMatches = 187066,
      branches = 164286,
      pushedFrames = 92875,
      poppedFrames = 92875,
      allocations = 181263,
      closures = 215,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
  )

  val other: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "benchmarks" / "other" / "emit.effekt" -> Some(Summary(
      staticDispatches = 12,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 11,
      pushedFrames = 2,
      poppedFrames = 2,
      allocations = 0,
      closures = 0,
      variableReads = 61,
      variableWrites = 30,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "church_exponentiation.effekt" -> Some(Summary(
      staticDispatches = 8,
      dynamicDispatches = 797188,
      patternMatches = 0,
      branches = 5,
      pushedFrames = 531468,
      poppedFrames = 531468,
      allocations = 0,
      closures = 26,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "variadic_combinators.effekt" -> Some(Summary(
      staticDispatches = 27058,
      dynamicDispatches = 9009,
      patternMatches = 30052,
      branches = 3003,
      pushedFrames = 12025,
      poppedFrames = 12025,
      allocations = 24060,
      closures = 12030,
      variableReads = 24048,
      variableWrites = 18036,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "unify.effekt" -> Some(Summary(
      staticDispatches = 2519233,
      dynamicDispatches = 1460350,
      patternMatches = 3969252,
      branches = 2791752,
      pushedFrames = 1970338,
      poppedFrames = 1970338,
      allocations = 2138279,
      closures = 90125,
      variableReads = 569362,
      variableWrites = 380936,
      resets = 2,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "stdlib" / "stream" / "fix.effekt" -> Some(Summary(
      staticDispatches = 48,
      dynamicDispatches = 52,
      patternMatches = 37,
      branches = 0,
      pushedFrames = 27,
      poppedFrames = 27,
      allocations = 37,
      closures = 37,
      variableReads = 10,
      variableWrites = 9,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),


    examplesDir / "pos" / "diy_binder.effekt" -> Some(Summary(
      staticDispatches = 54,
      dynamicDispatches = 0,
      patternMatches = 28,
      branches = 26,
      pushedFrames = 27,
      poppedFrames = 27,
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
    folklore_to_fact ++
    nofib ++
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
