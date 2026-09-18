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
      staticDispatches = 15202,
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
      staticDispatches = 11,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 25,
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
      patternMatches = 45,
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
      staticDispatches = 1146,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 2711,
      pushedFrames = 230,
      poppedFrames = 222,
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

    examplesDir / "benchmarks" / "effect_handlers_bench" / "generator.effekt" -> Some(Summary(
      staticDispatches = 101,
      dynamicDispatches = 31,
      patternMatches = 95,
      branches = 6,
      pushedFrames = 102,
      poppedFrames = 102,
      allocations = 38,
      closures = 31,
      variableReads = 0,
      variableWrites = 0,
      resets = 1,
      shifts = 31,
      resumes = 31
    )),

    examplesDir / "benchmarks" / "effect_handlers_bench" / "handler_sieve.effekt" -> Some(Summary(
      staticDispatches = 9,
      dynamicDispatches = 21,
      patternMatches = 0,
      branches = 34,
      pushedFrames = 9,
      poppedFrames = 9,
      allocations = 0,
      closures = 5,
      variableReads = 0,
      variableWrites = 0,
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
      resets = 0,
      shifts = 0,
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
      pushedFrames = 6001,
      poppedFrames = 6001,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
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
      shifts = 231,
      resumes = 350
    )),
  )

  val casestudies: Seq[(File, Option[Summary])] = Seq(
    examplesDir / "casestudies" / "ad.effekt.md" -> Some(Summary(
      staticDispatches = 103,
      dynamicDispatches = 61,
      patternMatches = 148,
      branches = 247,
      pushedFrames = 113,
      poppedFrames = 113,
      allocations = 96,
      closures = 6,
      variableReads = 0,
      variableWrites = 0,
      resets = 8,
      shifts = 29,
      resumes = 29
    )),

    examplesDir / "casestudies" / "buildsystem.effekt.md" -> Some(Summary(
      staticDispatches = 36,
      dynamicDispatches = 55,
      patternMatches = 31,
      branches = 40,
      pushedFrames = 24,
      poppedFrames = 21,
      allocations = 15,
      closures = 38,
      variableReads = 7,
      variableWrites = 3,
      resets = 9,
      shifts = 7,
      resumes = 4
    )),

    examplesDir / "casestudies" / "scheduler.effekt.md" -> Some(Summary(
      staticDispatches = 70,
      dynamicDispatches = 7,
      patternMatches = 80,
      branches = 41,
      pushedFrames = 51,
      poppedFrames = 51,
      allocations = 48,
      closures = 7,
      variableReads = 29,
      variableWrites = 18,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "casestudies" / "lexer.effekt.md" -> Some(Summary(
      staticDispatches = 288,
      dynamicDispatches = 8,
      patternMatches = 219,
      branches = 388,
      pushedFrames = 159,
      poppedFrames = 159,
      allocations = 105,
      closures = 4,
      variableReads = 164,
      variableWrites = 51,
      resets = 10,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "casestudies" / "parser.effekt.md" -> Some(Summary(
      staticDispatches = 10448,
      dynamicDispatches = 1094,
      patternMatches = 8765,
      branches = 14364,
      pushedFrames = 6429,
      poppedFrames = 6352,
      allocations = 3729,
      closures = 95,
      variableReads = 6742,
      variableWrites = 1901,
      resets = 326,
      shifts = 214,
      resumes = 213
    )),

    examplesDir / "casestudies" / "anf.effekt.md" -> Some(Summary(
      staticDispatches = 5639,
      dynamicDispatches = 959,
      patternMatches = 4730,
      branches = 7851,
      pushedFrames = 3847,
      poppedFrames = 3802,
      allocations = 2073,
      closures = 145,
      variableReads = 4080,
      variableWrites = 1343,
      resets = 205,
      shifts = 310,
      resumes = 306
    )),

    examplesDir / "casestudies" / "inference.effekt.md" -> Some(Summary(
      staticDispatches = 2076251,
      dynamicDispatches = 2059052,
      patternMatches = 1497933,
      branches = 304507,
      pushedFrames = 2942959,
      poppedFrames = 2365697,
      allocations = 4360015,
      closures = 289711,
      variableReads = 2955965,
      variableWrites = 1480868,
      resets = 288860,
      shifts = 298083,
      resumes = 9241
    )),

    examplesDir / "pos" / "raytracer.effekt" -> Some(Summary(
      staticDispatches = 64969,
      dynamicDispatches = 0,
      patternMatches = 220971,
      branches = 65951,
      pushedFrames = 25382,
      poppedFrames = 25382,
      allocations = 44434,
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
      staticDispatches = 65,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 100,
      pushedFrames = 30,
      poppedFrames = 30,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "ack_goto.effekt" -> Some(Summary(
      staticDispatches = 65,
      dynamicDispatches = 30,
      patternMatches = 0,
      branches = 100,
      pushedFrames = 30,
      poppedFrames = 30,
      allocations = 0,
      closures = 30,
      variableReads = 0,
      variableWrites = 0,
      resets = 30,
      shifts = 30,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "cps_tak.effekt" -> Some(Summary(
      staticDispatches = 821,
      dynamicDispatches = 0,
      patternMatches = 352,
      branches = 469,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 352,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "deriv.effekt" -> Some(Summary(
      staticDispatches = 132,
      dynamicDispatches = 0,
      patternMatches = 235,
      branches = 43,
      pushedFrames = 72,
      poppedFrames = 72,
      allocations = 121,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "divrec.effekt" -> Some(Summary(
      staticDispatches = 23,
      dynamicDispatches = 0,
      patternMatches = 17,
      branches = 11,
      pushedFrames = 23,
      poppedFrames = 23,
      allocations = 16,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "even_odd.effekt" -> Some(Summary(
      staticDispatches = 12,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 27,
      pushedFrames = 3,
      poppedFrames = 3,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "even_odd_goto.effekt" -> Some(Summary(
      staticDispatches = 11,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 25,
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
    examplesDir / "benchmarks" / "folklore_to_fact" / "life.effekt" -> Some(Summary(
      staticDispatches = 950575,
      dynamicDispatches = 0,
      patternMatches = 2554163,
      branches = 929615,
      pushedFrames = 51607,
      poppedFrames = 51607,
      allocations = 137800,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "merge.effekt" -> Some(Summary(
      staticDispatches = 65,
      dynamicDispatches = 0,
      patternMatches = 62,
      branches = 43,
      pushedFrames = 25,
      poppedFrames = 25,
      allocations = 61,
      closures = 0,
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
      staticDispatches = 7014,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 7014,
      pushedFrames = 5333,
      poppedFrames = 5333,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "motzkin_goto.effekt" -> Some(Summary(
      staticDispatches = 7014,
      dynamicDispatches = 4348,
      patternMatches = 0,
      branches = 7014,
      pushedFrames = 5333,
      poppedFrames = 5333,
      allocations = 0,
      closures = 4348,
      variableReads = 0,
      variableWrites = 0,
      resets = 4348,
      shifts = 4348,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "nqueens.effekt" -> Some(Summary(
      staticDispatches = 770,
      dynamicDispatches = 0,
      patternMatches = 734,
      branches = 1092,
      pushedFrames = 340,
      poppedFrames = 340,
      allocations = 167,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "perm.effekt" -> Some(Summary(
      staticDispatches = 29231,
      dynamicDispatches = 0,
      patternMatches = 19644,
      branches = 23450,
      pushedFrames = 12246,
      poppedFrames = 12246,
      allocations = 10180,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "primes.effekt" -> Some(Summary(
      staticDispatches = 36,
      dynamicDispatches = 0,
      patternMatches = 26,
      branches = 22,
      pushedFrames = 31,
      poppedFrames = 31,
      allocations = 21,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "sudan.effekt" -> Some(Summary(
      staticDispatches = 5,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 8,
      pushedFrames = 3,
      poppedFrames = 3,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "sudan_goto.effekt" -> Some(Summary(
      staticDispatches = 5,
      dynamicDispatches = 3,
      patternMatches = 0,
      branches = 8,
      pushedFrames = 3,
      poppedFrames = 3,
      allocations = 0,
      closures = 3,
      variableReads = 0,
      variableWrites = 0,
      resets = 3,
      shifts = 3,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "tail_fib.effekt" -> Some(Summary(
      staticDispatches = 11,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 11,
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
    examplesDir / "benchmarks" / "folklore_to_fact" / "tak.effekt" -> Some(Summary(
      staticDispatches = 469,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 469,
      pushedFrames = 352,
      poppedFrames = 352,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "tak_goto.effekt" -> Some(Summary(
      staticDispatches = 469,
      dynamicDispatches = 352,
      patternMatches = 0,
      branches = 469,
      pushedFrames = 352,
      poppedFrames = 352,
      allocations = 0,
      closures = 352,
      variableReads = 0,
      variableWrites = 0,
      resets = 352,
      shifts = 352,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "folklore_to_fact" / "takl.effekt" -> Some(Summary(
      staticDispatches = 4955,
      dynamicDispatches = 0,
      patternMatches = 8887,
      branches = 506,
      pushedFrames = 868,
      poppedFrames = 868,
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
      staticDispatches = 14377376,
      dynamicDispatches = 717624,
      patternMatches = 23083633,
      branches = 4865096,
      pushedFrames = 12560285,
      poppedFrames = 12560285,
      allocations = 29014797,
      closures = 3041208,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "constraints.effekt" -> Some(Summary(
      staticDispatches = 869112,
      dynamicDispatches = 60350,
      patternMatches = 1380107,
      branches = 432810,
      pushedFrames = 778554,
      poppedFrames = 778554,
      allocations = 851666,
      closures = 5,
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
      staticDispatches = 985527,
      dynamicDispatches = 180,
      patternMatches = 1363407,
      branches = 21,
      pushedFrames = 112547,
      poppedFrames = 112547,
      allocations = 1252952,
      closures = 180,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "gcd.effekt" -> Some(Summary(
      staticDispatches = 20658,
      dynamicDispatches = 0,
      patternMatches = 39341,
      branches = 17169,
      pushedFrames = 8575,
      poppedFrames = 8575,
      allocations = 27279,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "integer.effekt" -> Some(Summary(
      staticDispatches = 182,
      dynamicDispatches = 0,
      patternMatches = 135,
      branches = 50,
      pushedFrames = 104,
      poppedFrames = 104,
      allocations = 206,
      closures = 0,
      variableReads = 42,
      variableWrites = 36,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),
    examplesDir / "benchmarks" / "nofib" / "lcss.effekt" -> Some(Summary(
      staticDispatches = 100504,
      dynamicDispatches = 215,
      patternMatches = 186965,
      branches = 164286,
      pushedFrames = 92874,
      poppedFrames = 92874,
      allocations = 129998,
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
      staticDispatches = 5,
      dynamicDispatches = 551895,
      patternMatches = 0,
      branches = 5,
      pushedFrames = 531456,
      poppedFrames = 531456,
      allocations = 0,
      closures = 16,
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
      allocations = 21054,
      closures = 12030,
      variableReads = 24048,
      variableWrites = 18036,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "unify.effekt" -> Some(Summary(
      staticDispatches = 6555881,
      dynamicDispatches = 1276021,
      patternMatches = 3739883,
      branches = 13982006,
      pushedFrames = 1769627,
      poppedFrames = 1769627,
      allocations = 1929373,
      closures = 4,
      variableReads = 569362,
      variableWrites = 380936,
      resets = 2,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "unify_dictionary.effekt" -> Some(Summary(
      staticDispatches = 1265706,
      dynamicDispatches = 4097,
      patternMatches = 1359906,
      branches = 1069071,
      pushedFrames = 598040,
      poppedFrames = 598040,
      allocations = 704537,
      closures = 4,
      variableReads = 589843,
      variableWrites = 380936,
      resets = 2,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "wordfreq_dictionary.effekt" -> Some(Summary(
      staticDispatches = 1514108,
      dynamicDispatches = 298358,
      patternMatches = 438798,
      branches = 1583315,
      pushedFrames = 874254,
      poppedFrames = 874248,
      allocations = 77124,
      closures = 98836,
      variableReads = 1043091,
      variableWrites = 310465,
      resets = 2,
      shifts = 94732,
      resumes = 94730
    )),

    examplesDir / "benchmarks" / "other" / "tracking_map.effekt" -> Some(Summary(
      staticDispatches = 1421197,
      dynamicDispatches = 43937,
      patternMatches = 2278683,
      branches = 1450252,
      pushedFrames = 676270,
      poppedFrames = 676270,
      allocations = 690274,
      closures = 4,
      variableReads = 896183,
      variableWrites = 226310,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "knuth_man_or_boy.effekt" -> Some(Summary(
      staticDispatches = 2276,
      dynamicDispatches = 1224,
      patternMatches = 0,
      branches = 1444,
      pushedFrames = 1226,
      poppedFrames = 1226,
      allocations = 0,
      closures = 1452,
      variableReads = 1442,
      variableWrites = 721,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "benchmarks" / "other" / "checkpointing.effekt" -> Some(Summary(
      staticDispatches = 11,
      dynamicDispatches = 1,
      patternMatches = 11,
      branches = 25,
      pushedFrames = 5,
      poppedFrames = 6,
      allocations = 14,
      closures = 3,
      variableReads = 47,
      variableWrites = 18,
      resets = 1,
      shifts = 4,
      resumes = 4
    )),

    examplesDir / "stdlib" / "stream" / "fix.effekt" -> Some(Summary(
      staticDispatches = 37,
      dynamicDispatches = 41,
      patternMatches = 32,
      branches = 0,
      pushedFrames = 17,
      poppedFrames = 17,
      allocations = 24,
      closures = 39,
      variableReads = 10,
      variableWrites = 9,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),


    examplesDir / "pos" / "mutabletailrec.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 2,
      variableWrites = 1,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "resume_past_var.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 8,
      variableWrites = 4,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "resume_past_worker.effekt" -> Some(Summary(
      staticDispatches = 1,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 1,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 1,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "abort_past_var.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 1,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 2,
      variableReads = 2,
      variableWrites = 2,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "abort_past_joinpoint.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 11,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 18,
      variableWrites = 10,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "issue992.effekt" -> Some(Summary(
      staticDispatches = 8,
      dynamicDispatches = 0,
      patternMatches = 2,
      branches = 10,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 3,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "issue995.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "issue1064.effekt" -> Some(Summary(
      staticDispatches = 17,
      dynamicDispatches = 0,
      patternMatches = 17,
      branches = 10,
      pushedFrames = 12,
      poppedFrames = 12,
      allocations = 16,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    // issue 1272: Optimizing through useless constructs / obfuscated code (optimizer-wishlist)
    examplesDir / "pos" / "resume_or_return.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 1,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 1,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "resume_inside_nested_reset.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "abort_inside_handler.effekt" -> Some(Summary(
      staticDispatches = 5,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 5,
      pushedFrames = 2,
      poppedFrames = 1,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 1,
      shifts = 1,
      resumes = 0
    )),

    examplesDir / "pos" / "resume_twice_past_state.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 4,
      poppedFrames = 5,
      allocations = 0,
      closures = 0,
      variableReads = 4,
      variableWrites = 2,
      resets = 1,
      shifts = 1,
      resumes = 2
    )),

    examplesDir / "pos" / "resume_after_handler.effekt" -> Some(Summary(
      staticDispatches = 8,
      dynamicDispatches = 2,
      patternMatches = 15,
      branches = 4,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 8,
      closures = 2,
      variableReads = 5,
      variableWrites = 4,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "resume_closing_over_prompt.effekt" -> Some(Summary(
      staticDispatches = 5,
      dynamicDispatches = 2,
      patternMatches = 15,
      branches = 4,
      pushedFrames = 7,
      poppedFrames = 7,
      allocations = 8,
      closures = 2,
      variableReads = 5,
      variableWrites = 4,
      resets = 1,
      shifts = 2,
      resumes = 2
    )),

    examplesDir / "pos" / "lambdas" / "scheduler.effekt" -> Some(Summary(
      staticDispatches = 70,
      dynamicDispatches = 7,
      patternMatches = 80,
      branches = 41,
      pushedFrames = 50,
      poppedFrames = 50,
      allocations = 48,
      closures = 7,
      variableReads = 29,
      variableWrites = 18,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "with_val_else.effekt" -> Some(Summary(
      staticDispatches = 6,
      dynamicDispatches = 0,
      patternMatches = 6,
      branches = 3,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 6,
      closures = 0,
      variableReads = 3,
      variableWrites = 2,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "issue842.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 0,
      poppedFrames = 0,
      allocations = 0,
      closures = 0,
      variableReads = 0,
      variableWrites = 0,
      resets = 0,
      shifts = 0,
      resumes = 0
    )),

    examplesDir / "pos" / "issue972.effekt" -> Some(Summary(
      staticDispatches = 0,
      dynamicDispatches = 0,
      patternMatches = 0,
      branches = 0,
      pushedFrames = 4,
      poppedFrames = 4,
      allocations = 0,
      closures = 0,
      variableReads = 5,
      variableWrites = 2,
      resets = 3,
      shifts = 4,
      resumes = 4
    )),

    examplesDir / "pos" / "diy_binder.effekt" -> Some(Summary(
      staticDispatches = 28,
      dynamicDispatches = 0,
      patternMatches = 28,
      branches = 26,
      pushedFrames = 1,
      poppedFrames = 1,
      allocations = 29,
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
