import sbt.*
import sbt.Keys.*
import scala.io.AnsiColor._
import scala.sys.process.Process

object benchmarks {

  lazy val collectBenchmarks = taskKey[List[Benchmark]]("Collects all benchmarks and their arguments")
  lazy val buildBenchmarks = taskKey[List[Benchmark]]("Compiles the benchmark and returns the list of names")
  lazy val bench = taskKey[Unit]("Run the benchmarks")

  case class Benchmark(name: String, args: List[String])

  lazy val collect = Def.task {

    val benchmarksFolder = (ThisBuild / baseDirectory).value / "examples" / "benchmarks"

    def isEffektFile(file: File): Boolean =
      file.baseAndExt._2 == "effekt"

    // given a list of lines, reads out the whitespace-separated arguments in the first line
    def readArguments(lines: List[String]): List[String] =
      lines.headOption.getOrElse {
        sys error "arguments to a benchmark needs to be at least one line containing potentially multiple (whitespace separated) arguments"
      }.split(" ").toList

    def createBenchmark(file: File): Benchmark = {
      val base = file.base
      val argLines = IO.readLines(benchmarksFolder / s"${base}.args")
      Benchmark(base, readArguments(argLines))
    }

    IO.listFiles(benchmarksFolder)
      .toList
      .filter(isEffektFile)
      .map(createBenchmark)
  }

  lazy val build = Def.taskDyn {
    // Ensure that the project is compiled before running the benchmark
    (Compile / compile).value

    val benchmarks = collectBenchmarks.value

    val buildTasks = benchmarks.map { benchmark =>
        val args = s" --backend js --out ./out --build examples/benchmarks/${benchmark.name}.effekt"
        val task = (Compile / run).toTask(args)
        task.map { _ => benchmark }
      }

    flattenTasks(buildTasks)
  }

  lazy val measure = Def.task {

      val outputFolder = (ThisBuild / baseDirectory).value / "out"
      val resultsFile  = outputFolder / "results.csv"

      val benchmarks = buildBenchmarks.value

      println(s"\n${WHITE_B}Running benchmarks${RESET}")
      val runs = benchmarks.map {
        case Benchmark(name, args) => s"./out/${name} ${args.mkString(" ")}"
      }
      Process("hyperfine", List("--export-csv", resultsFile.toString) ++ runs).!

      // read back benchmark csv
      val results = IO.readLines(resultsFile).tail // drop first header line

      def millis(s: String) = s.toDouble * 1000

      // print report
      println(s"\n${WHITE_B}Results${RESET}")
      (benchmarks zip results.map(_.split(",").toList)).foreach {
        case (Benchmark(name, args), List(command, mean, stddev, median, user, system, min, max)) =>
          printf(s"  ${BOLD}%20s${RESET} %9.2f ms ± %5.2f ms\n", name, millis(mean), millis(stddev))
      }
    }

  // https://stackoverflow.com/questions/61055562/evaluating-a-list-of-tasks-inside-of-an-sbt-task
  def flattenTasks[A](tasks: Seq[Def.Initialize[Task[A]]]): Def.Initialize[Task[List[A]]] =
    tasks.toList match {
      case Nil => Def.task { Nil }
      case x :: xs => Def.taskDyn { flattenTasks(xs) map (x.value :: _) }
    }

}

