package effekt.util

import scala.collection.mutable
import scala.io.AnsiColor.*

/**
 * A timed event.
 */
case class Timed(name: String, time: Long)

trait Timers {
  val times: mutable.Map[String, mutable.ListBuffer[Timed]] = mutable.Map.empty

  def showTimes(): Unit = {
    val totalTimeSpent: Long = times.foldLeft(0: Long) { (acc, values) =>
      acc + values._2.foldLeft(0: Long)((acc, timed) => acc + timed.time)
    }
    for ((name, ts) <- times) {
      println(s"$UNDERLINED$BOLD$name$RESET:")
      val totalsubtime = ts.foldLeft(0: Long)((acc, timed) => acc + timed.time)
      for (Timed(id, time) <- ts) {
        val id1 = if (id.isEmpty) "<repl>" else id
        print(" ".repeat(4))
        println(s"$id1: ${time * 1e-6} ms")
      }
      print(" ".repeat(4))
      println(s"${UNDERLINED}Total$RESET: ${totalsubtime * 1e-6} ms")
      print(" ".repeat(4))
      println(s"${UNDERLINED}Percentage$RESET: ${(totalsubtime.toDouble / totalTimeSpent) * 100} %")
      println()
    }
    println(s"$BOLD${totalTimeSpent * 1e-6} ms$RESET")
  }

  /// Convenience function for timing the execution of a given function.
  private def timed[A](f: => A): (A, Long) = {
    val start = System.nanoTime()
    val res = f
    val duration = System.nanoTime() - start
    (res, duration)
  }

  /**
   * Time the execution of `f` and save the result in the times database under the "category" `timerName`
   * and the event `id`.
   */
  def timed[A](timerName: String, id: String)(f: => A): A = {
    val (res, duration) = timed(f)
    times.update(timerName, times.getOrElse(timerName, mutable.ListBuffer.empty).prepend(Timed(id, duration)))
    res
  }
}

case class Stopwatch() {
  /// stopwatch start time in nano seconds.
  private var startTime: Option[Long] = None
  /// stopwatch end time in nano seconds
  private var endTime: Option[Long] = None

  def start(): Long = {
    val timestamp = System.nanoTime()
    startTime = Some(timestamp)
    endTime = None
    timestamp
  }

  def stop(): Long = {
    val timestamp = System.nanoTime()
    endTime = Some(timestamp)
    startTime.map(t => timestamp - t).getOrElse(0)
  }

  def duration: Long =
    (for {
      start <- startTime
      end <- endTime
    } yield end - start).getOrElse(0)
}

case object Stopwatch {
  def timed[A](f: => A): (A, Long) = {
    val s = Stopwatch()
    s.start()
    val res = f
    (res, s.stop())
  } 
}
