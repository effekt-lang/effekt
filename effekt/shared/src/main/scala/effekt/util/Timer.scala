package effekt.util

import scala.collection.mutable
import scala.io.AnsiColor.*
case class Timed(name: String, time: Double)

/**
 * Trait for timing events. Using the `timed` function, a new event can be timed.
 * The result is saved in map under a specified category with a unique identifier.
 */
trait Timers {
  /** Saves measured times under a "category" (e.g. "parser" - a phase name) together with a unique
   * identifier, e.g., a filename. This is meant to be append only.
   */
  val times: mutable.LinkedHashMap[String, mutable.ListBuffer[Timed]] = mutable.LinkedHashMap.empty

  /** Whether the `timed` function is NOP or actual takes and saves the time. */
  var timersActive: Boolean

  /**
   * Time the execution of `f` and save the result in the times database under the "category" `timerName`
   * and the event `id`.
   */
  def timed[A](timerName: String, id: String)(f: => A): A = {
    if (!timersActive) return f
    val (res, duration) = timed(f)
    times.update(timerName, times.getOrElse(timerName, mutable.ListBuffer.empty).prepend(Timed(id, duration)))
    res
  }

  /// Convenience function for timing the execution of a given function.
  private def timed[A](f: => A): (A, Double) = {
    val start = System.nanoTime()
    val res = f
    val duration = (System.nanoTime() - start) * 1e-6
    (res, duration)
  }

  def timesToString(): String = {
    val buffer = StringBuilder()
    val totalTimeSpent = times.foldLeft(0d) { (acc, values) =>
      acc + values._2.foldLeft(0d)((acc, timed) => acc + timed.time)
    }
    for (((name, ts), i) <- times.zipWithIndex) {
      buffer ++= s"$UNDERLINED$BOLD$i. $name$RESET:\n"
      val totalsubtime = ts.foldLeft(0d)((acc, timed) => acc + timed.time)
      for (Timed(id, time) <- ts) {
        val id1 = if (id.isEmpty) "<repl>" else id
        buffer ++= s"${" ".repeat(4)}"
        buffer ++= f"$id1: ${time}%.2f ms\n"
      }
      buffer ++= s"\n${" ".repeat(4)}"
      buffer ++= f"${UNDERLINED}Total$RESET: $totalsubtime%.2f ms\n"
      buffer ++= " ".repeat(4)
      buffer ++= f"${UNDERLINED}Percentage$RESET: ${(totalsubtime / totalTimeSpent) * 100}%.2f %%\n\n"
    }
    buffer ++= f"$BOLD${totalTimeSpent}%.2f ms$RESET\n"
    buffer.toString()
  }

  def timesToJSON(): String = {
    val buffer = StringBuilder("{\n")
    for ((name, ts) <- times) {
      buffer ++= s"${" ".repeat(4)}\"$name\": {\n"
      for (Timed(name, time) <- ts) {
        val name1 = if (name.isEmpty) "<repl>" else name
        buffer ++= s"${" ".repeat(8)}\"$name1\": $time,\n"
      }
      buffer ++= s"${" ".repeat(4)}},\n"
    }
    buffer ++= "}"
    buffer.toString()
  }
}
