package effekt.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.AnsiColor.*
case class Timed(name: String, time: Double)

/**
 * Trait for timing events. Using the `timed` function, a new event can be timed.
 * The result is saved in map under a specified category with a unique identifier.
 */
trait Timers {
  /**
   * Saves measured times under a "category" (e.g. "parser" - a phase name) together with a unique
   * identifier, e.g., a filename. This is meant to be append only.
   */
  val times: mutable.LinkedHashMap[String, mutable.ListBuffer[Timed]] = mutable.LinkedHashMap.empty

  /** Whether the `timed` function is NOP or actual takes and saves the time. */
  var timersActive: Boolean

  def totalTime: Option[Double] = times.get("total").map(_.head.time)

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

  /**
   * Convenience function for timing the execution of a given function.
   */
  private def timed[A](f: => A): (A, Double) = {
    val start = System.nanoTime()
    val res = f
    val duration = (System.nanoTime() - start) * 1e-6
    (res, duration)
  }

  def timesToString(): String = {
    val spacetab = " ".repeat(4)
    val totalTimeSpent = totalTime.getOrElse {
      times.foldLeft(0d) { (acc, values) =>
        acc + values._2.foldLeft(0d)((acc, timed) => acc + timed.time)
      }
    }
    times.zipWithIndex.map { case ((name, ts), i) =>
      val totalsubtime = ts.foldLeft(0d)((acc, timed) => acc + timed.time)
      val subs = ts.map { case Timed(subname, time) =>
        val subname1 = if (subname.isEmpty) "<repl>" else subname
        f"$subname1: ${time}%.2f ms"
      }.mkString(spacetab, s",\n$spacetab", "")
      f"""$UNDERLINED$BOLD${i + 1}. $name$RESET:
         |$subs
         |$spacetab${UNDERLINED}Total$RESET: $totalsubtime%.2f ms
         |$spacetab${UNDERLINED}Percentage$RESET: ${(totalsubtime / totalTimeSpent) * 100}%.2f %%
         |""".stripMargin
    }.mkString("")
  }

  def timesToJSON(): String = {
    val spacetab = " ".repeat(4)
    val entries = times.map { (name, ts) =>
      val subs = ts.map { case Timed(subname, time) =>
        val subname1 = if (subname.isEmpty) "<repl>" else subname
        f"\"$subname1\": $time%.2f"
      }.mkString(spacetab.repeat(2), s",\n${spacetab.repeat(2)}", "")
      s"$spacetab\"$name\": {\n$subs\n$spacetab}"
    }.mkString(",\n")
    s"{\n$entries\n}\n"
  }
}
