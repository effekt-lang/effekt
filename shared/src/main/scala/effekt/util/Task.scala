package effekt.util

import effekt.context.Context
import effekt.util.messages.FatalPhaseError

import scala.collection.mutable

trait Task[In, Out] { self =>

  /**
   * The name of this task
   */
  def taskName: String

  /**
   * The result indicates whether running this task was successful.
   *
   * Error messages are written to the context
   *
   * Can throw FatalPhaseError to abort execution of the task
   */
  def run(input: In)(implicit C: Context): Option[Out]

  /**
   * Apply this task
   */
  def apply(input: In)(implicit C: Context): Option[Out] = try {
    run(input)
  } catch {
    case FatalPhaseError(msg) =>
      C.error(msg)
      None
  }

  /**
   * Helper method to find the currently implicit context
   */
  def Context(implicit ctx: Context): Context = ctx

  /**
   * sequentially composes two tasks
   */
  def andThen[Out2](other: Task[Out, Out2]): Task[In, Out2] = new Task[In, Out2] {
    val taskName = s"${self.taskName} -> ${other.taskName}"

    def run(input: In)(implicit C: Context): Option[Out2] =
      self.run(input).flatMap(other.run)
  }

  def cached: Task[In, Out] = new CachedTask(this)
}

class CachedTask[In, Out](task: Task[In, Out]) extends Task[In, Out] {
  val taskName = task.taskName

  private val cache: mutable.Map[In, Option[Out]] = mutable.Map.empty

  def run(input: In)(implicit C: Context): Option[Out] =
    cache.getOrElseUpdate(input, task.run(input))
}
