package effekt

import effekt.context.Context
import effekt.util.messages.FatalPhaseError

import scala.collection.mutable

trait Phase[In, Out] { self =>

  /**
   * The name of this phase
   */
  def phaseName: String

  /**
   * Apply this phase
   */
  def apply(input: In)(implicit C: Context): Option[Out] = try {
    run(input)
  } catch {
    case FatalPhaseError(msg) =>
      C.error(msg)
      None
  }

  /**
   * The result indicates whether running this phase was successful.
   *
   * Error messages are written to the context
   *
   * Can throw FatalPhaseError to abort execution of the phase
   */
  def run(input: In)(implicit C: Context): Option[Out]

  /**
   * Helper method to find the currently implicit context
   */
  def Context(implicit ctx: Context): Context = ctx

  /**
   * sequentially composes two phases
   */
  def andThen[Out2](other: Phase[Out, Out2]): Phase[In, Out2] = new Phase[In, Out2] {
    val phaseName = s"${self.phaseName} -> ${other.phaseName}"

    def run(input: In)(implicit C: Context): Option[Out2] =
      self.run(input).flatMap(other.run)
  }

  def cached: Phase[In, Out] = new CachedPhase(this)
}

class CachedPhase[In, Out](phase: Phase[In, Out]) extends Phase[In, Out] {
  val phaseName = phase.phaseName

  private val cache: mutable.Map[In, Option[Out]] = mutable.Map.empty

  def run(input: In)(implicit C: Context): Option[Out] =
    cache.getOrElseUpdate(input, phase.run(input))
}
