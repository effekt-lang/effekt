package effekt

import effekt.context.Context
import effekt.util.{ Task, paths }
import effekt.util.messages.FatalPhaseError
import kiama.util.Source

/**
 * A Phase is *not* itself a task! Phases might _use_ tasks, but
 * per se do not perform any caching and can be re-run on an
 * input to produce a new output. They do not track dependencies.
 *
 * Conceptually, phases are like functions ...
 * To perform any interesting computation, they also receive the current compiler [[Context]].
 * They can also fatally abort, which is converted to an error and the phase returns None
 *
 * Caching intermediate "micro-phases" somehow leads to inconsistencies
 * and should be avoided (until we know exactly why).
 */
trait Phase[In, Out] { curr =>

  val phaseName: String

  def run(input: In)(implicit C: Context): Option[Out]

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

  def andThen[Out2](next: Phase[Out, Out2]): Phase[In, Out2] =
    new Phase[In, Out2] {
      val phaseName = s"${curr.phaseName} -> ${next.phaseName}"
      def run(input: In)(implicit C: Context) = curr.run(input).flatMap(out => next.run(out))
    }
}

/**
 * A case class witnessing that we can compute the fingerprint of T
 *
 * Used for caching tasks.
 *
 * TODO there are only two instances. Maybe remove abstraction again.
 */
trait Fingerprint[T] {
  def fingerprint(value: T): Long
}
implicit object sourceFingerprint extends Fingerprint[Source] {
  def fingerprint(source: Source) = paths.lastModified(source)
}
implicit def phaseResultFingerprint[T <: PhaseResult]: Fingerprint[T] = new Fingerprint[T] {
  def fingerprint(result: T) = paths.lastModified(result.source)
}

object Phase {
  def run[A](a: A, phases: List[Phase[A, A]])(implicit C: Context): Option[A] =
    phases.foldLeft[Option[A]](Some(a)) {
      case (prev, phase) => prev.flatMap { a => phase(a) }
    }

  def cached[From, To](name: String)(phase: Phase[From, To])(implicit cacheKey: Fingerprint[From]): Phase[From, To] = {
    object task extends Task[From, To] {
      val taskName = name
      def run(input: From)(implicit C: Context) = phase.run(input)
      def fingerprint(input: From) = cacheKey.fingerprint(input)
    }
    // the returned phase uses task.apply to lookup the results in the cache
    new Phase[From, To] {
      val phaseName = name
      def run(input: From)(implicit C: Context): Option[To] = task.apply(input)
    }
  }
}
