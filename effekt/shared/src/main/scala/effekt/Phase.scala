package effekt

import effekt.context.Context
import effekt.util.{ Task, paths }
import effekt.util.messages.FatalPhaseError
import kiama.util.Source

/**
 * A phase in the Effekt compiler.
 *
 * We distinguish between [[Phase]]s and [[Task]]s. Phases, by default, perform no memoization
 * while task use the "build system" abstraction in [[Task]], track dependencies and
 * avoid rebuilding by memoization.
 *
 * Phases should *not* contain any state. That is, they should not have any mutable fields.
 *
 * A Phase is *not* itself a task! Phases might _use_ tasks, but per se do not perform any
 * caching and can be re-run on an input to produce a new output. They do not track dependencies.
 *
 * Tasks can be constructed by means of [[Phase.cached]].
 *
 * Conceptually, a `Phase[In, Out]` is like a function:
 *
 *     (In, Context) => Option[Out]
 *
 * To perform any interesting computation, they receive the current compiler [[Context]].
 * A phase can also abort (by throwing [[FatalPhaseError]]) -- this exception is then converted to
 * an [[Context.error]] and the phase returns [[None]].
 *
 * @note Caching intermediate "micro-phases" somehow leads to inconsistencies and should
 *       be avoided (until we know exactly why).
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

  def andThen[Out2](next: Context ?=> Out => Option[Out2]): Phase[In, Out2] =
    andThen(Phase("anonymous")(next))
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

    // The returned phase uses task.apply to lookup the results in the cache
    new Phase[From, To] {
      val phaseName = name
      def run(input: From)(implicit C: Context): Option[To] = task.apply(input)
    }
  }

  /**
   * Smart constructor for creating phases.
   *
   * The block passed to the smart constructor
   *
   *     Phase[From, To]("my-phase") { BLOCK }
   *
   * also receives the [[Context]] as an implicit argument. Using this smart constructor has the
   * advantage that we can immediately pattern match on `From` in the block.
   */
  def apply[From, To](name: String)(impl: Context ?=> From => Option[To]): Phase[From, To] = new Phase[From, To] {
    val phaseName = name
    def run(input: From)(implicit C: Context) = impl(input)
  }
}


/**
 * A type class witnessing that we can compute the fingerprint of T
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