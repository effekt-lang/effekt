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
 *       be avoided (The problem is that Typer performs backtracking; caching could potentially
 *       interact with the backtracking behavior).
 */
trait Phase[-In, +Out] { curr =>

  val phaseName: String

  def run(input: In)(using C: Context): Option[Out]

  def apply(input: In)(using C: Context): Option[Out] = try {
    run(input)
  } catch {
    case FatalPhaseError(msg) =>
      C.report(msg)
      None
  }

  def andThen[Out2](next: Phase[Out, Out2]): Phase[In, Out2] =
    new Phase[In, Out2] {
      val phaseName = s"${curr.phaseName} -> ${next.phaseName}"
      def run(input: In)(using C: Context) = curr.run(input).flatMap(out => next.run(out))
    }

  def map[Out2](f: Context ?=> Out => Out2): Phase[In, Out2] = new Phase[In, Out2] {
    val phaseName = curr.phaseName
    def run(input: In)(using C: Context) = curr.run(input).map(f)
  }
}

object Phase {
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
  def apply[In, Out](name: String)(impl: Context ?=> In => Out): Phase[In, Out] = new Phase[In, Out] {
    val phaseName = name
    def run(input: In)(using C: Context): Option[Out] = Some(impl(input))
  }

  def run[A](a: A, phases: List[Phase[A, A]])(using C: Context): Option[A] =
    phases.foldLeft[Option[A]](Some(a)) {
      case (prev, phase) => prev.flatMap { a => phase(a) }
    }

  /**
   * Constructs a cached version of this phase. The
   */
  def cached[From, To](name: String, cacheBy: From => Long)(phase: Phase[From, To]): Phase[From, To] = {
    object task extends Task[From, To] {
      val taskName = name
      def run(input: From)(using C: Context) = phase.run(input)
      def fingerprint(input: From) = cacheBy(input)
    }

    // The returned phase uses task.apply to lookup the results in the cache
    new Phase[From, To] {
      val phaseName = name
      def run(input: From)(using C: Context): Option[To] = task.apply(input)
    }
  }

  /**
   * The most common use case: using the lastModified timestamp on the Source as cache key.
   */
  def cached[To](name: String)(phase: Phase[Source, To]): Phase[Source, To] =
    cached(name, cacheBy = paths.lastModified) { phase }
}
