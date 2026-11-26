package effekt
package typer

import effekt.util.messages.Failure

import scala.annotation.tailrec

enum Outcome[+A, +R] {
  case Resolved(choice: A, result: R)
  case Ambiguous(fits: List[A])
  // the fits and the undecided candidates of the deciding scope
  case Undecided(competitors: List[(A, Option[Failure])])
  // the candidates of every scope, innermost scope first
  case Failed(tried: List[(A, Failure)])
}

/**
 * From the innermost scope outwards, the first scope with an accepted or undecided candidate decides.
 * A scope's candidates are tried only when the search reaches it.
 */
def resolveOverloaded[A, R](scopes: Iterator[List[A]])(attempt: A => Either[Failure, R]): Outcome[A, R] = {
  @tailrec
  def go(failed: List[(A, Failure)]): Outcome[A, R] =
    if (!scopes.hasNext) Outcome.Failed(failed) else {
      val tried = scopes.next().map { a => (a, attempt(a)) }
      val fits = tried.collect { case (a, Right(result)) => (a, result) }
      val undecided = tried.collect { case (a, Left(failure: Failure.Undecided)) => (a, failure) }

      (fits, undecided) match {
        case (Nil, Nil) => go(failed ++ tried.collect { case (a, Left(failure)) => (a, failure) })
        case (List((a, result)), Nil) => Outcome.Resolved(a, result)
        case (fits, _) if fits.size > 1 => Outcome.Ambiguous(fits.map(_._1))
        case (fits, undecided) =>
          Outcome.Undecided(fits.map { case (a, _) => (a, None) } ++ undecided.map { case (a, failure) => (a, Some(failure)) })
      }
    }
  go(Nil)
}
