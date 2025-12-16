package effekt
package util

import scala.annotation.tailrec

enum Trampoline[A] {
  case Done(value: A)
  case More(thunk: () => Trampoline[A])

  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Done(f(a)))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case Done(a) => More(() => f(a))
    case More(next) => More(() => next().flatMap(f))
  }

  @tailrec
  final def run(): A = this match {
    case Trampoline.Done(value) => value
    case Trampoline.More(thunk) => thunk().run()
  }
}
object Trampoline {
  def done[A](value: A): Trampoline[A] = Done(value)
}
