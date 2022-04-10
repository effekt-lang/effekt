/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Trampolines. Useful for converting stack-heavy operations into heap-based
 * ones and thereby avoiding stack overflow.
 *
 * Based on code from "Stackless Scala With Free Monads", Runar Oli Bjarnason.
 */
object Trampolines {

  /**
   * A computation that produces an `A`.
   */
  sealed abstract class Trampoline[+A] {

    import scala.annotation.tailrec

    /**
     * Run this computation to produce its `A`. The key idea is that this
     * method is directly tail recursive so the Scala compiler can convert
     * it into a loop.
     */
    @tailrec
    final def runT: A =
      resume match {
        case Left(k)  => k().runT
        case Right(a) => a
      }

    @tailrec
    final def resume: Either[() => Trampoline[A], A] =
      this match {
        case Done(v) => Right(v)
        case More(k) => Left(k)
        case FlatMap(a, f) =>
          a match {
            case Done(v)       => f(v).resume
            case More(k)       => Left(() => k() flatMap f)
            case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
          }
      }

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      this match {
        case FlatMap(a, g) =>
          FlatMap(a, (x: Any) => g(x) flatMap f)
        case x =>
          FlatMap(x, f)
      }

    def map[B](f: A => B): Trampoline[B] =
      flatMap(a => Done(f(a)))

  }

  /**
   * A computation whose value is `a`.
   */
  case class Done[+A](a: A) extends Trampoline[A]

  /**
   * A computation whose value is obtained from the computation that is
   * returned by the continuation `k`.
   */
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

  /**
   * A computation whose value is obtained by first runnning `ta` then
   * passing its value to the continutation `k` to get the computation
   * that provides the final value.
   */
  case class FlatMap[A, +B](ta: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

  /**
   * A computation that returns `a` when it is eventually run.
   */
  def step[A](a: => A): Trampoline[A] =
    More(() => Done(a))

}
