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
 * A memoiser that can store arbitrary values of type `U` under keys of
 * type `T`. The behaviour of the memoiser can be adjusted by selecting
 * an appropriate type.
 */
class Memoiser[T, U] {
  import java.util.IdentityHashMap
  import scala.jdk.CollectionConverters._

  val map: IdentityHashMap[T, U] = new IdentityHashMap

  /**
   * Get the value stored at key `t` or return null if no value.
   */
  def apply(t: T): U = map.get(t)

  /**
   * Duplicate an entry if possible. If `t1` has a memoised value associated
   * with it, set the value associated with `t2` to the same value. If there
   * is no value associated with `t1`, do nothing.
   */
  def dup(t1: T, t2: T): Unit =
    get(t1) foreach { v => put(t2, v) }

  /**
   * Return the value stored at key `t` as an option.
   */
  def get(t: T): Option[U] =
    Option(map.get(t))

  /**
   * Return the value stored at key `t` if there is one, otherwise
   * return `u`. `u` is only evaluated if necessary.
   */
  def getOrDefault(t: T, u: => U): U =
    get(t).getOrElse(u)

  /**
   * Has the value at `t` already been computed or not? By default, does
   * the memo table contain a value for `t`?
   */
  def hasBeenComputedAt(t: T): Boolean =
    map.containsKey(t)

  /**
   * A view of the set of keys that are currently in this memo table.
   */
  def keys: Vector[T] = map.keySet().asScala.toVector

  /**
   * Store the value `u` under the key `t`.
   */
  def put(t: T, u: U): Unit =
    map.put(t, u)

  /**
   * Store the value `u` under the key `t` if `t` does not already have an
   * associated value.
   */
  def putIfAbsent(t: T, u: U): Unit =
    if (!hasBeenComputedAt(t)) { put(t, u) }

  /**
   * Immediately reset the memo table.
   */
  def reset(): Unit =
    map.clear()

  /**
   * Immediately reset the memo table at all values in `ts`.
   */
  def resetAllAt(ts: Seq[T]): Unit =
    for (t <- ts) {
      resetAt(t)
    }

  /**
   * Immediately reset the memo table at `t`.
   */
  def resetAt(t: T): Unit =
    map.remove(t)

  /**
   * The number of entries in the memo table.
   */
  def size(): Long =
    map.size

  /**
   * Update the value associated with `t` by applying `f` to it. If there
   * is no value currently associated with `t`, associate it with `u`. `u`
   * is only evaluated if necessary.
   */
  def updateAt(t: T, f: U => U, u: => U): Unit =
    put(t, get(t).map { u => f(u) }.getOrElse(u))

  /**
   * A view of the set of values that are currently in this memo table.
   */
  def values: Vector[U] = map.values.asScala.toVector
}

/**
 * Support for memoisers.
 */
object Memoiser {

  /**
   * Make a new memoiser that weakly holds onto its keys and uses object
   * identity to compare them.
   */
  def makeIdMemoiser[T, U](): Memoiser[T, U] =
    new Memoiser

}
