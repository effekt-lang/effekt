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
 * Thread-safe counters. This class provides an operation that can be used
 * to generate a sequence of integer values. Instances of this class are
 * useful for generating things like unique names for generated entities.
 * The methods synchronize on the counter value so they can be called safely
 * from more than one thread.
 *
 * `init` is the initial value of the counter (default: -1).
 */
class Counter(init: Int = -1) {

  /**
   * The most recent value that was generated, or -1 if no values have
   * been generated.
   */
  private[this] var _value = init

  /**
   * Return the current value of the counter.
   */
  def value: Int =
    synchronized {
      _value
    }

  /**
   * Increment the stored value of the counter and return its new value.
   * `inc` is the amount to increment by (default: 1).
   */
  def next(inc: Int = 1): Int = {
    synchronized {
      _value = _value + inc
      _value
    }
  }

  /**
   * Reset the value, by default to the initial value of the counter.
   */
  def reset(to: Int = init): Unit = {
    synchronized {
      _value = to
    }
  }

}
