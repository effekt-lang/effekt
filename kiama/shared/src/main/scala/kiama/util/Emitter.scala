/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Class of objects that can emit text.
 */
abstract class Emitter {

  /**
   * Emit `any`.
   */
  def emit(any: Any): Unit

  /**
   * Emit `any` and start a new line.
   */
  def emitln(any: Any): Unit

  /**
   * Emit a new line.
   */
  def emitln(): Unit

  /**
   * Close this emitter. Default: do nothing.
   */
  def close(): Unit = {
  }

}

/**
 * Class of objects that can emit arbitrary output.  The output is sent
 * to standard output. Use an `ErrorEmitter` if your output is signalling
 * errors, warnings, log messages or similar.
 */
class OutputEmitter extends Emitter {

  /**
   * Emit `any`.
   */
  def emit(any: Any): Unit = {
    print(any.toString)
  }

  /**
   * Emit `any` and start a new line.
   */
  def emitln(any: Any): Unit = {
    println(any.toString)
  }

  /**
   * Emit a new line.
   */
  def emitln(): Unit = {
    println()
  }

}

/**
 * An emitter that records the output in a string that can be accessed
 * via the result method.
 */
class StringEmitter extends Emitter {
  val b = new StringBuilder
  override def emit(any: Any): Unit = { b.append(any.toString) }
  override def emitln(any: Any): Unit = { b.append(any.toString).append('\n') }
  override def emitln(): Unit = { b.append('\n') }
  def clear(): Unit = { b.clear() }
  def result(): String = b.result()
}

/**
 * A string emitter that also provides a `close` method to send the
 * result to the named UTF-8 encoded file.
 */
class FileEmitter(filename: String) extends StringEmitter {
  import kiama.util.IO.filewriter

  override def close(): Unit = {
    val out = filewriter(filename)
    out.write(result())
    out.close()
  }
}
