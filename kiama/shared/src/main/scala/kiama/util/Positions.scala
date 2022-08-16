/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Record of a source position at a particular line and column relative to
 * a given source. The line and column numbers are one-indexed.
 */
case class Position(line: Int, column: Int, source: Source) {

  /**
   * Format this position. The result is of the form `/foo/bar.txt:2:10:` if
   * a source is associated with the position and the source has a name, or
   * of the form `2:10:` otherwise. In each case the numbers are line followed
   * by column.
   */
  lazy val format: String = {
    val name = if (source.name == "") "" else s"${source.name}:"
    s"$name$line:$column:"
  }

  /**
   * Turn this position into a string that summarises the context of the input
   * referred to by the position. If the position has a source that provides
   * access to its lines then the context is the line containing the position
   * followed by a line containing a caret pointer. Otherwise, return `None`.
   */
  lazy val optContext: Option[String] =
    source.optLineContents(line).map(s => s"$s\n${" " * (column - 1)}^")

  /**
   * Return the offset that this position refers to in its source. `None`
   * is returned if the position is not valid for its source.
   */
  lazy val optOffset: Option[Int] =
    source.positionToOffset(this)

  /**
   * Apply a binary Boolean operation to this position and another one,
   * as offsets, return the result.
   */
  def op2(op: (Int, Int) => Boolean, p: Position): Boolean =
    (optOffset, p.optOffset) match {
      case (Some(l), Some(r)) =>
        op(l, r)
      case (l, r) =>
        false
    }

  /**
   * Does this position occur no later than `p`? The two positions
   * are assumed to refer to the same source. False is returned if one
   * of the positions is invalid.
   */
  def <=(p: Position): Boolean =
    op2(_ <= _, p)

  /**
   * Does this position occur before `p`? The two positions are assumed
   * to refer to the same source. False is returned if one of the
   * positions is invalid.
   */
  def <(p: Position): Boolean =
    op2(_ < _, p)

  /**
   * Does this position occur between two other positions, in the same
   * source, inclusive of start position and exclusive of finish position?
   */
  def between(start: Position, finish: Position): Boolean =
    (start <= this) && (this < finish)

}

case class Range(from: Position, to: Position)

object Range {
  def empty(source: Source): Range = Range(Position(1, 1, source), Position(1, 1, source))
}

/**
 * Record of source positions that correspond to program elements.
 */
class Positions {

  import kiama.util.Memoiser.makeIdMemoiser

  /**
   * Map between a value and a source code position.
   */
  type PositionMap = Memoiser[Any, Position]

  /**
   * Map between value and starting position.
   */
  private val startMap = makeIdMemoiser[Any, Position]()

  /**
   * Map between value and finishing position.
   */
  private val finishMap = makeIdMemoiser[Any, Position]()

  /**
   * Get the optional start position of `t`. If it doesn't have
   * one, return `None`.
   */
  def getStart[T](t: T): Option[Position] =
    startMap.get(t)

  /**
   * Get the optional finish position of `t`. If it doesn't have one,
   * return `None`.
   */
  def getFinish[T](t: T): Option[Position] =
    finishMap.get(t)

  /**
   * Set the start position of `t` to `p` if it has not already been set.
   */
  def setStart[T](t: T, p: Position): Unit = {
    startMap.putIfAbsent(t, p)
  }

  /**
   * Set the `finish` position of `t` to `p` if it has not already been set.
   */
  def setFinish[T](t: T, p: Position): Unit = {
    finishMap.putIfAbsent(t, p)
  }

  /**
   * Set all positions of `t` to `p`.
   */
  def setAllPositions[T](t: T, p: Position): Unit = {
    setStart(t, p)
    setFinish(t, p)
  }

  /**
   * Set the start and finish positions of `t` to the positions of `a`
   * if it has them. Return `t`.
   */
  def dupPos[T](a: Any, t: T): T = {
    startMap.dup(a, t)
    finishMap.dup(a, t)
    t
  }

  /**
   * Set the start and finish positions of `t` to the start positions of `a`
   * and the finish position of `b` if they have them. Return `t`.
   */
  def dupRangePos[T](a: Any, b: Any, t: T): T = {
    startMap.dup(a, t)
    finishMap.dup(b, t)
    t
  }

  /**
   * Get the range for a given element `t`.
   */
  def getRange(t: Any): Option[Range] =
    getStart(t).map { from =>
      val to = getFinish(t).getOrElse(from)
      Range(from, to)
    }

  /**
   * Reset the position maps to be empty.
   */
  def reset(): Unit = {
    startMap.reset()
    finishMap.reset()
  }

  /**
   * Reset the position maps to be empty at all values in `ts`.
   */
  def resetAllAt(ts: Seq[Any]): Unit = {
    startMap.resetAllAt(ts)
    finishMap.resetAllAt(ts)
  }

  /**
   * Reset the position maps to be empty at `t`.
   */
  def resetAt(t: Seq[Any]): Unit = {
    startMap.resetAt(t)
    finishMap.resetAt(t)
  }

  /**
   * Get the source text associated with the substring of a source
   * between given starting and finishing positions. The two positions
   * are assumed to reference the same source. If either of the
   * positions doesn't refer to a valid offset in the source then
   * `None` is returned.
   */
  def substring(s: Position, f: Position): Option[String] =
    (s.optOffset, f.optOffset) match {
      case (Some(soffset), Some(foffset)) =>
        Some(s.source.content.substring(soffset, foffset))
      case _ =>
        None
    }

  /**
   * If `t` has valid start and finish positions, return the source text
   * associated with `t`. Otherwise, return `None`. It is assumed that
   * the start and finish positions (if present) both refer to the same
   * source.
   */
  def textOf[T](t: T): Option[String] =
    (getStart(t), getFinish(t)) match {
      case (Some(start), Some(finish)) =>
        substring(start, finish)
      case _ =>
        None
    }

  /**
   * Find all of the given nodes whose range contains `position`.
   */
  def findNodesContaining[T](nodes: Vector[T], position: Position): Vector[T] =
    nodes.collect(t =>
      (getStart(t), getFinish(t)) match {
        case (Some(start), Some(finish)) if position.between(start, finish) =>
          t
      })

  /**
   * Indent text to the same nesting level as a value. The required spacing
   * given by the column of the value is placed after each newline in the
   * string. If the value has no position, then the original string is
   * returned.
   */
  def indent[T](text: String, t: T): String =
    getStart(t) match {
      case Some(start) =>
        text.replace("\n", "\n" + " " * (start.column - 1))
      case None =>
        text
    }

}
