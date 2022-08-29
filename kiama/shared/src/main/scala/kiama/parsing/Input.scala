/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package parsing

import kiama.util.Source

/**
 * The input consumed by a parser.
 */
case class Input(source: Source, offset: Int) {

  import kiama.util.Position

  /**
   * Are we at the end of the input?
   */
  val atEnd: Boolean =
    offset == source.content.length

  /**
   * The first character of the input if we are not at the end.
   */
  val first: Option[Char] =
    if (atEnd)
      None
    else
      Some(source.content.charAt(offset))

  /**
   * Return a formatted description of this input.
   */
  def format: String =
    s"${found} (${position.line},${position.column})"

  /**
   * Return a description of the current character found in the input,
   * either the actual character is there is one, or "end of source" if
   * we are at the end.
   */
  val found: String =
    if (offset == source.content.length)
      "end of source"
    else
      s"'${source.content.charAt(offset)}'"

  /**
   * Return the current position of the input.
   */
  val position: Position =
    source.offsetToPosition(offset)

  /**
   * Return the next position of the input.
   */
  val nextPosition: Position =
    source.offsetToPosition(offset + 1)

  /**
   * The rest of the input, unchanged if already at end.
   */
  def rest: Input =
    if (atEnd)
      this
    else
      Input(source, offset + 1)

}
