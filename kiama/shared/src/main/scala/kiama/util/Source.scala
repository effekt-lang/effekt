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
package util

import java.io.Reader

/**
 * A simple source of characters.
 */
trait Source {

  /**
   * The name of this source.
   */
  def name: String

  /**
   * The content of the source.
   */
  def content: String

  /**
   * A map of line offsets into the source character sequence and a
   * count of how many lines are present. The line offset map is
   * indexed starting at zero and contains at least one entry.
   */
  lazy val (lineStarts, charCount, lineCount) =
    content.foldLeft((Vector[Int](0), 1, 1)) {
      case ((v, chs, nls), ch) =>
        if (ch == '\n')
          (v :+ chs, chs + 1, nls + 1)
        else
          (v, chs + 1, nls)
    }

  /**
   * Return the offset after the last character of a line.
   */
  def lineFinish(line: Int) =
    if (line == lineCount) content.length else lineStarts(line) - 1

  /**
   * If the given line number is within range for this source, return
   * the content of that line, otherwise return `None`. As a special
   * case, support a line beyond the end of the input which contains
   * nothing since parsers
   */
  def optLineContents(line: Int): Option[String] = {
    if ((line >= 1) && (line <= lineCount))
      Some(content.substring(lineStarts(line - 1), lineFinish(line)))
    else if (line == lineCount + 1)
      Some("")
    else
      None
  }

  /**
   * Convert an offset into the content into a position.
   */
  def offsetToPosition(offset: Int): Position =
    lineStarts.lastIndexWhere(offset >= _) match {
      case -1 =>
        Position(0, 0, this)
      case line =>
        Position(line + 1, offset - lineStarts(line) + 1, this)
    }

  /**
   * If the position is valid for this source, return the corresponding
   * offset into the content, otherwise return `None`.
   */
  def positionToOffset(position: Position): Option[Int] = {
    val line = position.line
    if ((line >= 1) && (line <= lineCount)) {
      val lineStart = lineStarts(line - 1)
      val column = position.column
      if ((column >= 1) && (column <= lineFinish(line) - lineStart + 1))
        Some(lineStart + column - 1)
      else
        None
    } else
      None
  }

  /**
   * Return a reader on this source. Not normally used by Kiama but
   * useful if you want to use a source with other code that requires
   * a reader.
   */
  def reader: Reader

  /**
   * Run a function using this source as a file. The content of the source will be
   * in the file and the name of the file will be passed to `fn`.
   */
  def useAsFile[T](fn: String => T): T

}

/**
 * A source that is a string.
 */
case class StringSource(content: String, name: String = "") extends Source {

  def reader: Reader = IO.stringreader(content)

  def useAsFile[T](fn: String => T): T = {
    val filename = Filenames.makeTempFilename(name)
    IO.createFile(filename, content)
    val t = fn(filename)
    IO.deleteFile(filename)
    t
  }

}

/**
 * A source that is a named file.
 */
case class FileSource(name: String, encoding: String = "UTF-8") extends Source {

  val shortName = Filenames.dropCurrentPath(name)

  lazy val content = scala.io.Source.fromFile(name, encoding).mkString

  def reader: Reader = IO.filereader(name, encoding)

  def useAsFile[T](fn: String => T): T =
    fn(name)

}
