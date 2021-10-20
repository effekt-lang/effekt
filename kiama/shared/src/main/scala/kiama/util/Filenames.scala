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

/**
 * Support code for handling filenames.
 */
object Filenames {

  import java.io.File.separatorChar
  import java.lang.System.getProperty

  /**
   * Return a simplified filename where a string has been dropped if it
   * occurs as a prefix of the given filename. The system separator
   * character is also dropped if it occurs immediately after a
   * non-empty prefix.
   */
  def dropPrefix(filename: String, prefix: String): String = {

    def dropIgnoreSep(i: Int): String =
      if ((i == 0) || ((i == 1) && (filename(0) == separatorChar)))
        filename
      else if (i < filename.length)
        filename.drop(if (filename(i) == separatorChar) i + 1 else i)
      else
        ""

    if (filename.startsWith(prefix))
      dropIgnoreSep(prefix.length)
    else
      filename

  }

  /**
   * Return a simplified filename where the current path has been dropped
   * if it occurs as a prefix of the given filename.
   */
  def dropCurrentPath(filename: String): String =
    dropPrefix(filename, cwd())

  /**
   * Return a simplified filename where any directory part has been dropped.
   */
  def dropDirectory(filename: String): String =
    filename.lastIndexOf(separatorChar) match {
      case -1 =>
        filename
      case index =>
        filename.substring(index + 1)
    }

  /**
   * Return a temporary file name based on the current time. Append the
   * suffix (default: nothing).
   */
  def makeTempFilename(suffix: String = ""): String = {
    val tmpDir = getProperty("java.io.tmpdir")
    val currentTime = java.lang.System.currentTimeMillis()
    s"${tmpDir}${separatorChar}kiama${currentTime}${suffix}"
  }

  /**
   * Replace the extension of a filename with a new extension. E.g.
   * if the new extension is ".ll" and the filename is "foo.c", you
   * get "foo.ll". If the filename has no extension then the new
   * extension is just appended.
   */
  def replaceExtension(filename: String, newext: String): String = {
    (if (filename.lastIndexOf(".") >= 0)
      filename.substring(0, filename.lastIndexOf('.'))
    else
      filename) + newext
  }

  /**
   * Return the current working directory.
   */
  def cwd(): String =
    getProperty("user.dir")

}
