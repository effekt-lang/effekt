/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Utility wrapper for I/O to isolate Kiama code from some Java I/O details.
 */
object IO {

  import java.io.{
    BufferedReader,
    BufferedWriter,
    FileInputStream,
    FileOutputStream,
    InputStreamReader,
    OutputStreamWriter,
    StringReader
  }
  import java.nio.file.Files.deleteIfExists
  import java.nio.file.Paths.get

  /**
   * Return a new buffered reader on the file with the given name.
   * The `encoding` argument gives the character encoding of the
   * file (default: UTF-8). Throw `java.io.FileNotFoundException`
   * if the file cannot be found.
   */
  def filereader(name: String, encoding: String = "UTF-8"): BufferedReader =
    new BufferedReader(
      new InputStreamReader(
        new FileInputStream(name),
        encoding
      )
    )

  /**
   * Return a new writer reader on the file with the given name.
   * The `encoding` argument gives the character encoding of the
   * file (default: UTF-8).
   */
  def filewriter(name: String, encoding: String = "UTF-8"): BufferedWriter =
    new BufferedWriter(
      new OutputStreamWriter(
        new FileOutputStream(name),
        encoding
      )
    )

  /**
   * Return a new buffered reader on the given string.
   */
  def stringreader(string: String): BufferedReader =
    new BufferedReader(new StringReader(string))

  /**
   * Create a file with the given filename and content.
   */
  def createFile(filename: String, content: String): Unit = {
    val writer = filewriter(filename)
    writer.write(content)
    writer.close()
  }

  /**
   * Delete the file with the given filename.
   */
  def deleteFile(filename: String): Unit = {
    deleteIfExists(get(filename))
  }

}
