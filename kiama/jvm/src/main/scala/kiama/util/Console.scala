/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Compatibility layer for Console reading.
 */
class ConsoleBase {

  /**
   * Read a line after prompting with the given prompt.
   */
  def readLine(prompt: String): String =
    scala.io.StdIn.readLine(prompt)

}

/**
 * A console using which input data can be read from standard input.
 */
class Console extends ConsoleBase {

  /**
   * Read an integer after prompting with the given prompt.  Throws a
   * number format exception if something that is not an integer is entered.
   */
  def readInt(prompt: String): Int =
    readLine(prompt).toInt

}

/**
 * A console that provides line editing using JLine. This code follows sbt's
 * equivalent code to try to ensure that there are no incompatibilities.
 */
object JLineConsole extends Console {

  import jline.console.ConsoleReader
  import jline.Terminal

  /**
   * Return a handle for the current terminal.
   */
  def terminal: Terminal =
    jline.TerminalFactory.get

  /**
   * Run a computation that depends on the current terminal in a
   * synchromized fashion.
   */
  def withTerminal[T](f: jline.Terminal => T): T =
    synchronized {
      val t = terminal
      t.synchronized {
        f(t)
      }
    }

  /**
   * As for `withTerminal` but restores the terminal before running
   * the computation.
   */
  def usingTerminal[T](f: jline.Terminal => T): T =
    withTerminal {
      t =>
        t.restore
        f(t)
    }

  /**
   * The reader to use to access the console.
   */
  lazy val reader =
    usingTerminal {
      t =>
        val consoleReader = new ConsoleReader
        consoleReader.setExpandEvents(false)
        consoleReader.setBellEnabled(false)
        consoleReader
    }

  /**
   * Read a line under controlled conditions.  Need to do this since
   * console is a shared static resource.  In particular, it's shared
   * with sbt if run in that context.
   */
  override def readLine(prompt: String): String =
    withTerminal {
      t =>
        t.init
        try {
          reader.readLine(prompt)
        } finally {
          t.restore
        }
    }

  /**
   * Print representation for usage messages.
   */
  override def toString: String =
    "JLineConsole"

}

/**
 * A console that reads from a given buffered reader.
 */
trait ReaderConsole extends Console {

  import java.io.BufferedReader

  /**
   * The reader from which to read.
   */
  def reader: BufferedReader

  /**
   * Read a line from the file.  The prompt is ignored.
   */
  override def readLine(prompt: String): String =
    reader.readLine

}

/**
 * A console that reads from the given UTF-8 encoded file.
 */
class FileConsole(filename: String) extends ReaderConsole {

  import kiama.util.IO.filereader

  /**
   * A reader for the underlying file.
   */
  lazy val reader = filereader(filename)

}

/**
 * A console that returns from a specified string.
 */
class StringConsole(string: String) extends ReaderConsole {

  import kiama.util.IO.stringreader

  /**
   * A reader for the given string.
   */
  lazy val reader = stringreader(string)

}
