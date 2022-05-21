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

import org.rogach.scallop.ScallopConf
import org.rogach.scallop.intConverter // TODO-LLVM What does this do? SBT recommended it for ln 129 ...

/**
 * Configurations for Kiama programs. `args` gives the command-line
 * arguments that are used to determine many of the configuration
 * settings.
 */
class Config(args: Seq[String]) extends ScallopConf(args) {

  import org.rogach.scallop.{ ArgType, stringListConverter, ValueConverter }

  /**
   * The string emitter to use if a '--Koutput string' option is seen.
   */
  lazy val stringEmitter = new StringEmitter

  /**
   * Make a convertor for the output option.
   */
  val outputConverter =
    new ValueConverter[Emitter] {

      val argType = ArgType.LIST

      def parse(s: List[(String, List[String])]): Either[String, Option[Emitter]] =
        s match {
          case List((_, List("file", filename))) =>
            Right(Some(new FileEmitter(filename)))
          case List((_, List("string"))) =>
            Right(Some(stringEmitter))
          case List((_, _)) =>
            Left("expected 'file name' or 'string'")
          case _ =>
            Right(None)
        }
    }

  /**
   * The emitter to use for normal output. Defaults to standard output.
   * Options are output to a string emitter and a file console where the
   * option value specifies the file name.
   */
  lazy val output = opt[Emitter](
    "Koutput",
    descr = "Emitter for program output (default: standard output)",
    default = Some(new OutputEmitter),
    noshort = true,
    hidden = true
  )(outputConverter)

  /**
   * Convertor for console options.
   */
  val consoleConverter =
    new ValueConverter[Console] {

      val argType = ArgType.LIST

      def parse(s: List[(String, List[String])]): Either[String, Option[Console]] =
        s match {
          case List((_, List("file", filename))) =>
            Right(Some(new FileConsole(filename)))
          case List((_, List("string", contents))) =>
            Right(Some(new StringConsole(contents)))
          case List((_, _)) =>
            Left("expected 'file name' or 'string value'")
          case _ =>
            Right(None)
        }
    }

  /**
   * The console to use for reading input. Defaults to a JLine console.
   * Options are a string console where the option value specifies the
   * contents, and a file console where the option value specifies the
   * file name.
   */
  lazy val console = opt[Console](
    "Kconsole",
    descr = "Console for program input (default: JLine console)",
    default = Some(JLineConsole),
    noshort = true,
    hidden = true
  )(consoleConverter)

  /**
   * Language server mode for a compiler.
   */
  lazy val server = toggle(
    "server",
    descrYes = "Run compiler as a language server",
    descrNo = "Run compiler in standard batch mode",
    default = Some(false)
  )

  /**
   * Debug operations of a compiler.
   */
  lazy val debug = toggle(
    "debug",
    descrYes = "Debug compiler operations",
    descrNo = "Don't debug compiler operations",
    default = Some(false),
    noshort = true,
    hidden = true
  )

  /**
   * The zero or more filenames that were specified positionally after all of the options.
   */
  lazy val debugPort = opt[Int]("debugPort", descr = "The port to listen to when debugging the language server",
    required = false,
    default = Some(5007),
    noshort = true,
    hidden = true)

  /**
   * The zero or more filenames that were specified positionally after all of the options.
   */
  lazy val filenames = trailArg[List[String]]("files", descr = "Input files",
    required = false,
    default = Some(Nil))

  /**
   * Handle errors by printing them, then printing the help message, then
   * exiting. All output is performed to the errors emitter.
   */
  errorMessageHandler =
    (message: String) => {
      output().emitln(s"Command-line error: $message")
      output().emitln(builder.help)
      sys.exit(1)
    }

}

/**
 * Configurations for Kiama REPLS. Adds some options to the default
 * set that all Kiama programs support.
 */
class REPLConfig(args: Seq[String]) extends Config(args) {

  /**
   * Whitespace option. If set, pass input lines that are completely white space
   * to the REPL processing. By default, these lines are ignored.
   */
  lazy val processWhitespaceLines = toggle(
    "KprocessWhitespaceLines",
    descrYes = "Process whitespace lines",
    descrNo = "Don't process whitespace lines",
    default = Some(false),
    hidden = true
  )

}
