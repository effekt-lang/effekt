/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

import scala.math.Ordering._

/**
 * The severities of a message.
 */
object Severities {

  /**
   * Base class of message severities.
   */
  sealed abstract class Severity

  /**
   * A message severity that indicates a fatal problem after which later
   * phases of processing cannot proceed.
   */
  case object Error extends Severity

  /**
   * A message severity that indicates a problem that should be addressed
   * but does not inhibit later phases of processing.
   */
  case object Warning extends Severity

  /**
   * A message severity that indicates that the message provides information,
   * but does not constitute an error or warning.
   */
  case object Information extends Severity

  /**
   * A message severity that indicates that the message describes a hint
   * that the user may wish to act on.
   */
  case object Hint extends Severity

  /**
   * Convery a severity to a descriptive word for use in formatted messages.
   */
  def severityToWord(severity: Severity): String =
    severity match {
      case Error       => "error"
      case Warning     => "warning"
      case Information => "info"
      case Hint        => "hint"
    }
}

import Severities.*

trait Message {
  def severity: Severity
  def range: Option[Range]

  def startPosition: Option[Position] = range.map(_.from)
  def finishPosition: Option[Position] = range.map(_.to)
  def sourceName: Option[String] = startPosition.map(_.source.name)
}

/**
 * General facility for processing messages.
 *
 * `M` is the type of messages. It should be possible to extract severity and position
 */
trait Messaging[M <: Message] {

  type Messages = Vector[M]

  /**
   * Return a value representing no messages.
   */
  val noMessages = Vector[M]()

  /**
   * Has to be implemented by subclasses. How should strings be embedded into `M`?
   */
  def message(range: Option[Range], content: String, severity: Severity): M

  /**
   * Format the message content. Should be overridden in subclasses of Messaging.
   */
  def formatContent(msg: M): String

  def message(range: Range, content: String, severity: Severity = Error): M =
    message(Some(range), content, severity)

  /**
   * An ordering on messages that uses starting position and prioritises
   * line over column. The messages are assumed to refer to the same
   * source.
   */
  implicit object messageOrdering extends Ordering[M] {
    def compare(m1: M, m2: M) =
      Ordering[(Option[Int], Option[Int])].compare(
        (m1.startPosition.map(_.line), m1.startPosition.map(_.column)),
        (m2.startPosition.map(_.line), m2.startPosition.map(_.column))
      )
  }

  /**
   * Format the message for reporting as a line containing the starting
   * position and label, the input text line and line(s) containing the
   * context of the position. If no position is associated with this
   * message just format as a line containing the label.
   */
  def formatMessage(message: M): String =
    message.startPosition match {
      case Some(pos) =>
        val severity = severityToWord(message.severity)
        val context = pos.optContext.getOrElse("")
        s"${pos.format}$severity: ${formatContent(message)}\n$context\n"
      case None =>
        s"${formatContent(message)}\n"
    }

  /**
   * Return a string containing all the given messages sorted and formatted.
   */
  def formatMessages(messages: Messages): String =
    messages.sorted.map(formatMessage).mkString("")

  /**
   * Output the messages arising from the given source in order of position
   * using the given emitter, which defaults to terminal output.
   */
  def report(source: Source, messages: Messages, emitter: Emitter = new OutputEmitter): Unit = {
    emitter.emit(formatMessages(messages))
  }
}
