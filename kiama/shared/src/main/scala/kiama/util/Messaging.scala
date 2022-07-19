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

/**
 * A message record consisting of a value with which the message is associated,
 * a content (potentially not yet rendered), and a severity (which defaults to error).
 */
case class Message(range: Option[Range], content: Any) {
  val severity: Severities.Severity = Severities.Error

  def name: Option[String] = source.map(_.name)
  def source: Option[Source] = from.map(_.source)
  def from: Option[Position] = range.map(_.from)
  def to: Option[Position] = range.map(_.to)
}

type Messages = Vector[Message]

/**
 * Return a value representing no messages.
 */
val noMessages = Vector[Message]()

/**
 * If `cond` is true make a singleton message list that associates the
 * label with the start position recorded for `value` (if any).  If `cond`
 * is false make an empty message list. `cond` can be omitted and defaults
 * to true.
 */
def message(range: Option[Range], content: Any, newSeverity: Severity): Messages =
  Vector(new Message(range, content) {
    override val severity = newSeverity
  })

def message(range: Range, content: Any, newSeverity: Severity = Error): Messages =
  message(Some(range), content, newSeverity)

/**
 * General facility for processing messages.
 */
class Messaging {

  import kiama.util.Severities.severityToWord
  import scala.math.Ordering._

  /**
   * An ordering on messages that uses starting position and prioritises
   * line over column. The messages are assumed to refer to the same
   * source.
   */
  implicit object messageOrdering extends Ordering[Message] {
    def compare(m1: Message, m2: Message) =
      Ordering[(Option[Int], Option[Int])].compare(
        (m1.from.map(_.line), m1.from.map(_.column)),
        (m2.from.map(_.line), m2.from.map(_.column))
      )
  }

  /**
   * Format the message for reporting as a line containing the starting
   * position and label, the input text line and line(s) containing the
   * context of the position. If no position is associated with this
   * message just format as a line containing the label.
   */
  def formatMessage(message: Message): String =
    message.from match {
      case Some(pos) =>
        val severity = severityToWord(message.severity)
        val context = pos.optContext.getOrElse("")
        s"${pos.format}$severity: ${formatContent(message.content)}\n$context\n"
      case None =>
        s"${formatContent(message.content)}\n"
    }

  /**
   * Format the message content. Should be overriden in subclasses of Messaging.
   */
  def formatContent(content: Any): String = content.toString

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
