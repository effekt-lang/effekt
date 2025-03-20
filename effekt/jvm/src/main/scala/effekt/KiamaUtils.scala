/*
 * This file is based on Kiama code.
 *
 * Copyright (C) 2018-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package effekt

import effekt.util.messages.EffektError
import kiama.util.Severities.{Error, Hint, Information, Severity, Warning}
import kiama.util.{Messaging, Position, Source}
import org.eclipse.lsp4j.Range as LSPRange
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position as LSPPosition}

object KiamaUtils {
  def messageToDiagnostic(lspMessaging: Messaging[EffektError])(message: EffektError): Diagnostic = {
    diagnostic(message.range, lspMessaging.formatContent(message), message.severity)
  }

  def diagnostic(range: Option[kiama.util.Range], message: String, severity: Severity): Diagnostic = {
    val lspRange = range.map(convertRange).getOrElse(emptyRange)
    val lspSeverity = convertSeverity(severity)
    val lspDiagnostic = new Diagnostic(lspRange, message, lspSeverity, "effekt")
    lspDiagnostic
  }

  def emptyPosition = new LSPPosition(0, 0)
  def emptyRange = new LSPRange(emptyPosition, emptyPosition)
  def emptyLocation = new Location("<no-source>", emptyRange)

  def convertPosition(optPos: Option[Position]): LSPPosition =
    optPos.map(convertPosition).getOrElse(emptyPosition)

  def convertPosition(pos: Position): LSPPosition =
    new LSPPosition(pos.line - 1, pos.column - 1)

  def convertRange(optStart: Option[Position], optFinish: Option[Position]): LSPRange =
    new LSPRange(convertPosition(optStart), convertPosition(optFinish))

  def convertRange(r: kiama.util.Range): LSPRange =
    new LSPRange(convertPosition(r.from), convertPosition(r.to))

  def rangeToLocation(r: kiama.util.Range): Location =
    new Location(r.from.source.name, convertRange(r))

  def fromLSPPosition(position: LSPPosition, source: Source): Position =
    Position(position.getLine + 1, position.getCharacter + 1, source)

  def fromLSPRange(range: LSPRange, source: Source): kiama.util.Range =
    kiama.util.Range(
      fromLSPPosition(range.getStart, source),
      fromLSPPosition(range.getEnd, source)
    )

  def convertSeverity(severity: Severity): DiagnosticSeverity =
    severity match {
      case Error       => DiagnosticSeverity.Error
      case Warning     => DiagnosticSeverity.Warning
      case Information => DiagnosticSeverity.Information
      case Hint        => DiagnosticSeverity.Hint
    }
}
