package effekt

import effekt.source.Span
import effekt.util.messages.{Diagnostic, Label, Message, Severity}
import kiama.util.{Range, Severities}

/** Shim converting between Effekt's and Kiama's messages (for the REPL and so on) */
object KiamaMessages {

  def toKiama(severity: Severity): Severities.Severity = severity match {
    case Severity.Error   => Severities.Error
    case Severity.Warning => Severities.Warning
    case Severity.Info    => Severities.Information
    case Severity.Hint    => Severities.Hint
  }

  def fromKiama(severity: Severities.Severity): Severity = severity match {
    case Severities.Error       => Severity.Error
    case Severities.Warning     => Severity.Warning
    case Severities.Information => Severity.Info
    case Severities.Hint        => Severity.Hint
  }

  case class KiamaMessage(diagnostic: Diagnostic) extends kiama.util.Message {
    def severity: Severities.Severity = toKiama(diagnostic.severity)
    def range: Option[Range] = diagnostic.range
  }

  /** [[kiama.util.Messaging]] over [[KiamaMessage]], rendering through Effekt's custom messaging. */
  class KiamaMessaging(effekt: util.messages.EffektMessaging) extends kiama.util.Messaging[KiamaMessage] {

    def message(range: Option[Range], content: String, severity: Severities.Severity): KiamaMessage = {
      val span = range.flatMap { r =>
        for {
          from <- r.from.source.positionToOffset(r.from)
          to   <- r.to.source.positionToOffset(r.to)
        } yield Span(r.from.source, from, to)
      }
      KiamaMessage(Diagnostic(fromKiama(severity), span.map(Label(_)), Message(content)))
    }

    def formatContent(msg: KiamaMessage): String = effekt.formatContent(msg.diagnostic)

    override def formatMessages(messages: Messages): String =
      effekt.formatMessages(messages.map(_.diagnostic))
  }
}
