package effekt

import effekt.context.{ Context, VirtualFileSource, VirtualModuleDB }
import effekt.generator.JavaScriptVirtual
import effekt.util.messages.FatalPhaseError
import effekt.util.paths._
import kiama.util.{ Message, Messaging, Position, Positions, Severities, Source, StringSource }

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation._

// the LSP types
// https://github.com/microsoft/vscode-languageserver-node/blob/master/types/src/main.ts
object lsp {

  // General Messages
  // -----------

  class RequestMessage[T](val id: Int, val method: String, val params: T) extends js.Object
  class NotificationMessage[T](val method: String, val params: T) extends js.Object
  class ResponseError(val code: Int, val message: String, val data: Any) extends js.Object
  class ResponseMessage[T](val id: Int, val method: String, val params: T, val error: ResponseError = null) extends js.Object

  // Positions
  // ---------

  // for now we do not stick to the protocol and use simple filenames
  type DocumentUri = String

  // LSP positions are zero indexed (for both line and character)
  class Position(val line: Int, val character: Int) extends js.Object
  class Range(val start: Position, val end: Position) extends js.Object
  class Location(val uri: DocumentUri, val range: Range) extends js.Object

  // Diagnostics
  // -----------
  class Diagnostic(val range: Range, val severity: DiagnosticSeverity, val message: String) extends js.Object {
    val source = "effekt"
  }
  type DiagnosticSeverity = Int
  object DiagnosticSeverity {
    val Error = 1
    val Warning = 2
    val Information = 3
    val Hint = 4
  }

}

/**
 * A language server to be run in a webworker
 */
class LanguageServer extends Intelligence {

  val positions: Positions = new Positions

  object config extends EffektConfig

  implicit object context extends Context(positions) with VirtualModuleDB {
    /**
     * Don't output amdefine module declarations
     */
    override def Backend(implicit C: Context) = JavaScriptVirtual
  }

  object messaging extends Messaging(positions)

  context.setup(config)

  var source = StringSource("")

  @JSExport
  def infoAt(path: String, pos: lsp.Position): String = {
    val p = fromLSPPosition(pos, VirtualFileSource(path))
    for {
      (tree, sym) <- getSymbolAt(p)
      info <- getInfoOf(sym)
    } yield info.fullDescription
  }.orNull

  @JSExport
  def typecheck(path: String): js.Array[lsp.Diagnostic] = {
    context.buffer.clear()
    context.runFrontend(VirtualFileSource(path))
    context.buffer.get.distinct.map(messageToDiagnostic).toJSArray
  }

  @JSExport
  def writeFile(path: String, contents: String): Unit =
    file(path).write(contents)

  @JSExport
  def readFile(path: String): String =
    file(path).read

  @JSExport
  def lastModified(path: String): Long =
    file(path).lastModified

  @JSExport
  def compileFile(path: String): String = {
    val Compiled(main, outputFiles) = context.compileWhole(VirtualFileSource(path)).getOrElse {
      throw js.JavaScriptException(s"Cannot compile ${path}")
    }
    outputFiles.foreach { case (path, doc) => saveOutput(path, doc.layout) }
    main
  }

  private def saveOutput(path: String, js: String)(implicit C: Context): Unit =
    file(path).write(js)

  private def messageToDiagnostic(m: Message) = {
    val from = messaging.start(m).map(convertPosition).getOrElse(null)
    val to = messaging.finish(m).map(convertPosition).getOrElse(null)
    new lsp.Diagnostic(new lsp.Range(from, to), convertSeverity(m.severity), m.label)
  }

  private def convertPosition(p: Position): lsp.Position =
    new lsp.Position(p.line - 1, p.column - 1)

  private def fromLSPPosition(p: lsp.Position, source: Source): Position =
    Position(p.line + 1, p.character + 1, source)

  private def convertSeverity(s: Severities.Severity): lsp.DiagnosticSeverity = s match {
    case Severities.Error       => lsp.DiagnosticSeverity.Error
    case Severities.Warning     => lsp.DiagnosticSeverity.Warning
    case Severities.Information => lsp.DiagnosticSeverity.Information
    case Severities.Hint        => lsp.DiagnosticSeverity.Hint
  }

  @JSExport
  def updateContents(input: String) = {
    source = StringSource(input)
  }
}

@JSExportTopLevel("effekt")
object Effekt {
  @JSExport
  def LanguageServer(): LanguageServer = new LanguageServer()
}
