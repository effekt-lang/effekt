package effekt

import effekt.context.{ Context, VirtualFileSource, VirtualModuleDB }
import effekt.lifted.LiftInference
import effekt.util.{ PlainMessaging, getOrElseAborting }
import effekt.util.messages.{ BufferedMessaging, EffektError, EffektMessaging, FatalPhaseError }
import effekt.util.paths.*
import effekt.generator.js.JavaScriptWeb
import kiama.util.{ Messaging, Position, Positions, Severities, Source, StringSource }

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*

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

  // Domain Specific Messages
  // ------------------------
  class CaptureInfo(val pos: Position, val capture: String) extends js.Object

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
    object messaging extends PlainMessaging
  }

  context.setup(config)

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
    context.messaging.clear()
    context.compiler.runFrontend(VirtualFileSource(path))
    context.messaging.buffer.distinct.map(messageToDiagnostic).toJSArray
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
    val mainOutputPath = compileCached(VirtualFileSource(path)).getOrElseAborting {
      throw js.JavaScriptException(s"Cannot compile ${path}")
    }
    try {
      mainOutputPath
    } catch {
      case FatalPhaseError(msg) =>
        val formattedError = context.messaging.formatMessage(msg)
        throw js.JavaScriptException(formattedError)
    }
  }

  @JSExport
  def showCore(path: String): String = {
    val doc = context.compiler.prettyIR(VirtualFileSource(path), Stage.Core).getOrElseAborting {
      return null
    }
    doc.layout
  }

  @JSExport
  def inferredCaptures(path: String): js.Array[lsp.CaptureInfo] = {
    typecheck(path)
    getInferredCaptures(VirtualFileSource(path)).map {
      case (p, c) => new lsp.CaptureInfo(toLSPPosition(p), c.toString)
    }.toJSArray
  }

  /**
   * Has the side effect of saving the generated output to a file
   */
   object compileWhole extends Phase[Source, String] {
     val phaseName = "compile"
     def run(src: Source)(using Context) =
      context.compiler.compileWeb(src).map {
        case (files, mainPath) =>
          files.foreach { case (path, content) => writeFile(path, content) }
          mainPath
      }
  }

  // Here we cache the full pipeline for a single file, including writing the result
  // to an output file.
  private val compileCached = Phase.cached("compile-cached") { compileWhole }

  private def messageToDiagnostic(m: EffektError) = {
    val from = m.startPosition.map(toLSPPosition).orNull
    val to = m.finishPosition.map(toLSPPosition).orNull
    val text = context.messaging.formatContent(m)
    new lsp.Diagnostic(new lsp.Range(from, to), convertSeverity(m.severity), text)
  }

  private def toLSPPosition(p: Position): lsp.Position =
    new lsp.Position(p.line - 1, p.column - 1)

  private def fromLSPPosition(p: lsp.Position, source: Source): Position =
    Position(p.line + 1, p.character + 1, source)

  private def toLSPLocation(from: Position, to: Position): Option[lsp.Location] = from.source match {
    case VirtualFileSource(path) => Some(new lsp.Location(from.source.name, new lsp.Range(toLSPPosition(from), toLSPPosition(to))))
    case _ => None
  }

  private def convertSeverity(s: Severities.Severity): lsp.DiagnosticSeverity = s match {
    case Severities.Error       => lsp.DiagnosticSeverity.Error
    case Severities.Warning     => lsp.DiagnosticSeverity.Warning
    case Severities.Information => lsp.DiagnosticSeverity.Information
    case Severities.Hint        => lsp.DiagnosticSeverity.Hint
  }
}

@JSExportTopLevel("effekt")
object Effekt {
  @JSExport
  def LanguageServer(): LanguageServer = new LanguageServer()
}
