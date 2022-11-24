package effekt

import effekt.context.{ Context, VirtualFileSource, VirtualModuleDB }
import effekt.generator.js.JavaScriptVirtual
import effekt.lifted.LiftInference
import effekt.util.{ PlainMessaging, getOrElseAborting }
import effekt.util.messages.{ BufferedMessaging, EffektError, EffektMessaging, FatalPhaseError }
import effekt.util.paths.*
import kiama.util.{ Messaging, Position, Positions, Severities, Source, StringSource }

import scala.scalajs.js
import js.JSConverters.*
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
    /**
     * Don't output amdefine module declarations
     */
    override def Backend(implicit C: Context) = JavaScriptVirtual

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
    context.runFrontend(VirtualFileSource(path))
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
    val (mainOutputPath, mainCore) = compileCached(VirtualFileSource(path)).getOrElseAborting {
      throw js.JavaScriptException(s"Cannot compile ${path}")
    }
    try {
      context.checkMain(mainCore.mod)
      mainCore.mod.dependencies.foreach { dep => compileCached(dep.source) }
      mainOutputPath
    } catch {
      case FatalPhaseError(msg) =>
        val formattedError = context.messaging.formatMessage(msg)
        throw js.JavaScriptException(formattedError)
    }
  }

  @JSExport
  def showCore(path: String): String = {
    val (mainOutputPath, mainCore) = compileCached(VirtualFileSource(path)).getOrElseAborting {
      return null
    }
    core.PrettyPrinter.format(mainCore.core.definitions)
  }

  @JSExport
  def showLiftedCore(path: String): String = {
    val (mainOutputPath, mainCore) = compileCached(VirtualFileSource(path)).getOrElseAborting {
      return null
    }

    val liftedCore = LiftInference.run(mainCore).getOrElseAborting {
      return null
    }

    lifted.PrettyPrinter.format(liftedCore.core.definitions)
  }

  @JSExport
  def inferredCaptures(path: String): js.Array[lsp.CaptureInfo] = {
    typecheck(path)
    getInferredCaptures(VirtualFileSource(path)).map {
      case (p, c) => new lsp.CaptureInfo(toLSPPosition(p), c.toString)
    }.toJSArray
  }

  /**
   * Has the side effect of saving to generated output to a file
   */
  private def compileSingle(src: Source)(implicit C: Context): Option[(String, CoreTransformed)] =
    context.compileSeparate(src).map {
      case (core, doc) =>
        val path = JavaScriptVirtual.path(core.mod)
        writeFile(path, doc.layout)
        (path, core)
    }

  // Here we cache the full pipeline for a single file, including writing the result
  // to an output file. This is important since we only want to write the file, when it
  // really changed. Writing will change the timestamp and lazy reloading of modules
  // on the JS side uses the timestamp to determine whether we need to re-eval a
  // module or not.
  private val compileCached = Phase.cached("compile-cached") {
    Phase("compile") { compileSingle }
  }

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
