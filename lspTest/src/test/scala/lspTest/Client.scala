package lspTest

import scala.collection.mutable.HashMap
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout
import java.util.concurrent.TimeoutException
import org.scalablytyped.runtime.StObject
import typings.vscodeJsonrpc.libCommonMessagesMod.NotificationMessage
import typings.vscodeLanguageserverProtocol.anon.{Name, Text}
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.*
import typings.vscodeLanguageserverProtocol.mod.{CodeActionContext, FormattingOptions, Range, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier}
import typings.vscodeLanguageserverTypes.mod.Position
import typings.vscodeLanguageserverTypes.mod.ReferenceContext

import scala.collection.mutable

class Client(val connection: ProtocolConnection)(implicit ec: ExecutionContext) {
  def toURI(file: String) = s"file:///$file"

  // TODO: use different capabilities and test whether the server respects them
  def capabilities = ClientCapabilities()
    .setExperimentalUndefined
    .setGeneralUndefined
    .setNotebookDocumentUndefined
    .setTextDocumentUndefined
    .setWindowUndefined
    .setWorkspaceUndefined

  val diagnostics = mutable.HashMap[String, Promise[NotificationMessage]]()
  val requestTimeout: FiniteDuration = 10.seconds

  def timeoutFuture(waitingFor: String)(implicit ec: ExecutionContext): Future[NotificationMessage] = {
    val p = Promise[NotificationMessage]()
    setTimeout(requestTimeout.toMillis.toDouble) {
      p.failure(new TimeoutException(s"Timed out waiting for $waitingFor"))
    }
    p.future
  }

  // this is rather complex logic but is needed to prevent race conditions,
  // since relevant notifications may arrive before or after calling this function
  def waitForDiagnostics(file: String) =
    val uri = toURI(file)
    diagnostics.get(uri) match {
      case None =>
        val promise = Promise[NotificationMessage]()
        diagnostics(uri) = promise
        Future.firstCompletedOf(Seq(promise.future, timeoutFuture("diagnostics"))).andThen {
          case _ => diagnostics.remove(uri)
        }
      case Some(promise) =>
        diagnostics.remove(uri)
        Future.firstCompletedOf(Seq(promise.future, timeoutFuture("diagnostics")))
    }

  def initialize() = {
    connection.onUnhandledNotification { notification =>
      notification.method match
        case _ => assert(false, "unexpected notification")
    }
    connection.onNotification(PublishDiagnosticsNotification.`type`, (params: PublishDiagnosticsParams) => {
      val uri = params.uri
      val notification = js.Dynamic.literal(
        method = "textDocument/publishDiagnostics",
        params = params
      ).asInstanceOf[NotificationMessage]

      diagnostics.get(uri) match {
        case Some(promise) if !promise.isCompleted =>
          promise.success(notification); () // force Unit return
        case _ =>
          diagnostics(uri) = Promise[NotificationMessage]().success(notification); () // force Unit return
      }
    })

    val params = _InitializeParams(capabilities)
      .setClientInfo(Name("LSP testing client"))
      .setLocaleUndefined
      .setRootPathNull
      .setRootUriNull
      .setTraceUndefined
      .setWorkDoneTokenUndefined
      // TODO: also test showIR and showTree settings
      .setInitializationOptions(js.JSON.parse("{\"showIR\": \"none\", \"showTree\": false}"))
    // (1) send initialization request
    connection.sendRequest("initialize", params).toFuture.flatMap { (result: InitializeResult[js.Any]) =>
      // (2) on success, send initialized notification
      connection.sendNotification(InitializedNotification.`type`, StObject().asInstanceOf[InitializedParams]).toFuture.flatMap { _ =>
        Future.successful(result)
      }
    }
  }

  def shutdown() = {
    connection.sendRequest(ShutdownRequest.`type`).toFuture
  }

  def exit() = {
    connection.sendNotification(ExitNotification.`type`).toFuture
  }

  def openDocument(file: String, content: String) = {
    val document = TextDocumentItem.create(
      toURI(file),
      "effekt",
      1,
      content
    )
    val params = DidOpenTextDocumentParams(document)
    connection.sendNotification(DidOpenTextDocumentNotification.`type`, params).toFuture
  }

  def changeDocument(file: String, content: String) = {
    val params = DidChangeTextDocumentParams(
      js.Array(Text(content)), // TODO: support/use ranges
      VersionedTextDocumentIdentifier.create(toURI(file), 1)
    )
    connection.sendNotification(DidChangeTextDocumentNotification.`type`, params).toFuture
  }

  def saveDocument(file: String) = {
    val params = DidSaveTextDocumentParams(
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendNotification(DidSaveTextDocumentNotification.`type`, params).toFuture
  }

  def closeDocument(file: String) = {
    val params = DidCloseTextDocumentParams(
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendNotification(DidCloseTextDocumentNotification.`type`, params).toFuture
  }

  def requestDocumentSymbol(file: String) = {
    val params = DocumentSymbolParams(
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(DocumentSymbolRequest.`type`, params).toFuture
  }

  def requestCodeAction(file: String, start: Position, end: Position) = {
    val params = CodeActionParams(
      CodeActionContext.create(js.Array(), js.Array()),
      Range.create(start, end),
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(CodeActionRequest.`type`, params).toFuture
  }

  def requestDefinition(file: String, position: Position) = {
    val params = DefinitionParams(
      position,
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(DefinitionRequest.`type`, params).toFuture
  }

  def requestFormatting(file: String, indent: Int) = {
    val params = DocumentFormattingParams(
      FormattingOptions.create(indent, false),
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(DocumentFormattingRequest.`type`, params).toFuture
  }

  def requestHover(file: String, position: Position) = {
    val params = HoverParams(
      position,
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(HoverRequest.`type`, params).toFuture
  }

  def requestReferences(file: String, position: Position) = {
    val params = ReferenceParams(
      ReferenceContext(true),
      position,
      TextDocumentIdentifier.create(toURI(file))
    )
    connection.sendRequest(ReferencesRequest.`type`, params).toFuture
  }

  def executeCommand(file: String, command: String) = {
    // kiama wants the first argument to be JSON with uri
    val argument = js.JSON.parse(s"{\"uri\":\"${toURI(file)}\"}")

    val params = ExecuteCommandParams(command)
    params.setArguments(js.Array(argument))
    connection.sendRequest(ExecuteCommandRequest.`type`, params).toFuture
  }
}