package lspTest

import scala.collection.mutable.{HashMap,ListBuffer}
import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.scalajs.js
import org.scalablytyped.runtime.StObject
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessagesMod.NotificationMessage
import typings.vscodeLanguageserverProtocol.anon.Name
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod._
import typings.vscodeLanguageserverProtocol.mod.{TextDocumentItem, TextDocumentIdentifier, CodeActionContext, Range, FormattingOptions}
import typings.vscodeLanguageserverTypes.mod.Position
import typings.vscodeLanguageserverTypes.mod.ReferenceContext

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

  var notifications: HashMap[String, Promise[NotificationMessage]] = HashMap()

  def waitForNotification(method: String) =
    notifications.getOrElseUpdate(method, Promise()).future.flatMap { notification =>
      notifications.remove(method)
      Future.successful(notification)
    }

  def initialize() = {
    connection.onUnhandledNotification { notification =>
      notifications.get(notification.method) match {
        case Some(promise: Promise[NotificationMessage]) => promise.success(notification)
        case None => notifications(notification.method) = Promise().success(notification)
      }
    }

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
      } recoverWith {
        case error => Future.failed(error)
      }
    } recoverWith {
      case error => Future.failed(error)
    }
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
}