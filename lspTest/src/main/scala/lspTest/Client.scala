package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import org.scalablytyped.runtime.StObject
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod._
import typings.vscodeLanguageserverTypes.mod.Position
import typings.vscodeLanguageserverTypes.mod.ReferenceContext
import typings.vscodeLanguageserverProtocol.mod.{TextDocumentItem, TextDocumentIdentifier, CodeActionContext, Range, FormattingOptions}

class Client(val connection: ProtocolConnection)(implicit ec: ExecutionContext) {
  def toURI(file: String) = s"file:///$file"

  def initialize() = {
    connection.sendRequest(InitializeRequest.method.asInstanceOf[String], InitializeParams).toFuture
    // connection.sendNotification(InitializedNotification.`type`, StObject().asInstanceOf[InitializedParams]).toFuture
    // TODO: chaining these futures results in a java.lang.ClassCastException
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