package lspTest

import scala.scalajs.js
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.{InitializeParams, DidOpenTextDocumentParams, HoverParams}
import typings.vscodeLanguageserverProtocol.mod._
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.DocumentSymbolParams
import typings.vscodeLanguageserverTypes.mod

class Client(val connection: ProtocolConnection) {
  def openDocument(file: String, content: String) = {
    val document = TextDocumentItem.create(
      s"file:///$file",
      "effekt",
      1,
      content
    )
    val params = DidOpenTextDocumentParams(document)
    connection.sendNotification(DidOpenTextDocumentNotification.`type`, params).toFuture
  }

  def sendDocumentSymbol(file: String) = {
    val params = DocumentSymbolParams(
      TextDocumentIdentifier.create(s"file:///$file")
    )
    connection.sendRequest(DocumentSymbolRequest.`type`, params).toFuture
  }

  def sendHover(file: String, position: mod.Position) = {
    val params = HoverParams(
      Position.create(position.line, position.character),
      TextDocumentIdentifier.create(s"file:///$file")
    )
    connection.sendRequest(HoverRequest.`type`, params).toFuture
  }
}