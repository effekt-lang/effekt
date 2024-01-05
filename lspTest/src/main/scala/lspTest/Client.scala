package lspTest

import scala.scalajs.js
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.{InitializeParams, DidOpenTextDocumentParams, HoverParams}
import typings.vscodeLanguageserverProtocol.mod._

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

  def sendHover(file: String, line: Int, column: Int) = {
    val hoverParams = HoverParams(
        Position.create(line, column),
        TextDocumentIdentifier.create(s"file:///$file")
    )
    connection.sendRequest(HoverRequest.`type`, hoverParams).toFuture
  }
}