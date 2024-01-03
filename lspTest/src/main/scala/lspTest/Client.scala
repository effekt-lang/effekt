package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.{InitializeParams, DidOpenTextDocumentParams, HoverParams}
import typings.vscodeLanguageserverProtocol.mod._

class Client(val connection: ProtocolConnection) {
  def openDocument(name: String, content: String) = {
    val document = TextDocumentItem.create(
      s"file:///$name.effekt",
      "effekt",
      1,
      content
    )
    val params = DidOpenTextDocumentParams(document)
    connection.sendNotification(DidOpenTextDocumentNotification.`type`, params).toFuture
  }

  def sendHover(name: String, line: Int, column: Int) = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue
    val hoverParams = HoverParams(Position.create(line, column), TextDocumentIdentifier.create(s"file:///$name.effekt"))
    connection.sendRequest(HoverRequest.`type`, hoverParams).toFuture.foreach { hover =>
      println(js.JSON.stringify(hover.asInstanceOf[js.Object]))
    }
  }
}