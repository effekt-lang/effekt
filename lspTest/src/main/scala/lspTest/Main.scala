package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.util.{Success, Failure}
import typings.node.netMod.createConnection
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.createProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.InitializeParams
import typings.vscodeLanguageserverProtocol.mod.InitializeRequest

object Main {
  def test(client: Client) {
    implicit val ec: ExecutionContext = JSExecutionContext.queue
    client.openDocument("hover", "effect Exc(msg: String): Unit").onComplete {
      case Success(_) => client.sendHover("hover", 0, 8)
      case Failure(_) => println("error while opening document")
    }
  }

  def main(args: Array[String]): Unit = {
    // val process = typings.node.childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug"))
    // TODO: createConnection with opts and TCP timeout

    val socket = createConnection(5007, "127.0.0.1")
    val reader = socket.asInstanceOf[MessageReader]
    val writer = socket.asInstanceOf[MessageWriter]
    val logger = Logger(println, println, println, println)
    val connection = createProtocolConnection(reader, writer, logger)
    connection.listen()

    implicit val ec: ExecutionContext = JSExecutionContext.queue
    connection.sendRequest(InitializeRequest.method.asInstanceOf[String], InitializeParams).toFuture.onComplete {
      case Success(_) => test(new Client(connection))
      case Failure(_) => println("error during initialization")
    }
  }
}
