package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.util.{Success, Failure}
import typings.node.childProcessMod
import typings.node.netMod.NetConnectOpts
import typings.node.netMod.{NetConnectOpts, createConnection}
import scala.scalajs.js.timers
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.InitializeParams
import typings.vscodeLanguageserverProtocol.mod.InitializeRequest
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod

object Main {
  def test(client: Client) {
    implicit val ec: ExecutionContext = JSExecutionContext.queue
    client.openDocument("hover", "effect Exc(msg: String): Unit").onComplete {
      case Success(_) => client.sendHover("hover", 0, 8)
      case Failure(_) => println("error while opening document")
    }
  }

  def tryConnect(port: Int, host: String) {
    val socket = createConnection(port, host)

    // needed since server process doesn't start immediately
    socket.on("error", ev => {
      timers.setTimeout(500) {
        tryConnect(port, host)
      }
    })

    socket.on("connect", ev => {
      val reader = socket.asInstanceOf[MessageReader]
      val writer = socket.asInstanceOf[MessageWriter]
      val logger = Logger(println, println, println, println)
      val connection = libCommonConnectionMod.createProtocolConnection(reader, writer, logger)
      connection.listen()

      implicit val ec: ExecutionContext = JSExecutionContext.queue
      connection.sendRequest(InitializeRequest.method.asInstanceOf[String], InitializeParams).toFuture.onComplete {
        case Success(_) => test(new Client(connection))
        case Failure(_) => println("error during initialization")
      }
    })
  }

  def main(args: Array[String]): Unit = {
    val process = childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug"))

    tryConnect(5007, "127.0.0.1")
  }
}
