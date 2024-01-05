package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js.timers
import scala.util.{Success, Failure}
import typings.node.{childProcessMod, netMod}
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import typings.vscodeLanguageserverProtocol.libCommonProtocolMod.InitializeParams
import typings.vscodeLanguageserverProtocol.mod.InitializeRequest

object Main {
  def tryConnect(port: Int, host: String, callback: Function[ProtocolConnection, Unit]) {
    val socket = netMod.createConnection(port, host)

    // needed since server process doesn't start immediately
    socket.on("error", _ => {
      timers.setTimeout(500) {
        tryConnect(port, host, callback)
      }
    })

    socket.on("connect", _ => {
      val reader = socket.asInstanceOf[MessageReader]
      val writer = socket.asInstanceOf[MessageWriter]
      val logger = Logger(println, println, println, println)
      val connection = libCommonConnectionMod.createProtocolConnection(reader, writer, logger)
      connection.listen()

      implicit val ec: ExecutionContext = JSExecutionContext.queue
      connection.sendRequest(InitializeRequest.method.asInstanceOf[String], InitializeParams).toFuture.onComplete {
        case Success(_) => callback(connection)
        case Failure(_) => println("error during initialization")
      }
    })
  }

  def main(args: Array[String]): Unit = {
    val port = 5007
    val process = childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug", s"--debugPort $port"))

    tryConnect(port, "127.0.0.1", connection => {
      val client = new Client(connection)
      val tests = new Tests(client)
      tests.runAll()
    })
  }
}
