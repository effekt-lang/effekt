package lspTest

import scala.scalajs.js
import scala.scalajs.js.timers
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.util.{Success, Failure}
import typings.node.{childProcessMod, netMod}
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection

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

      callback(connection)
    })
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    val port = 5007
    childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug", s"--debugPort $port"))

    tryConnect(port, "127.0.0.1", connection => {
      val client = new Client(connection)

      client.initialize.onComplete {
        case Success(_) => {
          val tests = new Tests(client)

          tests.runAll()
          // client.exit()
          // connection.end()
        }
        case Failure(v) => println(v.printStackTrace())
      }
    })
  }
}
