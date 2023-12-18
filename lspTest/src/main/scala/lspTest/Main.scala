package lspTest

import scala.scalajs.js

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext

object Main {
  def initialize(socket: Net.Socket): Unit = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    val connection = Protocol.createProtocolConnection(socket, socket, Protocol.ConsoleLogger)
    connection.listen()

    connection.sendRequest(Protocol.InitializeRequest.tpe, Protocol.InitializeParams).toFuture
      .transform((res) => {
        println("ok")
        println(js.JSON.stringify(res))
      }, (err) => {
        throw new Error(err)
      })
  }

  // TODO: somehow only initialize when actually connected
  def listener() = {
    println("connected")
  }

  def main(args: Array[String]): Unit = {
    // Process.spawn("effekt.sh", js.Array("-s", "--debug"))

    val options = new Net.SocketOptions {
      port = 5007
      host = "127.0.0.1"
    }

    val socket = Net.createConnection(options, listener.asInstanceOf[js.Function0[Unit]])

    initialize(socket)
  }
}
