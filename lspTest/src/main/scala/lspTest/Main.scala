package lspTest

import scala.scalajs.js

object Main {
  def listener() {
    println("CONNECTED")
  }

  def main(args: Array[String]): Unit = {
    val options = new Facade.SocketOptions {
      port = 5007
      host = "127.0.0.1"
    }
    Facade.createConnection(options, listener.asInstanceOf[js.Function0[Unit]])
    println("Hello world!")
  }
}
