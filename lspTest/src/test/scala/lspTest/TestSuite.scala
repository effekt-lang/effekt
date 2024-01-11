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
import utest._
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._

object LspTestSuite extends TestSuite {
  def timeoutFuture[T](f: Future[T], duration: FiniteDuration)(implicit ec: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    val timeoutHandle = timers.setTimeout(duration.toMillis.toDouble) {
      p.tryFailure(new Error("Connection timed out"))
    }
    f.onComplete { result =>
      timers.clearTimeout(timeoutHandle)
      p.tryComplete(result)
    }
    p.future
  }

  def tryConnect(port: Int, host: String)(implicit ec: ExecutionContext): Future[ProtocolConnection] = {
    val promise = Promise[ProtocolConnection]()
    val connectionFuture = promise.future

    val socket = netMod.createConnection(port, host)

    socket.on("error", _ => {
      timers.setTimeout(100) {
        tryConnect(port, host).onComplete {
          case Success(connection) => promise.success(connection)
          case Failure(exception) => promise.failure(exception)
        }
      }
    })

    socket.on("connect", _ => {
      val reader = socket.asInstanceOf[MessageReader]
      val writer = socket.asInstanceOf[MessageWriter]
      val logger = Logger(println, println, println, println)
      val connection = libCommonConnectionMod.createProtocolConnection(reader, writer, logger)
      connection.listen()

      promise.success(connection)
    })

    timeoutFuture(connectionFuture, 3.seconds)
  }

  def tests = Tests {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    val port = 5007
    childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug", s"--debugPort $port"))

    // (1) try to connect to LSP server
    test("LSP server connection") {
      tryConnect(port, "127.0.0.1").flatMap { connection =>
        val client = new Client(connection)

        // (2) try to initialize server
        client.initialize.flatMap { _ =>
          println("connected, starting tests")
          val tests = new Tests(client)

          // (3) run all LSP tests
          TestRunner.runAndPrintAsync(tests.tests, "Tests").flatMap { results =>
            val leafResults = results.leaves.toSeq
            assert(leafResults(0).value.isSuccess)
            Future.successful(())
          }
        }.recoverWith {
          // initialization failed, probably fatal request logic error
          case error => Future.failed(error)
        }
      }.recoverWith {
        // connection failed, probably by connection timeout
        case error => Future.failed(error)
      }
    }
  }

  // TODO: Run this after everything
  //   client.exit().onComplete {
  //     case _ => connection.end()
  //   }
}
