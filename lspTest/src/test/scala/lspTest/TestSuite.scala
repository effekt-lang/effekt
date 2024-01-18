package lspTest

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.timers
import scala.util.{Success, Failure}
import typings.node.{childProcessMod, netMod}
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection
import utest._

object LspTestSuite extends TestSuite with SequentialExecutor {
  val connectionTimeout = 3000 // in ms
  val retryTimeout = 100 // in ms

  def timeoutFuture[T](f: Future[T], duration: Int)(implicit ec: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    val timeoutHandle = timers.setTimeout(duration) {
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
      timers.setTimeout(retryTimeout) {
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

    timeoutFuture(connectionFuture, connectionTimeout)
  }

  // variables have to be defined outside of the Tests block in order to be shared among tests
  var client: Client = null
  var connection: ProtocolConnection = null

  def tests = Tests {
    implicit val ec: ExecutionContext = framework.ExecutionContext.RunNow

    val port = 5007
    childProcessMod.spawn("effekt.sh", js.Array("-s", "--debug", s"--debugPort $port"))

    test("Connect to server") {
      tryConnect(port, "127.0.0.1").transform {
        case Success(_connection) => {
          connection = _connection
          client = new Client(connection)
          Success(())
        }
        case error => error
      }
    }

    test("Request initialization") {
      client.initialize().transform {
        case Success(result) => {
          Checker.checkStats("initialization", result)
          Success(())
        }
        case error => error
      }
    }

    test("Run all client tests") {
      val tests = new ClientTests(client)
      TestRunner.runAndPrintAsync(tests.tests, "Tests", executor = SequentialExecutor)
    }

    test("Exit client") {
      client.exit().transform {
        case Success(_) => {
          connection.end()
          Success(())
        }
        case error => error
      }
    }
  }
}
