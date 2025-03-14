package lspTest

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.timers
import typings.node.childProcessMod
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection

class ServerConnection(implicit ec: ExecutionContext) {
  // Delay before killing process after sending the exit notification
  private val KillDelayMs = 1000

  private var childProcess: childProcessMod.ChildProcessWithoutNullStreams = _
  private var jsProcess: js.Dynamic = _
  private var connection: ProtocolConnection = _
  private var client: Client = _

  def startServer(): Future[Unit] = {
    childProcess = childProcessMod.spawn("effekt.sh", js.Array("--experimental-server"))
    jsProcess = childProcess.asInstanceOf[js.Dynamic]
    Future.successful(())
  }

  def connect(): Future[Client] = {
    val reader = childProcess.stdout.asInstanceOf[MessageReader]
    val writer = childProcess.stdin.asInstanceOf[MessageWriter]

    val logger = Logger(println, println, println, println)
    connection = libCommonConnectionMod.createProtocolConnection(reader, writer, logger)
    connection.listen()

    client = new Client(connection)
    Future.successful(client)
  }

  def setupServerAndConnect(): Future[Client] = {
    startServer().flatMap(_ => connect())
  }

  def hasExited: Boolean = jsProcess.exitCode == null

  def cleanup(): Future[Unit] = {
    val cleanupPromise = Promise[Unit]()

    // We run the shutdown sequence as specified by the language server protocol:
    // - Shutdown Request
    // - Exit Notification
    // If the server process is still running after `KillDelayMs`,
    // we kill it and raise an exception to mark the test as failed.
    client.shutdown()
      .flatMap(_ => client.exit())
      .onComplete { result =>
        timers.setTimeout(KillDelayMs) {
          if (jsProcess.exitCode == null) {
            try {
              jsProcess.kill()
              cleanupPromise.failure(new Exception("Server did not terminate gracefully and had to be killed"))
            } catch {
              case e: Throwable =>
                cleanupPromise.failure(new Exception(s"Failed to kill server: ${e.getMessage}", e))
            }
          } else {
            cleanupPromise.success(())
          }
        }
      }

    cleanupPromise.future
  }
}
