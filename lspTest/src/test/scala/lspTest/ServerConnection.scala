package lspTest

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.timers
import typings.node.childProcessMod
import typings.node.{nodeStrings, bufferMod}
import typings.vscodeJsonrpc.libCommonConnectionMod.Logger
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection

class ServerConnection(implicit ec: ExecutionContext) {
  // Delay before killing process after sending the exit notification
  private val KillDelayMs = 1000

  // Set the TRACE_LSP_TESTS environment variable to enable tracing of LSP messages
  private val tracingEnabled: Boolean =
    js.Dynamic.global.process.env.TRACE_LSP_TESTS != js.undefined

  private var childProcess: childProcessMod.ChildProcessWithoutNullStreams = _
  private var jsProcess: js.Dynamic = _
  private var connection: ProtocolConnection = _
  private var client: Client = _

  def startServer(): Future[Unit] = {
    childProcess = childProcessMod.spawn("effekt", js.Array("--experimental-server"))
    jsProcess = childProcess.asInstanceOf[js.Dynamic]

    jsProcess.stdout.on(nodeStrings.data, { (data: bufferMod.BufferCls) =>
      if (tracingEnabled) {
        val output = data.toString()
        println("[Server -> Client]\n")
        print(output)
      }
    })

    jsProcess.stderr.on(nodeStrings.data, { (data: bufferMod.BufferCls) =>
      if (tracingEnabled) {
        val output = data.toString()
        println(s"[SERVER ERROR] $output")
      }
    })

    Future.successful(())
  }

  def connect(): Future[Client] = {
    jsProcess.stdin.on(nodeStrings.data, { (data: bufferMod.BufferCls) =>
      if (tracingEnabled) {
        val input = data.toString()
        println("[Client -> Server]\n")
        print(input)
      }
    })

    val reader = childProcess.stdout.asInstanceOf[MessageReader]
    val writer = childProcess.stdin.asInstanceOf[MessageWriter]

    val logger = Logger(
      info = message => if (tracingEnabled) println(s"[LSP INFO] $message"),
      warn = message => if (tracingEnabled) println(s"[LSP WARN] $message"),
      error = message => if (tracingEnabled) println(s"[LSP ERROR] $message"),
      log = message => if (tracingEnabled) println(s"[LSP LOG] $message")
    )

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
