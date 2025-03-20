package lspTest

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.{UndefOr, timers}
import typings.node.childProcessMod
import typings.node.{bufferMod, nodeStrings}
import typings.vscodeJsonrpc.libCommonConnectionMod.{Logger, Trace, Tracer}
import typings.vscodeJsonrpc.libCommonMessageReaderMod.MessageReader
import typings.vscodeJsonrpc.libCommonMessageWriterMod.MessageWriter
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod
import typings.vscodeLanguageserverProtocol.libCommonConnectionMod.ProtocolConnection

class ServerConnection(implicit ec: ExecutionContext) {
  // Delay before killing process after sending the exit notification
  private val KillDelayMs = 10000

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

    Future.successful(())
  }

  def connect(): Future[Client] = {
    val reader = childProcess.stdout.asInstanceOf[MessageReader]
    val writer = childProcess.stdin.asInstanceOf[MessageWriter]

    jsProcess.stderr.on(nodeStrings.data, { (data: bufferMod.BufferCls) =>
      if (tracingEnabled) {
        val output = data.toString()
        println(s"[SERVER STDERR] $output")
      }
    })

    val logger = Logger(
      info = message => if (tracingEnabled) println(s"[LSP INFO] $message"),
      warn = message => if (tracingEnabled) println(s"[LSP WARN] $message"),
      error = message => if (tracingEnabled) println(s"[LSP ERROR] $message"),
      log = message => if (tracingEnabled) println(s"[LSP LOG] $message")
    )

    connection = libCommonConnectionMod.createProtocolConnection(reader, writer, logger)

    if (tracingEnabled) {
      connection.trace(Trace.Verbose, EffektTracer.create)
    }

    connection.listen()

    client = new Client(connection)
    Future.successful(client)
  }

  def setupServerAndConnect(): Future[Client] = {
    startServer().flatMap(_ => connect())
  }

  def hasExited: Boolean = jsProcess.exitCode != null || !js.isUndefined(jsProcess.exitCode)

  // Ensure the server process performs a clean exit
  // This is very important; if for example the server process doesn't exit,
  // we will run out of available memory very quickly as the test suite will
  // start multiple server processes.
  //
  // If `fastExit` is set to `true`, we don't wait for the server process to shut down but kill it immediately.
  // This makes the test suite run through much faster.
  def cleanup(fastExit: Boolean): Future[Unit] = {
    if (fastExit) {
      for {
        _ <- client.shutdown()
        _ <- client.exit()
      } yield {
        try {
          jsProcess.kill()
        } catch {
          case e: Throwable =>
            throw new Exception(s"Failed to kill server: ${e.getMessage}", e)
        }
      }
    } else {
      cleanup()
    }
  }

  // Ensure the server process performs a clean exit
  // This is very important; if for example the server process doesn't exit,
  // we will run out of available memory very quickly as the test suite will
  // start multiple server processes.
  def cleanup(): Future[Unit] = {
    // A future that completes when the process exits.
    val processExitFuture: Future[Unit] = {
      val p = Promise[Unit]()
      jsProcess.on("exit", { (code: Int) =>
        p.success(())
      })
      p.future
    }

    // A future that completes after KillDelayMs.
    val timeoutFuture: Future[Unit] = delay(KillDelayMs);

    // Wait until either the process exits or the timeout is reached.
    val waitForExitOrTimeout: Future[Unit] =
      Future.firstCompletedOf(Seq(processExitFuture, timeoutFuture))

    for {
      _ <- client.shutdown()
      _ <- client.exit()
      _ <- waitForExitOrTimeout
      result <- {
        println(s"exit code: ${jsProcess.exitCode}")
        if (jsProcess.exitCode == null || js.isUndefined(jsProcess.exitCode)) {
          try {
            jsProcess.kill()
            Future.failed(new Exception("Server did not terminate gracefully and had to be killed"))
          } catch {
            case e: Throwable =>
              Future.failed(new Exception(s"Failed to kill server: ${e.getMessage}", e))
          }
        } else if (jsProcess.exitCode.asInstanceOf[Int] != 0) {
          Future.failed(new Exception(s"Server exited with non-zero exit code: ${jsProcess.exitCode}"))
        } else {
          Future.successful(())
        }
      }
    } yield result
  }

  def delay(ms: Int): Future[Unit] = {
    val p = Promise[Unit]()
    timers.setTimeout(ms)(p.success(()))
    p.future
  }
}

object EffektTracer {
  def create: Tracer = {
    js.Dynamic.literal(
      log = { (arg: js.Any, data: UndefOr[String]) =>
        data.fold(js.Dynamic.global.console.log(arg)) { d =>
          js.Dynamic.global.console.log(arg, d)
        }
      }
    ).asInstanceOf[Tracer]
  }
}