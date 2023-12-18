package lspTest

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport,JSGlobal,JSName}

object Process {
    @js.native
    @JSImport("child_process", "spawn")
    def spawn(
        command: String,
        args: js.Array[String],
    ): js.Object = js.native
}

object Net {
    @js.native
    @JSImport("net", "createConnection")
    def createConnection(options: SocketOptions, connectListener: js.Function0[Unit]): Socket = js.native

    trait SocketOptions extends js.Object {
        var port: js.UndefOr[Int] = js.undefined
        var host: js.UndefOr[String] = js.undefined
    }

    @js.native
    @JSImport("net", "Socket")
    class Socket extends js.Object {
        def this(options: SocketOptions) = this()
        def connect(path: String, connectListener: js.Function0[Unit]): Socket = js.native
        def connect(port: Int, host: String, connectListener: js.Function0[Unit]): Socket = js.native
        def destroyed: Boolean = js.native
        def readyState: String = js.native
        def localAddress: js.UndefOr[String] = js.native
        def localPort: js.UndefOr[Int] = js.native
        def remoteAddress: js.UndefOr[String] = js.native
        def remotePort: js.UndefOr[Int] = js.native
        def end(): Socket = js.native
        def setEncoding(encoding: String): Socket = js.native
        def setKeepAlive(enable: Boolean): Socket = js.native
        def setNoDelay(noDelay: Boolean): Socket = js.native
        def setTimeout(timeout: Double): Socket = js.native
    }
}

object Protocol {
    trait Logger extends js.Object {
        def error(msg: String): Unit
        def warn(msg: String): Unit
        def info(msg: String): Unit
        def log(msg: String): Unit
    }

    object NullLogger extends Logger {
        def error(msg: String) {}
        def warn(msg: String) {}
        def info(msg: String) {}
        def log(msg: String) {}
    }

    object ConsoleLogger extends Logger {
        def error(msg: String) { println(msg) }
        def warn(msg: String) { println(msg) }
        def info(msg: String) { println(msg) }
        def log(msg: String) { println(msg) }
    }

    @js.native
    @JSImport("vscode-languageserver-protocol", "TextDocumentItem")
    class TextDocumentItem extends js.Object {
        def uri: String = js.native
        def languageId: String = js.native
        def version: Integer = js.native
        def text: String = js.native
    }

    @js.native
    @JSImport("vscode-languageserver-protocol", "ClientCapabilities")
    class ClientCapabilities extends js.Object {
        // TODO: Specify client's capabilities
    }

    trait NotificationParams extends js.Object

    @js.native
    @JSImport("vscode-languageserver-protocol", "DidOpenTextDocumentParams")
    class DidOpenTextDocumentParams extends NotificationParams {
        def textDocument: TextDocumentItem = js.native
    }

    trait RequestParams extends js.Object

    @js.native
    @JSImport("vscode-languageserver-protocol", "InitializeParams")
    object InitializeParams extends RequestParams {
        def capabilities: ClientCapabilities = js.native
    }

    @js.native
    @JSImport("vscode-languageserver-protocol", "InitializeRequest")
    object InitializeRequest extends js.Object {
        val method: String = js.native
        val messageDirection: String = js.native

        @JSName("type")
        val tpe: js.Object = js.native
    }

    @js.native
    @JSImport("vscode-languageserver-protocol", "ProtocolConnection")
    class ProtocolConnection extends js.Object {
        def sendNotification(tpe: js.Object, params: js.UndefOr[NotificationParams]): js.Promise[Unit] = js.native
        def sendRequest(tpe: js.Object, params: js.UndefOr[RequestParams]): js.Promise[js.Object] = js.native

        def end(): Unit = js.native
        def dispose(): Unit = js.native
        def listen(): Unit = js.native
    }

    @js.native
    @JSImport("vscode-languageserver-protocol", "createProtocolConnection")
    def createProtocolConnection(input: Net.Socket, output: Net.Socket, logger: Logger): ProtocolConnection = js.native
}