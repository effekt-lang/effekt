package lspTest

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSGlobal

object Facade {
    @js.native
    @JSImport("net", "createConnection")
    def createConnection(options: SocketOptions, connectListener: js.Function0[Unit]): Socket = js.native

    trait SocketOptions extends js.Object {
        var port: js.UndefOr[Int] = js.undefined
        var host: js.UndefOr[String] = js.undefined
    }

    @JSImport("net", "Socket")
    @js.native
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