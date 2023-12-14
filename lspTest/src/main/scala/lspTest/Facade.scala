package lspTest.Facade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSGlobal

@js.native
trait LanguageClientOptions extends js.Object {
  var documentSelector: js.Array[String] = js.native
  var diagnosticCollectionName: String = js.native
}

object LanguageClientOptions {
    def apply(documentSelector: js.Array[String], diagnosticCollectionName: String) = {
        val options = (new js.Object).asInstanceOf[LanguageClientOptions]
        options.documentSelector = documentSelector
        options.diagnosticCollectionName = diagnosticCollectionName
        options
    }
}

@js.native
trait ServerOptions extends js.Object {
  var command: String = js.native
  var args: js.Array[String] = js.native
}

object ServerOptions {
    def apply(command: String, args: js.Array[String]) = {
        val options = (new js.Object).asInstanceOf[ServerOptions]
        options.command = command
        options.args = args
        options
    }
}

@js.native
@JSImport("vscode-languageclient", "LanguageClient")
class LanguageClient(val id: String, val name: String, serverOptions: ServerOptions, clientOptions: LanguageClientOptions) extends js.Object

