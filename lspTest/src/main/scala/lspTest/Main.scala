package lspTest

import lspTest.Facade._
import scala.scalajs.js

object Main {
  def main(args: Array[String]): Unit = {
    val clientOptions = LanguageClientOptions(js.Array(), "effekt")
    val serverOptions = ServerOptions("effekt", js.Array("-s"))
    val languageclient = new LanguageClient(
      "effektLanguageServer",
      "Effekt Language Server",
      serverOptions,
      clientOptions
    )
    println("Hello world!")
  }
}
