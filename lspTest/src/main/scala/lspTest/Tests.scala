package lspTest

import scala.scalajs.js
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import typings.node.fsMod
import scala.concurrent.Future
import typings.vscodeLanguageserverTypes.mod.{DocumentSymbol,Position}

class Tests(val client: Client) {
  def testsDir = "examples2"

  def testDocumentSymbol(file: String)(implicit ec: ExecutionContext) = {
    client.sendDocumentSymbol(file).transform {
      case Success(v) => Success(v.asInstanceOf[js.Array[DocumentSymbol]])
      case Failure(_) => Failure(new Error("documentSymbolRequest"))
    }
  }

  def testHover(file: String, position: Position)(implicit ec: ExecutionContext) = {
    client.sendHover(file, position).transform {
      case Success(v) => Success(v)
      case Failure(_) => Failure(new Error("hoverRequest"))
    }
  }

  def testSymbols(file: String, symbols: js.Array[DocumentSymbol])(implicit ec: ExecutionContext) = {
    symbols.foreach(symbol => {
      testHover(file, symbol.range.start)
    })
  }

  def runTests(file: String) = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    client.openDocument(file, fsMod.readFileSync(file).toString).onComplete {
      case Success(_) => {
        testDocumentSymbol(file).onComplete {
          case Success(symbols) => testSymbols(file, symbols)
          case Failure(_) => println("error during hoverRequest")
        }
      }
      case Failure(_) => println("error during didOpenTextDocumentNotification")
    }
  }

  def runTestsInDirectory(dir: String): Unit =
    fsMod.readdirSync(dir).foreach(sub => {
      val path = s"$dir/$sub"
      if (fsMod.statSync(path).get.isDirectory)
        runTestsInDirectory(path) // iterate in sub directory
      else if (path.endsWith(".effekt"))
        runTests(path)
    })

  def runAll() = runTestsInDirectory(testsDir)
}