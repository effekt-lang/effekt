package lspTest

import scala.scalajs.js
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.{Success, Failure}
import typings.node.fsMod
import typings.vscodeLanguageserverTypes.mod.{DocumentSymbol, Position}

class Tests(val client: Client)(implicit ec: ExecutionContext) extends munit.FunSuite {
  def testsDir = "examples2"

  def testDocumentSymbol(file: String) =
    client.requestDocumentSymbol(file).transform {
      // TODO: compare v with reference file
      case Success(v) => Success(v.asInstanceOf[js.Array[DocumentSymbol]])
      case Failure(_) => Failure(new Error("documentSymbolRequest"))
    }

  def testHover(file: String, position: Position) =
    client.requestHover(file, position).transform {
      // TODO: compare v with reference file
      case Success(v) => Success(v)
      case Failure(_) => Failure(new Error("hoverRequest"))
    }

  def testCodeAction(file: String, start: Position, end: Position) =
    client.requestCodeAction(file, start, end).transform {
      // TODO: compare v with reference file
      case Success(v) => Success(v)
      case Failure(_) => Failure(new Error("codeAction"))
    }
  
  def testDefinition(file: String, position: Position) =
    client.requestDefinition(file, position).transform {
      // TODO: compare v with reference file
      case Success(v) => Success(v)
      case Failure(_) => Failure(new Error("definition"))
    }

  def testReferences(file: String, position: Position) =
    client.requestReferences(file, position).transform {
      // TODO: compare v with reference file
      case Success(v) => Success(v)
      case Failure(_) => Failure(new Error("references"))
    }

  def testSymbols(file: String) =
    testDocumentSymbol(file).transform {
      case Success(symbols) => Success(symbols.foreach(symbol => {
        // TODO: accumulated error handling
        // TODO: test according to symbol kind
        testHover(file, symbol.range.start)
        testCodeAction(file, symbol.range.start, symbol.range.end)
        testDefinition(file, symbol.range.start) 
        testReferences(file, symbol.range.start)
      }))
      case Failure(_) => Failure(new Error("documentSymbolRequest"))
    }

  def runTests(file: String) =
    client.openDocument(file, fsMod.readFileSync(file).toString).onComplete {
      case Success(_) => {
        // TODO: accumulated error handling
        testSymbols(file)
      }
      case Failure(_) => println("didOpenTextDocumentNotification")
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