package lspTest

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Success, Failure}
import typings.node.fsMod
import typings.vscodeLanguageserverTypes.mod.{DocumentSymbol, SymbolInformation, Position}
import utest._
import javax.swing.text.Document

class ClientTests(val client: Client)(implicit ec: ExecutionContext) {
  def samplesDir = "lspTest/tests/samples"

  val symbols = HashMap[String, js.Array[DocumentSymbol | SymbolInformation]]()

  def tests = Tests {
    test("Open files") {
      forEachSample { file =>
        client.openDocument(file, fsMod.readFileSync(file).toString).transform {
          case Success(_) => Success(())
          case Failure(_) => Failure(new Error("didOpenTextDocumentNotification failed"))
        }
      }
    }

    test("Request symbols") {
      forEachSample { file =>
        client.requestDocumentSymbol(file).transform {
          case Success(v) => {
            Checker.checkSample("documentSymbolRequest", file, v.asInstanceOf[js.Object])
            symbols(file) = v
            Success(())
          }
          case Failure(_) => Failure(new Error("documentSymbolRequest failed"))
        }
      }
    }

    // TODO: fix flaky hovering?!
    test("Hover symbols") - retry(3) {
      forEachSample { file =>
        Future.sequence {
          symbols.getOrElse(file, js.Array()).map { symbol =>
            // TODO: support SymbolInformation (instanceOf doesn't work on JS traits!)
            // val position = symbol match
            //   case s: DocumentSymbol => s.range.start
            //   case s: SymbolInformation => s.location.range.start
            val position = symbol.asInstanceOf[DocumentSymbol].range.start

            client.requestHover(file, position).transform {
              case Success(v) => {
                Checker.checkContextualSample("hoverRequest", position, file, v.asInstanceOf[js.Object])
                Success(())
              }
              case Failure(_) => Failure(new Error("hoverRequest failed"))
            }
          }
        }.map(_ => ())
      }
    }
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

  // def testSymbols(file: String) =
  //   testDocumentSymbol(file).transform {
  //     case Success(symbols) => Success(symbols.foreach(symbol => {
  //       // TODO: accumulated error handling
  //       // TODO: test according to symbol kind
  //       testHover(file, symbol.range.start)
  //       testCodeAction(file, symbol.range.start, symbol.range.end)
  //       testDefinition(file, symbol.range.start) 
  //       testReferences(file, symbol.range.start)
  //     }))
  //     case Failure(_) => Failure(new Error("documentSymbolRequest"))
  //   }

  // def runTests(file: String) =
  //   client.openDocument(file, fsMod.readFileSync(file).toString).onComplete {
  //     case Success(_) => {
  //       // TODO: accumulated error handling
  //       testSymbols(file)
  //     }
  //     case Failure(_) => println("didOpenTextDocumentNotification")
  //   }

  def forEachSample(callback: String => Future[Unit]): Future[Unit] = {
    Future.sequence {
      fsMod.readdirSync(samplesDir).toSeq.flatMap { sub =>
        val path = s"$samplesDir/$sub"
        if (fsMod.statSync(path).get.isDirectory()) {
          Seq(forEachSample(callback)) // iterate in sub directory
        } else if (path.endsWith(".effekt")) {
          Seq(callback(path))
        } else {
          Seq.empty[Future[Unit]]
        }
      }
    }.map(_ => ())
  }
}