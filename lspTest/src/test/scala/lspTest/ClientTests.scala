package lspTest

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Success, Failure}
import typings.node.fsMod
import typings.vscodeLanguageserverProtocol.mod.PublishDiagnosticsNotification
import typings.vscodeLanguageserverTypes.mod.{DocumentSymbol, SymbolInformation, Position, Range}
import utest._

class ClientTests(val client: Client)(implicit ec: ExecutionContext) {
  def samplesDir = "lspTest/tests/samples"

  val symbols = HashMap[String, js.Array[DocumentSymbol | SymbolInformation]]()

  def tests = Tests {
    test("Open files and check diagnoses") {
      forEachSample { file =>
        client.openDocument(file, fsMod.readFileSync(file).toString).transform {
          case Success(_) => Success(())
          case Failure(_) => Failure(new Error("didOpenTextDocumentNotification failed"))
        }.flatMap { _ =>
          client.waitForNotification(PublishDiagnosticsNotification.method.asInstanceOf[String]).map {
            notification => Checker.checkSample("publishDiagnostics", file, notification.asInstanceOf[js.Object])
          }
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
    // test("Symbol hover") {
    //   forEachSample { file =>
    //     testEachSymbol(file, "hoverRequest", range =>
    //       client.requestHover(file, range.start)
    //     )
    //   }
    // }
    
    test("Symbol code action") {
      forEachSample { file =>
        testEachSymbol(file, "codeActionRequest", range =>
          client.requestCodeAction(file, range.start, range.end)
        )
      }
    }

    test("Symbol definition") {
      forEachSample { file =>
        testEachSymbol(file, "definitionRequest", range =>
          client.requestDefinition(file, range.start)
        )
      }
    }

    test("Symbol references") {
      forEachSample { file =>
        testEachSymbol(file, "referencesRequest", range =>
          client.requestReferences(file, range.start)
        )
      }
    }

    // TODO: inferredCaptures command
  }

  def symbolToRange(symbol: DocumentSymbol | SymbolInformation) = {
    // TODO: support SymbolInformation (instanceOf doesn't work on JS traits!)
    // val range = symbol match
    //   case s: DocumentSymbol => s.range
    //   case s: SymbolInformation => s.location.range
    symbol.asInstanceOf[DocumentSymbol].range
  }

  def testEachSymbol(file: String, request: String, callback: Range => Future[Any]) = {
    Future.sequence {
      symbols.getOrElse(file, js.Array()).map { symbol =>
        val range = symbolToRange(symbol)
        callback(range).transform {
          case Success(v) => {
            Checker.checkContextualSample(request, range, file, v.asInstanceOf[js.Object])
            Success(())
          }
          case Failure(_) => Failure(new Error(s"$request failed"))
        }
      }
    }.map(_ => ())
  }

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