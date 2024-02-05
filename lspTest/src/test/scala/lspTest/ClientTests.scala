package lspTest

import scala.collection.BuildFrom
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
        client.openDocument(file, fsMod.readFileSync(file).toString).transformWith { _ =>
          client.waitForDiagnostics(file).flatMap {
            notification => {
              Checker.checkSample("publishDiagnostics", file, notification.asInstanceOf[js.Object])
              Future.successful(())
            }
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
          // TODO: Infer captures here, when action is available
          // TODO: Close hole here
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

    test("Format document") {
      forEachSample { file =>
        checkContextualSample(file, "formattingRequest", Range.create(Position(0, 0), Position(0, 0)), _ => {
          client.requestFormatting(file, 2)
        })
      }
    }

    test("Execute inferredCaptures") {
      forEachSample { file => 
        checkContextualSample(file, "executeCommandInferredCaptures", Range.create(Position(0, 0), Position(0, 0)), _ => {
          client.executeCommand(file, "inferredCaptures")
        })
      }
    }

    test("Add newline to all files") {
      forEachSample { file => client.changeDocument(file, fsMod.readFileSync(file).toString + "\n") }
    }

    test("Save files") {
      forEachSample { file => client.closeDocument(file) }
    }

    test("Close files") {
      forEachSample { file => client.closeDocument(file) }
    }
  }

  def symbolToRange(symbol: DocumentSymbol | SymbolInformation) = {
    // TODO: support SymbolInformation (instanceOf doesn't work on JS traits!)
    // val range = symbol match
    //   case s: DocumentSymbol => s.range
    //   case s: SymbolInformation => s.location.range
    symbol.asInstanceOf[DocumentSymbol].range
  }

  // compared to Future.sequence, this function will also *start* the futures sequentially and not concurrently
  def linearizeFuture(in: Seq[() => Future[Unit]]): Future[Seq[Unit]] =
    val builder = Seq.newBuilder[Unit]
    in.foldLeft(Future.successful(())) {
        (a, b) => a.transformWith { _ => b() }
    } map (_ => builder.result())

  def checkContextualSample(file: String, request: String, range: Range, callback: Range => Future[Any]) = {
    callback(range).transform {
      case Success(v) => {
        Checker.checkContextualSample(request, range, file, v.asInstanceOf[js.Object])
        Success(())
      }
      case Failure(_) => Failure(new Error(s"$request failed"))
    }
  }
  
  // this currently seems to work fine using Future.sequence
  // TODO: use linearizeFuture if we happen to get race conditions (this will be a lot slower!)
  def testEachSymbol(file: String, request: String, callback: Range => Future[Any]) = {
    Future.sequence {
      symbols.getOrElse(file, js.Array()).map { symbol =>
        checkContextualSample(file, request, symbolToRange(symbol), callback)
      }
    }.map(_ => ())
  }

  def forEachFile(path: String, callback: String => Future[Unit]): Future[Unit] = {
    linearizeFuture {
      fsMod.readdirSync(path).toSeq.flatMap { sub =>
        val subPath = s"$path/$sub"
        if (fsMod.statSync(subPath).get.isDirectory()) {
          Seq(() => forEachFile(subPath, callback)) // iterate in sub directory
        } else if (subPath.endsWith(".effekt")) {
          Seq(() => callback(subPath))
        } else {
          Seq.empty[() => Future[Unit]]
        }
      }
    }.map(_ => ())
  }

  def forEachSample(callback: String => Future[Unit]): Future[Unit] =
    forEachFile(samplesDir, callback)
}