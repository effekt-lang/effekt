package effekt

import munit.FunSuite
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.{CompletableFuture, Executors}
import org.eclipse.lsp4j.{DidOpenTextDocumentParams, InitializeParams, InitializeResult, MessageActionItem, MessageParams, PublishDiagnosticsParams, ServerCapabilities, ShowMessageRequestParams, TextDocumentItem, TextDocumentSyncKind}
import org.eclipse.lsp4j.{Position, TextDocumentItem}
import java.util
import scala.collection.mutable
import scala.collection.mutable.Queue

class LSPTests extends FunSuite {
  // Import the extension method for String
  import TextDocumentSyntax._

  def withClientAndServer(testBlock: (MockLanguageClient, ServerNG) => Unit): Unit = {
    val driver = new Driver {}
    val config = EffektConfig(Seq("--experimental-server"))
    config.verify()

    val clientIn = new PipedInputStream()
    val clientOut = new PipedOutputStream()
    val serverIn = new PipedInputStream(clientOut)
    val serverOut = new PipedOutputStream(clientIn)

    val server = new ServerNG(driver, config)

    val mockClient = new MockLanguageClient()
    server.connect(mockClient)

    val launcher = server.launch(mockClient, serverIn, serverOut)

    testBlock(mockClient, server)
  }

  test("Initialization works") {
    withClientAndServer { (client, server) =>
      val initializeResult = server.initialize(new InitializeParams()).get()
      val expectedCapabilities = new ServerCapabilities()
      expectedCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
      expectedCapabilities.setHoverProvider(true)
      expectedCapabilities.setDefinitionProvider(true)
      expectedCapabilities.setReferencesProvider(true)
      expectedCapabilities.setDocumentSymbolProvider(true)
      expectedCapabilities.setCodeActionProvider(true)
      expectedCapabilities.setDocumentFormattingProvider(true)
      assertEquals(initializeResult, new InitializeResult(expectedCapabilities))
    }
  }

  test("didOpen yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val textDoc = raw"""
                       |def main() = { println("Hello, world!") }
                       |""".textDocument

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val diagnostics = client.diagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  test("Correct cursor position") {
    val (textDoc, cursor) = raw"""
                                |def main() = { println("Hello, world!") }
                                |    ↑
                                |""".textDocumentAndCursor

    assertEquals(cursor, new org.eclipse.lsp4j.Position(1, 4))
  }

  test("Missing cursor") {
    intercept[IllegalArgumentException] {
      raw"""
           |def main() = { println("Hello, world!") }
           |""".textDocumentAndCursor
    }
  }
}

class MockLanguageClient extends LanguageClient {
  private val diagnosticQueue: mutable.Queue[PublishDiagnosticsParams] = mutable.Queue.empty

  /**
   * Pops all diagnostics currently in the queue.
   */
  def diagnostics(): Seq[PublishDiagnosticsParams] = {
    val diagnostics = diagnosticQueue.toSeq
    diagnosticQueue.clear()
    diagnostics
  }

  override def telemetryEvent(`object`: Any): Unit = {
    // Not implemented for testing.
  }

  override def publishDiagnostics(diagnostics: PublishDiagnosticsParams): Unit = {
    diagnosticQueue.enqueue(diagnostics)
  }

  override def showMessage(messageParams: MessageParams): Unit = {
    // Not implemented for testing.
  }

  override def showMessageRequest(requestParams: ShowMessageRequestParams): CompletableFuture[MessageActionItem] = {
    // Not implemented for testing.
    CompletableFuture.completedFuture(null)
  }

  override def logMessage(message: MessageParams): Unit = {
    // Not implemented for testing.
  }
}

// .textDocument DSL as extension methods for String
object TextDocumentSyntax {
  implicit class TextDocumentOps(val content: String) extends AnyVal {
    def textDocument: TextDocumentItem =
      new TextDocumentItem("file://test.effekt", "effekt", 1, content.stripMargin)

    def textDocumentAndCursor: (TextDocumentItem, Position) = {
      // Remove margin formatting
      val lines = content.stripMargin.split("\n").toBuffer
      var cursor: Option[Position] = None
      var lineIdx = 0

      while (lineIdx < lines.length && cursor.isEmpty) {
        val line = lines(lineIdx)
        val arrowIdx = line.indexOf("↑")
        if (arrowIdx != -1) {
          if (line.trim != "↑")
            throw new IllegalArgumentException("Line with cursor arrow may not contain other characters.")
          if (lineIdx == 0)
            throw new IllegalArgumentException("Cursor arrow on first line cannot point to a previous line.")
          cursor = Some(new Position(lineIdx - 1, arrowIdx))
          lines.remove(lineIdx)
        }
        lineIdx += 1
      }

      if (cursor.isEmpty)
        throw new IllegalArgumentException("No cursor arrow (↑) found in the string.")

      val newContent = lines.mkString("\n")
      val document = new TextDocumentItem("file://test.effekt", "effekt", 1, newContent)
      (document, cursor.get)
    }
  }
}
