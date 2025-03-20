package effekt

import munit.FunSuite
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.{CompletableFuture, Executors}
import org.eclipse.lsp4j.{DidOpenTextDocumentParams, InitializeParams, InitializeResult, MessageActionItem, MessageParams, PublishDiagnosticsParams, ServerCapabilities, ShowMessageRequestParams, TextDocumentItem, TextDocumentSyncKind}

import java.util
import scala.collection.mutable
import scala.collection.mutable.Queue

class LSPTests extends FunSuite {
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

    val launcher = server.launch(mockClient, serverIn, serverOut, executor)

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
      val input = raw"""
                         |def main() = { println("Hello, world!") }
                         |""".stripMargin

        val testURI = "file://test.effekt"
        val textDoc = new TextDocumentItem("file://test.effekt", "effekt", 1, input)
        val didOpenParams = new DidOpenTextDocumentParams()
        didOpenParams.setTextDocument(textDoc)
        server.getTextDocumentService().didOpen(didOpenParams)

        val diagnostics = client.diagnostics()
        assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(testURI, new util.ArrayList())))
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
