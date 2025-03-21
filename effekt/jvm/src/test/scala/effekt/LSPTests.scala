package effekt

import com.google.gson.{JsonElement, JsonParser}
import munit.FunSuite
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, Hover, HoverParams, InitializeParams, InitializeResult, MarkupContent, MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ServerCapabilities, ShowMessageRequestParams, TextDocumentContentChangeEvent, TextDocumentItem, TextDocumentSyncKind, VersionedTextDocumentIdentifier}

import java.io.{PipedInputStream, PipedOutputStream}
import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.collection.mutable.Queue

class LSPTests extends FunSuite {
  // Import the extension method for String
  import TextDocumentSyntax.*

  // Test helpers
  //
  //

  def withClientAndServer(testBlock: (MockLanguageClient, ServerNG) => Unit): Unit = {
    val driver = new Driver {}
    val config = EffektConfig(Seq("--experimental-server"))
    config.verify()

    val clientIn = new PipedInputStream()
    val clientOut = new PipedOutputStream()
    val serverIn = new PipedInputStream(clientOut)
    val serverOut = new PipedOutputStream(clientIn)

    val server = new ServerNG(config)

    val mockClient = new MockLanguageClient()
    server.connect(mockClient)

    val launcher = server.launch(mockClient, serverIn, serverOut)

    testBlock(mockClient, server)
  }

  // Fixtures
  //
  //

  val helloWorld = raw"""
                        |def main() = { println("Hello, world!") }
                        |""".textDocument

  val helloEffekt = raw"""
                        |def main() = { println("Hello, Effekt!") }
                        |"""

  // LSP: lifecycle events
  //
  //

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

  // LSP: Changes to text documents
  //
  //

  test("didOpen yields error diagnostics") {
    withClientAndServer { (client, server) =>
      val (textDoc, range) = raw"""
                       |val x: Int = "String"
                       |             ↑      ↑
                       |""".textDocumentAndRange

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val diagnostic = new Diagnostic()
      diagnostic.setRange(range)
      diagnostic.setSeverity(DiagnosticSeverity.Error)
      diagnostic.setSource("effekt")
      diagnostic.setMessage("Expected Int but got String.")

      val diagnosticsWithError = new util.ArrayList[Diagnostic]()
      diagnosticsWithError.add(diagnostic)

      val expected = List(
        new PublishDiagnosticsParams("file://test.effekt", new util.ArrayList[Diagnostic]()),
        new PublishDiagnosticsParams("file://test.effekt", diagnosticsWithError)
      )

      val diagnostics = client.diagnostics()
      assertEquals(diagnostics, expected)
    }
  }

  test("didChange yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Pop the diagnostics from the queue before changing the document
      val _ = client.diagnostics()

      val (textDoc, changeEvent) = helloWorld.changeTo(helloEffekt)

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

      val diagnostics = client.diagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  test("didSave yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Pop the diagnostics from the queue before changing the document
      val _ = client.diagnostics()

      val (textDoc, changeEvent) = helloWorld.changeTo(helloEffekt)

      val didSaveParams = new DidSaveTextDocumentParams()
      didSaveParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didSaveParams.setText(textDoc.getText)
      server.getTextDocumentService().didSave(didSaveParams)

      val diagnostics = client.diagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  test("didClose yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      // We use an erroneous example to show that closing the document clears the diagnostics.
      val textDoc = raw"""val x: Int = "String"""".textDocument

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Pop the diagnostics from the queue before closing the document
      val _ = client.diagnostics()

      val didCloseParams = new DidCloseTextDocumentParams()
      didCloseParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      server.getTextDocumentService().didClose(didCloseParams)

      val diagnostics = client.diagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  // LSP: Hovering
  //
  //

  test("Hovering over symbol shows type information") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |val x: Int = 42
                                |    ↑
                                |""".textDocumentAndPosition
      val hoverContents =
        raw"""|#### Value binder
              |```effekt
              |test::x: Int
              |```
              |""".stripMargin

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val hoverParams = new HoverParams(textDoc.versionedTextDocumentIdentifier, cursor)
      val hover = server.getTextDocumentService().hover(hoverParams).get()

      val expectedHover = new Hover()
      expectedHover.setRange(new Range(cursor, cursor))
      expectedHover.setContents(new MarkupContent("markdown", hoverContents))
      assertEquals(hover, expectedHover)
    }
  }

  // FIXME: Hovering over holes does not work at the moment.
  // https://github.com/effekt-lang/effekt/issues/549
  test("Hovering over hole shows nothing") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |def foo(x: Int) = <>
                                |                  ↑
                                |""".textDocumentAndPosition
      val hoverContents = ""

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val hoverParams = new HoverParams(textDoc.versionedTextDocumentIdentifier, cursor)
      val hover = server.getTextDocumentService().hover(hoverParams).get()

      val expectedHover = new Hover()
      expectedHover.setRange(new Range(cursor, cursor))
      expectedHover.setContents(new MarkupContent("markdown", hoverContents))
      assertEquals(hover, expectedHover)
    }
  }

  /*
  * def main() = {
    var foo = 1
    <>
}
  * */

  test("Hovering over mutable binder without extended description") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |def main() = {
                                |  var foo = 1
                                |       ↑
                                |  <>
                                |}
                                |""".textDocumentAndPosition
      val hoverContents =
        raw"""#### Mutable variable binder
             |```effekt
             |foo: Int
             |```
             |""".stripMargin

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val hoverParams = new HoverParams(textDoc.versionedTextDocumentIdentifier, cursor)
      val hover = server.getTextDocumentService().hover(hoverParams).get()

      val expectedHover = new Hover()
      expectedHover.setRange(new Range(cursor, cursor))
      expectedHover.setContents(new MarkupContent("markdown", hoverContents))
      assertEquals(hover, expectedHover)
    }
  }

  test("Hovering over mutable binder with extended description") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |def main() = {
                                |  var foo = 1
                                |       ↑
                                |  <>
                                |}
                                |""".textDocumentAndPosition
      val hoverContents =
        raw"""#### Mutable variable binder
             |```effekt
             |foo: Int
             |```
             |Like in other languages, mutable variable binders like `foo`
             |can be modified (e.g., `foo = VALUE`) by code that has `foo`
             |in its lexical scope.
             |
             |However, as opposed to other languages, variable binders in Effekt
             |are stack allocated and show the right backtracking behavior in
             |combination with effect handlers.
             |""".stripMargin + "         \n"

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val configParams = new DidChangeConfigurationParams()
      val settings: JsonElement = JsonParser.parseString("""{"showExplanations": true}""")
      configParams.setSettings(settings)
      server.getWorkspaceService().didChangeConfiguration(configParams)

      val hoverParams = new HoverParams(textDoc.versionedTextDocumentIdentifier, cursor)
      val hover = server.getTextDocumentService().hover(hoverParams).get()

      val expectedHover = new Hover()
      expectedHover.setRange(new Range(cursor, cursor))
      expectedHover.setContents(new MarkupContent("markdown", hoverContents))
      assertEquals(hover, expectedHover)
    }
  }

  // Text document DSL
  //
  //

  test("Correct cursor position") {
    val (textDoc, cursor) = raw"""
                                |def main() = { println("Hello, world!") }
                                |    ↑
                                |""".textDocumentAndPosition

    assertEquals(cursor, new org.eclipse.lsp4j.Position(1, 4))
  }

  test("Missing cursor throws exception") {
    intercept[IllegalArgumentException] {
      raw"""
           |def main() = { println("Hello, world!") }
           |""".textDocumentAndPosition
    }
  }

  test("Correct multiline range") {
    val (textDoc, range) = raw"""
      | There is some content here.
      |     ↑
      | And here.
      |     ↑
      |""".textDocumentAndRange

    val textWithoutRanges = raw"""
      | There is some content here.
      | And here.""".stripMargin

    assertEquals(range.getStart, new org.eclipse.lsp4j.Position(1, 5))
    assertEquals(range.getEnd, new org.eclipse.lsp4j.Position(2, 6))
    assertEquals(textDoc.getText, textWithoutRanges)
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

// DSL for creating text documents using extension methods for String
object TextDocumentSyntax {
  implicit class StringOps(val content: String) extends AnyVal {
    def textDocument(version: Int): TextDocumentItem =
      new TextDocumentItem("file://test.effekt", "effekt", version, content.stripMargin)

    def textDocument: TextDocumentItem =
      new TextDocumentItem("file://test.effekt", "effekt", 0, content.stripMargin)

    def textDocumentAndPosition: (TextDocumentItem, Position) = {
      val (textDocument, positions) = content.textDocumentAndPositions

      if (positions.length != 1)
        throw new IllegalArgumentException("Exactly one marker line (with '" + "↑" + "') is required.")

      (textDocument, positions.head)
    }

    def textDocumentAndRange: (TextDocumentItem, Range) = {
      val (textDocument, positions) = content.textDocumentAndPositions
      if (positions.length != 2)
        throw new IllegalArgumentException("Exactly two marker lines (with '" + "↑" + "') are required.")
      val start = positions(0)
      val end = positions(1)
      // The end of the range is exclusive, so we need to increment the character position.
      val range = new Range(start, new Position(end.getLine, end.getCharacter + 1))
      (textDocument, range)
    }

    def textDocumentAndPositions: (TextDocumentItem, Seq[Position]) = {
      val lines = content.stripMargin.split("\n").toBuffer
      val positions = scala.collection.mutable.ArrayBuffer[Position]()
      var lineIdx = 0
      while (lineIdx < lines.length) {
        val line = lines(lineIdx)
        if (line.contains("↑")) {
          if (lineIdx == 0)
            throw new IllegalArgumentException("Marker on first line cannot refer to a previous line.")
          // There may be multiple markers on the same line, so we need to record all of them.
          for (i <- line.indices if line(i) == '↑') {
            positions += new Position(lineIdx - 1, i)
          }
          lines.remove(lineIdx)
          // adjust index because of removal
          lineIdx -= 1
        }
        lineIdx += 1
      }
      val newContent = lines.mkString("\n")
      (newContent.textDocument, positions.toList)
    }
  }

  implicit class TextDocumentOps(val textDocument: TextDocumentItem) extends AnyVal {
    def changeTo(newContent: String): (TextDocumentItem, TextDocumentContentChangeEvent) = {
      val newDoc = new TextDocumentItem(textDocument.getUri, textDocument.getLanguageId, textDocument.getVersion + 1, newContent.stripMargin)
      val changeEvent = new TextDocumentContentChangeEvent(newDoc.getText)
      (newDoc, changeEvent)
    }

    def versionedTextDocumentIdentifier: VersionedTextDocumentIdentifier =
      new VersionedTextDocumentIdentifier(textDocument.getUri, textDocument.getVersion)
  }
}
