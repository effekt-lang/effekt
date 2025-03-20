package effekt

import effekt.context.Context
import kiama.util.Source
import munit.FunSuite
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.{CompletableFuture, Executors}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult, MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ServerCapabilities, ShowMessageRequestParams, TextDocumentContentChangeEvent, TextDocumentItem, TextDocumentSyncKind, VersionedTextDocumentIdentifier}

import java.util
import scala.collection.mutable
import scala.collection.mutable.Queue

class LSPTests extends FunSuite {
  // Import the extension method for String
  import TextDocumentSyntax._

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

    val server = new MockServer(config)

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

  // Tests
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

  test("didOpen yields error diagnostics") {
    withClientAndServer { (client, server) =>
      val (textDoc, range) = raw"""
                       |val x: Int = "String"
                       |             ⟦      ⟧
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

      val (textDoc, changeEvent) = helloWorld.changeTo(
        raw"""
             |def main() = { println("Hello, Effekt!") }
             |""")

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

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

  test("Missing cursor throws exception") {
    intercept[IllegalArgumentException] {
      raw"""
           |def main() = { println("Hello, world!") }
           |""".textDocumentAndCursor
    }
  }

  test("Correct multiline range") {
    val (textDoc, range) = raw"""
      | There is some content here.
      |     ⟦
      | And here.
      |     ⟧
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

class MockServer(config: EffektConfig) extends ServerNG(config) {

  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    super.afterCompilation(source, config)
  }
}

// DSL for creating text documents using extension methods for String
object TextDocumentSyntax {
  implicit class StringOps(val content: String) extends AnyVal {
    def textDocument(version: Int): TextDocumentItem =
      new TextDocumentItem("file://test.effekt", "effekt", version, content.stripMargin)

    def textDocument: TextDocumentItem =
      new TextDocumentItem("file://test.effekt", "effekt", 0, content.stripMargin)

    def textDocumentAndCursor: (TextDocumentItem, Position) = {
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
      (newContent.textDocument, cursor.get)
    }

    def textDocumentAndRange: (TextDocumentItem, Range) = {
      val lines = content.stripMargin.split("\n").toBuffer

      var startOpt: Option[(Int, Int)] = None
      var endOpt: Option[(Int, Int)] = None

      val startMarker = "⟦"
      val endMarker = "⟧"

      var lineIdx = 0
      while (lineIdx < lines.length) {
        val line = lines(lineIdx)
        val trimmed = line.trim
        if (trimmed.contains(startMarker) || trimmed.contains(endMarker)) {
          // The marker line must not contain any other non-whitespace characters.
          // There are two allowed cases:
          // 1. The line contains both markers (and nothing else except whitespace).
          // 2. The line contains exactly one marker.
          if (trimmed.contains(startMarker) && trimmed.contains(endMarker)) {
            // Expect the line to match pattern "⟦" followed by whitespace then "⟧"
            if (!trimmed.matches(s"^$startMarker\\s+$endMarker$$"))
              throw new IllegalArgumentException("Line with range markers may not contain other characters.")
            // Markers on the same line refer to the previous line.
            if (lineIdx == 0)
              throw new IllegalArgumentException("Range markers on first line cannot refer to a previous line.")
            val startCol = line.indexOf(startMarker)
            val endCol = line.indexOf(endMarker) + 1
            if (startOpt.isDefined || endOpt.isDefined)
              throw new IllegalArgumentException("Multiple range markers found.")
            startOpt = Some((lineIdx - 1, startCol))
            endOpt = Some((lineIdx - 1, endCol))
            lines.remove(lineIdx)
            lineIdx -= 1 // adjust since we removed a line
          } else if (trimmed == startMarker) {
            if (startOpt.isDefined)
              throw new IllegalArgumentException("Multiple range start markers found.")
            if (lineIdx == 0)
              throw new IllegalArgumentException("Range start marker on first line cannot refer to a previous line.")
            val startCol = line.indexOf(startMarker)
            startOpt = Some((lineIdx - 1, startCol))
            lines.remove(lineIdx)
            lineIdx -= 1
          } else if (trimmed == endMarker) {
            if (endOpt.isDefined)
              throw new IllegalArgumentException("Multiple range end markers found.")
            if (lineIdx == 0)
              throw new IllegalArgumentException("Range end marker on first line cannot refer to a previous line.")
            val endCol = line.indexOf(endMarker) + 1
            endOpt = Some((lineIdx - 1, endCol))
            lines.remove(lineIdx)
            lineIdx -= 1
          } else {
            throw new IllegalArgumentException("Line with range marker may not contain other characters.")
          }
        }
        lineIdx += 1
      }

      if (startOpt.isEmpty || endOpt.isEmpty)
        throw new IllegalArgumentException("Both range start and end markers must be provided.")

      val (startLine, startCol) = startOpt.get
      val (endLine, endCol) = endOpt.get

      if (startLine > endLine || (startLine == endLine && startCol >= endCol))
        throw new IllegalArgumentException("Range start must come before range end.")

      val newContent = lines.mkString("\n")
      val range = new Range(new Position(startLine, startCol), new Position(endLine, endCol))
      (newContent.textDocument, range)
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
