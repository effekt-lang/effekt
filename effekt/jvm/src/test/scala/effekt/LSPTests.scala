package effekt

import com.google.gson.{JsonElement, JsonParser}
import munit.FunSuite
import org.eclipse.lsp4j.{DefinitionParams, Diagnostic, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, Hover, HoverParams, InitializeParams, InitializeResult, InlayHint, InlayHintKind, InlayHintParams, MarkupContent, MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ReferenceContext, ReferenceParams, SaveOptions, ServerCapabilities, SetTraceParams, ShowMessageRequestParams, SymbolInformation, SymbolKind, TextDocumentContentChangeEvent, TextDocumentItem, TextDocumentSyncKind, TextDocumentSyncOptions, VersionedTextDocumentIdentifier}
import org.eclipse.lsp4j.jsonrpc.messages

import java.io.{PipedInputStream, PipedOutputStream}
import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.jdk.CollectionConverters.*

class LSPTests extends FunSuite {
  // Import the extension method for String
  import TextDocumentSyntax.*

  // Test helpers
  //
  //

  def withClientAndServer(testBlock: (MockLanguageClient, Server) => Unit): Unit = {
    val driver = new Driver {}
    val config = EffektConfig(Seq("--server"))
    config.verify()

    val clientIn = new PipedInputStream()
    val clientOut = new PipedOutputStream()
    val serverIn = new PipedInputStream(clientOut)
    val serverOut = new PipedOutputStream(clientIn)

    // The server currently uses `compileOnChange = false` by default, but we set it to `true` for testing
    // because we would like to switch to `didChange` events once we have working caching for references.
    val server = new Server(config, compileOnChange = true)

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
      expectedCapabilities.setHoverProvider(true)
      expectedCapabilities.setDefinitionProvider(true)
      expectedCapabilities.setReferencesProvider(true)
      expectedCapabilities.setDocumentSymbolProvider(true)
      expectedCapabilities.setCodeActionProvider(true)
      expectedCapabilities.setInlayHintProvider(true)

      val saveOptions = new SaveOptions()
      saveOptions.setIncludeText(true)

      val syncOptions = new TextDocumentSyncOptions();
      syncOptions.setOpenClose(true);
      syncOptions.setChange(TextDocumentSyncKind.Full);
      syncOptions.setSave(saveOptions);
      expectedCapabilities.setTextDocumentSync(syncOptions);

      assertEquals(initializeResult, new InitializeResult(expectedCapabilities))
    }
  }

  test("didOpen yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)

      val diagnostics = client.receivedDiagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(helloWorld.getUri, new util.ArrayList())))
    }
  }

  test("setTrace is implemented") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = SetTraceParams("off")
      server.setTrace(params)
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

      val diagnostics = client.receivedDiagnostics()
      assertEquals(diagnostics, expected)
    }
  }

  test("didChange yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Pop the diagnostics from the queue before changing the document
      val _ = client.receivedDiagnostics()

      val (textDoc, changeEvent) = helloWorld.changeTo(helloEffekt)

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

      val diagnostics = client.receivedDiagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  test("didSave yields empty diagnostics") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Pop the diagnostics from the queue before changing the document
      val _ = client.receivedDiagnostics()

      val (textDoc, changeEvent) = helloWorld.changeTo(helloEffekt)

      val didSaveParams = new DidSaveTextDocumentParams()
      didSaveParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didSaveParams.setText(textDoc.getText)
      server.getTextDocumentService().didSave(didSaveParams)

      val diagnostics = client.receivedDiagnostics()
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
      val _ = client.receivedDiagnostics()

      val didCloseParams = new DidCloseTextDocumentParams()
      didCloseParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      server.getTextDocumentService().didClose(didCloseParams)

      val diagnostics = client.receivedDiagnostics()
      assertEquals(diagnostics, Seq(new PublishDiagnosticsParams(textDoc.getUri, new util.ArrayList())))
    }
  }

  test("didSave doesn't throw a NullPointerException when text is null") {
    withClientAndServer { (client, server) =>
      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)
      // Clear any initial diagnostics.
      val _ = client.receivedDiagnostics()

      val didSaveParams = new DidSaveTextDocumentParams()
      didSaveParams.setTextDocument(helloWorld.versionedTextDocumentIdentifier)
      // The text is set to null
      didSaveParams.setText(null)

      server.getTextDocumentService().didSave(didSaveParams)
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

  // LSP: Document symbols
  //
  //

  test("documentSymbols returns expected symbols") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |def mySymbol() = <>
             |↑   ↑       ↑      ↑
             |""".textDocumentAndPositions

      val expectedSymbols: List[messages.Either[SymbolInformation, DocumentSymbol]] = List(
        messages.Either.forRight(new DocumentSymbol(
          "mySymbol",
          SymbolKind.Method,
          new Range(positions(0), positions(3)),
          new Range(positions(1), positions(2)),
          "Function",
        ))
      )

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = new DocumentSymbolParams()
      params.setTextDocument(textDoc.versionedTextDocumentIdentifier)

      val documentSymbols = server.getTextDocumentService().documentSymbol(params).get()
      // FIXME: The server currently returns spurious symbols at position (0, 0) that we need to filter out.
      val filtered = server.getTextDocumentService().documentSymbol(params).get().asScala.filter {
        symbol => symbol.getRight.getRange.getStart != new Position(0, 0) && symbol.getRight.getRange.getEnd != new Position(0, 0)
      }.asJava

      assertEquals(filtered, expectedSymbols.asJava)
    }
  }

  // LSP Go to definition
  //
  //

  test("definition returns expected range") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |def foo() = <>
             |↑             ↑
             |def bar() = foo()
             |             ↑
           """.textDocumentAndPositions

      val expectedRange = new Range(positions(0), positions(1))

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = new DefinitionParams()
      params.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      params.setPosition(positions(2))

      val definition = server.getTextDocumentService().definition(params).get().getLeft.get(0)
      assertEquals(definition.getRange, expectedRange)
    }
  }

  // LSP References
  //
  //

  // FIXME: the server doesn't actually return the reference to `foo` in `bar` in this example
  // It only returns the declaration site.
  test("references with setIncludeDeclaration returns declaration site") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |def foo() = <>
             |    ↑  ↑
             |def bar() = foo()
           """.textDocumentAndPositions

      val expectedReferences: List[Range] = List(
        new Range(positions(0), positions(1)),
      )

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = new ReferenceParams()
      params.setPosition(positions(0))
      val context = new ReferenceContext()
      context.setIncludeDeclaration(true)
      params.setContext(context)
      params.setTextDocument(textDoc.versionedTextDocumentIdentifier)

      val references = server.getTextDocumentService().references(params).get()
      assertEquals(references.asScala.map(_.getRange).toList, expectedReferences)
    }
  }

  // LSP: Inlay hints
  //
  //

  test("inlayHints should show the io effect") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) = raw"""
                                |↑
                                |def main() = {
                                |↑
                                |  println("Hello, world!")
                                |}
                                |↑
                                |""".textDocumentAndPositions

      val inlayHint = new InlayHint()
      inlayHint.setKind(InlayHintKind.Type)
      inlayHint.setPosition(positions(1))
      inlayHint.setLabel("{io}")
      val markup = new MarkupContent()
      markup.setKind("markdown")
      markup.setValue("captures: `{io}`")
      inlayHint.setTooltip(markup)
      inlayHint.setPaddingRight(true)
      inlayHint.setData("capture")

      val expectedInlayHints = List(inlayHint)

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = new InlayHintParams()
      params.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      params.setRange(new Range(positions(0), positions(2)))

      val inlayHints = server.getTextDocumentService().inlayHint(params).get()
      assertEquals(inlayHints, expectedInlayHints.asJava)
    }
  }

  // Effekt: Publish IR
  //
  //

  test("When showIR=source, server should provide source AST") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""
             |def main() = { println("Hello, world!") }
             |"""
      val textDoc = new TextDocumentItem("file://path/to/test.effekt", "effekt", 0, source.stripMargin)
      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showIR": "source"}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)

      val expectedIRContents =
        raw"""ModuleDecl(
             |  test,
             |  Nil,
             |  List(
             |    FunDef(
             |      IdDef(main),
             |      Nil,
             |      Nil,
             |      Nil,
             |      None(),
             |      BlockStmt(
             |        Return(
             |          Call(
             |            IdTarget(IdRef(Nil, println)),
             |            Nil,
             |            List(Literal(Hello, world!, ValueTypeApp(String_whatever, Nil))),
             |            Nil
             |          )
             |        )
             |      )
             |    )
             |  )
             |)""".stripMargin

      val receivedIRContent = client.receivedIR()
      assertEquals(receivedIRContent.length, 1)
      val fixedReceivedIR = receivedIRContent.head.content.replaceAll("String_\\d+", "String_whatever")
      assertEquals(fixedReceivedIR, expectedIRContents)
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

class MockLanguageClient extends EffektLanguageClient {
  private val diagnosticQueue: mutable.Queue[PublishDiagnosticsParams] = mutable.Queue.empty
  private val publishIRQueue: mutable.Queue[EffektPublishIRParams] = mutable.Queue.empty

  /**
   * Pops all diagnostics received since the last call to this method.
   */
  def receivedDiagnostics(): Seq[PublishDiagnosticsParams] = {
    val diagnostics = diagnosticQueue.toSeq
    diagnosticQueue.clear()
    diagnostics
  }

  /**
   * Pops all publishIR events received since the last call to this method.
   */
  def receivedIR(): Seq[EffektPublishIRParams] = {
    val irs = publishIRQueue.toSeq
    publishIRQueue.clear()
    irs
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

  override def publishIR(params: EffektPublishIRParams): Unit = {
    publishIRQueue.enqueue(params)
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
