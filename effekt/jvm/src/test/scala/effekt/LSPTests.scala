package effekt

import com.google.gson.{Gson, GsonBuilder, JsonElement, JsonParser}
import effekt.Intelligence.{TermBinding, TypeBinding}
import munit.FunSuite
import org.eclipse.lsp4j.{CodeAction, CodeActionKind, CodeActionParams, Command, DefinitionParams, Diagnostic, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, Hover, HoverParams, InitializeParams, InitializeResult, InlayHint, InlayHintKind, InlayHintParams, MarkupContent, MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ReferenceContext, ReferenceParams, SaveOptions, ServerCapabilities, SetTraceParams, ShowMessageRequestParams, SymbolInformation, SymbolKind, TextDocumentContentChangeEvent, TextDocumentItem, TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit, VersionedTextDocumentIdentifier, WorkspaceEdit}
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

  /**
   * @param compileOnChange The server currently uses `compileOnChange = false` by default, but we set it to `true` for testing
   *                        because we would like to switch to `didChange` events once we have working caching for references.
   */
  def withClientAndServer(compileOnChange: Boolean)(testBlock: (MockLanguageClient, Server) => Unit): Unit = {
    val driver = new Driver {}
    val config = EffektConfig(Seq("--server"))
    config.verify()

    val clientIn = new PipedInputStream()
    val clientOut = new PipedOutputStream()
    val serverIn = new PipedInputStream(clientOut)
    val serverOut = new PipedOutputStream(clientIn)

    val server = new Server(config, compileOnChange)

    val mockClient = new MockLanguageClient()
    server.connect(mockClient)

    val launcher = server.launch(_ => mockClient, serverIn, serverOut)

    testBlock(mockClient, server)
  }

  def withClientAndServer(testBlock: (MockLanguageClient, Server) => Unit): Unit = {
    withClientAndServer(true)(testBlock)
  }
  
  /** Normalize the output of the IR by replacing the generated identifiers and stripping all whitespace
   */
  def normalizeIRString(ir: String): String = {
    ir.replaceAll("_\\d+", "_whatever")
      .replaceAll("\\s+", "")
  }
  
  def assertIREquals(ir: String, expected: String): Unit = {
    val normalizedIR = normalizeIRString(ir)
    val normalizedExpected = normalizeIRString(expected)
    assertEquals(normalizedIR, normalizedExpected)
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

  // Regression test: https://github.com/effekt-lang/effekt/issues/1041
  test("Initialization with initializationOptions set to null does not crash") {
    withClientAndServer { (client, server) =>
      val params = new InitializeParams()
      params.setInitializationOptions(JsonParser.parseString("null"))
      server.initialize(params).get()
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

  test("Hovering over hole shows inside and outside types") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |def foo(x: Int): Bool = <{ x }>
                                |                          ↑
                                |""".textDocumentAndPosition
      val hoverContents =
        raw"""| | Outside       | Inside        |
             | |:------------- |:------------- |
             | | `Bool` | `Int` |
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

  // TODO: Hovering over a hole should show effects as well!
  test("Hovering over hole doesn't show effects") {
    withClientAndServer { (client, server) =>
      val (textDoc, cursor) = raw"""
                                |effect raise(): Unit
                                |def foo(x: Int): Int / { raise } = <{ do raise(); 42 }>
                                |                                     ↑
                                |""".textDocumentAndPosition
      val hoverContents =
        raw"""| | Outside       | Inside        |
             | |:------------- |:------------- |
             | | `Int` | `Int` |
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

  test("Hovering works after editing") {
    withClientAndServer { (client, server) =>
      // Initial code
      //
      //

      val (textDoc, firstPos) = raw"""
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

      val hoverParams = new HoverParams(textDoc.versionedTextDocumentIdentifier, firstPos)
      val hover = server.getTextDocumentService().hover(hoverParams).get()

      val expectedHover = (pos: Position) => {
        val expectedHover = new Hover()
        expectedHover.setRange(new Range(pos, pos))
        expectedHover.setContents(new MarkupContent("markdown", hoverContents))
        expectedHover
      }
      assertEquals(hover, expectedHover(firstPos))

      // First edit: now we add a blank line in front
      //
      //

      val (newTextDoc, changeEvent) = textDoc.changeTo(
        raw"""
             |
             |val x: Int = 42
             |""".stripMargin
      )
      val secondPos = new Position(firstPos.getLine + 1, firstPos.getCharacter)

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(newTextDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

      val hoverParamsAfterChange = new HoverParams(newTextDoc.versionedTextDocumentIdentifier, secondPos)
      val hoverAfterChange = server.getTextDocumentService().hover(hoverParamsAfterChange).get()

      assertEquals(hoverAfterChange, expectedHover(secondPos))

      // Second edit: we revert the change
      //
      //

      val (revertedTextDoc, revertedChangeEvent) = newTextDoc.changeTo(textDoc.getText)

      val didChangeParamsReverted = new DidChangeTextDocumentParams()
      didChangeParamsReverted.setTextDocument(revertedTextDoc.versionedTextDocumentIdentifier)
      didChangeParamsReverted.setContentChanges(util.Arrays.asList(revertedChangeEvent))
      server.getTextDocumentService().didChange(didChangeParamsReverted)

      val hoverParamsAfterRevert = new HoverParams(revertedTextDoc.versionedTextDocumentIdentifier, firstPos)
      val hoverAfterRevert = server.getTextDocumentService().hover(hoverParamsAfterRevert).get()

      assertEquals(hoverAfterRevert, expectedHover(firstPos))
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

  test("inlayHint shows the io capture on a def") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) = raw"""
                                |↑
                                |def main(): Unit = {
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

  test("inlayHint shows the io capture in explicit box syntax") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) = raw"""def foo(): Unit = { println("foo") }
                                |
                                |↑
                                |val bar = box { () => foo() }
                                |                             ↑
                                |
                                |↑
                                |""".textDocumentAndPositions

      val inlayHint = new InlayHint()
      inlayHint.setKind(InlayHintKind.Type)
      inlayHint.setPosition(positions(1))
      inlayHint.setLabel("at {io}")
      val markup = new MarkupContent()
      markup.setKind("markdown")
      markup.setValue("captures: `{io}`")
      inlayHint.setTooltip(markup)
      inlayHint.setPaddingLeft(true)
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

  test("inlayHint shows omitted return type and effect") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""effect raise(): Unit
             |
             |↑
             |def foo() = { do raise(); 5 }
             |↑        ↑
             |
             |↑
             |""".textDocumentAndPositions

      val captureHint = new InlayHint()
      captureHint.setKind(InlayHintKind.Type)
      captureHint.setPosition(positions(1))
      captureHint.setLabel("{}")
      captureHint.setData("capture")
      captureHint.setTooltip(new MarkupContent("markdown", "captures: `{}`"))
      captureHint.setPaddingRight(true)

      val omittedHint = new InlayHint()
      omittedHint.setKind(InlayHintKind.Type)
      omittedHint.setPosition(positions(2))
      omittedHint.setLabel(": Int / { raise }")
      omittedHint.setData("return-type-annotation")
      omittedHint.setTextEdits(List(
        new TextEdit(
          new Range(positions(2), positions(2)),
          ": Int / { raise }"
        )
      ).asJava)
      omittedHint.setTooltip(new MarkupContent("markdown", "return type: Int / { raise }"))
      omittedHint.setPaddingLeft(true)

      val expectedInlayHints = List(captureHint, omittedHint)

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val params = new InlayHintParams()
      params.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      params.setRange(new Range(positions(0), positions(3)))

      val inlayHints = server.getTextDocumentService().inlayHint(params).get()
      assertEquals(expectedInlayHints.asJava, inlayHints)
    }
  }

  test("inlayHint works after editing") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |↑
             |def main(): Unit = {
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

      // First edit: now we add a blank line in front
      //
      //

      val (newTextDoc, changeEvent) = textDoc.changeTo(
        raw"""
             |
             |def main(): Unit = {
             |  println("Hello, world!")
             |}
             |""".stripMargin
      )
      val newPos = new Position(positions(1).getLine + 1, positions(1).getCharacter)

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(newTextDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

      val paramsAfterChange = new InlayHintParams()
      paramsAfterChange.setTextDocument(newTextDoc.versionedTextDocumentIdentifier)
      paramsAfterChange.setRange(new Range(positions(0), new Position(positions(2).getLine + 1, positions(2).getCharacter)))

      inlayHint.setPosition(newPos)
      val inlayHintsAfterChange = server.getTextDocumentService().inlayHint(paramsAfterChange).get()
      assertEquals(inlayHintsAfterChange, expectedInlayHints.asJava)

      // Second edit: we revert the change
      //
      //

      val (revertedTextDoc, revertedChangeEvent) = newTextDoc.changeTo(textDoc.getText)
      inlayHint.setPosition(positions(1))

      val didChangeParamsReverted = new DidChangeTextDocumentParams()
      didChangeParamsReverted.setTextDocument(revertedTextDoc.versionedTextDocumentIdentifier)
      didChangeParamsReverted.setContentChanges(util.Arrays.asList(revertedChangeEvent))
      server.getTextDocumentService().didChange(didChangeParamsReverted)

      val paramsAfterRevert = new InlayHintParams()
      paramsAfterRevert.setTextDocument(revertedTextDoc.versionedTextDocumentIdentifier)
      paramsAfterRevert.setRange(new Range(positions(0), positions(2)))

      val inlayHintsAfterRevert = server.getTextDocumentService().inlayHint(paramsAfterRevert).get()
      assertEquals(inlayHintsAfterRevert, expectedInlayHints.asJava)
    }

  }

  test("inlayHint works after invalid edits") {
    withClientAndServer(false) { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |↑
             |def main(): Unit = {
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

      // Edit: now we add some invalid syntax to the end
      //
      //

      val (newTextDoc, changeEvent) = textDoc.changeTo(
        raw"""
             |def main(): Unit = {
             |  println("Hello, world!")
             |}
             |invalid syntax
             |""".stripMargin
      )

      val didChangeParams = new DidChangeTextDocumentParams()
      didChangeParams.setTextDocument(newTextDoc.versionedTextDocumentIdentifier)
      didChangeParams.setContentChanges(util.Arrays.asList(changeEvent))
      server.getTextDocumentService().didChange(didChangeParams)

      val paramsAfterChange = new InlayHintParams()
      paramsAfterChange.setTextDocument(newTextDoc.versionedTextDocumentIdentifier)
      // The client may send a range that is outside of the text the server currently has
      // We use somewhat arbitrary values here.
      paramsAfterChange.setRange(new Range(positions(0), new Position(positions(2).getLine + 1, positions(2).getCharacter + 5)))

      val inlayHintsAfterChange = server.getTextDocumentService().inlayHint(paramsAfterChange).get()
      assertEquals(inlayHintsAfterChange, expectedInlayHints.asJava)
    }
  }

  // LSP: Code Actions
  //
  //

  test("codeAction infers return type and effects") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |effect bar(): Unit
             |def foo(x: Int) = { do bar(); x }
             |    ↑          ↑
             |""".textDocumentAndPositions

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val codeActionParams = new CodeActionParams()
      codeActionParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      codeActionParams.setRange(new Range(positions(0), positions(0)))

      val expected: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Update return type with inferred effects")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          textEdit.setRange(Range(positions(1), positions(1)))
          textEdit.setNewText(": Int / { bar }")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://test.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val response = server.codeAction(codeActionParams).get()
      assertEquals(response, expected.asJava)
    }
  }

  test("codeAction doesn't show empty effects list") {
    withClientAndServer { (client, server) =>
      val (textDoc, positions) =
        raw"""
             |def foo(x: Int) = x
             |    ↑          ↑
             |""".textDocumentAndPositions

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val codeActionParams = new CodeActionParams()
      codeActionParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      codeActionParams.setRange(new Range(positions(0), positions(0)))

      val expected: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Update return type with inferred effects")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          textEdit.setRange(Range(positions(1), positions(1)))
          // Rather than `: Int / {}`, we just show `: Int`
          textEdit.setNewText(": Int")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://test.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val response = server.codeAction(codeActionParams).get()
      assertEquals(response, expected.asJava)
    }
  }

  // Regression test: we had codeActions targeting the wrong file
  test("codeAction response targets correct file") {
    withClientAndServer { (client, server) =>
      val (textDoc1, positions1) =
        raw"""
             |def foo(x: Int) = <{ x }>
             |    ↑          ↑
             |""".textDocumentAndPositions

      textDoc1.setUri("file://foo.effekt")

      val (textDoc2, positions2) =
        raw"""
             |def bar(x: Int) = <{ x }>
             |     ↑         ↑
             |""".textDocumentAndPositions

      textDoc2.setUri("file://bar.effekt")

      val didOpenParams1 = new DidOpenTextDocumentParams()
      didOpenParams1.setTextDocument(textDoc1)
      server.getTextDocumentService().didOpen(didOpenParams1)

      val didOpenParams2 = new DidOpenTextDocumentParams()
      didOpenParams2.setTextDocument(textDoc2)
      server.getTextDocumentService().didOpen(didOpenParams2)

      val codeActionParams1 = new CodeActionParams()
      codeActionParams1.setTextDocument(textDoc1.versionedTextDocumentIdentifier)
      codeActionParams1.setRange(new Range(positions1(0), positions1(0)))

      val codeActionParams2 = new CodeActionParams()
      codeActionParams2.setTextDocument(textDoc2.versionedTextDocumentIdentifier)
      codeActionParams2.setRange(new Range(positions2(0), positions2(0)))

      val expected1: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Update return type with inferred effects")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          textEdit.setRange(Range(positions1(1), positions1(1)))
          textEdit.setNewText(": Nothing")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://foo.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val expected2: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Update return type with inferred effects")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          val range = new Range(positions2(1), positions2(1))
          textEdit.setRange(range)
          textEdit.setNewText(": Nothing")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://bar.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val response1 = server.codeAction(codeActionParams1).get()
      val response2 = server.codeAction(codeActionParams2).get()

      assertEquals(response1, expected1.asJava)
      assertEquals(response2, expected2.asJava)
    }
  }

  test("codeAction can close hole with an expression") {
    withClientAndServer { (client, server) =>
      val (textDoc, range) =
        raw"""
             |def foo(x: Int): Int = <{ x }>
             |                       ↑     ↑
             |""".textDocumentAndRange

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val codeActionParams = new CodeActionParams()
      codeActionParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      codeActionParams.setRange(range)

      val expected: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Close hole")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          textEdit.setRange(range)
          textEdit.setNewText("x")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://test.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val response = server.codeAction(codeActionParams).get()
      assertEquals(response, expected.asJava)
    }
  }

  test("codeAction can close hole with statements") {
    withClientAndServer { (client, server) =>
      val (textDoc, range) =
        raw"""
             |def foo[T](x: T): Unit = <{ println("1"); println("2") }>
             |                         ↑                              ↑
             |""".textDocumentAndRange

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(textDoc)
      server.getTextDocumentService().didOpen(didOpenParams)

      val codeActionParams = new CodeActionParams()
      codeActionParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      codeActionParams.setRange(range)

      val expected: Seq[messages.Either[Command, CodeAction]] = Seq(
        messages.Either.forRight[Command, CodeAction]({
          val action = new CodeAction()
          action.setTitle("Close hole")
          action.setKind(CodeActionKind.Refactor)

          val edit = new WorkspaceEdit()

          val textEdit = new TextEdit()
          textEdit.setRange(range)
          textEdit.setNewText("locally { println(\"1\"); println(\"2\") }")

          val changes = new util.HashMap[String, util.List[TextEdit]]()
          val textEdits = new util.ArrayList[TextEdit]()
          textEdits.add(textEdit)
          changes.put("file://test.effekt", textEdits)

          edit.setChanges(changes)

          action.setEdit(edit)

          action
        })
      )

      val response = server.codeAction(codeActionParams).get()
      assertEquals(response, expected.asJava)
    }
  }

  // Effekt: Publish IR
  //
  //

  test("When showIR=source, server should provide source AST") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""def main() = <>""".textDocument
      val textDoc = new TextDocumentItem("file://path/to/test.effekt", "effekt", 0, source.getText)
      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showIR": "source"}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(source)
      server.getTextDocumentService().didOpen(didOpenParams)

      val expectedIRContents =
        raw"""ModuleDecl(
             |  test,
             |  Nil,
             |  List(
             |    FunDef(
             |      IdDef(
             |        main,
             |        Span(StringSource(def main() = <>, file://test.effekt), 4, 8, Real())
             |      ),
             |      Many(
             |        Nil,
             |        Span(StringSource(def main() = <>, file://test.effekt), 8, 8, Real())
             |      ),
             |      Many(
             |        Nil,
             |        Span(StringSource(def main() = <>, file://test.effekt), 8, 10, Real())
             |      ),
             |      Many(
             |        Nil,
             |        Span(StringSource(def main() = <>, file://test.effekt), 10, 10, Real())
             |      ),
             |      Maybe(
             |        None(),
             |        Span(StringSource(def main() = <>, file://test.effekt), 10, 10, Real())
             |      ),
             |      Return(
             |        Hole(
             |          IdDef(
             |            hole,
             |            Span(
             |              StringSource(def main() = <>, file://test.effekt),
             |              13,
             |              15,
             |              Synthesized()
             |            )
             |          ),
             |          Return(Literal((), ValueTypeApp(Unit_405, Nil))),
             |          Span(StringSource(def main() = <>, file://test.effekt), 13, 15, Real())
             |        )
             |      ),
             |      None(),
             |      Span(StringSource(def main() = <>, file://test.effekt), 0, 15, Real())
             |    )
             |  ),
             |  None(),
             |  Span(StringSource(def main() = <>, file://test.effekt), 0, 15, Real())
             |)""".stripMargin

      val receivedIRContent = client.receivedIR()
      assertEquals(receivedIRContent.length, 1)
      assertIREquals(receivedIRContent.head.content, expectedIRContents)
    }
  }

  test("When configuration changes, server should publish requested IR") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""
             |val foo = 42
             |"""
      val textDoc = new TextDocumentItem("file://path/to/test.effekt", "effekt", 0, source.stripMargin)
      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showIR": "source"}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(helloWorld)
      server.getTextDocumentService().didOpen(didOpenParams)

      // Receive the initial IR
      assertEquals(client.receivedIR().length, 1)

      val configParams = new DidChangeConfigurationParams()
      // When configuring the VSCode language client with `synchronize: { configurationSection: 'effekt' }`,
      // we get a `DidChangeConfigurationParams` with the configuration nested under the "effekt" key.
      val settings: JsonElement = JsonParser.parseString("""{"effekt": {"showIR": "core", "showTree": true}}""")
      configParams.setSettings(settings)
      server.getWorkspaceService().didChangeConfiguration(configParams)

      // Send a didSave event to trigger recompilation and IR publication
      val didSaveParams = new DidSaveTextDocumentParams()
      didSaveParams.setTextDocument(textDoc.versionedTextDocumentIdentifier)
      didSaveParams.setText(textDoc.getText)
      server.getTextDocumentService().didSave(didSaveParams)

      val expectedIRContents =
        raw"""ModuleDecl(
             |  test,
             |  List(effekt, option, list, result, exception, array, char, string, ref),
             |  Nil,
             |  Nil,
             |  List(
             |    Val(foo_whatever, Data(Int_whatever, Nil), Return(Literal(42, Data(Int_whatever, Nil))))
             |  ),
             |  List(foo_whatever)
             |)""".stripMargin


      val receivedIRContent = client.receivedIR()
      assertEquals(receivedIRContent.length, 1)
      assertIREquals(receivedIRContent.head.content, expectedIRContents)
    }
  }

  // Effekt: Publish holes
  //
  //

  test("Server publishes list of holes in file") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""
             |type MyInt = Int
             |def foo(x: Int): Bool = <{ x }>
             |def bar(x: String): Int = <{ <> }> // a hole within a hole
             |""".textDocument
      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showHoles": true}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(source)
      server.getTextDocumentService().didOpen(didOpenParams)

      val expectedHoles = List()

      val termsFoo = List(
        TermBinding(
          qualifier = List(),
          name = "x",
          `type` = Some(
            "Int"
          )
        ),
        TermBinding(
          qualifier = List(),
          name = "bar",
          `type` = Some(
            "String => Int"
          )
        ),
        TermBinding(
          qualifier = List(),
          name = "foo",
          `type` = Some(
            "Int => Bool"
          )
        )
      )

      val receivedHoles = client.receivedHoles()
      assertEquals(receivedHoles.length, 1)
      assertEquals(receivedHoles.head.holes.length, 3)
      assertEquals(receivedHoles.head.holes(0).id, "foo0")
      assertEquals(receivedHoles.head.holes(0).innerType, Some("Int"))
      assertEquals(receivedHoles.head.holes(0).expectedType, Some("Bool"))
      assertEquals(receivedHoles.head.holes(0).terms, termsFoo)
      assertEquals(receivedHoles.head.holes(0).types, List(
        TypeBinding(
          qualifier = Nil,
          name = "MyInt",
          definition = "type MyInt = Int"
        )
      ))
      assertEquals(receivedHoles.head.holes(1).id, "bar0")
      assertEquals(receivedHoles.head.holes(1).innerType, Some("Nothing"))
      assertEquals(receivedHoles.head.holes(1).expectedType, Some("Int"))

      assertEquals(receivedHoles.head.holes(2).id, "bar1")
      assertEquals(receivedHoles.head.holes(2).innerType, Some("Unit"))
      assertEquals(receivedHoles.head.holes(2).expectedType, None)
    }
  }

  test("Server publishes holes for literate Effekt") {
    withClientAndServer { (client, server) =>
      val source = new TextDocumentItem("file://path/to/test.effekt.md", "literate effekt", 0,
        raw"""# Some title
             |```effekt
             |def foo(x: Int): Bool = <>
             |```
             |""".stripMargin)

      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showHoles": true}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(source)
      server.getTextDocumentService().didOpen(didOpenParams)

      val receivedHoles = client.receivedHoles()
      assertEquals(receivedHoles.length, 1)
      assertEquals(receivedHoles.head.holes.length, 1)
    }
  }
  
  test("Server publishes hole id for nested defs") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""
             |def foo(): Unit = {
             |  def bar() = <>
             |  bar()
             |}
             |""".textDocument

      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showHoles": true}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(source)
      server.getTextDocumentService().didOpen(didOpenParams)

      val receivedHoles = client.receivedHoles()
      assertEquals(receivedHoles.length, 1)
      assertEquals(receivedHoles.head.holes.length, 1)
      assertEquals(receivedHoles.head.holes.head.id, "foo_bar0")
    }
  }

  /**
   * By default, Scala types such as List and Option show pathological (de)serialization behavior with Gson.
   * We use a custom extension method `withScalaSupport` which adds support for Scala collections, fixing serialization.
   */
  test("Hole info serializes to expected JSON") {
    val holeInfo = EffektHoleInfo(
      id = "foo_bar0",
      range = new Range(
        new Position(0, 0),
        new Position(0, 0)
      ),
      innerType = None,
      expectedType = Some("Bool"),
      importedTerms = Nil,
      importedTypes = Nil,
      terms = List(
        TermBinding(
          qualifier = Nil,
          name = "x",
          `type` = Some("Int")
        )
      ),
      types = List(
        TypeBinding(
          qualifier = Nil,
          name = "MyInt",
          definition = "type MyInt = Int"
        )
      )
    )

    val expectedJsonStr =
      """{"id":"foo_bar0","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"expectedType":"Bool","importedTerms":[],"importedTypes":[],"terms":[{"qualifier":[],"name":"x","type":"Int"}],"types":[{"qualifier":[],"name":"MyInt","definition":"type MyInt = Int"}]}""".stripMargin

    val expectedJson: JsonElement = JsonParser.parseString(expectedJsonStr)

    val gson: Gson = new GsonBuilder().withScalaSupport.create()

    val actualJson: JsonElement = gson.toJsonTree(holeInfo)

    assertEquals(actualJson, expectedJson)
  }

  test("Empty holes list is sent when there are no holes") {
    withClientAndServer { (client, server) =>
      val source =
        raw"""
             |def foo(x: Int): Int = x + 1
             |""".textDocument

      val initializeParams = new InitializeParams()
      val initializationOptions = """{"showHoles": true}"""
      initializeParams.setInitializationOptions(JsonParser.parseString(initializationOptions))
      server.initialize(initializeParams).get()

      val didOpenParams = new DidOpenTextDocumentParams()
      didOpenParams.setTextDocument(source)
      server.getTextDocumentService().didOpen(didOpenParams)

      val receivedHoles = client.receivedHoles()
      assertEquals(receivedHoles.length, 1)
      assert(receivedHoles.head.holes.isEmpty)
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
  private val publishHolesQueue: mutable.Queue[EffektPublishHolesParams] = mutable.Queue.empty

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

  def receivedHoles(): Seq[EffektPublishHolesParams] = {
    val holes = publishHolesQueue.toSeq
    publishHolesQueue.clear()
    holes
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

  override def publishHoles(params: EffektPublishHolesParams): Unit = {
    publishHolesQueue.enqueue(params)
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
