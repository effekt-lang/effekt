/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2018-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

import kiama.util.Collections.{ mapToJavaMap, seqToJavaList }
import org.eclipse.lsp4j.{ Position => LSPPosition, Range => LSPRange, InlayHint => LSPInlayHint, InlayHintKind => LSPInlayHintKind, _ }
import org.eclipse.lsp4j.jsonrpc.messages. { Either => LSPEither }

import scala.jdk.CollectionConverters._

/**
 * A language server that is mixed with a compiler that provide the basis
 * for its services. Allows specialisation of configuration via `C`.
 */
trait Server[N, C <: Config, M <: Message] extends Compiler[C, M] with LanguageService[N] {

  import com.google.gson.{ JsonArray, JsonElement, JsonObject }
  import java.util.Collections
  import java.io.{ InputStream, OutputStream, PrintWriter }
  import scala.concurrent.ExecutionException
  import output.PrettyPrinterTypes.{ Document, emptyDocument, LinkRange, LinkValue }

  import kiama.util.Severities._
  import org.eclipse.lsp4j.jsonrpc.Launcher

  /**
   * The name of the language that this compiler processes. The best choice
   * is the extension used for files containing this language.
   */
  def name: String

  // Overriding endpoints to enable server functionality

  override def run(config: C): Unit =
    if (config.server())
      launch(config)
    else
      super.run(config)

  override def report(source: Source, messages: messaging.Messages, config: C): Unit =
    if (config.server())
      publishMessages(messages)
    else
      super.report(source, messages, config)

  // Client saving

  private[this] var client: Client = _

  def connect(aClient: Client): Unit = {
    client = aClient
  }

  // Client settings saving

  private[this] var _settings: JsonObject = _

  def settings(): JsonObject =
    _settings

  def setSettings(settings: JsonObject): Unit = {
    _settings = settings
  }

  def setSettings(settings: Object): Unit = {
    setSettings(settings.asInstanceOf[JsonElement].getAsJsonObject)
  }

  def setting[V](key: String, get: JsonElement => V, default: V): V =
    if (settings() == null)
      default
    else {
      val value = settings().get(key)
      if (value == null)
        default
      else
        get(value)
    }

  def settingBool(key: String, default: Boolean = false): Boolean =
    setting(key, _.getAsBoolean, default)

  def settingInt(key: String, default: Int = 0): Int =
    setting(key, _.getAsInt, default)

  def settingStr(key: String, default: String = ""): String =
    setting(key, _.getAsString, default)

  def settingArray(key: String, default: JsonArray = new JsonArray()): JsonArray =
    setting(key, _.getAsJsonArray, default)

  // Launching

  /**
   * When the --debug flag is used together with --server, we open the
   * server on port 5007 (or on --debugPort) instead of stdin and out. This way a modified
   * vscode client can connect to the running server, aiding development
   * of the language server and clients.
   *
   * In a vscode extension, the vscode client can connect to the server using
   * the following example code:
   *
   *   let serverOptions = () => {
   *     // Connect to language server via socket
   *     let socket: any = net.connect({ port: 5007 });
   *     let result: StreamInfo = {
   *       writer: socket,
   *       reader: socket
   *     };
   *     return Promise.resolve(result);
   *   };
   *
   * @see https://github.com/microsoft/language-server-protocol/issues/160
   */
  def launch(config: C): Unit = {
    if (config.debug()) {
      import java.net.InetSocketAddress
      import java.nio.channels.{ AsynchronousServerSocketChannel, Channels }
      import java.util.logging.{Logger, Level, ConsoleHandler}

      // Set up LSP logging
      val log = Logger.getLogger("LSP")
      log.setLevel(Level.ALL)
      val handler = new ConsoleHandler()
      handler.setLevel(Level.ALL)
      log.addHandler(handler)

      val port = config.debugPort()
      println("Starting debugging lsp server on port " + port)
      val addr = new InetSocketAddress("localhost", port)
      val socket = AsynchronousServerSocketChannel.open().bind(addr);

      try {
        println(s"Waiting on port ${port} for LSP clients to connect")
        val ch = socket.accept().get();
        println(s"Connected to LSP client")
        val in = Channels.newInputStream(ch)
        val out = Channels.newOutputStream(ch)

        launch(config, in, out, Some(new PrintWriter(System.err)))
      } catch {
        case e: InterruptedException =>
          e.printStackTrace()
        case e: ExecutionException =>
          e.printStackTrace()
      } finally {
        socket.close()
      }
    } else {
      launch(config, System.in, System.out, None)
    }
  }

  def launch(config: C, in: InputStream, out: OutputStream, tracing: Option[PrintWriter]): Unit = {
    val services = createServices(config)
    val launcherBase =
      new Launcher.Builder[Client]()
        .setLocalService(services)
        .setRemoteInterface(classOf[Client])
        .setInput(in)
        .setOutput(out)
    val launcher = tracing match {
      case Some(traceOut) => launcherBase.traceMessages(traceOut).create()
      case None => launcherBase.create()
    }
    val client = launcher.getRemoteProxy
    connect(client)
    launcher.startListening()
  }

  def createServices(config: C): Services[N, C, M] = new Services(this, config)


  // Notebooks

  private val notebooks = collection.mutable.Map[String, Notebook]()

  /**
   * Called when a notebook is opened. Creates initial notebook state and processes
   * all cells. The cells parameter contains pairs of (uri, content) where uri is the
   * cell's unique identifier and content is the cell's source code.
   **/
  def onNotebookOpen(notebookUri: String, cells: Seq[(String, String)]): Unit = {
    val notebook = Notebook(
      uri = notebookUri,
      cells = cells.map { case (uri, content) =>
        sources(uri) = BufferSource(GapBuffer(content), uri)
        uri -> NotebookCell(uri)
      }.toMap
    )
    notebooks(notebookUri) = notebook
  }

  /**
   * Handles structural changes to a notebook's cells
   */
  def onNotebookStructureChange(
    notebookUri: String,
    start: Int,
    deleteCount: Int,
    insertedUris: Seq[String]
  ): Unit = {
    notebooks.get(notebookUri).foreach { notebook =>
      val cells = notebook.cells.values.toVector
      val before = cells.take(start)
      val after = cells.drop(start + deleteCount)

      // create if not already exist (this can be the case if cells are re-ordered=delete+insert)
      insertedUris.foreach { uri => sources.getOrElseUpdate(uri, BufferSource(GapBuffer.empty, uri)) }
      val inserted = insertedUris.map(NotebookCell.apply)

      val updated = (before ++ inserted ++ after).map(c => c.uri -> c).toMap
      notebooks(notebookUri) = notebook.copy(cells = updated)
    }
  }

  /**
   * Handles content changes to a cell's text
   */
  def onNotebookContentChange(
    notebookUri: String,
    cellUri: String,
    changes: Seq[TextDocumentContentChangeEvent]
  ): Unit = {
    val bs = sources.get(cellUri) match {
      case Some(buffer: BufferSource) => buffer
      case Some(other) =>
        // Convert non-buffer source to GapBuffer
        val buffer = BufferSource(GapBuffer(other.content), cellUri)
        sources(cellUri) = buffer
        buffer
      case None => return
    }

    // Process each change in order
    changes.foreach { change =>
      val range = change.getRange
      val startLine = range.getStart.getLine
      val startChar = range.getStart.getCharacter
      val endLine = range.getEnd.getLine
      val endChar = range.getEnd.getCharacter

      // Apply the change using GapBuffer's replaceRange
      val newBuffer = bs.contents.replaceRange(
        startLine, startChar,
        endLine, endChar,
        change.getText
      )
      sources(cellUri) = BufferSource(newBuffer, cellUri)
    }

    // Update and process notebook accordingly
    notebooks.get(notebookUri).foreach { notebook =>
      notebook.cells.get(cellUri).foreach { cell =>
        processCell(cell, notebook)
      }
    }
  }

  /**
   * Called when a notebook is saved.
   **/
  def onNotebookSave(notebookUri: String): Unit = {
    notebooks.get(notebookUri).foreach { notebook =>
      notebook.cells.values.foreach { cell =>
        processCell(cell, notebook)
        // comment this in to see state of the notebook on save (in server debug mode)
        // println(sources.get(cell.uri))
      }
    }
  }

  /**
   * Called when a notebook is closed. Cleans up notebook state and removes all cell
   * sources and diagnostics.
   **/
  def onNotebookClose(notebookUri: String, cellUris: Seq[String]): Unit = {
    notebooks.remove(notebookUri)
    cellUris.foreach { uri =>
      clearDiagnostics(uri)
    }
  }

  // User messages

  def showMessage(tipe: MessageType, msg: String): Unit = {
    client.showMessage(new MessageParams(tipe, msg))
  }

  def logMessage(msg: String): Unit = {
    client.logMessage(new MessageParams(MessageType.Log, msg))
  }

  // Dynamic capabilities

  def registerCapability(id: String, method: String, options: Object): Unit = {
    val registration = new Registration(id, method, options)
    val params = new RegistrationParams(Collections.singletonList(registration))
    client.registerCapability(params)
  }

  // Diagnostics

  def publishMessages(messages: messaging.Messages): Unit = {
    val groups = messages.groupBy(msg => msg.sourceName.getOrElse(""))
    for ((name, msgs) <- groups) {
      publishDiagnostics(name, msgs.distinct.map(messageToDiagnostic))
    }
  }

  def toURI(filename: String): String = filename match {
    case _ if filename startsWith "file:" =>
      filename
    case _ if filename startsWith "vscode-notebook-cell:" =>
      filename
    case _ if filename startsWith "./" =>
      s"file://${Filenames.cwd()}/${Filenames.dropPrefix(filename, ".")}"
    case _ =>
      s"file://$filename"
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams(toURI(name), seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def messageToDiagnostic(message: M): Diagnostic =
    diagnostic(message.range, messaging.formatContent(message), message.severity)

  def diagnostic(range: Option[Range], message: String, severity: Severity, related: List[RelatedInfo] = Nil): Diagnostic = {
    val lspRange = range.map(convertRange).getOrElse(emptyRange)
    val lspSeverity = convertSeverity(severity)
    val lspDiagnostic = new Diagnostic(lspRange, message, lspSeverity, name)

    if (related.nonEmpty) {
      val lspRelated = related.map { r =>
        val loc = r.range.map(rangeToLocation).getOrElse(emptyLocation)
        new DiagnosticRelatedInformation(loc, r.message)
      }
      lspDiagnostic.setRelatedInformation(seqToJavaList(lspRelated))
    }

    lspDiagnostic
  }

  def emptyPosition = new LSPPosition(0, 0)
  def emptyRange = new LSPRange(emptyPosition, emptyPosition)
  def emptyLocation = new Location("<no-source>", emptyRange)

  def convertPosition(optPos: Option[Position]): LSPPosition =
    optPos.map(convertPosition).getOrElse(emptyPosition)

  def convertPosition(pos: Position): LSPPosition =
    new LSPPosition(pos.line - 1, pos.column - 1)

  def convertRange(optStart: Option[Position], optFinish: Option[Position]): LSPRange =
    new LSPRange(convertPosition(optStart), convertPosition(optFinish))

  def convertRange(r: Range): LSPRange =
    new LSPRange(convertPosition(r.from), convertPosition(r.to))

  def rangeToLocation(r: Range): Location =
    new Location(r.from.source.name, convertRange(r))

  def fromLSPPosition(position: LSPPosition, source: Source): Position =
    Position(position.getLine + 1, position.getCharacter + 1, source)

  def fromLSPRange(range: LSPRange, source: Source): Range =
    Range(
      fromLSPPosition(range.getStart, source),
      fromLSPPosition(range.getEnd, source)
    )

  def convertSeverity(severity: Severity): DiagnosticSeverity =
    severity match {
      case Error       => DiagnosticSeverity.Error
      case Warning     => DiagnosticSeverity.Warning
      case Information => DiagnosticSeverity.Information
      case Hint        => DiagnosticSeverity.Hint
    }

  // Monto

  // Monto is a service to send additional documents to the language client (like vscode). Depending on the client,
  // they can be rendered in additional window panes. This features is mostly used to show intermediate representations
  // and compilation output.
  // The additional documents can be linked to the original source file, allowing users making connections between
  // generated code and source code.

  case class RangePair(
    sstart: Int, send: Int,
    tstart: Int, tend: Int
  )

  def publishProduct(
    source: Source, name: String, language: String,
    document: Document = emptyDocument,
    append: Boolean = false
  ): Unit = {
    val uri = s"file://${source.name}"
    val content = document.layout
    val pairs = positionsOfDocument(document)
    val rangeMap = sortBySourceRangeSize(pairsToMap(pairs, pairToSourceRange, pairToTargetRange))
    val rangeMapRev = sortBySourceRangeSize(pairsToMap(pairs, pairToTargetRange, pairToSourceRange))
    client.publishProduct(
      Product(uri, name, language, content, append, rangeMap, rangeMapRev)
    )
  }

  def publishProductStr(
    source: Source, name: String, language: String,
    message: String = "", append: Boolean = false
  ): Unit = {
    publishProduct(source, name, language, Document(message, Nil), append)
  }

  def sortBySourceRangeSize(pairs: Array[RangeEntry]): Array[RangeEntry] =
    pairs.sortBy {
      entry => entry.source.end - entry.source.start
    }

  def pairsToMap(
    pairs: List[RangePair],
    key: RangePair => OffsetRange,
    value: RangePair => OffsetRange
  ): Array[RangeEntry] = {
    pairs.groupBy(key).toArray.map {
      case (s, ts) =>
        RangeEntry(s, ts.map(value).toArray)
    }
  }

  def pairToSourceRange(pair: RangePair): OffsetRange =
    OffsetRange(pair.sstart, pair.send)

  def pairToTargetRange(pair: RangePair): OffsetRange =
    OffsetRange(pair.tstart, pair.tend)

  def positionsOfDocument(document: Document): List[RangePair] =
    document.links.flatMap {
      case LinkValue(n, r) =>
        val start = positions.getStart(n)
        val finish = positions.getFinish(n)
        positionOfStartFinish(start, finish) match {
          case Some((s, f)) =>
            Some(RangePair(s, f, r.start, r.end))
          case None =>
            None
        }
      case LinkRange(f, t) =>
        Some(RangePair(f.start, f.end, t.start, t.end))
    }

  def positionOfStartFinish(optStart: Option[Position], optFinish: Option[Position]): Option[(Int, Int)] =
    (optStart, optFinish) match {
      case (Some(start), Some(finish)) =>
        (start.optOffset, finish.optOffset) match {
          case (Some(s), Some(f)) =>
            Some((s, f))
          case _ =>
            None
        }
      case _ =>
        None
    }

  // Support for services
  def locationOfNode(node: N): Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        val s = convertPosition(start)
        val f = convertPosition(finish)
        new Location(toURI(st.source.name), new LSPRange(s, f))
      case _ =>
        null
    }
  }

  def rangeOfNode(node: N): LSPRange =
    convertRange(positions.getStart(node), positions.getFinish(node))
}

trait LanguageService[N] {

  /**
   * A representation of a simple named code action that replaces
   * a tree node with other text.
   */
  // FIXME: can the "to" be a node too? But server can't access correct PP...
  case class TreeAction(name: String, uri: String, rangeFrom: Position, rangeTo: Position, to: String)

  case class RelatedInfo(range: Option[Range], message: String)

  enum InlayHintKind {
    case Type, Parameter

    def toLSP(): LSPInlayHintKind = this match
      case InlayHintKind.Type => LSPInlayHintKind.Type
      case InlayHintKind.Parameter => LSPInlayHintKind.Parameter
  }

  /**
   * A representation of a simple inlay hint.
   *
   * TODO: Support for attached text edit[s] (that must materialize the inlay hint, making it obsolete).
   * @see LSPInlayHint
   */
  case class InlayHint(kind: InlayHintKind, position: Position, label: String, markdownTooltip: Option[String], paddingLeft: Boolean = false, paddingRight: Boolean = false)

  /**
   * Return applicable code actions for the given position (if any).
   * Each action is in terms of an old tree node and a new node that
   * replaces it. Default is to return no actions.
   */
  def getCodeActions(position: Position): Option[Vector[TreeAction]] =
    None

  /**
   * Return the corresponding definition node for the given position
   * (if any). Default is to never return anything.
   */
  def getDefinition(position: Position): Option[N] =
    None

  /**
   * Return a formatted version of the whole of the given source.
   * By default, return `None` meaning there is no formatter.
   */
  def getFormatted(source: Source): Option[String] =
    None

  /**
   * Return markdown hover markup for the given position (if any).
   * Default is to never return anything.
   */
  def getHover(position: Position): Option[String] =
    None

  /**
   * Return the corresponding reference nodes (uses) of the symbol
   * at the given position (if any). If `includeDecl` is true, also
   * include the declaration of the symbol. Default is to never return
   * anything.
   */
  def getReferences(position: Position, includeDecl: Boolean): Option[Vector[N]] =
    None

  /**
   * Return the inlay hints in the range (if any).
   * Default is to never return anything.
   *
   * TODO: Remove debug info :)
   */
  def getInlayHints(range: Range): Option[Vector[InlayHint]] =
    println(s"Getting inlay hints for $range")
    Option(Vector(
      // Type hint example - shows type after a variable declaration
      InlayHint(
        kind = InlayHintKind.Type,
        position = Position(1, 5, range.from.source), // after variable name
        label = ": Int",
        markdownTooltip = Some("The _inferred type_ of this variable is `Int`"), // tooltip on hover
        paddingLeft = true
      ),

      // Parameter hint example - shows parameter names at call sites
      InlayHint(
        kind = InlayHintKind.Parameter,
        position = Position(4, 4, range.from.source), // before argument
        label = "x=",
        Some("Parameter `x` of type `Int`") // tooltip on hover
      )))

  /**
   * Return the symbols frmo a compilation unit. Default is to return
   * no symbols.
   */
  def getSymbols(source: Source): Option[Vector[DocumentSymbol]] =
    None

  /**
   * The parameters are passed as an array, potentially containing gson.Json objects or primitives.
   * The first argument is required to be { uri: String } and used to obtain the source.
   */
  def executeCommand(source: Source, executeCommandParams: ExecuteCommandParams): Option[Any] = None

  /**
   * Process a cell in the given notebook. Default is to do nothing,
   * meaning cells won't be processed until a compiler overrides this.
   */
  def processCell(cell: NotebookCell, notebook: Notebook): Unit = ()
}

class Services[N, C <: Config, M <: Message](
  server: Server[N, C, M],
  config: C
) {

  import java.util.concurrent.CompletableFuture
  import org.eclipse.lsp4j.jsonrpc.{ CancelChecker, CompletableFutures }
  import org.eclipse.lsp4j.jsonrpc.services._
  import scala.language.implicitConversions
  import com.google.gson.JsonObject

  implicit def toJavaFunction[U, V](f: Function1[U, V]): java.util.function.Function[U, V] =
    new java.util.function.Function[U, V] {
      override def apply(t: U): V = f(t)
    }

  // Life-cycle

  /**
   * Exit status to return when the server exits. `shutdown` sets this
   * to zero to reflect proper shutdown protocol.
   */
  var exitStatus = 1

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFuture.completedFuture {
      server.setSettings(params.getInitializationOptions)
      val serverCapabilities = new ServerCapabilities()
      serverCapabilities.setCodeActionProvider(true)
      serverCapabilities.setDefinitionProvider(true)
      serverCapabilities.setDocumentFormattingProvider(true)
      serverCapabilities.setDocumentSymbolProvider(true)
      serverCapabilities.setHoverProvider(true)
      serverCapabilities.setReferencesProvider(true)
      serverCapabilities.setInlayHintProvider(true)

      // This way the full file contents are transferred, instead of diffs.
      serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

      // Enable syncing of notebooks
      // @see https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notebookDocument_synchronization

      // Create notebook filters
      val notebookFilter = new NotebookDocumentFilter()
      notebookFilter.setNotebookType("effekt-notebook")
      notebookFilter.setScheme("file")

      // Create NotebookSelector
      val notebookSelector = new NotebookSelector()
      notebookSelector.setNotebook(LSPEither.forRight(notebookFilter))
      notebookSelector.setCells(List(new NotebookSelectorCell("effekt")).asJava)

      // Set notebook document sync options
      val notebookSyncOptions = new NotebookDocumentSyncRegistrationOptions(
        List(notebookSelector).asJava,
        true
      )
      serverCapabilities.setNotebookDocumentSync(notebookSyncOptions)

      new InitializeResult(serverCapabilities)
    }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): Unit = {
    val saveOptions = new TextDocumentSaveRegistrationOptions()
    saveOptions.setIncludeText(true)
    server.registerCapability("kiama/textDocument/didSave", "textDocument/didSave", saveOptions)
  }

  @JsonNotification("exit")
  def exit(): Unit = {
    sys.exit(exitStatus)
  }

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture {
      exitStatus = 0
      new Object
    }
  }

  // Basic text document processing

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    process(document.getUri, document.getText)
  }

  @JsonNotification("textDocument/didChange")
  def didChange(params: DidChangeTextDocumentParams): Unit = {
    if (server.settingBool("updateOnChange")) {
      process(params.getTextDocument.getUri, params.getContentChanges.get(0).getText)
    }
  }

  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): Unit = {
    process(params.getTextDocument.getUri, params.getText)
  }

  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = {
    server.clearDiagnostics(params.getTextDocument.getUri)
  }

  def process(uri: String, text: String): Unit = {
    server.clearDiagnostics(uri)
    server.compileString(uri, text, config)
  }

  def positionOfNotification(document: TextDocumentIdentifier, position: LSPPosition): Option[Position] =
    server.sources.get(document.getUri).map { source =>
      server.fromLSPPosition(position, source)
    }

  // Notebook services

  @JsonNotification("notebookDocument/didOpen")
  def notebookDidOpen(params: DidOpenNotebookDocumentParams): Unit = {
    val notebookDoc = params.getNotebookDocument
    val textDocs = params.getCellTextDocuments

    val cellsWithContents = params.getCellTextDocuments.asScala.map { doc => doc.getUri -> doc.getText }
    server.onNotebookOpen(notebookDoc.getUri, cellsWithContents.toSeq)
  }

  @JsonNotification("notebookDocument/didChange")
  def notebookDidChange(params: DidChangeNotebookDocumentParams): Unit = {
    val notebookUri = params.getNotebookDocument.getUri
    val cellChanges = Option(params.getChange.getCells)

    // 1. Handle structural changes
    if (cellChanges.exists(c => c.getStructure != null)) {
      val structure = cellChanges.get.getStructure
      val array = structure.getArray
      val start = array.getStart
      val deleteCount = array.getDeleteCount
      val newCells = Option(array.getCells).map(_.asScala).getOrElse(Seq.empty).map { c => c.getDocument }

      // Update notebook structure
      server.onNotebookStructureChange(notebookUri, start, deleteCount, newCells.toSeq)
    }

    // 2. Handle text content changes
    val textChanges = cellChanges.flatMap(c => Option(c.getTextContent))
    textChanges.foreach { changes =>
      changes.asScala.foreach { change =>
        server.onNotebookContentChange(
          notebookUri,
          change.getDocument.getUri,
          change.getChanges.asScala.toSeq
        )
      }
    }

    //    // 3. Handle cell metadata changes
    //    val dataChanges = cellChanges.flatMap(c => Option(c.getData))
    //    dataChanges.foreach { data =>
    //      server.onNotebookCellChange(notebookUri, data.asScala.toSeq)
    //    }
  }

  @JsonNotification("notebookDocument/didSave")
  def notebookDidSave(params: DidSaveNotebookDocumentParams): Unit = {
    server.onNotebookSave(params.getNotebookDocument.getUri)
  }

  @JsonNotification("notebookDocument/didClose")
  def notebookDidClose(params: DidCloseNotebookDocumentParams): Unit = {
    params.getCellTextDocuments
    val cellUris = params.getCellTextDocuments.asScala.map(_.getUri)
    server.onNotebookClose(
      params.getNotebookDocument.getUri,
      cellUris.toSeq
    )
  }

  @JsonNotification("textDocument/codeAction")
  def codeactions(params: CodeActionParams): CompletableFuture[Array[CodeAction]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getRange.getStart);
            treeActions <- server.getCodeActions(position);
            codeActions = treeActions.map {
              case server.TreeAction(name, uri, posFrom, posTo, newText) =>
                val textEdit = new TextEdit(server.convertRange(Some(posFrom), Some(posTo)), newText)
                val changes = Map(uri -> seqToJavaList(List(textEdit)))
                val workspaceEdit = new WorkspaceEdit(mapToJavaMap(changes))
                val action = new CodeAction(name)
                action.setKind(CodeActionKind.Refactor)
                action.setEdit(workspaceEdit)
                action
            }
          ) yield codeActions.toArray
        ).orNull
    )

  @JsonNotification("textDocument/definition")
  def definition(params: TextDocumentPositionParams): CompletableFuture[Location] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            definition <- server.getDefinition(position);
            location = server.locationOfNode(definition)
          ) yield location
        ).orNull
    )

  @JsonNotification("textDocument/documentSymbol")
  def symbols(params: DocumentSymbolParams): CompletableFuture[Array[DocumentSymbol]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            source <- server.sources.get(params.getTextDocument.getUri);
            symbols <- server.getSymbols(source)
          ) yield symbols.toArray
        ).getOrElse(Array())
    )

  @JsonNotification("textDocument/formatting")
  def formatting(params: DocumentFormattingParams): CompletableFuture[Array[TextEdit]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            source <- server.sources.get(params.getTextDocument.getUri);
            formatted <- server.getFormatted(source);
            start = new LSPPosition(0, 0);
            finish = new LSPPosition(source.lineCount, 0);
            edit = new TextEdit(new LSPRange(start, finish), formatted)
          ) yield Array(edit)
        ).orNull
    )

  /**
   * Converts a Markdown String into a `MarkupContent` required by LSP4J
   */
  def intoMarkdown(markdown: String): MarkupContent = {
    val markup = new MarkupContent()
    markup.setValue(markdown)
    markup.setKind("markdown")
    markup
  }

  @JsonNotification("textDocument/hover")
  def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            markdown <- server.getHover(position)
          ) yield new Hover(intoMarkdown(markdown))
        ).orNull
    )

  @JsonNotification("textDocument/references")
  def references(params: ReferenceParams): CompletableFuture[Array[Location]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            references <- server.getReferences(position, params.getContext.isIncludeDeclaration);
            locations = references.map(server.locationOfNode)
          ) yield locations.toArray
        ).orNull
    )

  @JsonRequest("textDocument/inlayHint")
  def inlayHint(params: InlayHintParams): CompletableFuture[Array[LSPInlayHint]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for { // TODO: use existing from/toLSP functions ("convert")
            source <- server.sources.get(params.getTextDocument.getUri)
            hints <- server.getInlayHints(server.fromLSPRange(params.getRange, source))
            lspHints: Vector[LSPInlayHint] = hints.map {
              case server.InlayHint(kind, position, label, markdownTooltip, paddingLeft, paddingRight) =>
                val lspHint = new LSPInlayHint(server.convertPosition(position), /* just a String label */ LSPEither.forLeft(label))
                lspHint.setKind(kind.toLSP())
                markdownTooltip.foreach { tooltip =>
                  lspHint.setTooltip(intoMarkdown(tooltip))
                }
                lspHint.setPaddingLeft(paddingLeft)
                lspHint.setPaddingRight(paddingRight)

                lspHint
            }
          } yield lspHints.toArray
        ).orNull
    )

  @JsonNotification("workspace/executeCommand")
  def commands(params: ExecuteCommandParams): CompletableFuture[Any] =
    CompletableFuture.completedFuture {
      (for {
        firstArg <- params.getArguments.toArray.headOption
        uri <- try {
          // ad-hoc parsing of json:
          // we require that the first argument is a json object with a field { uri: String }
          Some(firstArg.asInstanceOf[JsonObject].getAsJsonPrimitive("uri").getAsString)
        } catch { case e: Throwable => None }
        src <- server.sources.get(uri)
        res <- server.executeCommand(src, params)
      } yield res).orNull
    }

  // Workspace services

  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    server.setSettings(params.getSettings)
  }

  // Missing services not supported by LSP4J but sent by client side

  @JsonNotification("$/setTraceNotification")
  def setTrace(params: SetTraceNotificationParams): Unit = {
    // Do nothing
  }

}

class SetTraceNotificationParams(value: String = "off")

// Notebooks

case class NotebookCell(uri: String)

case class Notebook(
  uri: String,
  cells: Map[String, NotebookCell] = Map.empty
)
