package effekt

import com.google.gson.JsonElement
import effekt.Intelligence.CaptureInfo
import effekt.context.Context
import effekt.source.Def.FunDef
import effekt.source.Term.Hole
import effekt.source.{Span, Tree}
import effekt.symbols.Binder.{ValBinder, VarBinder}
import effekt.symbols.BlockTypeConstructor.{ExternInterface, Interface}
import effekt.symbols.TypeConstructor.{DataType, ExternType}
import effekt.symbols.{Anon, Binder, Callable, Effects, ErrorMessageInterpolator, Module, Param, Symbol, TypeAlias, TypePrinter, UserFunction, ValueType, isSynthetic}
import effekt.util.messages.EffektError
import effekt.util.{PlainMessaging, PrettyPrinter}
import kiama.util.Collections.{mapToJavaMap, seqToJavaList}
import kiama.util.Convert.*
import kiama.util.{Collections, Convert, Position, Source}
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.{Launcher, messages}
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.*
import org.eclipse.lsp4j.{CodeAction, CodeActionKind, CodeActionParams, Command, DefinitionParams, Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, Hover, HoverParams, InitializeParams, InitializeResult, InlayHint, InlayHintKind, InlayHintParams, Location, LocationLink, MarkupContent, MessageParams, MessageType, PublishDiagnosticsParams, ReferenceParams, SaveOptions, ServerCapabilities, SetTraceParams, SymbolInformation, SymbolKind, TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit, WorkspaceEdit, Range as LSPRange}

import java.io.{InputStream, OutputStream, PrintWriter}
import java.net.ServerSocket
import java.nio.file.Paths
import java.util
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}

/**
 * Effekt Language Server
 *
 * @param compileOnChange Whether to compile on `didChange` events
 *                        Currently disabled because references are erased when there are any compiler errors.
 *                        Therefore, we currently only update on `didSave` until we have working caching for references.
 */
class Server(config: EffektConfig, compileOnChange: Boolean=false) extends LanguageServer with Driver with Intelligence with TextDocumentService with WorkspaceService {
  private var client: EffektLanguageClient = _
  private val textDocumentService = this
  private val workspaceService = this

  // Track whether shutdown has been requested
  private var shutdownRequested: Boolean = false
  // Configuration sent by the language client
  var settings: JsonElement = null

  val getDriver: Driver = this
  val getConfig: EffektConfig = config

  object lspMessaging extends PlainMessaging

  // LSP Lifecycle
  //
  //

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setHoverProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setReferencesProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setCodeActionProvider(true)
    capabilities.setInlayHintProvider(true)

    // We need to explicitly ask the client to include the text on save events.
    // Otherwise, when not listening to `didChange`, we have no way to get the text of the file,
    // when the client decides not to include the text in the `didSave` event.
    val saveOptions = new SaveOptions()
    saveOptions.setIncludeText(true)

    val syncOptions = new TextDocumentSyncOptions();
    syncOptions.setOpenClose(true);
    syncOptions.setChange(TextDocumentSyncKind.Full);
    syncOptions.setSave(saveOptions);
    capabilities.setTextDocumentSync(syncOptions);

    // Load the initial settings from client-sent `initializationOptions` (if any)
    // This is not part of the LSP standard, but seems to be the most reliable way to have the correct initial settings
    // on first compile.
    // There is a `workspace/didChangeConfiguration` notification and a `workspace/configuration` request, but we cannot
    // guarantee that the client will send these before the file is first compiled, leading to either duplicate work
    // or a bad user experience.
    if (params.getInitializationOptions != null)
      workspaceService.didChangeConfiguration(new DidChangeConfigurationParams(params.getInitializationOptions))

    val result = new InitializeResult(capabilities)
    CompletableFuture.completedFuture(result)
  }

  override def shutdown(): CompletableFuture[Object] = {
    shutdownRequested = true
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {
    System.exit(if (shutdownRequested) 0 else 1)
  }

  override def setTrace(params: SetTraceParams): Unit = {
    // Do nothing
  }

  // The LSP services are also implemented by the Server class as they are strongly coupled anyway.
  override def getTextDocumentService(): TextDocumentService = this
  override def getWorkspaceService(): WorkspaceService = this

  // LSP Diagnostics
  //
  //

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams(Convert.toURI(name), Collections.seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

  // Custom Effekt extensions
  //
  //

  /**
   * Publish Effekt IR for a given source file
   *
   * @param source The Kiama source file
   * @param config The Effekt configuration
   * @param C The compiler context
   */
  def publishIR(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // Publish Effekt IR
    val showIR = workspaceService.settingString("showIR").getOrElse("none")
    val showTree = workspaceService.settingBool("showTree")

    if (showIR == "none") {
      return;
    }

    if (showIR == "source") {
      val tree = C.compiler.getAST(source)
      if (tree.isEmpty) return
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".scala",
        PrettyPrinter.format(tree.get).layout
      ))
      return;
    }

    def unsupported =
      throw new RuntimeException(s"Combination of '${showIR}' and showTree=${showTree} not supported by backend ${C.config.backend().name}")

    val stage = showIR match {
      case "core" => Stage.Core
      case "machine" => Stage.Machine
      case "target" => Stage.Target
      case _ => unsupported
    }

    if (showTree) {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".scala",
        PrettyPrinter.format(C.compiler.treeIR(source, stage).getOrElse(unsupported)).layout
      ))
    } else if (showIR == "target") {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + "." + C.runner.extension,
        C.compiler.prettyIR(source, Stage.Target).getOrElse(unsupported).layout
      ))
    } else {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".ir",
        C.compiler.prettyIR(source, stage).getOrElse(unsupported).layout
      ))
    }
  }

  /**
   * Publish holes in the given source file
   */
  def publishHoles(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    if (!workspaceService.settingBool("showHoles")) return
    val holes = getHoles(source)
    client.publishHoles(EffektPublishHolesParams(source.name, holes.map(EffektHoleInfo.fromHoleInfo)))
  }

  // Driver methods
  //
  //

  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // Publish LSP diagnostics
    val messages = C.messaging.buffer
    val groups = messages.groupBy(msg => msg.sourceName.getOrElse(""))
    for ((name, msgs) <- groups) {
      publishDiagnostics(name, msgs.distinct.map(Convert.messageToDiagnostic(lspMessaging)))
    }
    try {
      publishIR(source, config)
    } catch {
      case e => client.logMessage(new MessageParams(MessageType.Error, e.toString + ":" + e.getMessage))
    }
    try {
      publishHoles(source, config)
    } catch {
      case e => client.logMessage(new MessageParams(MessageType.Error, e.toString + ":" + e.getMessage))
    }
  }

  // Other methods
  //
  //

  def basename(filename: String): String = {
    val name = Paths.get(filename).getFileName.toString
    val dotIndex = name.lastIndexOf('.')
    if (dotIndex > 0) name.substring(0, dotIndex) else name
  }

  def connect(client: EffektLanguageClient): Unit = {
    this.client = client
  }

  // LSP Document Lifecycle
  //
  //

  def didChange(params: DidChangeTextDocumentParams): Unit = {
    if (!compileOnChange) return
    val document = params.getTextDocument
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, params.getContentChanges.get(0).getText, getConfig)
  }

  def didClose(params: DidCloseTextDocumentParams): Unit = {
    clearDiagnostics(params.getTextDocument.getUri)
  }

  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, document.getText, getConfig)
  }

  def didSave(params: DidSaveTextDocumentParams): Unit = {
    val document = params.getTextDocument
    val text = Option(params.getText) match {
      case Some(t) => t
      case None =>
        return
    }
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, text, getConfig)
  }

  // LSP Hover
  //
  //

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val position = sources.get(params.getTextDocument.getUri).map { source =>
      Convert.fromLSPPosition(params.getPosition, source)
    }
    position match
      case Some(position) => {
        val hover = getSymbolHover(position, settingBool("showExplanations"))(using context) orElse getHoleHover(position)(using context)
        val markup = new MarkupContent("markdown", hover.getOrElse(""))
        val result = new Hover(markup, new LSPRange(params.getPosition, params.getPosition))
        CompletableFuture.completedFuture(result)
      }
      case None => CompletableFuture.completedFuture(new Hover())
  }

  // LSP Document Symbols
  //
  //

  override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[util.List[messages.Either[SymbolInformation, DocumentSymbol]]] = {
    val source = sources.get(params.getTextDocument.getUri)
    if (source.isEmpty) return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    context.compiler.runFrontend(source.get)(using context)

    val documentSymbols = for {
      sym <- context.sourceSymbolsFor(source.get).toVector
      if !sym.isSynthetic
      id <- context.definitionTreeOption(sym)
      decl <- getSourceTreeFor(sym)
      kind <- getSymbolKind(sym)
      detail <- getInfoOf(sym)(using context)
      declRange = convertRange(positions.getStart(decl), positions.getFinish(decl))
      idRange = convertRange(positions.getStart(id), positions.getFinish(id))
    } yield new DocumentSymbol(sym.name.name, kind, declRange, idRange, detail.header)

    val result = Collections.seqToJavaList(
      documentSymbols.map(sym => messages.Either.forRight[SymbolInformation, DocumentSymbol](sym))
    )
    CompletableFuture.completedFuture(result)
  }

  def getSourceTreeFor(sym: effekt.symbols.Symbol): Option[Tree] = sym match {
    case a: Anon => Some(a.decl)
    case f: UserFunction => Some(f.decl)
    case b: Binder => Some(b.decl)
    case _ => context.definitionTreeOption(sym)
  }

  def getSymbolKind(sym: Symbol): Option[SymbolKind] =
    sym match {
      case _: Module =>
        Some(SymbolKind.Module)
      case _: Interface | _: ExternInterface =>
        Some(SymbolKind.Interface)
      case _: DataType | _: ExternType | _: TypeAlias =>
        Some(SymbolKind.Enum)
      case _: Callable =>
        Some(SymbolKind.Method)
      case _: Param | _: ValBinder | _: VarBinder =>
        Some(SymbolKind.Variable)
      case _ =>
        None
    }

  // LSP Go To Definition
  //
  //

  override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]]] = {
    val location = for {
      position <- sources.get(params.getTextDocument.getUri).map { source =>
        fromLSPPosition(params.getPosition, source)
      };
      definition <- getDefinitionAt(position)(using context);
      location = locationOfNode(positions, definition)
    } yield location

    val result = location.map(l => messages.Either.forLeft[util.List[_ <: Location], util.List[_ <: LocationLink]](Collections.seqToJavaList(List(l))))
      .getOrElse(messages.Either.forLeft(Collections.seqToJavaList(List())))

    CompletableFuture.completedFuture(result)
  }

  // LSP References
  //
  //

  override def references(params: ReferenceParams): CompletableFuture[util.List[_ <: Location]] = {
    val position = sources.get(params.getTextDocument.getUri).map { source =>
      fromLSPPosition(params.getPosition, source)
    }
    if (position.isEmpty)
      return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    val locations = for {
      (tree, sym) <- getSymbolAt(position.get)(using context)
      refs = context.distinctReferencesTo(sym)
      // getContext may be null!
      includeDeclaration = Option(params.getContext).exists(_.isIncludeDeclaration)
      allRefs = if (includeDeclaration) tree :: refs else refs
      locations = allRefs.map(ref => locationOfNode(positions, ref))
    } yield locations

    CompletableFuture.completedFuture(Collections.seqToJavaList(locations.getOrElse(Seq[Location]())))
  }

  // LSP Inlay Hints
  //
  //

  override def inlayHint(params: InlayHintParams): CompletableFuture[util.List[InlayHint]] = {
    val hints = for {
      source <- sources.get(params.getTextDocument.getUri)
      hints = {
        val range = fromLSPRange(params.getRange, source)
        val captures = getInferredCaptures(range)(using context).map {
          case CaptureInfo(p, c, atSyntax) =>
            val prettyCaptures = TypePrinter.show(c)
            val codeEdit = if atSyntax then s"at ${prettyCaptures}" else prettyCaptures
            val inlayHint = new InlayHint(convertPosition(p), messages.Either.forLeft(codeEdit))
            inlayHint.setKind(InlayHintKind.Type)
            val markup = new MarkupContent()
            markup.setValue(s"captures: `${prettyCaptures}`")
            markup.setKind("markdown")
            inlayHint.setTooltip(markup)
            if (atSyntax) then
              inlayHint.setPaddingLeft(true)
            else
              inlayHint.setPaddingRight(true)
            inlayHint.setData("capture")
            inlayHint
        }.toVector

        val unannotated = getTreesInRange(range)(using context).map { trees =>
          trees.collect {
            // Functions without an annotated type:
            case fun: FunDef if fun.ret.isEmpty => for {
              sym <- context.symbolOption(fun.id)
              tpe <- context.functionTypeOption(sym)
              pos = fun.ret.span.range.from
            } yield {
              val prettyType = pp": ${tpe.result} / ${tpe.effects}"
              val inlayHint = new InlayHint(convertPosition(pos), messages.Either.forLeft(prettyType))
              inlayHint.setKind(InlayHintKind.Type)
              val markup = new MarkupContent()
              markup.setValue(s"return type${prettyType}")
              markup.setKind("markdown")
              inlayHint.setTooltip(markup)
              inlayHint.setPaddingLeft(true)
              inlayHint.setData("return-type-annotation")
              val textEdit = new TextEdit(convertRange(fun.ret.span.range), prettyType)
              inlayHint.setTextEdits(Collections.seqToJavaList(List(textEdit)))
              inlayHint
            }
          }.flatten
        }.getOrElse(Vector())

        captures ++ unannotated
      }
    } yield hints

    CompletableFuture.completedFuture(Collections.seqToJavaList(hints.getOrElse(Vector())))
  }

  // LSP Code Actions
  //
  //

  // FIXME: This is the code actions code from the previous language server implementation.
  // It doesn't even work in the previous implementation.
  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] = {
    val codeActions = for {
      position <- sources.get(params.getTextDocument.getUri).map { source =>
        fromLSPPosition(params.getRange.getStart, source)
      };
      codeActions = for {
        trees <- getTreesAt(position)(using context).toVector
        actions <- trees.flatMap { t => action(t)(using context) }
      } yield actions
    } yield codeActions.toList

    val result = codeActions.getOrElse(List[CodeAction]()).map(messages.Either.forRight[Command, CodeAction])
    CompletableFuture.completedFuture(Collections.seqToJavaList(result))
  }

  def action(tree: Tree)(using C: Context): Option[CodeAction] = tree match {
    case f: FunDef => inferEffectsAction(f)
    case h: Hole => closeHoleAction(h)
    case _ => None
  }

  def EffektCodeAction(description: String, span: Span, newText: String): CodeAction = {
    val textEdit = new TextEdit(convertRange(span.range), newText)
    val changes = Map(span.source.name -> seqToJavaList(List(textEdit)))
    val workspaceEdit = new WorkspaceEdit(mapToJavaMap(changes))
    val action = new CodeAction(description)
    action.setKind(CodeActionKind.Refactor)
    action.setEdit(workspaceEdit)
    action
  }

  def inferEffectsAction(fun: FunDef)(using C: Context): Option[CodeAction] = for {
    (tpe, eff) <- C.inferredTypeAndEffectOption(fun)
    ann = for {
      result <- fun.symbol.annotatedResult
      effects <- fun.symbol.annotatedEffects
    } yield (result, effects)
    if ann.map(needsUpdate(_, (tpe, eff))).getOrElse(true)
  } yield {
    val newText = if (eff.effects.nonEmpty)
      s": ${TypePrinter.show(tpe)} / ${TypePrinter.show(eff)}"
    else
      s": ${TypePrinter.show(tpe)}"
    EffektCodeAction("Update return type with inferred effects", fun.ret.span, newText)
  }

  def closeHoleAction(hole: Hole)(using C: Context): Option[CodeAction] = for {
    holeTpe <- C.inferredTypeOption(hole)
    contentTpe <- C.inferredTypeOption(hole.stmts)
    if holeTpe == contentTpe
    res <- hole match {
      case Hole(id, source.Return(exp, _), span) => for {
        text <- positions.textOf(exp)
      } yield EffektCodeAction("Close hole", span, text)

      // <{ s1 ; s2; ... }>
      case Hole(id, stmts, span) => for {
        text <- positions.textOf(stmts)
      } yield EffektCodeAction("Close hole", span, s"locally { ${text} }")
    }
  } yield res

  def needsUpdate(annotated: (ValueType, Effects), inferred: (ValueType, Effects))(using Context): Boolean = {
    val (tpe1, effs1) = annotated
    val (tpe2, effs2) = inferred
    tpe1 != tpe2 || effs1 != effs2
  }

  // LSP methods
  //
  //

  def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    // The configuration arrives as a JSON object nested under the "effekt" key
    // `{ "effekt": { "showIR": "core", ... } }`
    val newSettings = params.getSettings.asInstanceOf[JsonElement]
    // When the settings come via `initializationOptions`, they can be null as per the LSP spec.
    if (newSettings.isJsonNull) {
      this.settings = null;
      return;
    }
    val newSettingsObj = newSettings.getAsJsonObject
    val effektSection = newSettingsObj.get("effekt")
    if (effektSection != null) {
      this.settings = effektSection
    }
  }

  def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}

  // Settings
  //
  //

  def settingBool(name: String): Boolean = {
    if (settings == null || settings.isJsonNull) return false
    val obj = settings.getAsJsonObject
    val value = obj.get(name)
    if (value == null) return false
    value.getAsBoolean
  }

  def settingString(name: String): Option[String] = {
    if (settings == null || settings.isJsonNull) return None
    val obj = settings.getAsJsonObject
    val value = obj.get(name)
    if (value == null) return None
    Some(value.getAsString)
  }

  /**
   * Launch a language server using provided input/output streams.
   * This allows tests to connect via in-memory pipes.
   */
  def launch(getClient: Launcher[EffektLanguageClient] => EffektLanguageClient, in: InputStream, out: OutputStream, trace: Boolean = false): Launcher[EffektLanguageClient] = {
    // Create a single-threaded executor to serialize all requests.
    val executor: ExecutorService = Executors.newSingleThreadExecutor()

    val builder =
      new LSPLauncher.Builder()
        .setLocalService(this)
        .setRemoteInterface(classOf[EffektLanguageClient])
        .setInput(in)
        .setOutput(out)
        .setExecutorService(executor)
        // This line makes sure that the List and Option Scala types serialize correctly
        .configureGson(_.withScalaSupport)

    if (trace) {
      builder.traceMessages(new PrintWriter(System.err, true))
    }

    val launcher = builder.create()
    this.connect(getClient(launcher))
    launcher.startListening()
    launcher
  }

  /**
   * Launch a language server with a given `ServerConfig`
   */
  def launch(config: ServerConfig): Unit = {
    // Create a single-threaded executor to serialize all requests.
    val executor: ExecutorService = Executors.newSingleThreadExecutor()

    if (config.debug) {
      val serverSocket = new ServerSocket(config.debugPort)
      System.err.println(s"Starting language server in debug mode on port ${config.debugPort}")
      val socket = serverSocket.accept()
      launch(_.getRemoteProxy, socket.getInputStream, socket.getOutputStream, trace = true)
    } else {
      launch(_.getRemoteProxy, System.in, System.out)
    }
  }
}

case class ServerConfig(debug: Boolean = false, debugPort: Int = 5000)

trait EffektLanguageClient extends LanguageClient {
  /**
   * Custom LSP notification to publish Effekt IR
   *
   * @param params The parameters for the notification
   */
  @JsonNotification("$/effekt/publishIR")
  def publishIR(params: EffektPublishIRParams): Unit

  @JsonNotification("$/effekt/publishHoles")
  def publishHoles(params: EffektPublishHolesParams): Unit
}

/**
 * Custom LSP notification to publish Effekt IR
 *
 * @param filename The filename of the resulting IR file
 * @param content The IR content
 */
case class EffektPublishIRParams(filename: String,
                                 content: String
)

/**
 * Custom LSP notification to publish Effekt holes
 *
 * @param uri The URI of the source file
 * @param holes The holes in the source file
 */
case class EffektPublishHolesParams(uri: String, holes: List[EffektHoleInfo])

/**
 * Information about a typed hole
 *
 * The difference to Intelligence.HoleInfo is that it uses the appropriate LSP type for the range
 */
case class EffektHoleInfo(id: String,
                    range: LSPRange,
                    innerType: Option[String],
                    expectedType: Option[String],
                    importedTerms: List[Intelligence.TermBinding], importedTypes: List[Intelligence.TypeBinding],
                    terms: List[Intelligence.TermBinding], types: List[Intelligence.TypeBinding])

object EffektHoleInfo {
  def fromHoleInfo(info: Intelligence.HoleInfo): EffektHoleInfo = {
    EffektHoleInfo(
      info.id,
      convertRange(info.span.range),
      info.innerType,
      info.expectedType,
      info.importedTerms,
      info.importedTypes,
      info.terms,
      info.types
    )
  }
}
