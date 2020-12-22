package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.source.{ FunDef, Hole, Tree }
import org.bitbucket.inkytonik.kiama
import kiama.util.{ Position, Source }
import org.eclipse.lsp4j.{ DocumentSymbol, SymbolKind, ExecuteCommandParams }
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Command
import scala.collection.immutable.Vector0
import org.bitbucket.inkytonik.kiama.util.StringSource
import effekt.source.NoSource
import java.util.ArrayList
import java.util.concurrent.CompletableFuture
import scala.concurrent.Future

/**
 * effekt.Intelligence <--- gathers information -- LSPServer --- provides LSP interface ---> kiama.Server
 *     |
 *     v
 * effekt.Compiler
 */
trait LSPServer extends Driver with Intelligence {

  object prettyCore extends PrettyPrinter

  import effekt.symbols._

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  override def getDefinition(position: Position): Option[Tree] =
    getDefinitionAt(position)(context)

  def maybeExplain(explanation: String): String =
    if (!settingBool("showExplanations")) "" else explanation.stripMargin('|')

  /**
   * Overriding hook to also publish core and target for LSP server
   */
  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    super.afterCompilation(source, config)
    for (mod <- C.frontend(source); core <- C.lower(source); js <- C.generate(source)) {

      if (C.config.server() && settingBool("showCore")) {
        publishProduct(source, "target", "effekt", prettyCore.format(core))
      }

      if (C.config.server() && settingBool("showTarget")) {
        publishProduct(source, "target", "js", js)
      }
    }
  }

  override def getHover(position: Position): Option[String] =
    getSymbolHover(position) orElse getHoleHover(position)

  def getSymbolHover(position: Position): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)(context)
    info <- getInfoOf(sym)(context)
  } yield if (settingBool("showExplanations")) info.fullDescription else info.shortDescription

  def getHoleHover(position: Position): Option[String] = for {
    trees <- getTreesAt(position)(context)
    tree <- trees.collectFirst { case h: source.Hole => h }
    info <- getHoleInfo(tree)(context)
  } yield info

  // The implementation in kiama.Server does not support file sources
  override def locationOfNode(node: Tree): Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        val s = convertPosition(start)
        val f = convertPosition(finish)
        new Location(st.source.name, new LSPRange(s, f))
      case _ =>
        null
    }
  }

  override def getSymbols(source: Source): Option[Vector[DocumentSymbol]] = Some(for {
    sym <- context.sources.keys
    if !sym.synthetic
    mod = context.owner(sym)
    if mod.source == source
    id <- context.sources.get(sym)
    decl = id // TODO for now we use id as the declaration. This should be improved in SymbolsDB
    kind <- getSymbolKind(sym)
    detail <- getInfoOf(sym)(context)
  } yield new DocumentSymbol(sym.name.localName, kind, rangeOfNode(decl), rangeOfNode(id), detail.header))

  override def getReferences(position: Position, includeDecl: Boolean): Option[Vector[Tree]] =
    for {
      (tree, sym) <- getSymbolAt(position)(context)
      refs = context.references.getOrDefault(sym, Nil).distinctBy(r => System.identityHashCode(r))
      allRefs = if (includeDecl) tree :: refs else refs
    } yield refs.toVector

  // settings might be null
  override def setSettings(settings: Object): Unit = {
    import com.google.gson.JsonObject
    if (settings == null) super.setSettings(new JsonObject())
    else super.setSettings(settings)
  }

  //references

  def getSymbolKind(sym: Symbol): Option[SymbolKind] =
    sym match {
      case _: Module =>
        Some(SymbolKind.Class)
      case _: Fun =>
        Some(SymbolKind.Method)
      case _: Param | _: ValBinder | _: VarBinder =>
        Some(SymbolKind.Variable)
      case _ =>
        None
    }

  def getLensSyms(documentsymbols: Option[Vector[DocumentSymbol]]): Option[Vector[DocumentSymbol]] = documentsymbols match {
    case None => None
    case Some(vec) => vec match {
      case head +: tail => Some(
        for {
          e <- vec
          if e.getKind() == SymbolKind.Method
        } yield e
      )
      case _ => None
    }
  }

  def getSource(uri: String) = {
    val src = sources.get(uri);
    src match {
      case None         => new StringSource("")
      case Some(source) => source
    }
  }

  override def executeCommand(executeCommandParams: ExecuteCommandParams): Any =
    executeCommandParams.getCommand() match {
      case "println" => {
        executeCommandParams.getArguments().forEach((x) => logMessage(x.toString()))
        //return CompletableFuture.runAsync(executeCommandParams.getArguments().forEach((x) => println(x.toString())))
        return Future { executeCommandParams.getArguments.forEach((x) => println(x.toString())) }
        // ( () -> executeCommandParams.getArguments().forEach((x) => println(x.toString())) )
      }
      case _ => {
        logMessage("No arguments given.")
        return null
      }
    }

  override def getCodeLenses(uri: String): Option[Vector[TreeLens]] = lens(getSymbols(getSource(uri)))(context)

  def lens(documentsymbols: Option[Vector[DocumentSymbol]])(implicit C: Context): Option[Vector[TreeLens]] = documentsymbols match {
    case Some(vec) => vec match {
      case tail :+ head => Some(
        vec.flatMap(s => getLens(s)(C))
      )
      case _ => None
    }
    case None => None
  }

  def getLens(symbol: DocumentSymbol)(implicit C: Context): Option[TreeLens] = symbol.getKind() match {
    case SymbolKind.Method => {
      val args = new ArrayList[Object]();
      args.add("Foo")
      this.registerCapability("Infer or remove return type and effects", "println", args)

      Some(new TreeLens(
        "Fix or unfix function by adding or removing inferred return type and effects.",
        new Command("Infer / remove ret-type & effects", "println", args),
        //      new Command("Fix/unfix return type and effects", "testCommand"),
        symbol.getRange()
      ))
    }
    case _ => None
  }

  /*
  def functionLens(f: FunDef)(implicit C: Context): Option[TreeLens] = for {
    action <- inferEffectsAction(f)(C)
  } yield TreeLens(
    "Fix or unfix function by adding or removing inferred return type and effects.",
    new Command("Fix/unfix return type and effects", "testCommand")
  )
  */

  override def getCodeActions(position: Position): Option[Vector[TreeAction]] =
    Some(for {
      trees <- getTreesAt(position)(context).toVector
      actions <- trees.flatMap { t => action(t)(context) }
    } yield actions)

  def action(tree: Tree)(implicit C: Context): Option[TreeAction] = tree match {
    case f: FunDef => inferEffectsAction(f)
    case h: Hole   => closeHoleAction(h)
    case _         => None
  }

  /**
   * TODO it would be great, if Kiama would allow setting the position of the code action separately
   * from the node to replace. Here, we replace the annotated return type, but would need the
   * action on the function (since the return type might not exist in the original program).
   *
   * Also, it is necessary to be able to manually set the code action kind (and register them on startup).
   * This way, we can use custom kinds like `refactor.closehole` that can be mapped to keys.
   */
  def inferEffectsAction(fun: FunDef)(implicit C: Context): Option[TreeAction] = for {
    pos <- positions.getStart(fun)
    ret <- fun.ret
    // the inferred type
    tpe <- C.typeOf(fun)
    // the annotated type
    ann = fun.symbol.ret
    if ann.map { a => needsUpdate(a, tpe) }.getOrElse(true)
  } yield TreeAction(
    "Update return type with inferred effects",
    pos.source.name,
    ret,
    tpe.toString
  )

  def closeHoleAction(hole: Hole)(implicit C: Context): Option[TreeAction] = for {
    pos <- positions.getStart(hole)
    (holeTpe / _) <- C.typeOf(hole)
    (contentTpe / _) <- C.typeOf(hole.stmts)
    if holeTpe == contentTpe
    res <- hole match {
      case Hole(source.Return(exp)) => for {
        text <- positions.textOf(exp)
      } yield TreeAction("Close hole", pos.source.name, hole, text)

      case Hole(stmts) => for {
        text <- positions.textOf(stmts)
      } yield TreeAction("Close hole", pos.source.name, hole, s"locally { ${text} }")
    }
  } yield res

  def needsUpdate(annotated: Effectful, inferred: Effectful)(implicit C: Context): Boolean = {
    val (tpe1 / effs1) = annotated
    val (tpe2 / effs2) = inferred
    tpe1 != tpe2 || effs1 != effs2
  }
}

/**
 * Main entry point for Effekt
 */
object Server extends LSPServer
