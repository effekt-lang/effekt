package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.source.Tree
import org.bitbucket.inkytonik.kiama
import kiama.util.{ Position, Source }
import org.eclipse.lsp4j.{ DocumentSymbol, SymbolKind }

trait LSPServer extends Driver with Intelligence {

  object prettyCore extends PrettyPrinter

  import effekt.symbols._

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  override def getDefinition(position: Position): Option[Tree] =
    getDefinitionAt(position)(context)

  def maybeExplain(explanation: String): String =
    if (!settingBool("showExplanations")) "" else explanation.stripMargin('|')

  /**
   * Overriding hook to also publish core and target for LSP server
   */
  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    super.afterCompilation(source, config)
    for (mod <- frontend(source); core <- computeCore(source); js <- generateJS(source)) {

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
  } yield new DocumentSymbol(sym.name.name, kind, rangeOfNode(decl), rangeOfNode(id), detail.header))

  override def getReferences(position: Position, includeDecl: Boolean): Option[Vector[Tree]] =
    for {
      (tree, sym) <- getSymbolAt(position)(context)
      refs = context.references.getOrDefault(sym, Nil).distinctBy(r => System.identityHashCode(r))
      allRefs = if (includeDecl) tree :: refs else refs
    } yield refs.toVector

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
}

object Server extends LSPServer
