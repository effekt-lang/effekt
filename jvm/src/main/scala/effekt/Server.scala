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
import effekt.context.Annotations
import org.eclipse.lsp4j.MessageType
import scala.reflect.ClassTag
import netscape.javascript.JSObject
import com.google.gson.JsonObject

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
    for (mod <- C.frontend(source); core <- C.backend(source); js <- C.generate(source)) {

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
    sym <- context.sourceSymbols
    if !sym.synthetic
    mod = context.sourceModuleOf(sym)
    if mod.source == source
    id <- context.definitionTreeOption(sym)
    _ = context.annotationOption(Annotations.TypeAndEffect, id)
    decl = id // TODO for now we use id as the declaration. This should be improved in SymbolsDB
    kind <- getSymbolKind(sym)
    detail <- getInfoOf(sym)(context)
  } yield new DocumentSymbol(sym.name.localName, kind, rangeOfNode(decl), rangeOfNode(id), detail.header))

  override def getReferences(position: Position, includeDecl: Boolean): Option[Vector[Tree]] =
    for {
      (tree, sym) <- getSymbolAt(position)(context)
      refs = context.distinctReferencesTo(sym)
      allRefs = if (includeDecl) tree :: refs else refs
    } yield allRefs.toVector

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
    // println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    // println("URI: " + uri)
    // println("Keys: " + sources.keys.mkString(","))

    // sources.keys.foreach((k) => println)
    // sources.keys.foreach((x) => println("FOOO " + sources.get(x)))
    val src = sources.get(uri);
    // println("Source: " + src.map(_.name))
    // println("<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    // src.get

    src match {
      case None         => new StringSource("")
      case Some(source) => source
    }
  }

  class TypeAnnotation(val line: Int, val column: Int, val effects: Array[Effect]) {
    override def equals(obj: Any): Boolean = obj match {
      case a: TypeAnnotation => {
        a.line == this.line && a.column == this.column && a.effects.sameElements(this.effects)
      }
    }
  }

  def getTypeAnnotations(uri: String): CompletableFuture[Array[TypeAnnotation]] = {
    var funs = getFunDefs(getSource(uri)).toList
    var annotations: scala.collection.immutable.Set[TypeAnnotation] = Set();
    //var annotations: List[TypeAnnotation] = List();
    funs.foreach((fun) => {
      var pos = context.positions.getStart(fun.decl)
      var ret = fun.ret
      pos match {
        case Some(posval) => ret match {
          case Some(retval) => {
            logMessage("Appending annotation at " + posval.line + ":" + posval.column + " -> " + retval.toString())
            annotations = annotations + (new TypeAnnotation(posval.line, posval.column, retval.effects.toList.toArray)) // :: annotations
          }
          case None => logMessage("ret was None")
        }
        case None => logMessage("pos was None for " + ret.get.toString())
      }
    })
    return CompletableFuture.completedFuture { annotations.toArray }
  }

  override def executeCommand(executeCommandParams: ExecuteCommandParams): Any =
    executeCommandParams.getCommand() match {
      case "println" => {
        //executeCommandParams.getArguments().forEach((x) => logMessage(x.toString()))
        //return CompletableFuture.runAsync(executeCommandParams.getArguments().forEach((x) => println(x.toString())))
        //return Future { executeCommandParams.getArguments.forEach((x) => println(x.toString())) }
        CompletableFuture.completedFuture {
          executeCommandParams.getArguments.forEach((x) => println(x.toString()))
        }
        //return CompletableFuture.supplyAsync( () -> "Finished");
        // ( () -> executeCommandParams.getArguments().forEach((x) => println(x.toString())) )
      }
      case "getHover" => {
        val args = executeCommandParams.getArguments()
        var obj = args.get(0)
        var pos = obj.asInstanceOf[JsonObject]
        var line = pos.get("line").getAsInt()
        var char = pos.get("character").getAsInt
        println("Getting source " + args.get(1).toString())
        var src = getSource(args.get(1).toString().filter(!"\"".contains(_)))

        //println(positionOfNotification(src, pos))
        var res = getSymbolHover(new Position(line + 1, char, src))
        println("Line:", line, "Char:", char)
        println(res)
        return CompletableFuture.completedFuture {
          res
        }
      }
      case "testfun" => {
        return CompletableFuture.completedFuture {
          "Hello, you called me?"
        }
      }
      case "getTypeAnnotations" => {
        val args = executeCommandParams.getArguments()
        if (args.size() == 1) {
          logMessage("Getting type annotations for source file: " + args.get(0).toString())
          logMessage("Used sources content: " + getSource(args.get(0).toString()).content)
          val funs = getFunDefs(getSource(args.get(0).toString())).toList
          // for {
          //   fun <- funs
          // } yield logMessage("Function: " + fun.name.localName)
          logMessage("Found " + funs.size + " functions")

          val annotations = getTypeAnnotations(args.get(0).toString())
          return annotations
        }

      }
      case _ => {
        logMessage("Unknown command: " + executeCommandParams.getCommand())
        return null
      }
    }

  def getFunDefs(source: Source): Vector[UserFunction] = {
    val syms = context.sourceSymbols
    syms.collect {
      //case u: UserFunction if context.sourceModuleOf(u).source == source => u
      case u: UserFunction => u
    }
  }

  // override def getCodeLenses(uri: String): Option[Vector[TreeLens]] = {
  //   val funDefs = getFunDefs(getSource(uri))
  // }

  override def getCommandNames(): Option[Vector[String]] = Some(
    Vector("println")
  )

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
      args.add(symbol.getName())
      //showMessage(MessageType.Info, symbol.getName())
      args.add(symbol.getRange())
      this.registerCapability("Infer or remove return type and effects", "println", args)
      Some(new TreeLens(
        "Fix or unfix function by adding or removing inferred return type and effects.",
        new Command(symbol.getDetail() + " - Infer / remove ret-type & effects", "println", args),
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
    tpe <- C.inferredTypeOption(fun)
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
    (holeTpe / _) <- C.inferredTypeOption(hole)
    (contentTpe / _) <- C.inferredTypeOption(hole.stmts)
    if holeTpe == contentTpe
    res <- hole match {
      case Hole(source.Return(exp)) => for {
        text <- positions.textOf(exp)
      } yield TreeAction("Close hole", pos.source.name, hole, text)

      // <{ s1 ; s2; ... }>
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
