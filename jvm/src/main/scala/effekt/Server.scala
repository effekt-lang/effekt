package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.source.{ FunDef, Hole, Tree }
import org.bitbucket.inkytonik.kiama
import kiama.util.{ Position, Source }
import org.eclipse.lsp4j.{ DocumentSymbol, SymbolKind, ExecuteCommandParams }
import org.bitbucket.inkytonik.kiama.util.StringSource
import java.util.ArrayList
import java.util.concurrent.CompletableFuture
import effekt.context.Annotations
import effekt.context.Annotation
import com.google.gson.Gson

/**
 * effekt.Intelligence <--- gathers information -- LSPServer --- provides LSP interface ---> kiama.Server
 *     |
 *     v
 * effekt.Compiler
 */
trait LSPServer extends Driver with Intelligence {

  object prettyCore extends PrettyPrinter

  import effekt.symbols._

  import org.eclipse.lsp4j.{ Location, Range => LSPRange, Position => LSPPosition }

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
    val src = sources.get(uri);
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

  case class simplePosition(line: Int, column: Int)
  case class capability(name: String, scopeStart: simplePosition, scopeEnd: simplePosition)
  case class passedCapability(capabilities: Array[capability], line: Int, column: Int)

  def positionToLSPPosition(position: Position): LSPPosition = {
    return new LSPPosition(position.line - 1, position.column - 1)
  }

  /**
   * Information on a capability
   * @param capabilityKind a capabilityKind string. Expected are: "CapabilityBinder" | "CapabilityArgument" | "CapabilityReceiver"
   * @param capabilityName the name of the capability as a string
   * @param sourceRange the range of the symbol in the source code that caused the annotation
   * @param scopeRange the range of the scope of the capability
   */
  case class CapabilityInfo(capabilityKind: String, capabilityName: String, sourceRange: LSPRange, scopeRange: LSPRange)

  /**
   * Extract all Trees from sourceTrees that could possibly have a CapabilityBinder annotation
   * @param sourceTrees a list of Trees
   * @return a list of trees that can potentially bind a capability
   */
  def getPossibleCapabilityBinders(sourceTrees: Vector[Tree]): Vector[Tree] = {
    sourceTrees.collect {
      case f: source.FunDef     => f
      case th: source.TryHandle => th
      case ba: source.BlockArg  => ba
    }
  }

  /**
   * extract all the CapabilityBinders from the given source, pack them in CapabilityInfos and return an array of all CapabilityInfos
   */
  def getCapabilityBinderInfos(trees: Vector[Tree]): Array[CapabilityInfo] = {
    val capabilityBinders = getPossibleCapabilityBinders(trees).toArray

    val capabilityBinderInfos = for {
      cb: Tree <- capabilityBinders
      ann <- context.annotationOption(Annotations.CapabilityBinder, cb)
    } yield {
      val callStart = positionToLSPPosition(positions.getStart(cb).get)
      val callEnd = positionToLSPPosition(positions.getFinish(cb).get)

      ann.toArray.map(arg => {
        val argsym = context.symbolOf(arg.id)
        val argdef = context.definitionTreeOption(argsym).get
        val argStart = positionToLSPPosition(positions.getStart(argdef).get)
        val argEnd = positionToLSPPosition(positions.getFinish(argdef).get)
        CapabilityInfo("CapabilityBinder", arg.tpe.eff.name.localName, new LSPRange(callStart, callEnd), new LSPRange(argStart, argEnd))
      })
    }

    return capabilityBinderInfos.flatten
  }

  def getFunctionCalls(sourceTrees: Vector[Tree]): Vector[source.Call] = {
    sourceTrees.collect {
      case c: source.Call => c
    }
  }

  /**
   * extract all the CapabilityReceivers from the given source, pack them in CapabilityInfos and return an array of all CapabilityInfos
   */
  def getCapabilityReceiverInfos(trees: Vector[Tree]): Array[CapabilityInfo] = {
    val capabilityReceivers = getFunctionCalls(trees).toArray

    val capabilityReceiverInfos = for {
      cr <- capabilityReceivers
      z <- context.annotationOption(Annotations.CapabilityReceiver, cr)
    } yield {
      val callStart = positionToLSPPosition(positions.getStart(cr).get)
      val callEnd = positionToLSPPosition(positions.getFinish(cr).get)

      val argsym = z.symbol(context)
      val argdef = context.definitionTreeOption(argsym).get
      val argStart = positionToLSPPosition(positions.getStart(argdef).get)
      val argEnd = positionToLSPPosition(positions.getFinish(argdef).get)

      CapabilityInfo("CapabilityReceiver", argsym.asInstanceOf[CapabilityParam].tpe.eff.name.localName, new LSPRange(callStart, callEnd), new LSPRange(argStart, argEnd))
    }

    return capabilityReceiverInfos
  }

  /**
   * extract all the CapabilityArguments from the given source, pack them in CapabilityInfos and return an array of all CapabilityInfos
   */
  def getCapabilityArgumentInfos(trees: Vector[Tree]): Array[CapabilityInfo] = {
    val funCalls = getFunctionCalls(trees).toArray

    val capabilityArgumentInfos = for {
      fc: source.Call <- funCalls: Array[source.Call]
      ann: List[source.CapabilityArg] <- context.annotationOption(Annotations.CapabilityArguments, fc): Option[List[source.CapabilityArg]]
    } yield {
      ann.toArray.map(arg => {
        val callStart = positionToLSPPosition(positions.getStart(fc).getOrElse({ println("No position for " + fc.toString()); new Position(1, 1, StringSource("")); }))
        val callEnd = positionToLSPPosition(positions.getFinish(fc).get)
        val argsym = arg.id.symbol(context)
        val argdef = context.definitionTreeOption(argsym).get
        val argStart = positionToLSPPosition(positions.getStart(argdef).get)
        val argEnd = positionToLSPPosition(positions.getFinish(argdef).get)
        CapabilityInfo("CapabilityArgument", argsym.asInstanceOf[CapabilityParam].tpe.eff.name.localName, new LSPRange(callStart, callEnd), new LSPRange(argStart, argEnd))
      })
    }

    return capabilityArgumentInfos.flatten
  }

  /**
   * collect all info on capabilities of the given source file.
   * This means that we collect:
   * - CapabilityBinders of functions and handlers and anonymous functions
   * - CapabilityArguments of function calls
   * - CapabilityReceivers of effect operation calls
   *
   * We return a list of CapabilityInfo where each CapabilityInfo holds:
   * - a capabilityKind string: "CapabilityBinder" | "CapabilityArgument" | "CapabilityReceiver"
   * - the name of the capability as a string
   * - the range of the symbol in the source code that caused the annotation
   * - the range of the scope of the capability
   */
  def getCapabilitiesInfo(source: Source): Array[CapabilityInfo] = {
    val trees = getTreeFrom(source)(context).get
    val binderInfos = getCapabilityBinderInfos(trees)
    val argumentInfos = getCapabilityArgumentInfos(trees)
    val receiverInfos = getCapabilityReceiverInfos(trees)
    return binderInfos ++ argumentInfos ++ receiverInfos
  }

  /**
   * Implements LSPs "workspace/executeCommand" request handler
   * TODO: right now we pass the command as a string. A real type would be safer.
   */
  override def executeCommand(executeCommandParams: ExecuteCommandParams): Any =
    executeCommandParams.getCommand() match {

      case "println" => {
        CompletableFuture.completedFuture {
          executeCommandParams.getArguments.forEach((x) => println(x.toString()))
        }
      }

      case "getCapabilitiesInfo" => {
        val args = executeCommandParams.getArguments()
        var src = getSource(args.get(0).toString().filter(!"\"".contains(_)))
        val capabilitiesInfo = getCapabilitiesInfo(src)
        val gson = new Gson
        return CompletableFuture.completedFuture {
          gson.toJson(capabilitiesInfo)
        }
      }

      case _ => {
        logMessage("Unknown command: " + executeCommandParams.getCommand())
        return null
      }
    }

  def getFunCalls(source: Source) = {
    val syms = context.sourceSymbols
    syms.collect {
      case eo: EffectOp => {
        eo
      }
      case ct: CallTarget => ct
    }
  }

  def getFunDefs(source: Source): Vector[UserFunction] = {
    val syms = context.sourceSymbols
    syms.collect {
      //case u: UserFunction if context.sourceModuleOf(u).source == source => u
      case u: UserFunction => u
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
      args.add(symbol.getName())
      args.add(symbol.getRange())
      this.registerCapability("Infer or remove return type and effects", "println", args)
      // Some(new TreeLens(
      //   "Fix or unfix function by adding or removing inferred return type and effects.",
      //   new Command(symbol.getDetail() + " - Infer / remove ret-type & effects", "println", args),
      //   //      new Command("Fix/unfix return type and effects", "testCommand"),
      //   symbol.getRange()
      // ))
      None
    }
    case _ => None
  }

  override def getCodeActions(position: Position): Option[Vector[TreeAction]] =
    Some(for {
      trees <- getTreesAt(position)(context).toVector
      actions <- trees.flatMap { t => action(t)(context) }
      action <- actions.iterator
    } yield action)

  def action(tree: Tree)(implicit C: Context): Option[Vector[TreeAction]] = tree match {
    case f: source.FunDef => inferEffectsAction(f) match {
      case Some(ta) => Some(Vector(ta))
      case None     => None
    }
    case h: source.Hole => closeHoleAction(h) match {
      case Some(ta) => Some(Vector(ta))
      case None     => None
    }
    case c: source.Call => addEffectHandlerActions(c)
    case _              => None
  }

  def expressionsToString(args: List[source.Expr])(implicit C: Context): String = {
    var res = args.map(arg => arg match {
      case source.IntLit(value)     => value.toString()
      case source.BooleanLit(value) => value.toString()
      case source.StringLit(value)  => "\"" + value.toString() + "\""
      case source.DoubleLit(value)  => value.toString()
      case source.UnitLit()         => "()"
      case source.Var(id)           => id.name
      case c: source.Call           => callToString(c)(C)
      case z @ _                    => z.toString()
    })
    res.mkString(", ")
  }

  def callToString(call: source.Call)(implicit C: Context) = {
    var callString = ""
    call.target match {
      case e @ source.ExprTarget(receiver) => logMessage("Unhandled: " + e.toString())
      case i @ source.IdTarget(id) => {
        callString += id.name
      }
      case m @ source.MemberTarget(receiver, id) => logMessage("Unhandled: " + m.toString())
    }

    callString += "("

    val args = call.args.map(arg => {
      logMessage("arg: " + arg.toString())
      arg match {
        case source.ValueArgs(args) => expressionsToString(args)(C)
        case _ =>
      }
    })
    callString += args.mkString(", ")
    callString += ")"
    logMessage("Returning callstring: " + callString)
    callString
  }

  /**
   * Create a vector of TreeActions for a call that introduces an effect.
   * A resulting code action would be the introduction of an effect handler.
   */
  def addEffectHandlerActions(c: source.Call)(implicit C: Context): Option[Vector[TreeAction]] = {
    logMessage("Adding effect handler Action")

    var pos = C.positions.getStart(c).getOrElse(return None)
    var ann = C.annotationOption(Annotations.CapabilityArguments, c)
    var rec = C.annotationOption(Annotations.CapabilityReceiver, c)
    var callString = callToString(c)(C)
    var callArgs = c.target
    var actions = List[TreeAction]()

    rec match {
      case None => ()
      case Some(capabilityID: source.IdRef) => {

        var startPos = positions.getStart(c).get
        var endPos = positions.getFinish(c).get
        val src = startPos.source
        var startOffset = src.positionToOffset(startPos).get
        var endOffset = src.positionToOffset(endPos).get
        var originalCall = src.content.subSequence(startOffset, endOffset)

        var capSym = C.symbolOf(capabilityID)
        var eff = capSym.name.localName

        actions = actions :+ TreeAction(
          "Add an effect handler for <" + eff + ">",
          pos.source.name,
          c,
          s"""|try {
                |\t$originalCall
                |} with $eff {
                |\t<>
                |}""".stripMargin
        )
      }
    }

    ann match {
      case None => None
      case Some(capabilityArguments: List[source.CapabilityArg]) => {
        logMessage("Capability arguments: " + ann.toString())
        actions = actions ++ capabilityArguments.map(capability => {
          var startPos = positions.getStart(c).get
          var endPos = positions.getFinish(c).get
          val src = startPos.source
          var startOffset = src.positionToOffset(startPos).get
          var endOffset = src.positionToOffset(endPos).get
          logMessage(("Range offset: " + startOffset + " to " + endOffset))
          var originalCall = src.content.subSequence(startOffset, endOffset)

          var capSym = C.symbolOf(capability)
          logMessage("Got " + capSym.name + " - " + capSym)
          var eff = capSym.tpe.eff.name.localName
          val userEff = capSym.tpe.eff.asInstanceOf[UserEffect]
          val ops = userEff.ops.map {
            case EffectOp(name, _, params, _, _) => "def " + name.localName + params.map {
              ps =>
                "(" + ps.map {
                  p => p.name.localName
                }.mkString(", ") + ")"
            }.mkString + " = <>"
          }.mkString("\n")
          logMessage("Adding handler action for effect " + eff)
          TreeAction(
            "Add an effect handler for effect <" + eff + ">",
            pos.source.name,
            c,
            s"""|try {
                |\t$originalCall
                |} with $eff {
                |\t$ops
                |}""".stripMargin
          )
        })
      }
    }

    Some(actions.toVector)
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
      case source.Hole(source.Return(exp)) => for {
        text <- positions.textOf(exp)
      } yield TreeAction("Close hole", pos.source.name, hole, text)

      // <{ s1 ; s2; ... }>
      case source.Hole(stmts) => for {
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
