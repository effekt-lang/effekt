package effekt

import effekt.context.{Annotations, Context}
import effekt.source.{FunDef, Include, Maybe, ModuleDecl, Span, Tree, Doc}
import effekt.symbols.{CaptureSet, Hole}
import kiama.util.{Position, Source}
import effekt.symbols.scopes.Scope
import effekt.source.sourceOf

trait Intelligence {

  import effekt.symbols._
  import builtins.TState
  import Intelligence._

  type EffektTree = kiama.relation.Tree[AnyRef & Product, ModuleDecl]

  case class SymbolInfo(
    symbol: Symbol,
    header: String,
    signature: Option[String],
    description: Option[String],
    documentation: Doc
  ) {
    def fullDescription: String = {
      val sig = signature.map(sig => s"```effekt\n$sig\n```").getOrElse { "" }
      val desc = description.getOrElse("")
      val doc = showDocumentation(documentation)

      s"""|#### $header
          |$sig
          |$desc$doc
          |""".stripMargin
    }

    def shortDescription: String = {
      val sig = signature.map(sig => s"```effekt\n$sig\n```").getOrElse { "" }
      val doc = showDocumentation(documentation)

      s"""|#### $header
          |$sig$doc
          |""".stripMargin
    }
  }

  def showDocumentation(documentation: Doc): String =
    documentation.map('\n' +: _)
      .getOrElse("")
      .replace("\\n", "\n")

  private def sortByPosition(trees: Vector[Tree])(using C: Context): Vector[Tree] =
    val pos = C.positions
    trees.sortWith {
      (t1, t2) =>
        val p1s = pos.getStart(t1).get
        val p2s = pos.getStart(t2).get

        if (p2s == p1s) {
          val p1e = pos.getFinish(t1).get
          val p2e = pos.getFinish(t2).get
          p1e < p2e
        } else {
          p2s < p1s
        }
    }

  def getTreesAt(position: Position)(using C: Context): Option[Vector[Tree]] = for {
    decl <- C.compiler.getAST(position.source)
    tree = new EffektTree(decl)
    allTrees = tree.nodes.collect { case t: Tree => t }
    trees = C.positions.findNodesContaining(allTrees, position)
    nodes = sortByPosition(trees)
  } yield nodes

  def getTreesInRange(range: kiama.util.Range)(using C: Context): Option[Vector[Tree]] =  for {
    decl <- C.compiler.getAST(range.from.source)
    tree = new EffektTree(decl)
    allTrees = tree.nodes.collect { case t: Tree => t }
    trees = C.positions.findNodesInRange(allTrees, range)
    nodes = sortByPosition(trees)
  } yield nodes

  def getIdTreeAt(position: Position)(using C: Context): Option[source.Id] = for {
    trees <- getTreesAt(position)
    id <- trees.collectFirst { case id: source.Id => id }
  } yield id

  def getSymbolAt(position: Position)(using C: Context): Option[(Tree, Symbol)] =
    def identifiers = for {
      id <- getIdTreeAt(position)
      sym <- C.symbolOption(id)
    } yield (id, resolveCallTarget(sym))

    def others = for {
      trees <- getTreesAt(position).toVector
      tree <- trees
      sym <- getSymbolOf(tree)
    } yield (tree, sym)

    identifiers.orElse(others.headOption)

  def getSymbolOf(tree: Tree)(using C: Context): Option[Symbol] =
    tree match {
      case i @ Include(path, span) => C.annotationOption(Annotations.IncludedSymbols, i)
      case _ => None
    }

  def getDefinitionAt(position: Position)(using C: Context): Option[Tree] = for {
    (_, sym) <- getSymbolAt(position)
    decl <- getDefinitionOf(resolveCallTarget(sym))
  } yield decl

  def getDefinitionOf(s: Symbol)(using C: Context): Option[Tree] = s match {
    case u: UserFunction => Some(u.decl)
    case u: Binder       => Some(u.decl)
    case d: Operation    => C.definitionTreeOption(d.interface)
    case a: Anon         => Some(a.decl)
    case m: Module       => Some(m.decl)
    case u => C.definitionTreeOption(u)
  }

  def getDocumentationOf(s: Symbol)(using C: Context): Doc =
    getDefinitionOf(s).asInstanceOf[Option[Any]] match {
      case Some(p: Product) =>
        p.productElementNames.zip(p.productIterator)
          .collectFirst {
            case ("doc", Some(s: String)) => s
            case ("info", source.Info(Some(s: String), _, _, _)) => s
          }
      case _ => None
  }

  // For now, only show the first call target
  def resolveCallTarget(sym: Symbol): Symbol = sym match {
    case t: CallTarget => t.symbols.flatten.headOption.getOrElse(sym)
    case s             => s
  }

  def getSymbolHover(position: Position, fullDescription: Boolean = true)(using C: Context): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)
    info <- getInfoOf(sym)
  } yield if (fullDescription) info.fullDescription else info.shortDescription

  def getHoleHover(position: Position)(using C: Context): Option[String] = for {
    trees <- getTreesAt(position)
    tree <- trees.collectFirst { case h: source.Hole => h }
    info <- getHoleInfo(tree)
  } yield info

  def getHoleInfo(hole: source.Hole)(using C: Context): Option[String] = for {
    (outerTpe, outerEff) <- C.inferredTypeAndEffectOption(hole)
  } yield pp"Hole of type `${outerTpe}`".stripMargin

  def getHoles(src: Source)(using C: Context): List[HoleInfo] = for {
    (hole, scope) <- C.annotationOption(Annotations.HolesForFile, src).getOrElse(Nil)
  } yield {
    val innerType = hole.argTypes match {
      case Some(t) :: Nil => Some(pp"${t}")
      case _ => None
    };
    val expectedType = hole.expectedType.map { t => pp"${t}" }
    val scopeInfo = allBindings(scope)
    val body = holeBody(hole.decl.body, hole.argTypes)
    HoleInfo(
      hole.name.name,
      hole.decl.span,
      innerType,
      expectedType,
      scopeInfo,
      body
    )
  }

  def allBindings(scope: Scope)(using C: Context): ScopeInfo =
    scope match {
      case Scope.Global(imports, bindings) =>
        val bindingsOut = allBindings(BindingOrigin.Imported, imports)
          ++ allBindings(BindingOrigin.Defined, bindings)
        ScopeInfo(None, ScopeKind.Global, bindingsOut, None)
      case Scope.Named(name, bindings, outer) =>
        val bindingsOut = allBindings(BindingOrigin.Defined, bindings)
        ScopeInfo(Some(name), ScopeKind.Namespace, bindingsOut, Some(allBindings(outer)))
      case Scope.Local(name, imports, bindings, outer) =>
        val bindingsOut = allBindings(BindingOrigin.Imported, imports)
          ++ allBindings(BindingOrigin.Defined, bindings)
        ScopeInfo(name, ScopeKind.Local, bindingsOut, Some(allBindings(outer)))
    }

  def allBindings(origin: String, bindings: Bindings, path: List[String] = Nil)(using C: Context): List[BindingInfo] =
    val symbols = allSymbols(origin, bindings, path)

    val sorted = if (origin == BindingOrigin.Imported) {
      symbols.sortBy(_._1.toLowerCase())
    } else {
      symbols.sortBy((name, path, sym) => sym match {
        case s: TypeSymbol => s.decl.span
        case s: TermSymbol => s.decl.span
      })
    }

    sorted.map((name, path, sym) => sym match {
      case sym: TypeSymbol => TypeBinding(path, name, origin, DeclPrinter(sym))
      case sym: ValueSymbol => TermBinding(path, name, origin, C.valueTypeOption(sym).map(t => pp"${t}"))
      case sym: BlockSymbol => TermBinding(path, name, origin, C.blockTypeOption(sym).map(t => pp"${t}"))
    }).toList

  def allSymbols(origin: String, bindings: Bindings, path: List[String] = Nil)(using C: Context): Array[(String, List[String], TypeSymbol | TermSymbol)] = {
    bindings.types.toArray.map((name, sym) => (name, path, sym))
      ++ bindings.terms.toArray.flatMap((name, syms) => syms.map((name, path, _)))
      ++ bindings.namespaces.toArray.flatMap {
      case (name, namespace) => allSymbols(origin, namespace, path :+ name)
    }
  }

  def holeBody(template: Template[source.Stmt], argTypes: List[Option[ValueType]])(using C: Context): List[HoleItem] = {
    val Template(strings, args) = template
    val argsAndTypes = if (argTypes.isEmpty) {
      args.map((_, None))
    } else {
      args.zip(argTypes)
    }
    val buf = List.newBuilder[HoleItem]
    val strIt = strings.iterator
    val argIt = argsAndTypes.iterator

    // Case 1: there is a single argument and no strings
    // This happens when the hole is defined as a single statement/expression using the `<{ stmt }>` syntax
    if (strIt.isEmpty && argIt.hasNext) {
      val (stmt, tpeOpt) = argIt.next()
      buf += Code(stmt.sourceOf, tpeOpt.map(t => pp"${t}"))
      return buf.result()
    }

    // Case 2: starting with a string, it alternates between strings and arguments
    // This happens when the hole is defined as a template using the `<"text ${ arg } text ${arg} ...">` syntax
    while (strIt.hasNext) {
      buf += NaturalLanguage(strIt.next())
      if (argIt.hasNext) {
        val (stmt, tpeOpt) = argIt.next()
        buf += Code(stmt.sourceOf, tpeOpt.map(t => pp"${t}"))
      }
    }

    buf.result()
  }

  def allCaptures(src: Source)(using C: Context): List[(Tree, CaptureSet)] =
    C.annotationOption(Annotations.CaptureForFile, src).getOrElse(Nil)

  // For now, we only show captures of function definitions and calls to box
  def getInferredCaptures(range: kiama.util.Range)(using C: Context): List[CaptureInfo] =
    val src = range.from.source
    allCaptures(src).filter {
      // keep only captures in the current range
      case (t, c) => C.positions.getStart(t) match
        case Some(p) => p.between(range.from, range.to)
        case None => false
    }.collect {
      case (t: source.FunDef, c) => for {
        pos <- C.positions.getStart(t)
      } yield CaptureInfo(pos, c)
      case (t: source.DefDef, c) => for {
        pos <- C.positions.getStart(t)
      } yield CaptureInfo(pos, c)
      case (source.Box(Maybe(None, span), block, _), _) if C.inferredCaptureOption(block).isDefined => for {
        capt <- C.inferredCaptureOption(block)
      } yield CaptureInfo(span.range.from, capt, true)
    }.flatten

  def getInfoOf(sym: Symbol)(using C: Context): Option[SymbolInfo] = PartialFunction.condOpt(resolveCallTarget(sym)) {

    case b: ExternFunction =>
      val doc = getDocumentationOf(b)
      SymbolInfo(b, "External function definition", Some(DeclPrinter(b)), None, doc)

    case f: UserFunction if C.functionTypeOption(f).isDefined =>
      val doc = getDocumentationOf(f)
      SymbolInfo(f, "Function", Some(DeclPrinter(f)), None, doc)

    case f: Operation =>
      val doc = getDocumentationOf(f)
      val ex =
        pp"""|Effect operations, like `${f.name}` allow to express non-local control flow.
             |
             |Other than blocks, the implementation of an effect operation is provided by
             |the closest
             |```effekt
             |try { EXPR } with ${f.interface.name} { def ${f.name}(...) => ...  }
             |```
             |that _dynamically_ surrounds the call-site `do ${f.name}(...)`.
             |
             |However, note that opposed to languages like Java, effect operations
             |cannot be _captured_ in Effekt. That is, if the type of a function or block
             |```effekt
             |def f(): Int / {}
             |```
             |does not mention the effect `${f.interface.name}`, then this effect will not be
             |handled by the handler. This is important when considering higher-order functions.
             |""".stripMargin

      SymbolInfo(f, "Effect operation", Some(DeclPrinter(f)), Some(ex), doc)

    case f: EffectAlias =>
      val doc = getDocumentationOf(f)
      SymbolInfo(f, "Effect alias", Some(DeclPrinter(f)), None, doc)

    case t: TypeAlias =>
      val doc = getDocumentationOf(t)
      SymbolInfo(t, "Type alias", Some(DeclPrinter(t)), None, doc)

    case t: ExternType =>
      val doc = getDocumentationOf(t)
      SymbolInfo(t, "External type definition", Some(DeclPrinter(t)), None, doc)

    case t: ExternInterface =>
      val doc = getDocumentationOf(t)
      SymbolInfo(t, "External interface definition", Some(DeclPrinter(t)), None, doc)

    case t: ExternResource =>
      val doc = getDocumentationOf(t)
      SymbolInfo(t, "External resource definition", Some(DeclPrinter(t)), None, doc)

    case c: Constructor =>
      val doc = getDocumentationOf(c)
      val ex = pp"""|Instances of data types like `${c.tpe}` can only store
                    |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                    |value parameter lists, not block parameters.
                    |""".stripMargin

      SymbolInfo(c, s"Constructor of data type `${c.tpe}`", Some(DeclPrinter(c)), Some(ex), doc)

    case c: BlockParam =>
      val doc = getDocumentationOf(c)
      val signature = C.functionTypeOption(c).map { tpe => pp"{ ${c.name}: ${tpe} }" }

      val ex =
        s"""|Blocks, like `${c.name}`, are similar to functions in other languages.
            |
            |However, there is an important difference to functions (like in JavaScript):
            |Blocks cannot be returned or stored in data types.
            |
            |They are thus more like blocks (hence the name) in Ruby that can only be
            |yielded to.
            |""".stripMargin

      SymbolInfo(c, "Block parameter", signature, Some(ex), doc)

    case c: ResumeParam =>
      val doc = getDocumentationOf(c)
      val tpe = C.functionTypeOption(c)
      val signature = tpe.map { tpe => pp"{ ${c.name}: ${tpe} }" }
      val hint = tpe.map { tpe => pp"(i.e., `${tpe.result}`)" }.getOrElse { " " }

      val ex =
        s"""|Resumptions are block parameters, implicitly bound
            |when handling effect operations.
            |
            |The following three types have to be the same$hint:
            |- the return type of the operation clause
            |- the type of the handled expression enclosed by `try { EXPR } with EFFECT { ... }`, and
            |- the return type of the resumption.
            |""".stripMargin

      SymbolInfo(c, "Resumption", signature, Some(ex), doc)

    case c: VarBinder =>
      val doc = getDocumentationOf(c)
      val signature = C.blockTypeOption(c).map(TState.extractType).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }

      val ex =
        s"""|Like in other languages, mutable variable binders like `${c.name}`
            |can be modified (e.g., `${c.name} = VALUE`) by code that has `${c.name}`
            |in its lexical scope.
            |
            |However, as opposed to other languages, variable binders in Effekt
            |are stack allocated and show the right backtracking behavior in
            |combination with effect handlers.
         """.stripMargin

      SymbolInfo(c, "Mutable variable binder", signature, Some(ex), doc)

    case s: RegBinder =>
      val doc = getDocumentationOf(s)
      val signature = C.blockTypeOption(s).map(TState.extractType).orElse(s.tpe).map { tpe => pp"${s.name}: ${tpe}" }

      val ex =
        s"""|The region `${s.region.name}` the variable `${s.name}` is allocated into
            |not only affects its lifetime, but also its backtracking behavior
            |in combination with continuation capture and resumption.
            |""".stripMargin

      SymbolInfo(s, "Variable in region", signature, Some(ex), doc)

    case c: ValueParam =>
      val doc = getDocumentationOf(c)
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value parameter", signature, None, doc)

    case c: ValBinder =>
      val doc = getDocumentationOf(c)
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value binder", signature, None, doc)

    case c: DefBinder =>
      val doc = getDocumentationOf(c)
      val signature = C.blockTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Block binder", signature, None, doc)
  }
}

object Intelligence {
  case class HoleInfo(
     id: String,
     span: Span,
     /**
      * If the hole contains a single expression, this is the type of that expression.
      */
     innerType: Option[String],
     /**
      * The expected type of the hole, if available.
      */
     expectedType: Option[String],
     /**
      * The scope in which the hole is defined, including all bindings.
      */
     scope: ScopeInfo,
     /**
      * The body of the hole: a list of natural language and code.
      */
     body: List[HoleItem]
  )

  // These need to be strings (rather than cases of an enum) so that they get serialized correctly
  object BindingOrigin {
    /**
     * The binding was defined in this scope
     */
    final val Defined = "Defined"
    /**
     * The binding was imported in this scope
     */
    final val Imported = "Imported"
  }

  object BindingKind {
    final val Term = "Term"
    final val Type = "Type"
  }

  sealed trait BindingInfo {
    val qualifier: List[String]
    val name: String
    val origin: String
    val kind: String
  }

  case class TermBinding(qualifier: List[String], name: String, origin: String, `type`: Option[String], kind: String = BindingKind.Term) extends BindingInfo
  case class TypeBinding(qualifier: List[String], name: String, origin: String, definition: String, kind: String = BindingKind.Type) extends BindingInfo

  // These need to be strings (rather than cases of an enum) so that they get serialized correctly
  object ScopeKind {
    final val Namespace: String = "Namespace"
    final val Local: String = "Local"
    final val Global: String = "Global"
  }

  case class ScopeInfo(
      name: Option[String],
      kind: String,
      bindings: List[BindingInfo],
      outer: Option[ScopeInfo]
  )

  sealed trait HoleItem {
    val kind: String
  }

  object HoleItemKind {
    final val NaturalLanguage = "NaturalLanguage"
    final val Code = "Code"
  }

  case class NaturalLanguage(text: String) extends HoleItem {
    val kind = HoleItemKind.NaturalLanguage
  }
  case class Code(text: String, `type`: Option[String]) extends HoleItem {
    val kind = HoleItemKind.Code
  }

  case class CaptureInfo(
    position: Position,
    captures: CaptureSet,
    /**
     * Whether this capture set could be written into the source code at `position` using the `at { captures }` syntax
     */
    atSyntax: Boolean = false,
  )
}
