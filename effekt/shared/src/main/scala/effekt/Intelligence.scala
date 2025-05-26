package effekt

import effekt.context.{Annotations, Context}
import effekt.source.{FunDef, Include, Maybe, ModuleDecl, Span, Tree}
import effekt.symbols.{CaptureSet, Hole}
import kiama.util.{Position, Source}
import effekt.symbols.scopes.Scope

trait Intelligence {

  import effekt.symbols._
  import builtins.TState
  import Intelligence._

  type EffektTree = kiama.relation.Tree[AnyRef & Product, ModuleDecl]

  case class SymbolInfo(
    symbol: Symbol,
    header: String,
    signature: Option[String],
    description: Option[String]
  ) {
    def fullDescription: String = {
      val sig = signature.map(sig => s"```effekt\n$sig\n```").getOrElse { "" }
      val desc = description.getOrElse("")

      s"""|#### $header
          |$sig
          |$desc
          |""".stripMargin
    }

    def shortDescription: String = {
      val sig = signature.map(sig => s"```effekt\n$sig\n```").getOrElse { "" }

      s"""|#### $header
          |$sig
          |""".stripMargin
    }
  }

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
      case i @ Include(path) => C.annotationOption(Annotations.IncludedSymbols, i)
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
    (innerTpe, innerEff) <- C.inferredTypeAndEffectOption(hole.stmts)
  } yield pp"""| | Outside       | Inside        |
               | |:------------- |:------------- |
               | | `${outerTpe}` | `${innerTpe}` |
               |""".stripMargin

  def getHoles(src: Source)(using C: Context): List[HoleInfo] = for {
    (hole, scope) <- C.annotationOption(Annotations.HolesForFile, src).getOrElse(Nil)
    innerType = hole.innerType.map { t => pp"${t}" }
    expectedType = hole.expectedType.map { t => pp"${t}" }
  } yield {
    val BindingInfo(importedTerms, importedTypes, terms, types) = allBindings(scope)
    HoleInfo(hole.name.name, hole.decl.span, innerType, expectedType, importedTerms.toList.distinct, importedTypes.toList.distinct, terms.toList.distinct, types.toList.distinct)
  }

  def allBindings(scope: Scope)(using C: Context): BindingInfo =
    scope match {
      case Scope.Global(imports, bindings) =>
        val (te1, ty1) = allBindings(imports)
        val (te2, ty2) = allBindings(bindings)
        BindingInfo(te1, ty1, te2, ty2)
      case Scope.Named(name, bindings, outer) =>
        val (te, ty) = allBindings(bindings)
        BindingInfo(Seq.empty, Seq.empty, te, ty) ++ allBindings(outer)
      case Scope.Local(imports, bindings, outer) =>
        val outerBindings = allBindings(outer)
        val (te1, ty1) = allBindings(imports)
        val (te2, ty2) = allBindings(bindings)
        BindingInfo(te1, ty1, te2, ty2) ++ allBindings(outer)
    }

  def allBindings(bindings: Namespace, path: List[String] = Nil)(using C: Context): (Iterable[TermBinding], Iterable[TypeBinding]) =
    val types = bindings.types.flatMap {
      case (name, sym) =>
        // TODO this is extremely hacky, printing is not defined for all types at the moment
        try { Some(TypeBinding(path, name, DeclPrinter(sym))) } catch { case e => None }
    }
    val terms = bindings.terms.flatMap { case (name, syms) =>
      syms.collect {
        case sym: ValueSymbol => TermBinding(path, name, C.valueTypeOption(sym).map(t => pp"${t}"))
        case sym: BlockSymbol => TermBinding(path, name, C.blockTypeOption(sym).map(t => pp"${t}"))
      }
    }
    val (nestedTerms, nestedTypes) = bindings.namespaces.map {
      case (name, namespace) => allBindings(namespace, path :+ name)
    }.unzip

    (terms ++ nestedTerms.flatten, types ++ nestedTypes.flatten)

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
      case (source.Box(Maybe(None, span), block), _) if C.inferredCaptureOption(block).isDefined => for {
        capt <- C.inferredCaptureOption(block)
      } yield CaptureInfo(span.range.from, capt, true)
    }.flatten

  def getInfoOf(sym: Symbol)(using C: Context): Option[SymbolInfo] = PartialFunction.condOpt(resolveCallTarget(sym)) {

    case b: ExternFunction =>
      SymbolInfo(b, "External function definition", Some(DeclPrinter(b)), None)

    case f: UserFunction if C.functionTypeOption(f).isDefined =>
      SymbolInfo(f, "Function", Some(DeclPrinter(f)), None)

    case f: Operation =>
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

      SymbolInfo(f, "Effect operation", Some(DeclPrinter(f)), Some(ex))

    case f: EffectAlias =>
      SymbolInfo(f, "Effect alias", Some(DeclPrinter(f)), None)

    case t: TypeAlias =>
      SymbolInfo(t, "Type alias", Some(DeclPrinter(t)), None)

    case t: ExternType =>
      SymbolInfo(t, "External type definition", Some(DeclPrinter(t)), None)

    case t: ExternInterface =>
      SymbolInfo(t, "External interface definition", Some(DeclPrinter(t)), None)

    case t: ExternResource =>
      SymbolInfo(t, "External resource definition", Some(DeclPrinter(t)), None)

    case c: Constructor =>
      val ex = pp"""|Instances of data types like `${c.tpe}` can only store
                    |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                    |value parameter lists, not block parameters.
                    |""".stripMargin

      SymbolInfo(c, s"Constructor of data type `${c.tpe}`", Some(DeclPrinter(c)), Some(ex))

    case c: BlockParam =>
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

      SymbolInfo(c, "Block parameter", signature, Some(ex))

    case c: ResumeParam =>
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

      SymbolInfo(c, "Resumption", signature, Some(ex))

    case c: VarBinder =>
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

      SymbolInfo(c, "Mutable variable binder", signature, Some(ex))

    case s: RegBinder =>
      val signature = C.blockTypeOption(s).map(TState.extractType).orElse(s.tpe).map { tpe => pp"${s.name}: ${tpe}" }

      val ex =
        s"""|The region `${s.region.name}` the variable `${s.name}` is allocated into
            |not only affects its lifetime, but also its backtracking behavior
            |in combination with continuation capture and resumption.
            |""".stripMargin

      SymbolInfo(s, "Variable in region", signature, Some(ex))

    case c: ValueParam =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value parameter", signature, None)

    case c: ValBinder =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value binder", signature, None)

    case c: DefBinder =>
      val signature = C.blockTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Block binder", signature, None)
  }
}

object Intelligence {
  case class HoleInfo(
     id: String,
     span: Span,
     innerType: Option[String],
     expectedType: Option[String],
     importedTerms: List[TermBinding], importedTypes: List[TypeBinding],
     terms: List[TermBinding], types: List[TypeBinding]
  )

  case class TermBinding(qualifier: List[String], name: String, `type`: Option[String])

  case class TypeBinding(qualifier: List[String], name: String, definition: String)

  case class BindingInfo(
    importedTerms: Iterable[TermBinding],
    importedTypes: Iterable[TypeBinding],
    terms: Iterable[TermBinding],
    types: Iterable[TypeBinding]
  ) {
    def ++(other: BindingInfo): BindingInfo =
      BindingInfo(
        importedTerms ++ other.importedTerms,
        importedTypes ++ other.importedTypes,
        terms ++ other.terms,
        types ++ other.types)
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
