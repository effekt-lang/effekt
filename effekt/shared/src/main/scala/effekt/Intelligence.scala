package effekt

import effekt.context.{ Annotations, Context }
import effekt.source.{ FunDef, ModuleDecl, Tree }
import kiama.util.{ Position, Source }

trait Intelligence {

  import effekt.symbols._
  import builtins.TState

  type EffektTree = kiama.relation.Tree[Tree, ModuleDecl]

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

  def getTreesAt(position: Position)(implicit C: Context): Option[Vector[Tree]] = for {
    decl <- C.compiler.getAST(position.source)
    tree = new EffektTree(decl)
    pos = C.positions
    trees = pos.findNodesContaining(tree.nodes, position)
    nodes = trees.sortWith {
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
  } yield nodes

  def getIdTreeAt(position: Position)(implicit C: Context): Option[source.Id] = for {
    trees <- getTreesAt(position)
    id <- trees.collectFirst { case id: source.Id => id }
  } yield id

  def getSymbolAt(position: Position)(implicit C: Context): Option[(Tree, Symbol)] = for {
    id <- getIdTreeAt(position)
    sym <- C.symbolOption(id)
  } yield (id, resolveCallTarget(sym))

  def getDefinitionAt(position: Position)(implicit C: Context): Option[Tree] = for {
    (_, sym) <- getSymbolAt(position)
    decl <- getDefinitionOf(resolveCallTarget(sym))
  } yield decl

  def getDefinitionOf(s: Symbol)(implicit C: Context): Option[Tree] = s match {
    case u: UserFunction => Some(u.decl)
    case u: Binder       => Some(u.decl)
    case d: Operation    => C.definitionTreeOption(d.interface)
    case a: Anon         => Some(a.decl)
    case s: SelfParam => s.tree match {
      case d: source.Def => Some(d.ids.head) // TODO MRV 14
      case _             => Some(s.tree)
    }
    case u => C.definitionTreeOption(u)
  }

  // For now, only show the first call target
  def resolveCallTarget(sym: Symbol): Symbol = sym match {
    case t: CallTarget => t.symbols.flatten.head
    case s             => s
  }

  def getHoleInfo(hole: source.Hole)(implicit C: Context): Option[String] = for {
    outerTpe <- C.inferredTypeAndEffectOption(hole)
    innerTpe <- C.inferredTypeAndEffectOption(hole.stmts)
  } yield pp"""| | Outside       | Inside        |
               | |:------------- |:------------- |
               | | `${outerTpe}` | `${innerTpe}` |
               |""".stripMargin

  def allCaptures(src: Source)(using C: Context): List[(Tree, CaptureSet)] =
    C.annotationOption(Annotations.CaptureForFile, src).getOrElse(Nil)

  // For now we only show captures of function definitions and calls to box
  def getInferredCaptures(src: Source)(using C: Context): List[(Position, CaptureSet)] =
    allCaptures(src).filter {
      case (t, c) =>
        val p = C.positions.getStart(t)
        p.isDefined
    }.collect {
      case (t: source.FunDef, c) => for {
        pos <- C.positions.getStart(t)
      } yield (pos, c)
      case (t: source.DefDef, c) => for {
        pos <- C.positions.getStart(t)
      } yield (pos, c)
      case (source.Box(None, block), _) if C.inferredCaptureOption(block).isDefined => for {
        pos <- C.positions.getStart(block)
        capt <- C.inferredCaptureOption(block)
      } yield (pos, capt)
    }.flatten

  def getInfoOf(sym: Symbol)(implicit C: Context): Option[SymbolInfo] = PartialFunction.condOpt(resolveCallTarget(sym)) {

    case b: ExternFunction =>
      SymbolInfo(b, "External function definition", Some(DeclPrinter(List(b))), None)

    case f: UserFunction if C.functionTypeOption(f).isDefined =>
      SymbolInfo(f, "Function", Some(DeclPrinter(List(f))), None)

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

      SymbolInfo(f, "Effect operation", Some(DeclPrinter(List(f))), Some(ex))

    case f: EffectAlias =>
      SymbolInfo(f, "Effect alias", Some(DeclPrinter(List(f))), None)

    case t: TypeAlias =>
      SymbolInfo(t, "Type alias", Some(DeclPrinter(List(t))), None)

    case t: ExternType =>
      SymbolInfo(t, "External type definition", Some(DeclPrinter(List(t))), None)

    case t: ExternInterface =>
      SymbolInfo(t, "External interface definition", Some(DeclPrinter(List(t))), None)

    case t: ExternResource =>
      SymbolInfo(t, "External resource definition", Some(DeclPrinter(List(t))), None)

    case c: Constructor =>
      val ex = pp"""|Instances of data types like `${c.tpe}` can only store
                    |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                    |value parameter lists, not block parameters.
                    |""".stripMargin

      SymbolInfo(c, s"Constructor of data type `${c.tpe}`", Some(DeclPrinter(List(c))), Some(ex))

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

    case s: SelfParam =>

      val ex =
        s"""|Each function definition and handler implicitly introduces a
            |a local region to allocate mutable variables into.
            |
            |The region a variable is allocated into not only affects its lifetime, but
            |also its backtracking behavior in combination with continuation capture and
            |resumption.
            |""".stripMargin

      SymbolInfo(s, "Self region", None, Some(ex))

    case c: ValueParam =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value parameter", signature, None)

    case c: ValBinder =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value binder", signature, None)

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

    case c: DefBinder =>
      val signature = C.blockTypeOption(c).orElse(c.tpe).map { tpe => pp"${c.name}: ${tpe}" }
      SymbolInfo(c, "Block binder", signature, None)
  }
}
