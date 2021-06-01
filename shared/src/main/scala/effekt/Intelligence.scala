package effekt

import effekt.context.Context
import effekt.source.{ Tree, ModuleDecl }

import org.bitbucket.inkytonik.kiama
import kiama.util.Position

trait Intelligence {

  import effekt.symbols._

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
    decl <- C.getAST(position.source)
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
    case d: EffectOp     => C.definitionTreeOption(d.effect)
    case u               => C.definitionTreeOption(u)
  }

  // For now, only show the first call target
  def resolveCallTarget(sym: Symbol): Symbol = sym match {
    case t: CallTarget => t.symbols.flatten.head
    case s             => s
  }

  def getHoleInfo(hole: source.Hole)(implicit C: Context): Option[String] = for {
    outerTpe <- C.inferredTypeOption(hole)
    innerTpe <- C.inferredTypeOption(hole.stmts)
  } yield s"""| | Outside       | Inside        |
              | |:------------- |:------------- |
              | | `${outerTpe}` | `${innerTpe}` |
              |""".stripMargin

  def getInfoOf(sym: Symbol)(implicit C: Context): Option[SymbolInfo] = PartialFunction.condOpt(resolveCallTarget(sym)) {

    case b: BuiltinFunction =>
      SymbolInfo(b, "Builtin function", Some(DeclPrinter(b)), None)

    case f: UserFunction if C.blockTypeOption(f).isDefined =>
      SymbolInfo(f, "Function", Some(DeclPrinter(f)), None)

    case f: BuiltinEffect =>
      val ex = s"""|Builtin effects like `${f.name}` are tracked by the effect system,
                   |but cannot be handled with `try { ... } with ${f.name} { ... }`. The return type
                   |of the main function can still have unhandled builtin effects.
                   |""".stripMargin

      SymbolInfo(f, "Builtin Effect", None, Some(ex))

    case f: EffectOp =>
      val ex =
        s"""|Effect operations, like `${f.name}` allow to express non-local control flow.
            |
            |Other than blocks, the implementation of an effect operation is provided by
            |the closest
            |```effekt
            |try { EXPR } with ${f.effect.name} { def ${f.name}(...) => ...  }
            |```
            |that _dynamically_ surrounds the call-site `do ${f.name}(...)`.
            |
            |However, note that opposed to languages like Java, effect operations
            |cannot be _captured_ in Effekt. That is, if the type of a function or block
            |```effekt
            |def f(): Int / {}
            |```
            |does not mention the effect `${f.effect.name}`, then this effect will not be
            |handled by the handler. This is important when considering higher-order functions.
            |""".stripMargin

      SymbolInfo(f, "Effect operation", Some(DeclPrinter(f)), Some(ex))

    case f: EffectAlias =>
      SymbolInfo(f, "Effect alias", Some(DeclPrinter(f)), None)

    case t: TypeAlias =>
      SymbolInfo(t, "Type alias", Some(DeclPrinter(t)), None)

    case c: Record =>
      val ex = s"""|Instances of data types like `${c.tpe}` can only store
                   |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                   |value parameter lists, not block parameters.
                   |""".stripMargin

      SymbolInfo(c, s"Constructor of data type `${c.tpe}`", Some(DeclPrinter(c)), Some(ex))

    case c: BlockParam =>
      val signature = C.blockTypeOption(c).map { tpe => s"{ ${c.name}: ${tpe} }" }

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
      val tpe = C.blockTypeOption(c)
      val signature = tpe.map { tpe => s"{ ${c.name}: ${tpe} }" }
      val hint = tpe.map { tpe => s"(i.e., `${tpe.ret.tpe}`)" }.getOrElse { " " }

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

    case c: ValueParam =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => s"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value parameter", signature, None)

    case c: ValBinder =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => s"${c.name}: ${tpe}" }
      SymbolInfo(c, "Value binder", signature, None)

    case c: VarBinder =>
      val signature = C.valueTypeOption(c).orElse(c.tpe).map { tpe => s"${c.name}: ${tpe}" }

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
  }
}
