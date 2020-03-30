package effekt

import effekt.context.Context
import effekt.source.{ ModuleDecl, Tree }

import org.bitbucket.inkytonik.kiama
import kiama.util.Position

trait Intelligence extends Compiler {

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
    decl <- C.compiler.trees.get(position.source)
    tree = new EffektTree(decl)
    trees = positions.findNodesContaining(tree.nodes, position)
    nodes = trees.sortWith {
      (t1, t2) =>
        val p1s = positions.getStart(t1).get
        val p2s = positions.getStart(t2).get

        if (p2s == p1s) {
          val p1e = positions.getFinish(t1).get
          val p2e = positions.getFinish(t2).get
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
  } yield (id, sym)

  def getDefinitionAt(position: Position)(implicit C: Context): Option[Tree] = for {
    id <- getIdTreeAt(position)
    decl <- C.symbolOf(id) match {
      case u: UserFunction =>
        Some(u.decl)
      case u: Binder   => Some(u.decl)
      case d: EffectOp => C.definitionTreeOf(d.effect)
      case u           => C.definitionTreeOf(u)
    }
  } yield decl

  def getInfoOf(sym: Symbol)(implicit C: Context): Option[SymbolInfo] = PartialFunction.condOpt(sym) {
    case b: BuiltinFunction =>
      SymbolInfo(sym, "Builtin function", Some(DeclPrinter(sym)), None)

    case f: UserFunction =>
      val tpe = C.blockTypeOf(f)
      SymbolInfo(sym, "Function", Some(DeclPrinter(sym)), None)

    case f: BuiltinEffect =>
      val ex = s"""|Builtin effects like `${f.name}` are tracked by the effect system,
                   |but cannot be handled with `try { ... } with { ... }`. The return type
                   |of the main function can still have unhandled builtin effects.
                   |""".stripMargin

      SymbolInfo(sym, "Builtin Effect", None, Some(ex))

    case f: EffectOp =>
      val ex =
        s"""|Effect operations, like `${f.name}` allow to express non-local control flow.
            |
            |Other than blocks, the implementation of an effect operation is provided by
            |the closest
            |```effekt
            |try { EXPR } with { case ${f.name}(...) => ...  }
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

      SymbolInfo(sym, "Effect operation", Some(DeclPrinter(sym)), Some(ex))

    case f: EffectAlias =>
      SymbolInfo(sym, "Effect alias", Some(DeclPrinter(sym)), None)

    case t: TypeAlias =>
      SymbolInfo(sym, "Type alias", Some(DeclPrinter(sym)), None)

    case c: Constructor =>
      val ex = s"""|Instances of data types like `${c.datatype.name}` can only store
                   |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                   |value parameter lists, not block parameters.
                   |""".stripMargin

      SymbolInfo(sym, s"Constructor of data type `${c.datatype.name}`", Some(DeclPrinter(sym)), Some(ex))

    case c: BlockParam =>
      val tpe = C.blockTypeOf(c)

      val ex =
        s"""|Blocks, like `${c.name}`, are similar to functions in other languages.
            |
            |However, there is an important difference to functions (like in JavaScript):
            |Blocks cannot be returned or stored in data types.
            |
            |They are thus more like blocks (hence the name) in Ruby that can only be
            |yielded to.
            |""".stripMargin

      SymbolInfo(sym, "Block parameter", Some(s"{ ${c.name}: ${tpe} }"), Some(ex))

    case c: ResumeParam =>
      val tpe = C.blockTypeOf(c)

      val ex =
        s"""|Resumptions are block parameters, implicitly bound
            |when handling effect operations.
            |
            |The following three types have to be the same (i.e., `${tpe.ret.tpe}`):
            |- the return type of the operation clause
            |- the type of the handled expression enclosed by `try { EXPR } with { ... }`, and
            |- the return type of the resumption.
            |""".stripMargin

      SymbolInfo(sym, "Resumption", Some(s"{ ${c.name}: ${tpe} }"), Some(ex))

    case c: ValueParam =>
      val tpe = C.valueTypeOption(c).getOrElse { c.tpe.get }
      SymbolInfo(sym, "Value parameter", Some(s"${c.name}: ${tpe}"), None)

    case c: VarBinder =>
      val tpe = C.valueTypeOption(c).getOrElse { c.tpe.get }

      val ex =
        s"""|Like in other languages, mutable variable binders like `${c.name}`
            |can be modified (e.g., `${c.name} = VALUE`) by code that has `${c.name}`
            |in its lexical scope.
            |
            |However, as opposed to other languages, variable binders in Effekt
            |are stack allocated and show the right backtracking behavior in
            |combination with effect handlers.
         """.stripMargin

      SymbolInfo(sym, "Mutable variable binder", Some(s"${c.name}: ${tpe}"), Some(ex))
  }
}
