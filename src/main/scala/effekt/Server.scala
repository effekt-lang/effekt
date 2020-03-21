package effekt

import effekt.{ CompilationUnit, Driver, source }
import effekt.source.{ ModuleDecl, Tree }
import org.bitbucket.inkytonik.kiama
import org.bitbucket.inkytonik.kiama.util.Position

trait LSPServer extends Driver {

  import effekt.symbols._
  import effekt.source.{ Reference, Definition, Id, Literal }

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  type EffektTree = kiama.relation.Tree[Tree, ModuleDecl]

  def getInfoAt(position: Position): Option[(Vector[Tree], CompilationUnit)] = for {
    unit <- context.frontend(position.source)
    tree = new EffektTree(unit.module)
    nodes = positions.findNodesContaining(tree.nodes, position).sortWith {
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
  } yield (nodes, unit)

  override def getDefinition(position: Position): Option[Tree] = for {
    id <- getIdTreeAt(position)
    decl <- context.lookup(id) match {
      case u: UserFunction =>
        Some(u.decl)
      case u: Binder => Some(u.decl)
      case d: EffectOp => context.getDefinitionTree(d.effect)
      case u => context.getDefinitionTree(u)
    }
  } yield decl

  def getIdTreeAt(position: Position): Option[source.Id] = for {
    (trees, unit) <- getInfoAt(position)
    id <- trees.collectFirst { case id: source.Id => id  }
  } yield id

  def getSymbolAt(position: Position): Option[(Tree, Symbol)] = for {
    id <- getIdTreeAt(position)
    sym <- context.get(id)
  } yield (id, sym)


  def showInfo(header: String, sig: String = "", ex: String = ""): String =
    s"""|#### $header
        |${ if (sig != "") s"```effekt\n${sig}\n```" else ""}
        |${ if (!settingBool("showExplanations")) "" else "---\n" + ex.stripMargin('|') }
        |""".stripMargin('|')

  def getInfoOf(sym: Symbol): String = sym match {
    case b: BuiltinFunction =>
      showInfo("Builtin function", DeclPrinter(sym, context))

    case f: UserFunction =>
      val tpe = context.blockType(f)
      showInfo("Function", DeclPrinter(sym, context))

    case f: BuiltinEffect =>
      val ex = s"""|Builtin effects like `${f.name}` are tracked by the effect system,
                   |but cannot be handled with `try { ... } with { ... }`. The return type
                   |of the main function can still have unhandled builtin effects.
                   |"""

      showInfo("Builtin Effect", ex = ex)

    case f: EffectOp =>
      val tpe = context.blockType(f)

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
            |"""
      showInfo("Effect operation", DeclPrinter(sym, context), ex)

    case c: Constructor =>
      val tpe = context.blockType(c)

      val ex = s"""|Instances of data types like `${c.datatype.name}` can only store
                   |_values_, not _blocks_. Hence, constructors like `${c.name}` only have
                   |value parameter lists, not block parameters.
                   |"""

      showInfo(s"Constructor of data type `${c.datatype.name}`", DeclPrinter(sym, context), ex)

    case c: BlockParam =>
      val tpe = context.blockType(c)

      val ex =
        s"""|Blocks, like `${c.name}`, are similar to functions in other languages.
            |
            |However, there is an important difference to functions (like in JavaScript):
            |Blocks cannot be returned or stored in data types.
            |
            |They are thus more like blocks (hence the name) in Ruby that can only be
            |yielded to.
            |"""

      showInfo("Block parameter", s"{ ${c.name}: ${tpe} }", ex)

    case c: ResumeParam =>
      val tpe = context.blockType(c)

      val ex =
        s"""|Resumptions are block parameters, implicitly bound
            |when handling effect operations.
            |
            |The following three types have to be the same (i.e., `${tpe.ret.tpe}`):
            |- the return type of the operation clause
            |- the type of the handled expression enclosed by `try { EXPR } with { ... }`, and
            |- the return type of the resumption.
            |"""

      showInfo("Resumption", s"{ ${c.name}: ${tpe} }", ex)

    case c: ValueParam =>
      val tpe = context.valueTypeOrDefault(c, c.tpe.get)
      showInfo("Value parameter", s"${c.name}: ${tpe}")

    case c: VarBinder =>
      val tpe = context.valueTypeOrDefault(c, c.tpe.get)

      val ex =
        s"""|Like in other languages, mutable variable binders like `${c.name}`
            |can be modified (e.g., `${c.name} = VALUE`) by code that has `${c.name}`
            |in its lexical scope.
            |
            |However, as opposed to other languages, variable binders in Effekt
            |are stack allocated and show the right backtracking behavior in
            |combination with effect handlers.
         """

      showInfo("Mutable variable binder", s"${c.name}: ${tpe}", ex)

    case _ => ""
  }

  def maybeExplain(explanation: String): String =
    if (!settingBool("showExplanations")) "" else explanation.stripMargin('|')

  override def getHover(position: Position): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)
  } yield getInfoOf(sym)


  // The implementation in kiama.Server does not support file sources
  override def locationOfNode(node : Tree) : Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        val s = convertPosition(start)
        val f = convertPosition(finish)
        new Location(st.source.name, new LSPRange(s, f))
      case _ =>
          null
    }
  }
}

object Server extends LSPServer