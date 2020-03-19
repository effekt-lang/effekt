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
    unit <- context.resolve(position.source).toOption
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
    (trees, unit) <- getInfoAt(position)
    id <- trees.collectFirst { case id: source.Id => id  }
    decl <- context.lookup(id) match {
      case u: UserFunction =>
        Some(u.decl)
      case u: Binder => Some(u.decl)
      case d: EffectOp => context.getDefinitionTree(d.effect)
      case u => context.getDefinitionTree(u)
    }
  } yield decl

  override def getHover(position : Position): Option[String] = for {
    (trees, unit) <- getInfoAt(position)

    (tree, tpe) <- trees.collectFirst {
      case id: Id if context.get(id).isDefined =>
        (id, context.get(id).get match {
          case b: BuiltinFunction => b.toType
          case s: ValueSymbol => context.valueType(s)
          case b: BlockSymbol => context.blockType(b)
          case t: TypeSymbol => t
          case other => sys error s"unknown symbol kind ${other}"
        })
      case e: Literal[t] if context.annotation(e).isDefined =>
        (e, context.annotation(e).get)
    }
  } yield tpe.toString

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