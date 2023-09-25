package effekt.core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.symbols
import effekt.symbols.builtins.{ TState, TUnit }
import effekt.symbols.{ TmpBlock, TmpValue }
import effekt.{ CoreTransformed, Phase }

import scala.annotation.targetName

/**
 * [[Phase]] on [[CoreTransformed]] to translate access to mutable state cells to
 * direct style.
 *
 * e.g. val a = x.get(); s will become: let a = x.get(); s
 *
 * This is necessary for backends like JS, where we want to represent state using host language
 * reference cells, without paying for the monadic abstraction.
 */
object DirectStyleMutableState extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "direct style mutable state"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) =>
      val direct = directStyle.rewrite(core)
      Some(CoreTransformed(source, tree, mod, direct))
  }

  object directStyle extends Tree.Rewrite {
    override def stmt: PartialFunction[Stmt, Stmt] = {

      case Stmt.Val(y, Get(x, tpe), s) => Let(y, Get(rewrite(x), tpe), rewrite(s))

      case Stmt.Val(y, Put(x, tpe, v), s) => Let(y, Put(x, tpe, rewrite(v)), rewrite(s))

      case Get(x, tpe) =>
        val ids = List.tabulate(tpe.result.size)(i => Id("tmp" + i))
        Let(ids, Get(x, tpe), Stmt.Return(ids zip tpe.result map {(id, tpe) => Pure.ValueVar(id, tpe)}))

      case Put(x, tpe, v) =>
        val ids = List(Id("tmp"))
        Let(ids, Put(x, tpe, v), Stmt.Return(ids map {Pure.ValueVar(_, Type.TUnit)}))
    }
  }

  object Get {
    def unapply(s: Stmt): Option[(Block, BlockType.Function)] = s match {
      case Stmt.App(Block.Member(x, TState.get, tpe: BlockType.Function), Nil, Nil, Nil) => Some((x, tpe))
      case _ => None
    }
    def apply(x: Block, tpe: BlockType): Expr =
      DirectApp(Block.Member(x, TState.get, tpe), Nil, Nil, Nil)
  }

  object Put {
    def unapply(s: Stmt): Option[(Block, BlockType.Function, Pure)] = s match {
      case Stmt.App(Block.Member(x, TState.put, tpe: BlockType.Function), Nil, List(v), Nil) => Some((x, tpe, v))
      case _ => None
    }
    def apply(x: Block, tpe: BlockType, value: Pure): Expr =
      DirectApp(Block.Member(x, TState.put, tpe), Nil, List(value), Nil)
  }
}
