package effekt
package core

import effekt.context.Context
import effekt.symbols.{ TmpBlock, TmpValue }
import effekt.Phase
import effekt.PhaseResult.CoreTransformed
import effekt.core.Block.BlockLit
import effekt.lifted.Expr.ValueVar

/**
 * [[Phase]] on [[CoreTransformed]] to make programs stack safe on paltforms
 * that do not support TCO.
  *
 * This is necessary for backends like JS, where we should use the monadic trampoline
 * for recursive functions.
 */
object MakeStackSafe extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "stacksafe"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) =>
      val safe = stacksafe.rewrite(core)
      Some(CoreTransformed(source, tree, mod, safe))
  }

  object stacksafe extends Tree.Rewrite {

    var scopes: List[Id] = Nil
    def within[T](id: Id)(f: => T) : T = {
      val before = scopes
      scopes = id :: before
      val result = f
      scopes = before
      result
    }
    def isRecursive(id: Id): Boolean = scopes contains id
    def clear() = scopes = Nil

    override def stmt: PartialFunction[Stmt, Stmt] = {
      case Stmt.Val(id, binding, body) =>
        Stmt.Val(id, rewrite(binding), {
          // stop transformation under val binders, they already perform trampolining
          clear();
          rewrite(body)
        })

      case Stmt.App(x : BlockVar, targs, vargs, bargs) if isRecursive(x.id) =>
        thunk { Stmt.App(x, targs, vargs.map(rewrite), bargs.map(rewrite)) }
    }

    override def defn: PartialFunction[Definition, Definition] = {
      case Definition.Def(id, b : Block.BlockLit) =>
        Definition.Def(id, within(id) { rewrite(b) })
    }
  }

  // [[ s ]] = val tmp = return (); s
  def thunk(s: Stmt): Stmt = Stmt.Val(TmpValue(), Stmt.Return(List(Literal((), core.Type.TUnit))), s)
}
