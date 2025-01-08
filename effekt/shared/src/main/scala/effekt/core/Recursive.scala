package effekt.core

import scala.collection.mutable

/**
 * Gather all functions and arguments of their recursive calls
 */
case class RecursiveFunction(
  definition: BlockLit,
  targs: mutable.Set[List[ValueType]] = mutable.Set.empty,
  vargs: mutable.Set[List[Pure]] = mutable.Set.empty,
  bargs: mutable.Set[List[Block]] = mutable.Set.empty
)

class Recursive(
  val defs: mutable.Map[Id, RecursiveFunction],
  var stack: List[Id]
) {
  def process(d: Toplevel): Unit =
    d match {
      case Toplevel.Def(id, block) => process(id, block)
      case Toplevel.Val(id, _, binding) => process(binding)
    }

  def process(b: Block): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => ()
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(id: Id, block: Block): Unit =
    block match {
      case b : BlockLit =>
        defs(id) = RecursiveFunction(b)
        val before = stack
        stack = id :: stack
        process(block)
        stack = before
      case _ => ()
    }

  def process(s: Stmt): Unit = s match {
    case Stmt.Def(id, block, body) =>  process(id, block); process(body)
    case Stmt.Let(id, tpe, binding, body) => process(binding); process(body)
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, tpe, binding, body) => process(binding); process(body)
    case a @ Stmt.App(callee, targs, vargs, bargs) =>
      process(callee)
      vargs.foreach(process)
      bargs.foreach(process)

      callee match {
        case BlockVar(id, annotatedTpe, annotatedCapt) =>
          if (stack.contains(id)) // is recursive
            defs(id).targs += targs
            defs(id).vargs += vargs
            defs(id).bargs += bargs
        case _ => ()
      }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      process(callee)
      vargs.foreach(process)
      bargs.foreach(process)
    case Stmt.If(cond, thn, els) => process(cond); process(thn); process(els)
    case Stmt.Match(scrutinee, clauses, default) =>
      process(scrutinee)
      clauses.foreach { case (id, value) => process(value) }
      default.foreach(process)
    case Stmt.Alloc(id, init, region, body) =>
      process(init)
      process(body)
    case Stmt.Var(id, init, capture, body) =>
      process(init)
      process(body)
    case Stmt.Get(id, capt, tpe) => ()
    case Stmt.Put(id, tpe, value) => process(value)
    case Stmt.Reset(body) => process(body)
    case Stmt.Shift(prompt, body) => process(prompt); process(body)
    case Stmt.Resume(k, body) => process(k); process(body)
    case Stmt.Region(body) => process(body)
    case Stmt.Hole() => ()
  }

  def process(e: Expr): Unit = e match {
    case DirectApp(b, targs, vargs, bargs) =>
      process(b)
      vargs.foreach(process)
      bargs.foreach(process)
    case Pure.ValueVar(id, annotatedType) => ()
    case Pure.Literal(value, annotatedType) => ()
    case Pure.PureApp(b, targs, vargs) => process(b); vargs.foreach(process)
    case Pure.Make(data, tag, vargs) => vargs.foreach(process)
    case Pure.Box(b, annotatedCapture) => process(b)
  }

  def process(i: Implementation): Unit =
    i.operations.foreach { op => process(op.body) }

  def process(m: ModuleDecl): Unit =
    m.definitions.map(process)
}

object Recursive {
  def apply(m: ModuleDecl) = {
    val analysis = new Recursive(mutable.Map.empty, List())
    analysis.process(m)
    analysis.defs
  }
}
