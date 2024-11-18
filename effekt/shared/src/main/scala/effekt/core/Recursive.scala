package effekt.core

import scala.collection.mutable

// TODO: Is there a simpler way of iterating the functions and calls (like a fold)?
//       Reachable etc. use similar recursions
//       We could probably remove a lot of recursion here!

/**
 * Gather all functions, their arguments, and arguments of their recursive calls
 */
class Recursive(
  var defs: mutable.Map[Id, (
    List[Id], // cparams
    List[Id], // vparams
    List[Id], // bparams
    (mutable.Set[List[Block]], mutable.Set[List[Pure]], mutable.Set[List[Block]])
  )],
  var stack: List[Id]
) {
  def process(d: Definition): Unit =
    d match {
      case Definition.Def(id, block) =>
        block match {
          case BlockLit(tparams, cparams, vparams, bparams, body) =>
            defs(id) = (cparams, vparams.map(_.id), bparams.map(_.id), (mutable.Set(), mutable.Set(), mutable.Set()))
            val before = stack
            stack = id :: stack
            process(block)
            stack = before
          case _ => ()
        }
      case Definition.Let(id, _, binding) =>
        process(binding)
    }

  // TODO: Necessary?
  def process(id: Id): Unit =
    ()

  def process(b: Block): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => () // TODO?
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(s: Stmt): Unit = s match {
    case Stmt.Scope(definitions, body) =>
      definitions.foreach {
        case d: Definition.Def =>
          process(d)
        case d: Definition.Let =>
          process(d)
      }
      process(body)
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, tpe, binding, body) => process(binding); process(body)
    case a @ Stmt.App(callee, targs, vargs, bargs) =>
      process(callee)
      vargs.foreach(process)
      bargs.foreach(process)

      callee match {
        case BlockVar(id, annotatedTpe, annotatedCapt) => 
          if (stack.contains(id)) // is recursive
            defs(id)._4._1 += bargs // TOOD: can we always handle cargs as bargs?
            defs(id)._4._2 += vargs
            defs(id)._4._3 += bargs
        case _ => ()
      }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      process(callee)
      process(method)
      vargs.foreach(process)
      bargs.foreach(process)
    case Stmt.If(cond, thn, els) => process(cond); process(thn); process(els)
    case Stmt.Match(scrutinee, clauses, default) =>
      process(scrutinee)
      clauses.foreach { case (id, value) => process(value) }
      default.foreach(process)
    case Stmt.Alloc(id, init, region, body) =>
      process(init)
      process(region)
      process(body)
    case Stmt.Var(id, init, capture, body) =>
      process(init)
      process(body)
    case Stmt.Get(id, capt, tpe) => process(id)
    case Stmt.Put(id, tpe, value) => process(id); process(value)
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
    case Run(s) => process(s)
    case Pure.ValueVar(id, annotatedType) => process(id)
    case Pure.Literal(value, annotatedType) => ()
    case Pure.PureApp(b, targs, vargs) => process(b); vargs.foreach(process)
    case Pure.Make(data, tag, vargs) => process(tag); vargs.foreach(process)
    case Pure.Select(target, field, annotatedType) => process(field); process(target)
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