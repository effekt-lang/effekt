package effekt
package core
package optimizer

/**
 * A simple reachability analysis.
 *
 * TODO reachability should also process externs since they now contain splices.
 */
class Reachable(
  var reachable: Map[Id, Usage],
  var stack: List[Id],
  var seen: Set[Id]
) {

  // TODO we could use [[Binding]] here.
  type Definitions = Map[Id, Block | Expr | Stmt]

  private def update(id: Id, u: Usage): Unit = reachable = reachable.updated(id, u)
  private def usage(id: Id): Usage = reachable.getOrElse(id, Usage.Never)

  def processDefinition(id: Id, d: Block | Expr | Stmt)(using defs: Definitions): Unit = {
    if stack.contains(id) then { update(id, Usage.Recursive); return }

    seen = seen + id

    d match {
      case block: Block =>

        val before = stack
        stack = id :: stack

        process(block)
        stack = before

      case binding: Expr => process(binding)
      case binding: Stmt => process(binding)
    }
  }

  def process(id: Id)(using defs: Definitions): Unit =
    if (stack.contains(id)) {
      update(id, Usage.Recursive)
      return;
    }

    update(id, usage(id) + Usage.Once)

    if (!seen.contains(id)) {
      seen = seen + id
      defs.get(id).foreach { d => processDefinition(id, d) }
    }


  def process(b: Block)(using defs: Definitions): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => process(id)
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(s: Stmt)(using defs: Definitions): Unit = s match {
    case Stmt.Def(id, block, body) =>
      // Do NOT process `block` here, since this would mean the definition is used
      process(body)(using defs + (id -> block))
    case Stmt.Let(id, tpe, binding, body) =>
      // DO only process if impure, since we need to keep it in this case
      // for its side effects
      if (binding.capt.nonEmpty) { processDefinition(id, binding) }
      process(body)(using defs + (id -> binding))
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, tpe, binding, body) => process(binding); process(body)
    case Stmt.App(callee, targs, vargs, bargs) =>
      process(callee)
      vargs.foreach(process)
      bargs.foreach(process)
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

  def process(e: Expr)(using defs: Definitions): Unit = e match {
    case DirectApp(b, targs, vargs, bargs) =>
      process(b)
      vargs.foreach(process)
      bargs.foreach(process)
    case Pure.ValueVar(id, annotatedType) => process(id)
    case Pure.Literal(value, annotatedType) => ()
    case Pure.PureApp(b, targs, vargs) => process(b); vargs.foreach(process)
    case Pure.Make(data, tag, vargs) => process(tag); vargs.foreach(process)
    case Pure.Box(b, annotatedCapture) => process(b)
  }

  def process(i: Implementation)(using defs: Definitions): Unit =
    i.operations.foreach { op => process(op.body) }
}

object Reachable {
  def apply(entrypoints: Set[Id], m: ModuleDecl): Map[Id, Usage] = {
    val definitions: Map[Id, Block | Expr | Stmt] = m.definitions.map {
      case Toplevel.Def(id, block) => id -> block
      case Toplevel.Val(id, tpe, binding) => id -> binding
    }.toMap
    val initialUsage = entrypoints.map { id => id -> Usage.Recursive }.toMap
    val analysis = new Reachable(initialUsage, Nil, Set.empty)

    entrypoints.foreach(d => analysis.process(d)(using definitions))

    analysis.reachable
  }
}

enum Usage {
  case Never
  case Once
  case Many
  case Recursive

  def +(other: Usage): Usage = (this, other) match {
    case (Usage.Never, other) => other
    case (other, Usage.Never) => other
    case (other, Usage.Recursive) => Usage.Recursive
    case (Usage.Recursive, other) => Usage.Recursive
    case (Usage.Once, Usage.Once) => Usage.Many
    case (Usage.Many, Usage.Many) => Usage.Many
    case (Usage.Many, Usage.Once) => Usage.Many
    case (Usage.Once, Usage.Many) => Usage.Many
  }

  // -1
  def decrement: Usage = this match {
    case Usage.Never => Usage.Never
    case Usage.Once => Usage.Never
    case Usage.Many => Usage.Many
    case Usage.Recursive => Usage.Recursive
  }
}
