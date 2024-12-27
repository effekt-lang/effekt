package effekt
package core
package optimizer

/**
 * A simple reachability analysis.
 */
class Reachable(
  var reachable: Map[Id, Usage],
  var stack: List[Id],
  var seen: Set[Id]
) {

  private def update(id: Id, u: Usage): Unit = reachable = reachable.updated(id, u)
  private def usage(id: Id): Usage = reachable.getOrElse(id, Usage.Never)

  def process(d: Definition)(using defs: Map[Id, Definition]): Unit =
    if stack.contains(d.id) then update(d.id, Usage.Recursive)
    else d match {
      case Definition.Def(id, block) =>
        seen = seen + id

        val before = stack
        stack = id :: stack

        process(block)
        stack = before

      case Definition.Let(id, _, binding) =>
        seen = seen + id

        process(binding)
    }

  def process(id: Id)(using defs: Map[Id, Definition]): Unit =
    if (stack.contains(id)) {
      update(id, Usage.Recursive)
      return;
    }

    update(id, usage(id) + Usage.Once)

    if (!seen.contains(id)) {
      defs.get(id).foreach(process)
    }

  def process(b: Block)(using defs: Map[Id, Definition]): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => process(id)
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(s: Stmt)(using defs: Map[Id, Definition]): Unit = s match {
    case Stmt.Scope(definitions, body) =>
      var currentDefs = defs
      definitions.foreach {
        case d: Definition.Def =>
          currentDefs += d.id -> d // recursive
          // Do NOT process them here, since this would mean the definition is used
          // process(d)(using currentDefs)
        case d: Definition.Let =>
          // DO only process if NOT pure
          if (d.binding.capt.nonEmpty) {
            process(d)(using currentDefs)
          }
          currentDefs += d.id -> d // non-recursive
      }
      process(body)(using currentDefs)
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

  def process(e: Expr)(using defs: Map[Id, Definition]): Unit = e match {
    case DirectApp(b, targs, vargs, bargs) =>
      process(b);
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

  def process(i: Implementation)(using defs: Map[Id, Definition]): Unit =
    i.operations.foreach { op => process(op.body) }
}

object Reachable {
  def apply(entrypoints: Set[Id], m: ModuleDecl): Map[Id, Usage] = {
    val definitions = m.definitions.map(d => d.id -> d).toMap
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
