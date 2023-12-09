package effekt
package core

import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.core.normal.*

class Deadcode(entrypoints: Set[Id], definitions: Map[Id, Definition]) extends core.Tree.Rewrite {

  val reachable = Reachable(entrypoints, definitions)

  override def stmt = {
    // Remove local unused definitions
    case Scope(defs, stmt) =>
      scope(defs.collect {
        case d: Definition.Def if reachable.isDefinedAt(d.id) => rewrite(d)
        // we only keep non-pure OR reachable let bindings
        case d: Definition.Let if d.capt.nonEmpty || reachable.isDefinedAt(d.id) => rewrite(d)
      }, rewrite(stmt))
  }

  override def rewrite(m: ModuleDecl): ModuleDecl = m.copy(
    // Remove top-level unused definitions
    definitions = m.definitions.collect { case d if reachable.isDefinedAt(d.id) => rewrite(d) },
    externs = m.externs.collect {
      case e: Extern.Def if reachable.isDefinedAt(e.id) => e
      case e: Extern.Include => e
    }
  )
}

object Deadcode {
  def remove(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl =
    Deadcode(entrypoints, m.definitions.map(d => d.id -> d).toMap).rewrite(m)
  def remove(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    remove(Set(entrypoint), m)
}

/**
 * A simple reachability analysis for toplevel definitions
 *
 * TODO this could also be extended to cover record and interface declarations.
 */
class Reachable(
  var reachable: Map[Id, Usage],
  var stack: List[Id],
  var seen: Set[Id]
) {

  def within(id: Id)(f: => Unit): Unit = {
    stack = id :: stack
    f
    stack = stack.tail
  }

  def process(d: Definition)(using defs: Map[Id, Definition]): Unit =
    if stack.contains(d.id) then
      reachable = reachable.updated(d.id, Usage.Recursive)
    else d match {
      case Definition.Def(id, block) =>
        seen = seen + id
        within(id) { process(block) }

      case Definition.Let(id, binding) =>
        seen = seen + id
        process(binding)
    }

  def process(id: Id)(using defs: Map[Id, Definition]): Unit =
    if (stack.contains(id)) {
      reachable = reachable.updated(id, Usage.Recursive)
      return;
    }

    val count = reachable.get(id) match {
      case Some(Usage.Once) => Usage.Many
      case Some(Usage.Many) => Usage.Many
      case Some(Usage.Recursive) => Usage.Recursive
      case None => Usage.Once
    }
    reachable = reachable.updated(id, count)
    if (!seen.contains(id)) {
      defs.get(id).foreach(process)
    }

  def process(b: Block)(using defs: Map[Id, Definition]): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => process(id)
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Member(block, field, annotatedTpe) => process(block)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(s: Stmt)(using defs: Map[Id, Definition]): Unit = s match {
    case Stmt.Scope(definitions, body) =>
      var currentDefs = defs
      definitions.foreach {
        case d: Definition.Def =>
          currentDefs += d.id -> d // recursive
          process(d)(using currentDefs)
        case d: Definition.Let =>
          process(d)(using currentDefs)
          currentDefs += d.id -> d // non-recursive
      }
      process(body)(using currentDefs)
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, binding, body) => process(binding); process(body)
    case Stmt.App(callee, targs, vargs, bargs) =>
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
      process(region)
      process(body)
    case Stmt.Var(id, init, capture, body) =>
      process(init)
      process(body)
    case Stmt.Get(id, capt, tpe) => process(id)
    case Stmt.Put(id, tpe, value) => process(id); process(value)
    case Stmt.Try(body, handlers) => process(body); handlers.foreach(process)
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
    case Pure.Make(id, tpe, targs, vargs) => process(id); vargs.foreach(process)
    case Pure.Select(target, field, annotatedType) => process(target)
    case Pure.Box(b, annotatedCapture) => process(b)
  }

  def process(i: Implementation)(using defs: Map[Id, Definition]): Unit =
    i.operations.foreach { op => process(op.body) }

}

object Reachable {
  def apply(entrypoints: Set[Id], definitions: Map[Id, Definition]): Map[Id, Usage] = {
    val analysis = new Reachable(Map.empty, Nil, Set.empty)
    entrypoints.foreach(d => analysis.process(d)(using definitions))
    analysis.reachable
  }

  def apply(m: ModuleDecl): Map[Id, Usage] = {
    val analysis = new Reachable(Map.empty, Nil, Set.empty)
    val defs = m.definitions.map(d => d.id -> d).toMap
    m.definitions.foreach(d => analysis.process(d)(using defs))
    analysis.reachable
  }

  def apply(s: Stmt.Scope): Map[Id, Usage] = {
    val analysis = new Reachable(Map.empty, Nil, Set.empty)
    analysis.process(s)(using Map.empty)
    analysis.reachable
  }
}

enum Usage {
  case Once
  case Many
  case Recursive
}

