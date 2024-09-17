package effekt
package core

import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.core.normal.*

import scala.annotation.tailrec
import scala.collection.mutable

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
class Reachable(entrypoints: Set[Id], definitions: Map[Id, Definition]) {
  private val reachable = mutable.Map[Id, Usage]()
  private val seen = mutable.Set[Id]()
  private val stack = mutable.Stack[Id]()

  def analyze(): Map[Id, Usage] = {
    entrypoints.foreach(process)
    reachable.toMap
  }

  private def within(id: Id)(f: => Unit): Unit = {
    stack.push(id)
    f
    stack.pop()
  }

  private def process(id: Id): Unit = {
    if (stack.contains(id)) {
      reachable(id) = Usage.Recursive
      return
    }

    reachable.get(id) match {
      case Some(Usage.Once) => reachable(id) = Usage.Many
      case Some(Usage.Many | Usage.Recursive) => // Do nothing
      case None => reachable(id) = Usage.Once
    }

    if (!seen.contains(id)) {
      seen.add(id)
      definitions.get(id).foreach(process)
    }
  }

  private def process(d: Definition): Unit = d match {
    case Definition.Def(id, block) =>
      within(id) { process(block) }
    case Definition.Let(id, _, binding) =>
      process(binding)
  }

  @tailrec
  private def process(b: Block): Unit = b match {
    case Block.BlockVar(id, _, _) => process(id)
    case Block.BlockLit(_, _, _, _, body) => process(body)
    case Block.Member(block, _, _) => process(block)
    case Block.Unbox(pure) => process(pure)
    case Block.New(impl) => process(impl)
  }

  private def process(s: Stmt): Unit = s match {
    case Stmt.Scope(definitions, body) =>
      definitions.foreach(process)
      process(body)
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, tpe, binding, body) => process(binding); process(body)
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

  private def process(e: Expr): Unit = e match {
    case DirectApp(b, _, vargs, bargs) =>
      process(b)
      vargs.foreach(process)
      bargs.foreach(process)
    case Run(s) => process(s)
    case Pure.ValueVar(id, _) => process(id)
    case Pure.Literal(_, _) => ()
    case Pure.PureApp(b, _, vargs) =>
      process(b)
      vargs.foreach(process)
    case Pure.Make(_, tag, vargs) =>
      process(tag)
      vargs.foreach(process)
    case Pure.Select(target, _, _) => process(target)
    case Pure.Box(b, _) => process(b)
  }

  @inline
  private def process(i: Implementation): Unit =
    i.operations.foreach(op => process(op.body))
}

object Reachable {
  def apply(entrypoints: Set[Id], definitions: Map[Id, Definition]): Map[Id, Usage] =
    new Reachable(entrypoints, definitions).analyze()

  def apply(m: ModuleDecl): Map[Id, Usage] =
    apply(m.definitions.map(_.id).toSet, m.definitions.map(d => d.id -> d).toMap)

  def apply(s: Stmt.Scope): Map[Id, Usage] = {
    val analysis = new Reachable(Set.empty, Map.empty)
    analysis.process(s)
    analysis.reachable.toMap
  }
}

enum Usage {
  case Once
  case Many
  case Recursive
}

