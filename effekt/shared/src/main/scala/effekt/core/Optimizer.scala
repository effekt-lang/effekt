package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, module) =>
        // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
        val mainSymbol = Context.checkMain(mod)
        val withoutUnused = RemoveUnusedDefinitions(Set(mainSymbol), module)

        // (2) simple optimizations like return-run elimination
        val simpleOpts = SimpleOptimizations(withoutUnused)

        Some(CoreTransformed(source, tree, mod, simpleOpts))
    }
}

object SimpleOptimizations {
  def apply(m: ModuleDecl)(using Context): ModuleDecl = {
    m.copy(definitions = m.definitions.map { d =>
      val opt = eliminateReturnRun.rewrite(d)
      directStyleVal.rewrite(opt)
    })
  }

  // a very small and easy post processing step...
  // reduces run-return pairs
  object eliminateReturnRun extends core.Tree.Rewrite {
    override def expr = {
      case core.Run(core.Return(p)) => rewrite(p)
    }
  }

  // rewrite (Val (Return e) s) to (Let e s)
  object directStyleVal extends core.Tree.Rewrite {
    override def stmt = {
      case core.Val(id, core.Return(expr), body) =>
        Let(id, rewrite(expr), rewrite(body))
    }
  }
}


object RemoveUnusedDefinitions {

  def apply(entrypoints: Set[Id], m: ModuleDecl)(using Context): ModuleDecl = {
    val reachable = Reachable(entrypoints, m.definitions.map(d => d.id -> d).toMap)
    m.copy(
      definitions = m.definitions.filter { d => reachable.contains(d.id) },
      externs = m.externs.collect {
        case e: Extern.Def if reachable.contains(e.id) => e
        case e: Extern.Include => e
      }
    )
  }
}

/**
 * A simple reachability analysis for toplevel definitions
 *
 * TODO this could also be extended to cover record and interface declarations.
 */
class Reachable(
  var reachable: Set[Id]
) {
  def process(d: Definition)(using defs: Map[Id, Definition]): Unit =
    if reachable.contains(d.id) then return else d match {
    case Definition.Def(id, block) =>
      reachable = reachable + id
      process(block)

    case Definition.Let(id, binding) =>
      reachable = reachable + id
      process(binding)
  }

  def markAsReachable(id: Id)(using defs: Map[Id, Definition]): Unit =
    defs.get(id).foreach(process)
    reachable = reachable + id

  def process(b: Block)(using defs: Map[Id, Definition]): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => markAsReachable(id)
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => process(body)
      case Block.Member(block, field, annotatedTpe) => process(block)
      case Block.Unbox(pure) => process(pure)
      case Block.New(impl) => process(impl)
    }

  def process(s: Stmt)(using defs: Map[Id, Definition]): Unit = s match {
    case Stmt.Scope(definitions, body) =>
      val allDefs = defs ++ definitions.map(d => d.id -> d).toMap
      definitions.foreach {
        case Definition.Def(id, block) => process(block)
        case Definition.Let(id, binding) => process(binding)
      }
      process(body)(using allDefs)
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
    case Stmt.State(id, init, region, body) =>
      process(init)
      markAsReachable(region)
      process(body)
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
    case Pure.ValueVar(id, annotatedType) => markAsReachable(id)
    case Pure.Literal(value, annotatedType) => ()
    case Pure.PureApp(b, targs, vargs) => process(b); vargs.foreach(process)
    case Pure.Select(target, field, annotatedType) => process(target)
    case Pure.Box(b, annotatedCapture) => process(b)
  }

  def process(i: Implementation)(using defs: Map[Id, Definition]): Unit =
    i.operations.foreach { op => process(op.body) }

}

object Reachable {
  def apply(entrypoints: Set[Id], definitions: Map[Id, Definition]): Set[Id] = {
    val analysis = new Reachable(Set.empty)
    entrypoints.foreach(d => analysis.markAsReachable(d)(using definitions))
    analysis.reachable
  }
}
