package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.symbols.TmpValue
import effekt.util.messages.INTERNAL_ERROR

object Optimizer extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-optimizer"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, module) =>
        // (1) first thing we do is simply remove unused definitions (this speeds up all following analysis and rewrites)
        val mainSymbol = Context.checkMain(mod)
        val withoutUnused = RemoveUnusedDefinitions(Set(mainSymbol), module)

        // (2) inline unique block definitions
        val inlined = InlineUnique(Set(mainSymbol), withoutUnused)

        Some(CoreTransformed(source, tree, mod, inlined))
    }
}

object RemoveUnusedDefinitions {

  def apply(entrypoints: Set[Id], m: ModuleDecl)(using Context): ModuleDecl = {
    val reachable = Reachable(entrypoints, m.definitions.map(d => d.id -> d).toMap)

    m.copy(
      definitions = m.definitions.filter { d => reachable.isDefinedAt(d.id) },
      externs = m.externs.collect {
        case e: Extern.Def if reachable.isDefinedAt(e.id) => e
        case e: Extern.Include => e
      }
    )
  }
}

/**
 * Inlines block definitions that are only used exactly once.
 *
 * 1. First computes usage (using [[Reachable.apply]])
 * 2. Top down traversal
 *    - Definitions: definitions that are used at most once are dropped
 *      (since they will be inlined at the callsite).
 *      All other definitions are kept.
 *    - References: we inline unique definitions
 *
 * Invariants:
 *   - the context `defs` always contains the _original_ definitions, not rewritten ones.
 *     Rewriting them has to be performed at the inline-site.
 */
object InlineUnique {

  case class InlineContext(
    // is mutable to update when introducing temporaries
    var usage: Map[Id, Usage],
    defs: Map[Id, Definition]
  ) {
    def ++(other: Map[Id, Definition]): InlineContext = InlineContext(usage, defs ++ other)

    def ++=(fresh: Map[Id, Usage]): Unit = { usage = usage ++ fresh }
  }

  def apply(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl = {
    val usage = Reachable(m) ++ entrypoints.map(id => id -> Usage.Many).toMap
    val defs = m.definitions.map(d => d.id -> d).toMap

    val (updatedDefs, _) = scope(m.definitions)(using InlineContext(usage, defs))
    m.copy(definitions = updatedDefs)
  }

  def shouldInline(id: Id)(using ctx: InlineContext): Boolean =
    ctx.usage.get(id) match {
      case None => false
      case Some(Usage.Once) => true
      case Some(Usage.Recursive) => false // we don't inline recursive functions for the moment
      case Some(Usage.Many) => false
    }

  def shouldKeep(id: Id)(using ctx: InlineContext): Boolean =
    ctx.usage.get(id) match {
      case None => false
      case Some(Usage.Once) => false
      case Some(Usage.Recursive) => true // we don't inline recursive functions for the moment
      case Some(Usage.Many) => true
    }

  def scope(definitions: List[Definition])(using ctx: InlineContext): (List[Definition], InlineContext) =
    given allDefs: InlineContext = ctx ++ definitions.map(d => d.id -> d).toMap

    val filtered = definitions.collect {
      case Definition.Def(id, block) if shouldKeep(id) => Definition.Def(id, rewrite(block))
      case Definition.Let(id, binding) => Definition.Let(id, rewrite(binding))
    }
    (filtered, allDefs)

  def blockDefFor(id: Id)(using ctx: InlineContext): Option[Block] =
    ctx.defs.get(id) map {
      case Definition.Def(id, block) => rewrite(block)
      case Definition.Let(id, binding) => INTERNAL_ERROR("Should not happen")
    }

  def dealias(b: Block.BlockVar)(using ctx: InlineContext): BlockVar =
    ctx.defs.get(b.id) match {
      case Some(Definition.Def(id, aliased : Block.BlockVar)) => dealias(aliased)
      case _ => b
    }

  def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using ctx: InlineContext): Stmt = {

    // Only bind if not already a variable!!!
    var ids: Set[Id] = Set.empty
    var bindings: List[Definition.Def] = Nil
    var bvars: List[Block.BlockVar] = Nil

    // (1) first bind
    bargs foreach {
      case x: Block.BlockVar => bvars = bvars :+ x
      // introduce a binding
      case block =>
        val id = symbols.TmpBlock()
        bindings = bindings :+ Definition.Def(id, block)
        bvars = bvars :+ Block.BlockVar(id, block.tpe, block.capt)
        ids += id
    }


    // (2) substitute
    val body = substitutions.substitute(b, targs, vargs, bvars)

    //    println(s"IDS: ${ids}")
    //    println("BEFORE")
    //    debug(b.body)
    //    println("AFTER")
    //    debug(Stmt.Scope(bindings, body))

    // Only introduce scope, if necessary
    if bindings.isEmpty then rewrite(body) else {
      val result: Stmt.Scope = scope(bindings, body)

      // (3) inline unique block args again
      val newUsage = Reachable(result).filter { case (id, usage) => ids.contains(id) }
      // Record fresh usage in context. It is ok to just add the new usage information to the global
      // usage database since we only inline unique bindings. We would need to reconsider everyting,
      // as soon as we start inlining multiple occurrences.
      ctx ++= newUsage

      val rewritten = rewrite(result)
      //      println("REWRITTEN")
      //      debug(rewritten)
      rewritten
    }
  }

  def debug(s: Stmt): Unit = println(core.PrettyPrinter.format(s))

  def rewrite(d: Definition)(using InlineContext): Definition = d match {
    case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
    case Definition.Let(id, binding) => Definition.Let(id, rewrite(binding))
  }

  def rewrite(s: Stmt)(using InlineContext): Stmt = s match {
    case Stmt.Scope(definitions, body) =>
      val (filtered, ctx) = scope(definitions)
      scope(filtered, rewrite(body)(using ctx))

    case Stmt.App(b, targs, vargs, bargs) =>
      app(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))

    // congruences
    case Stmt.Return(expr) => Return(rewrite(expr))
    case Stmt.Val(id, binding, body) => valDef(id, rewrite(binding), rewrite(body))
    case Stmt.If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      Match(rewrite(scrutinee), clauses.map { case (id, value) => id -> rewrite(value) }, default.map(rewrite))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, rewrite(init), region, rewrite(body))
    case Stmt.Try(body, handlers) => Try(rewrite(body), handlers.map(rewrite))
    case Stmt.Region(body) => Region(rewrite(body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, rewrite(init), capture, rewrite(body))
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, tpe)
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, rewrite(value))
    case Stmt.Hole() => s
  }
  def rewrite(b: BlockLit)(using InlineContext): BlockLit =
    b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams, rewrite(body))
    }

  def rewrite(b: Block)(using InlineContext): Block = b match {
    case Block.BlockVar(id, _, _) if shouldInline(id) => blockDefFor(id).getOrElse(b)
    case b @ Block.BlockVar(id, _, _) => dealias(b)

    // congruences
    case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) => rewrite(b)
    case Block.Member(block, field, annotatedTpe) => member(rewrite(block), field, annotatedTpe)
    case Block.Unbox(pure) => unbox(rewrite(pure))
    case Block.New(impl) => New(rewrite(impl))
  }

  def rewrite(s: Implementation)(using InlineContext): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = rewrite(op.body))
      })
    }

  def rewrite(p: Pure)(using InlineContext): Pure = p match {
    case Pure.PureApp(b, targs, vargs) => pureApp(rewrite(b), targs, vargs.map(rewrite))
    // currently, we don't inline values
    case Pure.ValueVar(id, annotatedType) => p

    // congruences
    case Pure.Literal(value, annotatedType) => p
    case Pure.Select(target, field, annotatedType) => Select(rewrite(target), field, annotatedType)
    case Pure.Box(b, annotatedCapture) => box(rewrite(b), annotatedCapture)
  }

  def rewrite(e: Expr)(using InlineContext): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) => directApp(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))

    // congruences
    case Run(s) => run(rewrite(s))
    case pure: Pure => rewrite(pure)
  }

  case class Binding[A](run: (A => Stmt) => Stmt) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))


  // smart constructors to establish a normal form
  def valDef(id: Id, binding: Stmt, body: Stmt): Stmt =
    binding match {
      // This opt is too good for JS: it blows the stack on
      // recursive functions that are used to encode while...
      //
      // The solution to this problem is implemented in core.MakeStackSafe:
      //   all recursive functions that could blow the stack are trivially wrapped
      //   again, after optimizing.
      case Stmt.Return(expr) =>
        scope(List(Definition.Let(id, expr)), body)

      // here we are flattening scopes; be aware that this extends
      // life-times of bindings!
      //
      // { val x = { def...; BODY }; REST } = { def ...; val x = BODY }
      case Stmt.Scope(definitions, binding) =>
        scope(definitions, valDef(id, binding, body))

      case _ => Stmt.Val(id, binding, body)
    }

  // { def f=...; { def g=...; BODY } } = { def f=...; def g; BODY }
  def scope(definitions: List[Definition], body: Stmt): Stmt.Scope = body match {
    case Stmt.Scope(others, body) => scope(definitions ++ others, body)
    case _ => Stmt.Scope(definitions, body)
  }

  def app(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])(using InlineContext): Stmt =
    callee match {
      case b : Block.BlockLit => reduce(b, targs, vargs, bargs)
      case other => Stmt.App(callee, targs, vargs, bargs)
    }

  def pureApp(callee: Block, targs: List[ValueType], vargs: List[Pure]): Pure =
    callee match {
      case b : Block.BlockLit =>
        INTERNAL_ERROR(
          """|This should not happen!
             |User defined functions always have to be called with App, not PureApp.
             |If this error does occur, this means this changed.
             |Check `core.Transformer.makeFunctionCall` for details.
             |""".stripMargin)
      case other => Pure.PureApp(callee, targs, vargs)
    }

  def directApp(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])(using InlineContext): Expr =
    callee match {
      case b : Block.BlockLit => run(reduce(b, targs, vargs, Nil))
      case other => DirectApp(callee, targs, vargs, bargs)
    }

  def member(b: Block, field: Id, annotatedTpe: BlockType): Block = b match {
    case Block.New(impl) =>
      val Operation(name, tps, cps, vps, bps, resume, body) =
        impl.operations.find(op => op.name == field).getOrElse {
          INTERNAL_ERROR("Should not happen")
        }
      assert(resume.isEmpty, "We do not inline effectful capabilities at that point")
      BlockLit(tps, cps, vps, bps, body)
    case _ => Block.Member(b, field, annotatedTpe)
  }

  def run(s: Stmt): Expr = s match {
    case Stmt.Return(expr) => expr
    case _ => Run(s)
  }

  def box(b: Block, capt: Captures): Pure = b match {
    case Block.Unbox(pure) => pure
    case b => Box(b, capt)
  }

  def unbox(p: Pure): Block = p match {
    case Pure.Box(b, _) => b
    case p => Unbox(p)
  }
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
      val allDefs = defs ++ definitions.map(d => d.id -> d).toMap
      definitions.foreach(process)
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

