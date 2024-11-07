package effekt
package core

import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.core.normal.*
import effekt.util.messages.INTERNAL_ERROR

import scala.collection.mutable
import kiama.util.Counter

/**
 * Inlines block definitions.
 *
 * 1. First computes usage (using [[Reachable.apply]])
 * 2. Top down traversal where we inline definitions
 *
 * Invariants:
 *   - the context `defs` always contains the _original_ definitions, not rewritten ones.
 *     Rewriting them has to be performed at the inline-site.
 */
object Inline {

  case class InlineContext(
    // is mutable to update when introducing temporaries;
    // they should also be visible after leaving a scope (so mutable.Map and not `var usage`).
    usage: mutable.Map[Id, Usage],
    defs: Map[Id, Definition],
    maxInlineSize: Int,
    inlineCount: Counter = Counter(0)
  ) {
    def ++(other: Map[Id, Definition]): InlineContext = InlineContext(usage, defs ++ other, maxInlineSize, inlineCount)

    def ++=(fresh: Map[Id, Usage]): Unit = { usage ++= fresh }
  }

  def once(entrypoints: Set[Id], m: ModuleDecl, maxInlineSize: Int): (ModuleDecl, Int) = {
    val usage = Reachable(m) ++ entrypoints.map(id => id -> Usage.Many).toMap
    val defs = m.definitions.map(d => d.id -> d).toMap
    val context = InlineContext(mutable.Map.from(usage), defs, maxInlineSize)

    val (updatedDefs, _) = rewrite(m.definitions)(using context)
    (m.copy(definitions = updatedDefs), context.inlineCount.value)
  }

  def full(entrypoints: Set[Id], m: ModuleDecl, maxInlineSize: Int): ModuleDecl =
    var lastCount = 1
    var tree = m
    while (lastCount > 0) {
      val (inlined, count) = Inline.once(entrypoints, tree, maxInlineSize)
      // (3) drop unused definitions after inlining
      tree = Deadcode.remove(entrypoints, inlined)
      lastCount = count
    }
    tree

  def shouldInline(id: Id)(using ctx: InlineContext): Boolean =
    ctx.usage.get(id) match {
      case None => false
      case Some(Usage.Once) => true
      case Some(Usage.Recursive) => false // we don't inline recursive functions for the moment
      case Some(Usage.Many) =>
        ctx.defs.get(id).exists { d =>
          def isSmall = d.size <= ctx.maxInlineSize
          def isHigherOrder = d match {
            case Definition.Def(id, BlockLit(_, _, _, bparams, _)) =>
              bparams.exists(p => p.tpe match {
                case t: BlockType.Function => true
                case t: BlockType.Interface => false
              })
            case _ => false
          }
          isSmall || isHigherOrder
        }
    }

  def shouldKeep(id: Id)(using ctx: InlineContext): Boolean =
    ctx.usage.get(id) match {
      case None => false
      case Some(Usage.Once) => false
      case Some(Usage.Recursive) => true // we don't inline recursive functions for the moment
      case Some(Usage.Many) => true
    }

  def used(id: Id)(using ctx: InlineContext): Boolean =
    ctx.usage.isDefinedAt(id)

  def rewrite(definitions: List[Definition])(using ctx: InlineContext): (List[Definition], InlineContext) =
    given allDefs: InlineContext = ctx ++ definitions.map(d => d.id -> d).toMap

    val filtered = definitions.collect {
      case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
      // we drop aliases
      case Definition.Let(id, tpe, binding) if !binding.isInstanceOf[ValueVar] =>
        Definition.Let(id, tpe, rewrite(binding))
    }
    (filtered, allDefs)

  def blockDefFor(id: Id)(using ctx: InlineContext): Option[Block] =
    ctx.defs.get(id) map {
      // TODO rewriting here leads to a stack overflow in one test, why?
      case Definition.Def(id, block) => block //rewrite(block)
      case Definition.Let(id, _, binding) => INTERNAL_ERROR("Should not happen")
    }

  def dealias(b: Block.BlockVar)(using ctx: InlineContext): BlockVar =
    ctx.defs.get(b.id) match {
      case Some(Definition.Def(id, aliased : Block.BlockVar)) => dealias(aliased)
      case _ => b
    }

  def dealias(b: Pure.ValueVar)(using ctx: InlineContext): ValueVar =
    ctx.defs.get(b.id) match {
      case Some(Definition.Let(id, _, aliased : Pure.ValueVar)) => dealias(aliased)
      case _ => b
    }

  def rewrite(d: Definition)(using InlineContext): Definition = d match {
    case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
    case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, rewrite(binding))
  }

  def rewrite(s: Stmt)(using C: InlineContext): Stmt = s match {
    case Stmt.Scope(definitions, body) =>
      val (defs, ctx) = rewrite(definitions)
      scope(defs, rewrite(body)(using ctx))

    case Stmt.App(b, targs, vargs, bargs) =>
      app(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))

    case Stmt.Reset(body) =>
      rewrite(body) match {
        case BlockLit(tparams, cparams, vparams, List(prompt), body) if !used(prompt.id) => body
        case b => Stmt.Reset(b)
      }

    // congruences
    case Stmt.Return(expr) => Return(rewrite(expr))
    case Stmt.Val(id, tpe, binding, body) => valDef(id, tpe, rewrite(binding), rewrite(body))
    case Stmt.If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      patternMatch(rewrite(scrutinee), clauses.map { case (id, value) => id -> rewrite(value) }, default.map(rewrite))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, rewrite(init), region, rewrite(body))
    case Stmt.Shift(prompt, b @ BlockLit(tparams, cparams, vparams, List(k), body)) if tailResumptive(k.id, body) =>
      C.inlineCount.next()
      rewrite(removeTailResumption(k.id, body))

    case Stmt.Shift(prompt, body) => Shift(prompt, rewrite(body))


    case Stmt.Resume(k, body) => Resume(k, rewrite(body))
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

  def rewrite(b: Block)(using C: InlineContext): Block = b match {
    case Block.BlockVar(id, _, _) if shouldInline(id) =>
      blockDefFor(id) match {
        case Some(value) =>
          C.inlineCount.next()
          Renamer.rename(value)
        case None => b
      }
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
    case Pure.Make(data, tag, vargs) => make(data, tag, vargs.map(rewrite))
    // currently, we don't inline values, but we can dealias them
    case x @ Pure.ValueVar(id, annotatedType) => dealias(x)

    // congruences
    case Pure.Literal(value, annotatedType) => p
    case Pure.Select(target, field, annotatedType) => select(rewrite(target), field, annotatedType)
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

  // A simple syntactic check whether this stmt is tailresumptive in k
  def tailResumptive(k: Id, stmt: Stmt): Boolean =
    def freeInStmt(stmt: Stmt): Boolean = Variables.free(stmt).containsBlock(k)
    def freeInExpr(expr: Expr): Boolean = Variables.free(expr).containsBlock(k)
    def freeInDef(definition: Definition): Boolean = Variables.free(definition).containsBlock(k)

    stmt match {
      case Stmt.Scope(definitions, body) => definitions.forall { d => !freeInDef(d) } && tailResumptive(k, body)
      case Stmt.Return(expr) => false
      case Stmt.Val(id, annotatedTpe, binding, body) => tailResumptive(k, body) && !freeInStmt(binding)
      case Stmt.App(callee, targs, vargs, bargs) => false
      case Stmt.If(cond, thn, els) => !freeInExpr(cond) && tailResumptive(k, thn) && tailResumptive(k, els)
      // Interestingly, we introduce a join point making this more difficult to implement properly
      case Stmt.Match(scrutinee, clauses, default) => !freeInExpr(scrutinee) && clauses.forall {
        case (_, BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body)
      } && default.forall { stmt => tailResumptive(k, stmt) }
      case Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body)
      case Stmt.Region(_) => ???
      case Stmt.Alloc(id, init, region, body) => tailResumptive(k, body) && !freeInExpr(init)
      case Stmt.Var(id, init, capture, body) => tailResumptive(k, body) && !freeInExpr(init)
      case Stmt.Get(id, annotatedCapt, annotatedTpe) => false
      case Stmt.Put(id, annotatedCapt, value) => false
      case Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, body)) => tailResumptive(k, body) // is this correct?
      case Stmt.Shift(prompt, body) => false
      case Stmt.Resume(k2, body) => k2.id == k // what if k is free in body?
      case Stmt.Hole() => true
    }

  def removeTailResumption(k: Id, stmt: Stmt): Stmt = stmt match {
    case Stmt.Scope(definitions, body) => Stmt.Scope(definitions, removeTailResumption(k, body))
    case Stmt.Val(id, annotatedTpe, binding, body) => Stmt.Val(id, annotatedTpe, binding, removeTailResumption(k, body))
    case Stmt.If(cond, thn, els) => Stmt.If(cond, removeTailResumption(k, thn), removeTailResumption(k, els))
    case Stmt.Match(scrutinee, clauses, default) => Stmt.Match(scrutinee, clauses.map {
      case (tag, block) => tag -> removeTailResumption(k, block)
    }, default.map(removeTailResumption(k, _)))
    case Stmt.Region(body : BlockLit) =>
      Stmt.Region(removeTailResumption(k, body))
    case Stmt.Region(_) => ???
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, body)
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, init, capture, body)
    case Stmt.Reset(body) => Stmt.Reset(removeTailResumption(k, body))
    case Stmt.Resume(k2, body) if k2.id == k => body

    case Stmt.Resume(k, body) => stmt
    case Stmt.Shift(prompt, body) => stmt
    case Stmt.Hole() => stmt
    case Stmt.Return(expr) => stmt
    case Stmt.App(callee, targs, vargs, bargs) => stmt
    case Stmt.Get(id, annotatedCapt, annotatedTpe) => stmt
    case Stmt.Put(id, annotatedCapt, value) => stmt
  }

  def removeTailResumption(k: Id, block: BlockLit): BlockLit = block match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, removeTailResumption(k, body))
  }
}
