package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.core.Block.BlockLit
import effekt.core.Usage.{ Many, Once, Recursive }

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

        // (3) inline unique block definitions
        val inlined = InlineUnique(Set(mainSymbol), simpleOpts)

        Some(CoreTransformed(source, tree, mod, inlined))
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
      case Definition.Let(id, binding) => sys error "Should not happen"
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
    val body = tooling.substitute(b, targs, vargs, bvars)

    //    println(s"IDS: ${ids}")
    //    println("BEFORE")
    //    debug(b.body)
    //    println("AFTER")
    //    debug(Stmt.Scope(bindings, body))

    // Only introduce scope, if necessary
    if bindings.isEmpty then rewrite(body) else {
      val result: Stmt.Scope = Stmt.Scope(bindings, body)

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
      Scope(filtered, rewrite(body)(using ctx))

    case Stmt.App(b, targs, vargs, bargs) =>
      app(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))

    // congruences
    case Stmt.Return(expr) => Return(rewrite(expr))
    case Stmt.Val(id, binding, body) => Val(id, rewrite(binding), rewrite(body))
    case Stmt.If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      Match(rewrite(scrutinee), clauses.map { case (id, value) => id -> rewrite(value) }, default.map(rewrite))
    case Stmt.State(id, init, region, body) => State(id, rewrite(init), region, rewrite(body))
    case Stmt.Try(body, handlers) => Try(rewrite(body), handlers.map(rewrite))
    case Stmt.Region(body) => Region(rewrite(body))
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


  // smart constructors to establish a normal form

  def app(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])(using InlineContext): Stmt =
    callee match {
      case b : Block.BlockLit => reduce(b, targs, vargs, bargs)
      case other => Stmt.App(callee, targs, vargs, bargs)
    }

  def pureApp(callee: Block, targs: List[ValueType], vargs: List[Pure]): Pure =
    callee match {
      //case b : Block.BlockLit => reduce(b, targs, vargs)
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
          sys error "Should not happen"
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
    case Stmt.State(id, init, region, body) =>
      process(init)
      process(region)
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

// TODO CONTINUE: REWRITE
object tooling {

  //Starting point for inlining, creates Maps(params -> args) and passes to normal substitute
  def substitute(block: BlockLit, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Stmt =
    block match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val tSubst = (tparams zip targs).toMap
        val cSubst = (cparams zip bargs.map(_.capt)).toMap
        val vSubst = (vparams.map(_.id) zip vargs).toMap
        val bSubst = (bparams.map(_.id) zip bargs).toMap

        substitute(body)(using tSubst, cSubst, vSubst, bSubst)
    }

  //Replaces all variables contained in one of the Maps with their value
  def substitute(definition: Definition)(using Map[Id, ValueType], Map[Id, Captures], Map[Id, Pure], Map[Id, Block]): Definition =
    definition match {
      case Definition.Def(id, block) => Definition.Def(id, substitute(block))
      case Definition.Let(id, binding) => Definition.Let(id, substitute(binding))
    }

  def substitute(expression: Expr)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                   vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Expr =
    expression match {
      case DirectApp(b, targs, vargs, bargs) =>
        DirectApp(substitute(b), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute), bargs.map(substitute))

      case Run(s) =>
        Run(substitute(s))

      case p: Pure =>
        substitute(p)
    }

  def substitute(statement: Stmt)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                  vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Stmt =
    statement match {
      case Scope(definitions, body) =>
        Scope(definitions.map(substitute), substitute(body)(using tSubst, cSubst, vSubst, bSubst -- definitions.map { d => d.id }))

      case Return(expr) =>
        Return(substitute(expr))

      case Val(id, binding, body) =>
        Val(id, substitute(binding), substitute(body)(using tSubst, cSubst, vSubst - id, bSubst))

      case App(callee, targs, vargs, bargs) =>
        App(substitute(callee), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute), bargs.map(substitute))

      case If(cond, thn, els) =>
        If(substitute(cond), substitute(thn), substitute(els))

      case Match(scrutinee, clauses, default) =>
        Match(substitute(scrutinee), clauses.map {case (id, b) => (id, substitute(b).asInstanceOf[BlockLit]) }, default.map(substitute))

      // TODO WHAT IS THIS AGAIN?
      case State(id, init, region, body) =>
        if (bSubst.contains(region))
          bSubst(region) match
            //case _: BlockLit => Context.panic("Should not happen since block lits never have type Region")
            case BlockVar(x, _, _) => State(id, substitute(init), x, substitute(body)(using tSubst, cSubst, vSubst, bSubst - id))
            case b => ???
            //              val name = symbols.TmpBlock()
            //              Scope(List(Definition.Def(name, b)),
            //                State(id,
            //                  substitute(init)(using tSubst, cSubst, vSubst, bSubst - name),
            //                  name,
            //                  substitute(body)(using tSubst, cSubst, vSubst, bSubst -- Set(name, id))))

        else
          State(id, substitute(init), region, substitute(body)(using tSubst, cSubst, vSubst, bSubst - id))


      case Try(body, handlers) =>
        Try(substitute(body), handlers.map(substitute))

      case Region(body) =>
        Region(substitute(body))

      case h : Hole => h
    }

  def substitute(block: Block)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                               vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Block =
    block match {
      case BlockVar(id, tpe, capt) if bSubst.isDefinedAt(id) => bSubst(id)
      case BlockVar(id, tpe, capt) => BlockVar(id, Type.substitute(tpe, tSubst, cSubst), Type.substitute(capt, cSubst))

      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams,
          substitute(body)(using tSubst -- tparams, cSubst -- cparams, vSubst -- vparams.map(_.id), bSubst -- bparams.map(_.id)))

      case Member(block, field, annotatedTpe) =>
        Member(substitute(block), field, Type.substitute(annotatedTpe, tSubst, cSubst))

      case Unbox(pure) =>
        Unbox(substitute(pure))

      case New(impl) =>
        New(substitute(impl))
    }

  def substitute(pure: Pure)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                             vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Pure =
    pure match {
      case ValueVar(id, _) if vSubst.isDefinedAt(id) => vSubst(id)
      case ValueVar(id, annotatedType) => ValueVar(id, Type.substitute(annotatedType, tSubst, cSubst))

      case Literal(value, annotatedType) =>
        Literal(value, Type.substitute(annotatedType, tSubst, cSubst))

      case PureApp(b, targs, vargs) =>
        PureApp(substitute(b), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute))

      case Select(target, field, annotatedType) =>
        Select(substitute(target), field, Type.substitute(annotatedType, tSubst, cSubst))

      case Box(b, annotatedCapture) =>
        Box(substitute(b), Type.substitute(annotatedCapture, cSubst))
    }

  def substitute(impl: Implementation)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures], vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Implementation =
    impl match {
      case Implementation(interface, operations) =>
        Implementation(Type.substitute(interface, tSubst, cSubst).asInstanceOf, operations.map(substitute))
    }

  def substitute(op: Operation)(using Map[Id, ValueType], Map[Id, Captures], Map[Id, Pure], Map[Id, Block]): Operation =
    op match {
      case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
        Operation(name, tparams, cparams, vparams, bparams, resume, substitute(body))
    }
}
