package effekt
package core

import effekt.core.Block.BlockLit
import effekt.core.Pure.ValueVar
import effekt.util.messages.INTERNAL_ERROR

import scala.collection.mutable

/**
 * Removes "cuts", that is it performs a step of computation if enough information
 * is available.
 *
 *    def foo(n: Int) = return n + 1
 *
 *    foo(42)
 *
 * becomes
 *
 *    def foo(n: Int) = return n + 1
 *    return 42 + 1
 *
 * removing the overhead of the function call.
 */
object Normalizer {

  case class Context(
    blocks: Map[Id, Block],
    exprs: Map[Id, Expr],
    decls: DeclarationContext, // for field selection
    stack: List[Id], // to detect recursion
    recursive: mutable.Set[Id] // functions that have been identified to be recursive!
  ) {
    def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
    def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
    def enter(id: Id): Context = copy(stack = id :: stack)
  }

  def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  def markRecursive(id: Id)(using ctx: Context): Unit = ctx.recursive.update(id, true)
  def isRecursive(id: Id)(using ctx: Context): Boolean = ctx.recursive.contains(id) || ctx.stack.contains(id)

  def normalize(entrypoints: Set[Id], m: ModuleDecl): ModuleDecl = {
    val defs = m.definitions.collect {
      case Definition.Def(id, block) => id -> block
    }.toMap
    val context = Context(defs, Map.empty, DeclarationContext(m.declarations, m.externs), Nil, mutable.Set.empty)

    val (normalizedDefs, _) = normalize(m.definitions)(using context)
    m.copy(definitions = normalizedDefs)
  }

  def normalize(definitions: List[Definition])(using ctx: Context): (List[Definition], Context) =
    var contextSoFar = ctx
    val defs = definitions.map {
      case Definition.Def(id, block) =>
        given Context = contextSoFar.enter(id)
        val normalized = active(block) {
          case Left(_) => normalize(block)
          case Right(b) => b
        }
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Def(id, normalized)
      case Definition.Let(id, tpe, expr) =>
        val normalized = active(expr)(using contextSoFar)
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Let(id, tpe, normalized)
    }
    (defs, contextSoFar)

  /**
   * The function is recursive, revert inlining!
   */
  case class IsRecursive(b: BlockVar) extends Throwable

  // we need a better name for this... it is doing a bit more than looking up
  // this is written in CPS to detect cycles
  def active[R](b: Block)(using C: Context): (Context ?=> Either[Block, BlockLit | New] => R) => R = k =>
    def value(b: BlockLit | New) = k(Right(b))
    def stuck(x: Block) = k(Left(x))

    normalize(b) match {
      case x @ Block.BlockVar(id, annotatedTpe, annotatedCapt) if isRecursive(id) =>
        markRecursive(id)
        throw IsRecursive(x)
      case b @ Block.BlockVar(id, annotatedTpe, annotatedCapt) => blockFor(id) match {
        case Some(value) => active(value)(using C.enter(id))(k)
        case None => stuck(b)
      }
      case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        value(b)
      case Block.Unbox(pure) => active(pure) match {
        case Pure.Box(b, annotatedCapture) => active(b)(k)
        // we are stuck
        case p: Pure => stuck(Block.Unbox(p))
        case p: Expr => stuck(Block.Unbox(pure))
      }
      case b @ Block.New(impl) => value(b)
    }

  def active(e: Expr)(using Context): Expr = normalize(e) match {
    case e @ Pure.ValueVar(id, annotatedType) => exprFor(id) match {
      // We cannot inline side-effecting expressions
      case Some(value) if value.capt.isEmpty => active(value)
      case _ => e
    }
    case e => e
  }

  def normalize(d: Definition)(using C: Context): Definition = d match {
    case Definition.Def(id, block) => Definition.Def(id, normalize(block)(using C.enter(id)))
    case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, normalize(binding))
  }

  def normalize(s: Stmt)(using C: Context): Stmt = s match {

    case Stmt.Scope(definitions, body) =>
      val (defs, ctx) = normalize(definitions)
      scope(defs, normalize(body)(using ctx))


    // Redexes
    // -------

    case Stmt.App(b, targs, vargs, bargs) =>
      try active(b) {
        case Right(lit : Block.BlockLit) =>
          normalize(reduce(lit, targs, vargs.map(normalize), bargs.map(normalize)))
        case Right(b) => ???
        case Left(x) => Stmt.App(x, targs, vargs.map(normalize), bargs.map(normalize))
      } catch {
        case IsRecursive(x) => Stmt.App(x, targs, vargs.map(normalize), bargs.map(normalize))
      }
    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      active(b) {
        case Right(Block.New(impl)) =>
          normalize(reduce(impl, method, targs, vargs.map(normalize), bargs.map(normalize)))
        case Right(b) => ???
        case Left(x) => Stmt.Invoke(x, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Match(scrutinee, clauses, default) => active(scrutinee) match {
      case Pure.Make(data, tag, vargs) if clauses.exists { case (id, _) => id == tag } =>
        val clause: BlockLit = clauses.collectFirst { case (id, cl) if id == tag => cl }.get
        normalize(reduce(clause, Nil, vargs.map(normalize), Nil))
      case Pure.Make(data, tag, vargs) if default.isDefined =>
        normalize(default.get)
      case _ =>
        Stmt.Match(normalize(scrutinee), clauses.map { case (id, value) => id -> normalize(value) }, default.map(normalize))
    }

    // "Congruences"
    // -------------

    case Stmt.Reset(body) => Stmt.Reset(normalize(body))
    case Stmt.Return(expr) => Return(normalize(expr))
    case Stmt.Val(id, tpe, binding, body) => normalize(binding) match {
      case Stmt.Return(expr) =>
        scope(List(Definition.Let(id, tpe, expr)),
          normalize(body)(using C.bind(id, expr)))

      case Stmt.Scope(definitions, Stmt.Return(expr)) =>
        scope(definitions :+ Definition.Let(id, tpe, expr),
          normalize(body)(using C.bind(id, expr)))

      case other => Stmt.Val(id, tpe, other, normalize(body))
    }
    case Stmt.If(cond, thn, els) => If(normalize(cond), normalize(thn), normalize(els))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, normalize(init), region, normalize(body))
    case Stmt.Shift(prompt, body) => Shift(prompt, normalize(body))

    case Stmt.Resume(k, body) => Resume(k, normalize(body))
    case Stmt.Region(body) => Region(normalize(body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, normalize(init), capture, normalize(body))
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, tpe)
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, normalize(value))
    case Stmt.Hole() => s
  }
  def normalize(b: BlockLit)(using Context): BlockLit =
    b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams, normalize(body))
    }

  def normalize(b: Block)(using Context): Block = b match {
    case b @ Block.BlockVar(id, _, _) => b
    case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) => normalize(b)
    case Block.Unbox(pure) => normalize(pure) match {
      case Pure.Box(b, capt) => b
      case other => Block.Unbox(other)
    }
    case Block.New(impl) => New(normalize(impl))
  }

  def normalize(s: Implementation)(using Context): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = normalize(op.body))
      })
    }

  def normalize(p: Pure)(using ctx: Context): Pure = p match {
    case Pure.PureApp(b, targs, vargs) => Pure.PureApp(normalize(b), targs, vargs.map(normalize))
    case Pure.Make(data, tag, vargs) => Pure.Make(data, tag, vargs.map(normalize))
    case x @ Pure.ValueVar(id, annotatedType) => x

    // congruences
    case Pure.Literal(value, annotatedType) => p
    case Pure.Select(target, field, annotatedType) => active(target) match {
      case Pure.Make(datatype, tag, fields) =>
        val constructor = ctx.decls.findConstructor(tag).get
        val expr = (constructor.fields zip fields).collectFirst { case (f, expr) if f == field => expr }.get
        normalize(expr)
      case other => Pure.Select(normalize(target), field, annotatedType)
    }
    case Pure.Box(b, annotatedCapture) => active(b) {
      case Left(Block.Unbox(e)) => normalize(e)
      case _ => Pure.Box(normalize(b), annotatedCapture)
    }
  }

  def normalize(e: Expr)(using Context): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) => DirectApp(normalize(b), targs, vargs.map(normalize), bargs.map(normalize))
    case Run(s) => run(normalize(s))
    case pure: Pure => normalize(pure)
  }


  // Smart Constructors
  // ------------------

  // { def f=...; { def g=...; BODY } }  =  { def f=...; def g; BODY }
  // TODO maybe drop bindings that are not free in body...
  def scope(definitions: List[Definition], body: Stmt): Stmt = body match {
    case Stmt.Scope(others, body) => scope(definitions ++ others, body)
    case _ => if (definitions.isEmpty) body else Stmt.Scope(definitions, body)
  }

  def run(s: Stmt): Expr = s match {
    case Stmt.Return(expr) => expr
    case _ => Run(s)
  }

  // Helpers for beta-reduction
  // --------------------------

  // TODO we should rename when inlining to maintain barendregdt, but need to copy usage information...
  def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using C: Context): Stmt = {

    // Only bind if not already a variable!!!
    var ids: Set[Id] = Set.empty
    var bindings: List[Definition.Def] = Nil
    var bvars: List[Block.BlockVar] = Nil

    // (1) first bind
    bargs foreach {
      case x: Block.BlockVar => bvars = bvars :+ x
      // introduce a binding
      case block =>
        val id = symbols.TmpBlock("blockBinding")
        bindings = bindings :+ Definition.Def(id, block)
        bvars = bvars :+ Block.BlockVar(id, block.tpe, block.capt)
        ids += id
    }

    val (renamedLit: BlockLit, renamedIds) = Renamer.rename(b)

    // (2) substitute
    val body = substitutions.substitute(renamedLit, targs, vargs, bvars)

    scope(bindings, body)
  }

  def reduce(impl: Implementation, method: Id, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using Context): Stmt =
    val Operation(name, tps, cps, vps, bps, body) =
      impl.operations.find(op => op.name == method).getOrElse {
        INTERNAL_ERROR("Should not happen")
      }
    reduce(BlockLit(tps, cps, vps, bps, body), targs, vargs, bargs)
}
