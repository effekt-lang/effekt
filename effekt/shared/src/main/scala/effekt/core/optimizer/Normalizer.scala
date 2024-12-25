package effekt
package core
package optimizer

import effekt.util.messages.INTERNAL_ERROR

import scala.annotation.tailrec
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
 * removing the overhead of the function call. Under the following conditions,
 * cuts are _not_ removed:
 *
 * - the definition is recursive
 * - inlining would exceed the maxInlineSize
 *
 * If the function is called _exactly once_, it is inlined regardless of the maxInlineSize.
 */
object Normalizer { normal =>

  case class Context(
    blocks: Map[Id, Block],
    exprs: Map[Id, Expr],
    decls: DeclarationContext,     // for field selection
    usage: mutable.Map[Id, Usage], // mutable in order to add new information after renaming
    maxInlineSize: Int             // to control inlining and avoid code bloat
  ) {
    def bind(id: Id, expr: Expr): Context = copy(exprs = exprs + (id -> expr))
    def bind(id: Id, block: Block): Context = copy(blocks = blocks + (id -> block))
  }

  def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  def isRecursive(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Recursive
      // We assume it is recursive, if (for some reason) we do not have information.
      // This is, however, a strange case since this means we call a function we deemed unreachable.
      case None => true // TODO sys error s"No info for ${id}"
    }

  def isOnce(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Once
      case None => false
    }

  def normalize(entrypoints: Set[Id], m: ModuleDecl, maxInlineSize: Int): ModuleDecl = {
    // usage information is used to detect recursive functions (and not inline them)
    val usage = Reachable(entrypoints, m)

    val defs = m.definitions.collect {
      case Definition.Def(id, block) => id -> block
    }.toMap
    val context = Context(defs, Map.empty, DeclarationContext(m.declarations, m.externs), mutable.Map.from(usage), maxInlineSize)

    val (normalizedDefs, _) = normalize(m.definitions)(using context)
    m.copy(definitions = normalizedDefs)
  }

  def normalize(definitions: List[Definition])(using ctx: Context): (List[Definition], Context) =
    var contextSoFar = ctx
    val defs = definitions.map {
      case Definition.Def(id, block) =>
        given Context = contextSoFar
        val normalized = active(block).get
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Def(id, normalized)
      case Definition.Let(id, tpe, expr) =>
        val normalized = active(expr)(using contextSoFar).get
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Let(id, tpe, normalized)
    }
    (defs, contextSoFar)

  // TODO better name
  enum Active[V, S] {
    case Value(v: V)
    case Stuck(s: S)

    def get: V | S = this match {
      case Active.Value(v) => v
      case Active.Stuck(s) => s
    }
  }

  // we need a better name for this... it is doing a bit more than looking up
  def active[R](b: Block)(using C: Context): Active[BlockLit | New, Block] =
    normalize(b) match {
      case x @ Block.BlockVar(id, annotatedTpe, annotatedCapt) if isRecursive(id) =>
        Active.Stuck(x)
      case x @ Block.BlockVar(id, annotatedTpe, annotatedCapt) => blockFor(id) match {
        case Some(value) if isOnce(id) || value.size <= C.maxInlineSize => active(value)
        case Some(value) => Active.Stuck(x)
        case None        => Active.Stuck(x)
      }
      case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        Active.Value(b)
      case Block.Unbox(pure) => active(pure) match {
        case Active.Value(Pure.Box(b, annotatedCapture)) => active(b)
        case other => Active.Stuck(Block.Unbox(pure))
      }
      case b @ Block.New(impl) => Active.Value(b)
    }

  def active(e: Expr)(using Context): Active[Pure.Make | Pure.Literal | Pure.Box, Expr] =
    normalize(e) match {
      case x @ Pure.ValueVar(id, annotatedType) => exprFor(id) match {
        case Some(p: Pure.Make)    => Active.Value(p)
        case Some(p: Pure.Literal) => Active.Value(p)
        case Some(p: Pure.Box)     => Active.Value(p)
        // We only inline non side-effecting expressions
        case Some(other) if other.capt.isEmpty  => Active.Stuck(other)
        case _ => Active.Stuck(x)
      }
      case other => Active.Stuck(other)
    }

  def normalize(d: Definition)(using C: Context): Definition = d match {
    case Definition.Def(id, block)        => Definition.Def(id, normalize(block))
    case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, normalize(binding))
  }

  def normalize(s: Stmt)(using C: Context): Stmt = s match {

    case Stmt.Scope(definitions, body) =>
      val (defs, ctx) = normalize(definitions)
      normal.Scope(defs, normalize(body)(using ctx))

    // Redexes
    // -------
    case Stmt.App(b, targs, vargs, bargs) =>
      active(b) match {
        case Active.Value(lit : Block.BlockLit) =>
          reduce(lit, targs, vargs.map(normalize), bargs.map(normalize))
        case Active.Value(b) => ???
        case Active.Stuck(x) =>
          Stmt.App(x, targs, vargs.map(normalize), bargs.map(normalize))
      }
    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      active(b) match {
        case Active.Value(Block.New(impl)) =>
          normalize(reduce(impl, method, targs, vargs.map(normalize), bargs.map(normalize)))
        case Active.Value(b) => ???
        case Active.Stuck(x) => Stmt.Invoke(x, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Match(scrutinee, clauses, default) => active(scrutinee) match {
      case Active.Value(Pure.Make(data, tag, vargs)) if clauses.exists { case (id, _) => id == tag } =>
        val clause: BlockLit = clauses.collectFirst { case (id, cl) if id == tag => cl }.get
        normalize(reduce(clause, Nil, vargs.map(normalize), Nil))
      case Active.Value(Pure.Make(data, tag, vargs)) if default.isDefined =>
        normalize(default.get)
      case Active.Value(other) => ???
      case Active.Stuck(_) =>
        val normalized = normalize(scrutinee)
        Stmt.Match(normalized, clauses.map { case (id, value) => id -> normalize(value) }, default.map(normalize))
    }

    // [[ if (true) stmt1 else stmt2 ]] = [[ stmt1 ]]
    case Stmt.If(cond, thn, els) => active(cond) match {
      case Active.Value(Pure.Literal(true, annotatedType)) => normalize(thn)
      case Active.Value(Pure.Literal(false, annotatedType)) => normalize(els)
      case _ => If(normalize(cond), normalize(thn), normalize(els))
    }

    // [[ val x = return e; s ]] = let x = [[ e ]]; [[ s ]]
    case Stmt.Val(id, tpe, binding, body) => normalize(binding) match {

      // TODO maintain normal form and avoid aliasing
      //      case Stmt.Return(x : ValueVar) =>
      //        normalize(body)(using C.bind(id, exprFor(x.id).getOrElse(x)))

      case Stmt.Return(expr) =>
        normal.Scope(List(Definition.Let(id, tpe, expr)),
          normalize(body)(using C.bind(id, expr)))

      case Stmt.Scope(definitions, Stmt.Return(expr)) =>
        normal.Scope(definitions :+ Definition.Let(id, tpe, expr),
          normalize(body)(using C.bind(id, expr)))

      case other => Stmt.Val(id, tpe, other, normalize(body))
    }

    // "Congruences"
    // -------------

    case Stmt.Reset(body) => Stmt.Reset(normalize(body))
    case Stmt.Shift(prompt, body) => Shift(prompt, normalize(body))
    case Stmt.Return(expr) => Return(normalize(expr))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, normalize(init), region, normalize(body))
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

    // [[ unbox (box b) ]] = [[ b ]]
    case Block.Unbox(pure) => normal.Unbox(normalize(pure))
    case Block.New(impl) => New(normalize(impl))
  }

  def normalize(s: Implementation)(using Context): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = normalize(op.body))
      })
    }

  def normalize(p: Pure)(using ctx: Context): Pure = p match {
    // [[ Constructor(f = v).f ]] = [[ v ]]
    case Pure.Select(target, field, annotatedType) => active(target) match {
      case Active.Value(Pure.Make(datatype, tag, fields)) =>
        val constructor = ctx.decls.findConstructor(tag).get
        val expr = (constructor.fields zip fields).collectFirst { case (f, expr) if f.id == field => expr }.get
        normalize(expr)
      case _ => Pure.Select(normalize(target), field, annotatedType)
    }

    // [[ box (unbox e) ]] = [[ e ]]
    case Pure.Box(b, annotatedCapture) => normal.Box(active(b).get, annotatedCapture)

    // congruences
    case Pure.PureApp(b, targs, vargs) => Pure.PureApp(normalize(b), targs, vargs.map(normalize))
    case Pure.Make(data, tag, vargs) => Pure.Make(data, tag, vargs.map(normalize))
    case Pure.ValueVar(id, annotatedType) => p
    case Pure.Literal(value, annotatedType) => p
  }

  def normalize(e: Expr)(using Context): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) => DirectApp(normalize(b), targs, vargs.map(normalize), bargs.map(normalize))

    // [[ run (return e) ]] = [[ e ]]
    case Run(s) => normal.Run(normalize(s))

    case pure: Pure => normalize(pure)
  }


  // Smart Constructors
  // ------------------
  // They are purposefully not _that_ smart to be usable by other phases
  // as well.

  // { def f=...; { def g=...; BODY } }  =  { def f=...; def g; BODY }
  def Val(id: Id, tpe: ValueType, binding: Stmt, body: Stmt): Stmt =
    (binding, body) match {

      // [[ val x = STMT; return x ]] == STMT
      case (_, Stmt.Return(Pure.ValueVar(other, _))) if other == id =>
        binding

      //  [[ val x = return EXPR; STMT ]] = [[ let x = EXPR; STMT ]]
      //
      // This opt is too good for JS: it blows the stack on
      // recursive functions that are used to encode while...
      //
      // The solution to this problem is implemented in core.MakeStackSafe:
      //   all recursive functions that could blow the stack are trivially wrapped
      //   again, after optimizing.
      case (Stmt.Return(expr), body) =>
        normal.Scope(List(Definition.Let(id, tpe, expr)), body)

      case _ => Stmt.Val(id, tpe, binding, body)
    }

  // { def f=...; { def g=...; BODY } }  =  { def f=...; def g; BODY }
  @tailrec
  def Scope(definitions: List[Definition], body: Stmt): Stmt = body match {
    case Stmt.Scope(others, body) => normal.Scope(definitions ++ others, body)
    case _ => if (definitions.isEmpty) body else Stmt.Scope(definitions, body)
  }

  // run { return e }  =  e
  def Run(s: Stmt): Expr = s match {
    case Stmt.Return(expr) => expr
    case _ => core.Run(s)
  }

  // box (unbox p)  =  p
  def Box(b: Block, capt: Captures): Pure = b match {
    case Block.Unbox(pure) => pure
    case b => Pure.Box(b, capt)
  }

  // unbox (box b)  =  b
  def Unbox(p: Pure): Block = p match {
    case Pure.Box(b, _) => b
    case p => Block.Unbox(p)
  }


  // Helpers for beta-reduction
  // --------------------------

  // TODO we should rename when inlining to maintain barendregdt, but need to copy usage information...
  def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using C: Context): Stmt = {

    // To update usage information
    val usage = C.usage
    def copyUsage(from: Id, to: Id) = usage.get(from) match {
      case Some(info) => usage.update(to, info)
      case None => ()
    }

    // Only bind if not already a variable!!!
    var ids: Set[Id] = Set.empty
    var bindings: List[Definition.Def] = Nil
    var bvars: List[Block.BlockVar] = Nil

    // (1) first bind
    (b.bparams zip bargs) foreach {
      case (bparam, x: Block.BlockVar) =>

        // Update usage: u1 + (u2 - 1)
        val newUsage = (usage.get(bparam.id), usage.get(x.id)) match {
          case (u1, Some(Usage.Once)) => u1
          case (u1, None) => u1
          case (Some(Usage.Once | Usage.Many) | None, Some(Usage.Many)) => Some(Usage.Many)
          case (Some(Usage.Recursive), u2) => Some(Usage.Recursive)
          case (u1, Some(Usage.Recursive)) => Some(Usage.Recursive)
        }
        newUsage.foreach { u => usage.update(x.id, u) }
        bvars = bvars :+ x
      // introduce a binding
      case (bparam, block) =>
        val id = symbols.TmpBlock("blockBinding")
        bindings = bindings :+ Definition.Def(id, block)
        bvars = bvars :+ Block.BlockVar(id, block.tpe, block.capt)
        copyUsage(bparam.id, id)
        ids += id
    }

    val (renamedLit: BlockLit, renamedIds) = Renamer.rename(b)

    renamedIds.foreach(copyUsage)

    val newUsage = usage.collect { case (id, usage) if util.show(id) contains "foreach" => (id, usage) }

    // (2) substitute
    val body = substitutions.substitute(renamedLit, targs, vargs, bvars)

    normalize(normal.Scope(bindings, body))
  }

  def reduce(impl: Implementation, method: Id, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using Context): Stmt =
    val Operation(name, tps, cps, vps, bps, body) =
      impl.operations.find(op => op.name == method).getOrElse {
        INTERNAL_ERROR("Should not happen")
      }
    reduce(BlockLit(tps, cps, vps, bps, body), targs, vargs, bargs)
}
