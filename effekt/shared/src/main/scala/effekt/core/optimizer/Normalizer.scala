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

  private def blockFor(id: Id)(using ctx: Context): Option[Block] =
    ctx.blocks.get(id)

  private def exprFor(id: Id)(using ctx: Context): Option[Expr] =
    ctx.exprs.get(id)

  private def isRecursive(id: Id)(using ctx: Context): Boolean =
    ctx.usage.get(id) match {
      case Some(value) => value == Usage.Recursive
      // We assume it is recursive, if (for some reason) we do not have information;
      // since reducing might diverge, otherwise.
      //
      // This is, however, a strange case since this means we call a function we deemed unreachable.
      // It _can_ happen, for instance, by updating the usage (subtracting) and not deadcode eliminating.
      // This is the case for examples/pos/bidirectional/scheduler.effekt
      case None => true // sys error s"No info for ${id}"
    }

  private def isOnce(id: Id)(using ctx: Context): Boolean =
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
        val normalized = active(block)
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Def(id, normalized)
      case Definition.Let(id, tpe, expr) =>
        val normalized = active(expr)(using contextSoFar)
        contextSoFar = contextSoFar.bind(id, normalized)
        Definition.Let(id, tpe, normalized)
    }
    (defs, contextSoFar)

  // we need a better name for this... it is doing a bit more than looking up
  def active[R](b: Block)(using C: Context): Block =
    normalize(b) match {
      case x @ Block.BlockVar(id, annotatedTpe, annotatedCapt) => blockFor(id) match {
        case Some(value) if isRecursive(id) => x  // stuck
        case Some(value) if isOnce(id) || value.size <= C.maxInlineSize => active(value)
        case Some(value) => x // stuck
        case None        => x // stuck
      }
      case b: Block.BlockLit => b
      case Block.Unbox(pure) => active(pure) match {
        case Pure.Box(b, annotatedCapture) => active(b)
        case other => Block.Unbox(pure) // stuck
      }
      case b @ Block.New(impl) => b
    }

  def active(e: Expr)(using Context): Expr =
    normalize(e) match {
      case x @ Pure.ValueVar(id, annotatedType) => exprFor(id) match {
        case Some(p: Pure.Make)    => p
        case Some(p: Pure.Literal) => p
        case Some(p: Pure.Box)     => p
        // We only inline non side-effecting expressions
        case Some(other) if other.capt.isEmpty  => other
        case _ => x // stuck
      }
      case other => other // stuck
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
        case lit : Block.BlockLit =>
          reduce(lit, targs, vargs.map(normalize), bargs.map(normalize))
        case x =>
          Stmt.App(x, targs, vargs.map(normalize), bargs.map(normalize))
      }
    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      active(b) match {
        case Block.New(impl) =>
          normalize(reduce(impl, method, targs, vargs.map(normalize), bargs.map(normalize)))
        case x => Stmt.Invoke(x, method, methodTpe, targs, vargs.map(normalize), bargs.map(normalize))
      }

    case Stmt.Match(scrutinee, clauses, default) => active(scrutinee) match {
      case Pure.Make(data, tag, vargs) if clauses.exists { case (id, _) => id == tag } =>
        val clause: BlockLit = clauses.collectFirst { case (id, cl) if id == tag => cl }.get
        normalize(reduce(clause, Nil, vargs.map(normalize), Nil))
      case Pure.Make(data, tag, vargs) if default.isDefined =>
        normalize(default.get)
      case _ =>
        val normalized = normalize(scrutinee)
        Stmt.Match(normalized, clauses.map { case (id, value) => id -> normalize(value) }, default.map(normalize))
    }

    // [[ if (true) stmt1 else stmt2 ]] = [[ stmt1 ]]
    case Stmt.If(cond, thn, els) => active(cond) match {
      case Pure.Literal(true, annotatedType) => normalize(thn)
      case Pure.Literal(false, annotatedType) => normalize(els)
      case _ => If(normalize(cond), normalize(thn), normalize(els))
    }

    case Stmt.Val(id, tpe, binding, body) =>

      def normalizeVal(id: Id, tpe: ValueType, binding: Stmt, body: Stmt): Stmt = binding match {

        // [[ val x = return e; s ]] = let x = [[ e ]]; [[ s ]]
        case Stmt.Return(expr) =>
          normal.Scope(List(Definition.Let(id, tpe, expr)),
            normalize(body)(using C.bind(id, expr)))

        // Commute val and bindings
        // TODO does this leak??? For definitions it should be ok.
        // [[ val x = { def f = ...; let y = ...; STMT }; STMT ]] = def f = ...; let y = ...; val x = STMT; STMT
        case Stmt.Scope(ds, bodyBinding) =>
          normal.Scope(ds, normalizeVal(id, tpe, bodyBinding, body))


        // Flatten vals. This should be non-leaking since we use garbage free refcounting.
        // [[ val x = { val y = stmt1; stmt2 }; stmt3 ]] = [[ val y = stmt1; val x = stmt2; stmt3 ]]
        case Stmt.Val(id2, tpe2, binding2, body2) =>
          normalizeVal(id2, tpe2, binding2, Stmt.Val(id, tpe, body2, body))

        // [[ val x = stmt; return x ]]   =   [[ stmt ]]
        case other => normalize(body) match {
          case Stmt.Return(x: ValueVar) if x.id == id => other
          case normalizedBody => Stmt.Val(id, tpe, other, normalizedBody)
        }
      }
      normalizeVal(id, tpe, normalize(binding), body)

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
      case Pure.Make(datatype, tag, fields) =>
        val constructor = ctx.decls.findConstructor(tag).get
        val expr = (constructor.fields zip fields).collectFirst { case (f, expr) if f.id == field => expr }.get
        normalize(expr)
      case _ => Pure.Select(normalize(target), field, annotatedType)
    }

    // [[ box (unbox e) ]] = [[ e ]]
    case Pure.Box(b, annotatedCapture) => normal.Box(active(b), annotatedCapture)

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

    @tailrec
  def Scope(definitions: List[Definition], body: Stmt): Stmt = body match {

    // flatten scopes
    //   { def f = ...; { def g = ...; BODY } }  =  { def f = ...; def g; BODY }
    case Stmt.Scope(others, body) => normal.Scope(definitions ++ others, body)

    // commute bindings
    //   let x = run { let y = e; s }  =   let y = e; let x = s
    case _ => if (definitions.isEmpty) body else {
      var defsSoFar: List[Definition] = Nil

      definitions.foreach {
        case Definition.Let(id, tpe, Run(Stmt.Scope(ds, body))) =>
          defsSoFar = defsSoFar ++ (ds :+ Definition.Let(id, tpe, normal.Run(body)))
        case d => defsSoFar = defsSoFar :+ d
      }
      Stmt.Scope(defsSoFar, body)
    }
  }

  def Run(s: Stmt): Expr = s match {

    // run { let x = e; return x }  =  e
    case Stmt.Scope(Definition.Let(id1, _, binding) :: Nil, Stmt.Return(Pure.ValueVar(id2, _))) if id1 == id2 =>
      binding

    // run { return e }  =  e
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
  private def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using C: Context): Stmt = {
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
        usage.update(x.id, usage.getOrElse(bparam.id, Usage.Never) + usage.getOrElse(x.id, Usage.Never).decrement)
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

  private def reduce(impl: Implementation, method: Id, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block])(using Context): Stmt =
    val Operation(name, tps, cps, vps, bps, body) =
      impl.operations.find(op => op.name == method).getOrElse {
        INTERNAL_ERROR("Should not happen")
      }
    reduce(BlockLit(tps, cps, vps, bps, body), targs, vargs, bargs)
}
