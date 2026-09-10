package effekt
package cps

import core.Id
import effekt.cps.Cont.{ ContLam, ContVar }
import substitutions.*

object Contify {

  def rewrite(module: ModuleDecl): ModuleDecl = module match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs.map(rewrite), definitions.map(rewrite), exports)
  }

  def rewrite(definition: ToplevelDefinition): ToplevelDefinition = definition match {
    case ToplevelDefinition.Def(id, block) => ToplevelDefinition.Def(id, rewrite(block))
    case ToplevelDefinition.Val(id, ks, k, binding) => ToplevelDefinition.Val(id, ks, k, rewrite(binding))
    case ToplevelDefinition.Let(id, binding) => ToplevelDefinition.Let(id, rewrite(binding))
  }

  def rewrite(extern: Extern): Extern = extern match {
    case Extern.Def(id, vparams, bparams, async, body) =>
      Extern.Def(id, vparams, bparams, async, rewrite(body))
    case Extern.Data(id, tps, body) =>
      Extern.Data(id, tps, body)
    case Extern.Interface(id, tps, body) =>
      Extern.Interface(id, tps, body)
    case include: Extern.Include => include
  }

  def rewrite(body: ExternBody[Expr]): ExternBody[Expr] = body match {
    case ExternBody.StringExternBody(ff, template) =>
      ExternBody.StringExternBody(ff, Template(template.strings, template.args.map(rewrite)))
    case unsupported: ExternBody.Unsupported => unsupported
  }

  def rewrite(pure: Expr): Expr = pure match {
    case Expr.ValueVar(id) => Expr.ValueVar(id)
    case Expr.Literal(value, tpe) => Expr.Literal(value, tpe)
    case Expr.PureApp(id, vargs) => Expr.PureApp(id, vargs.map(rewrite))
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite))
    case Expr.Box(b) => Expr.Box(rewrite(b))
  }

  def rewrite(block: Block): Block = block match {
    case Block.BlockVar(id) => Block.BlockVar(id)
    case Block.BlockLit(vparams, bparams, ks, k, body) =>
      Block.BlockLit(vparams, bparams, ks, k, rewrite(body))
    case Block.Unbox(pure) => Block.Unbox(rewrite(pure))
    case Block.New(impl) => Block.New(rewrite(impl))
  }

  def rewrite(stmt: Stmt): Stmt = stmt match {

    case Stmt.LetDef(id, b @ BlockLit(vparams, Nil, ks, k, body), rest) =>
      val rewrittenBody = rewrite(body)
      val rewrittenRest = rewrite(rest)
      val inBody = occurrences(id, rewrittenBody)
      val inRest = occurrences(id, rewrittenRest)

      def returnsUnique = inRest.returnsTo.size == 1
      def isRecursive = inBody.returnsTo.nonEmpty
      // a block that is also used as a value cannot become a continuation
      def escapes = inRest.usedAsValue || inBody.usedAsValue

      // this is problematic:
      //
      //   function l(k1) {
      //     ... k1 ...
      //   }
      //   function loop(k2) {
      //     ... l(k2) ...
      //   }
      //
      // we cannot simply specialize l to k2 since it is not lexically in scope!
      //
      // This is not a problem in LLVM since there all blocks are toplevel
      //
      // We approximate whether k is in scope by checking whether it is free in `rest`
      //
      // In the future we could perform lambda-dropping to discover more cases
      def inScope(k: Id) = Variables.free(rewrittenRest) contains k

      inRest.returnsTo.headOption match {
        case Some(Cont.ContVar(k2)) if returnsUnique && !isRecursive && inScope(k2) && !escapes =>
          given Substitution = Substitution(conts = Map(k -> ContVar(k2)))
          Stmt.LetCont(id, ContLam(vparams, ks, substitute(rewrittenBody)), contify(id, rewrittenRest))

        // leads to `k_6 is not defined` when disabling optimizations on issue861.effekt
        //        case Some(cont: ContLam) if returnsUnique && !isRecursive =>
        //          val k2 = Id("k")
        //          given Substitution = Substitution(conts = Map(k -> ContVar(k2)))
        //          LetCont(k2, cont,
        //            Stmt.LetCont(id, ContLam(vparams, ks, substitute(rewrittenBody)),
        //              contify(id, rewrittenRest)))
        case _ =>
          Stmt.LetDef(id, BlockLit(vparams, Nil, ks, k, rewrittenBody), rewrittenRest)
      }

    // Congruences

    case Stmt.LetDef(id, binding, body) =>
      Stmt.LetDef(id, rewrite(binding), rewrite(body))

    case Stmt.Jump(k, vargs, ks) =>
      Stmt.Jump(k, vargs.map(rewrite), ks)

    case Stmt.App(callee, vargs, bargs, ks, k) =>
      Stmt.App(rewrite(callee), vargs.map(rewrite), bargs.map(rewrite), ks, rewrite(k))

    case Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      Stmt.Invoke(rewrite(callee), method, vargs.map(rewrite), bargs.map(rewrite), ks, rewrite(k))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond), rewrite(thn), rewrite(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        rewrite(scrutinee),
        clauses.map { case (id, cl) => (id, rewrite(cl)) },
        default.map(rewrite))

    case Stmt.LetExpr(id, binding, body) =>
      Stmt.LetExpr(id, rewrite(binding), rewrite(body))

    case Stmt.LetCont(id, binding, body) =>
      Stmt.LetCont(id, rewrite(binding), rewrite(body))

    case Stmt.ImpureApp(id, callee, vargs, bargs, body) =>
      Stmt.ImpureApp(id, callee, vargs.map(rewrite), bargs.map(rewrite), rewrite(body))

    case Stmt.Region(id, ks, body) =>
      Stmt.Region(id, ks, rewrite(body))

    case Stmt.Alloc(id, init, region, body) =>
      Stmt.Alloc(id, rewrite(init), region, rewrite(body))

    case Stmt.Var(id, init, ks, body) =>
      Stmt.Var(id, rewrite(init), ks, rewrite(body))

    case Stmt.Dealloc(ref, body) =>
      Stmt.Dealloc(ref, rewrite(body))

    case Stmt.Get(ref, id, body) =>
      Stmt.Get(ref, id, rewrite(body))

    case Stmt.Put(ref, value, body) =>
      Stmt.Put(ref, rewrite(value), rewrite(body))

    case Stmt.Reset(prog, ks, k) =>
      Stmt.Reset(rewrite(prog), ks, rewrite(k))

    case Stmt.Shift(prompt, body, ks, k) =>
      Stmt.Shift(prompt, rewrite(body), ks, rewrite(k))

    case Stmt.Resume(r, body, ks, k) =>
      Stmt.Resume(r, rewrite(body), ks, rewrite(k))

    case Stmt.Hole(span) => Stmt.Hole(span)
  }

  def rewrite(clause: Clause): Clause = clause match {
    case Clause(vparams, body) => Clause(vparams, rewrite(body))
  }

  def rewrite(k: Cont): Cont = k match {
    case Cont.ContVar(id) => Cont.ContVar(id)
    case Cont.ContLam(results, ks, body) => Cont.ContLam(results, ks, rewrite(body))
    case Cont.Abort => Cont.Abort
  }

  def rewrite(impl: Implementation): Implementation = impl match {
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(rewrite))
  }

  def rewrite(op: Operation): Operation = op match {
    case Operation(name, vparams, bparams, ks, k, body) =>
      Operation(name, vparams, bparams, ks, k, rewrite(body))
  }
  def rewrite(b: BlockLit): BlockLit = b match {
    case BlockLit(vparams, bparams, ks, k, body) =>
      BlockLit(vparams, bparams, ks, k, rewrite(body))
  }

  def rewrite(c: Cont.ContLam): Cont.ContLam = c match {
    case Cont.ContLam(results, ks, body) =>
      Cont.ContLam(results, ks, rewrite(body))
  }



  /**
   * How a block is used inside a statement.
   *
   * Contification needs two facts about the block:
   * 1. [[returnsTo]]: the continuations that calls to the block return to
   * 2. [[usedAsValue]]: whether it occurs anywhere that is not the callee of a call:
   *    passed as a block argument, boxed, or the receiver of an [[Stmt.Invoke]].
   */
  case class Occurrences(returnsTo: Set[Cont], usedAsValue: Boolean) {
    def ++(other: Occurrences): Occurrences =
      Occurrences(returnsTo ++ other.returnsTo, usedAsValue || other.usedAsValue)
  }
  object Occurrences {
    val none: Occurrences = Occurrences(Set.empty, false)
    val used: Occurrences = Occurrences(Set.empty, true)
    def call(k: Cont): Occurrences = Occurrences(Set(k), false)
  }

  def all[T](t: IterableOnce[T], f: T => Occurrences): Occurrences =
    t.iterator.foldLeft(Occurrences.none) { case (xs, t) => f(t) ++ xs }

  def occurrences(id: Id, s: Stmt): Occurrences = s match {
    case Stmt.App(callee, vargs, bargs, ks, k) =>
      // the callee position is a call
      // anything else, including the callee when it is not us, is an ordinary occurrence
      val self = callee match {
        case Block.BlockVar(id2) if id == id2 => Occurrences.call(k)
        case other => occurrences(id, other)
      }
      self ++ all(vargs, occurrences(id, _)) ++ all(bargs, occurrences(id, _)) ++ occurrences(id, k)
    case Stmt.Invoke(callee, _, vargs, bargs, ks, k) =>
      // the receiver of an invoke is a value use: you cannot jump to it
      occurrences(id, callee) ++ all(vargs, occurrences(id, _)) ++ all(bargs, occurrences(id, _)) ++ occurrences(id, k)
    case Stmt.If(cond, thn, els) =>
      occurrences(id, cond) ++ occurrences(id, thn) ++ occurrences(id, els)
    case Stmt.Match(scrutinee, clauses, default) =>
      occurrences(id, scrutinee) ++ all(clauses, { case (_, cl) => occurrences(id, cl.body) }) ++ all(default, occurrences(id, _))
    case Stmt.LetDef(_, binding, body) => occurrences(id, binding) ++ occurrences(id, body)
    case Stmt.LetExpr(_, binding, body) => occurrences(id, binding) ++ occurrences(id, body)
    case Stmt.LetCont(_, binding, body) => occurrences(id, binding) ++ occurrences(id, body)
    case Stmt.ImpureApp(_, callee, vargs, bargs, body) =>
      all(vargs, occurrences(id, _)) ++ all(bargs, occurrences(id, _)) ++ occurrences(id, body)
    case Stmt.Region(_, _, body) => occurrences(id, body)
    case Stmt.Alloc(_, init, _, body) => occurrences(id, init) ++ occurrences(id, body)
    case Stmt.Var(_, init, _, body) => occurrences(id, init) ++ occurrences(id, body)
    case Stmt.Dealloc(_, body) => occurrences(id, body)
    case Stmt.Get(_, _, body) => occurrences(id, body)
    case Stmt.Put(_, value, body) => occurrences(id, value) ++ occurrences(id, body)
    case Stmt.Reset(prog, _, k) => occurrences(id, prog) ++ occurrences(id, k)
    case Stmt.Shift(_, body, _, k) => occurrences(id, body) ++ occurrences(id, k)
    case Stmt.Resume(_, body, _, k) => occurrences(id, body) ++ occurrences(id, k)
    case Stmt.Jump(_, vargs, _) => all(vargs, occurrences(id, _))
    case Stmt.Hole(_) => Occurrences.none
  }

  def occurrences(id: Id, b: Block): Occurrences = b match {
    case Block.BlockVar(id2) => if id == id2 then Occurrences.used else Occurrences.none
    case b: Block.BlockLit => occurrences(id, b.body)
    case Block.Unbox(p) => occurrences(id, p)
    case Block.New(impl) => all(impl.operations, op => occurrences(id, op.body))
  }

  def occurrences(id: Id, e: Expr): Occurrences = e match {
    case Expr.ValueVar(_) => Occurrences.none
    case Expr.Literal(_, _) => Occurrences.none
    case Expr.PureApp(_, vargs) => all(vargs, occurrences(id, _))
    case Expr.Make(_, _, vargs) => all(vargs, occurrences(id, _))
    case Expr.Box(b) => occurrences(id, b)
  }

  def occurrences(id: Id, k: Cont): Occurrences = k match {
    case Cont.ContVar(_) => Occurrences.none
    case Cont.ContLam(results, ks, body) => occurrences(id, body)
    case Cont.Abort => Occurrences.none
  }

  def contify(id: Id, s: Stmt): Stmt = s match {
    case Stmt.App(Block.BlockVar(callee), vargs, Nil, ks, k) if callee == id =>
      Stmt.Jump(id, vargs, ks)

    // Congruences

    case Stmt.App(callee, vargs, bargs, ks, k) =>
      Stmt.App(contify(id, callee), vargs.map(contify(id, _)), bargs.map(contify(id, _)), ks, contify(id, k))

    case Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      Stmt.Invoke(contify(id, callee), method, vargs.map(contify(id, _)), bargs.map(contify(id, _)), ks, contify(id, k))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(contify(id, cond), contify(id, thn), contify(id, els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        contify(id, scrutinee),
        clauses.map { case (tag, Clause(vparams, body)) => (tag, Clause(vparams, contify(id, body))) },
        default.map(contify(id, _)))

    case Stmt.LetDef(id2, binding, body) =>
      Stmt.LetDef(id2, contify(id, binding), contify(id, body))

    case Stmt.LetExpr(id2, binding, body) =>
      Stmt.LetExpr(id2, contify(id, binding), contify(id, body))

    case Stmt.LetCont(id2, binding, body) =>
      Stmt.LetCont(id2, contify(id, binding), contify(id, body))

    case Stmt.ImpureApp(id2, callee, vargs, bargs, body) =>
      Stmt.ImpureApp(id2, callee, vargs.map(contify(id, _)), bargs.map(contify(id, _)), contify(id, body))

    case Stmt.Region(id2, ks, body) =>
      Stmt.Region(id2, ks, contify(id, body))

    case Stmt.Alloc(id2, init, region, body) =>
      Stmt.Alloc(id2, contify(id, init), region, contify(id, body))

    case Stmt.Var(id2, init, ks, body) =>
      Stmt.Var(id2, contify(id, init), ks, contify(id, body))

    case Stmt.Dealloc(ref, body) =>
      Stmt.Dealloc(ref, contify(id, body))

    case Stmt.Get(ref, id2, body) =>
      Stmt.Get(ref, id2, contify(id, body))

    case Stmt.Put(ref, value, body) =>
      Stmt.Put(ref, contify(id, value), contify(id, body))

    case Stmt.Reset(prog, ks, k) =>
      Stmt.Reset(contify(id, prog), ks, contify(id, k))

    case Stmt.Shift(prompt, body, ks, k) =>
      Stmt.Shift(prompt, contify(id, body), ks, contify(id, k))

    case Stmt.Resume(r, body, ks, k) =>
      Stmt.Resume(r, contify(id, body), ks, contify(id, k))

    case other => other
  }

  def contify(id: Id, b: Block): Block = b match {
    case b: Block.BlockVar => b
    case b: Block.BlockLit => Block.BlockLit(b.vparams, b.bparams, b.ks, b.k, contify(id, b.body))
    case Block.Unbox(p) => Block.Unbox(contify(id, p))
    case Block.New(impl) => Block.New(contify(id, impl))
  }

  def contify(id: Id, p: Expr): Expr = p match {
    case Expr.ValueVar(_) => p
    case Expr.Literal(_, _) => p
    case Expr.PureApp(id2, vargs) => Expr.PureApp(id2, vargs.map(contify(id, _)))
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(contify(id, _)))
    case Expr.Box(b) => Expr.Box(contify(id, b))
  }

  def contify(id: Id, k: Cont): Cont = k match {
    case Cont.ContVar(_) => k
    case Cont.ContLam(results, ks, body) => Cont.ContLam(results, ks, contify(id, body))
    case Cont.Abort => k
  }

  def contify(id: Id, impl: Implementation): Implementation =
    Implementation(impl.interface, impl.operations.map(op =>
      Operation(op.name, op.vparams, op.bparams, op.ks, op.k, contify(id, op.body))))

  def contify(id: Id, b: BlockLit): BlockLit =
    BlockLit(b.vparams, b.bparams, b.ks, b.k, contify(id, b.body))

  def contify(id: Id, c: Cont.ContLam): Cont.ContLam =
    Cont.ContLam(c.results, c.ks, contify(id, c.body))
}
