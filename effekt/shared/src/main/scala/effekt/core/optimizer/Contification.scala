package effekt
package core
package optimizer

import scala.collection.mutable

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Turns statically known continuations into jumps:
 * [[identify]] names the body of every `val`, [[contify]] uses [[returnsTo]] to replace the returns of definitions
 * returning to a single continuation by jumps there and moves them into the scope of that continuation,
 * and [[cleanup]] removes unused definitions, inlines those used once, and replaces names bound to variables.
 *
 * What a shift resumes with returns where the shift returns, so the frame of a shift whose resumption is only resumed
 * moves into its resumptions.
 */
def contification(m: ModuleDecl): ModuleDecl = {
  val identified = identify(m)
  val analysis = returnsTo(identified)
  cleanup(contify(identified, analysis))
}


// Identify
// --------

/**
 * Names the body of every `val` and lets every exit of its binding jump there:
 *
 *   [[ val x = s; body ]]    =  def k(x) = [[ body ]]; [[ s ]]_k
 *
 *   [[ val x = s; body ]]_k        =  def k'(x) = [[ body ]]_k; [[ s ]]_k'
 *   [[ return v ]]_k               =  k(v)
 *   [[ shift(p) { {r} => b } ]]_k  =  shift(p) { {r} => [[ b ]][ resume(r) { s } := resume(r) { [[ s ]]_k } ] }    if `r` is only resumed
 *   [[ s ]]_k                      =  val r = [[ s ]]; k(r)    for any other exit, such as calls or delimiters
 *
 * Afterwards, the only `val`s left are frames `val r = s; k(r)`.
 */
def identify(m: ModuleDecl): ModuleDecl = {
  val frames = mutable.Map.empty[Id, BlockVar]
  object identification extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Resume(BlockVar(r, BlockType.Resume(_, answer), capt), resumed) if frames.contains(r) =>
        val k = frames(r)
        Stmt.Resume(BlockVar(r, BlockType.Resume(result(k), answer), capt), identified(resumed, k))
      case Stmt.Val(ids, binding, body) =>
        val continuation = BlockLit(Nil, Nil, (ids zip binding.tpe).map(ValueParam.apply), Nil, rewrite(body))
        val k = Id("k")
        Stmt.Def(k, continuation, identified(binding, BlockVar(k, continuation.tpe, continuation.capt)))
      case other => super.rewrite(other)
    }

    def identified(s: Stmt, k: BlockVar): Stmt = s match {
      case Stmt.Return(exprs) => Stmt.App(k, Nil, exprs.map(rewrite), Nil)
      case Stmt.Val(ids, binding, body) =>
        val continuation = BlockLit(Nil, Nil, (ids zip binding.tpe).map(ValueParam.apply), Nil, identified(body, k))
        val k2 = Id("k")
        Stmt.Def(k2, continuation, identified(binding, BlockVar(k2, continuation.tpe, continuation.capt)))
      case Stmt.Def(id, block, rest) => Stmt.Def(id, rewrite(block), identified(rest, k))
      case Stmt.Let(id, binding, rest) => Stmt.Let(id, rewrite(binding), identified(rest, k))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        Stmt.ImpureApp(id, callee, targs, vargs.map(rewrite), bargs.map(rewrite), identified(rest, k))
      case Stmt.If(cond, thn, els) => Stmt.If(rewrite(cond), identified(thn, k), identified(els, k))
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        Stmt.Match(rewrite(scrutinee), result(k),
          clauses.map { case (id, block) => (id, block.copy(body = identified(block.body, k))) },
          default.map(identified(_, k)))
      case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, rewrite(init), region, identified(rest, k))
      case Stmt.Get(id, tpe, ref, capt, rest) => Stmt.Get(id, tpe, ref, capt, identified(rest, k))
      case Stmt.Put(ref, capt, value, rest) => Stmt.Put(ref, capt, rewrite(value), identified(rest, k))
      case Stmt.Hole(tpe, span) => Stmt.Hole(result(k), span)
      case shift @ Stmt.Shift(prompt, BlockParam(r, BlockType.Resume(from, answer), capt), body)
          if StaticResumptions.Shift(shift, r, from).resumesOnly =>
        frames(r) = k
        Stmt.Shift(prompt, BlockParam(r, BlockType.Resume(result(k), answer), capt), rewrite(body))
      case other =>
        val results = other.tpe.map { _ => Id("r") }
        Stmt.Val(results, rewrite(other), Stmt.App(k, Nil, (results zip other.tpe).map(ValueVar.apply), Nil))
    }
  }
  identification.rewrite(m)
}

/** The result type of the continuation [[k]], which [[identify]] and [[contify]] give to what jumps there. */
def result(k: BlockVar): List[ValueType] = k.annotatedTpe match {
  case BlockType.Function(_, _, _, _, result) => result
  case _ => sys error "A continuation needs a function type"
}

/** Whether `val ids = s; k(vargs)` is a frame `val r = s; k(r)` as [[identify]] leaves them, for [[returnsTo]] and [[contify]]. */
def isFrame(ids: List[Id], vargs: List[Expr]): Boolean =
  vargs.size == ids.size && vargs.collect { case ValueVar(x, _) => x } == ids

// Returns to
// ----------

enum Target {
  case Known(k: BlockVar)
  case Unknown
}

/**
 * Where each local definition and each resumption returns to, in one pass: the context is what a tail call in the
 * current position returns to.
 *
 * A call in the binding of a frame `val r = s; k(r)` returns to `k`; a tail call in the body of a local definition `g`
 * returns wherever `g` returns to; any other occurrence, a tail call under a delimiter, in the body of a shift,
 * or in a block literal, or a use other than as the callee of a call, returns to somewhere unknown.
 * A resumption that is only resumed returns wherever its shift returns to, and so does what it is resumed with.
 *
 * The rest of a definition comes before its body, so that the body sees all the callers of the definition;
 * a definition without callers returns to somewhere unknown.
 * When the body calls the definition itself other than in tail position, it has added to where the definition
 * returns to, so we look at the body once more.
 */
def returnsTo(m: ModuleDecl): Map[Id, Set[Target]] = {
  val targets = mutable.Map.empty[Id, Set[Target]]

  object collecting extends Tree.Query[Set[Target], Unit] {
    def empty = ()
    def combine = (_, _) => ()
    override def stmt(using tail: Set[Target]) = {
      case Stmt.Def(id, block: BlockLit, rest) =>
        targets(id) = Set.empty
        query(rest)
        if targets(id).isEmpty then targets(id) = Set(Target.Unknown)
        val before = targets(id)
        query(block.body)(using before)
        if targets(id) != before then query(block.body)(using targets(id))
      case Stmt.Val(ids, binding, Stmt.App(k: BlockVar, Nil, vargs, Nil)) if isFrame(ids, vargs) =>
        query(binding)(using Set(Target.Known(k)))
        targets.updateWith(k.id)(_.map(_ ++ tail))
      case Stmt.Val(ids, binding, body) =>
        query(binding)(using Set(Target.Unknown))
        query(body)
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) =>
        targets.updateWith(f)(_.map(_ ++ tail))
        all(vargs, query)
        all(bargs, query)
      case Stmt.Var(ref, init, capture, body) =>
        query(init)
        query(body)(using Set(Target.Unknown))
      case Stmt.Region(block) => query(block.body)(using Set(Target.Unknown))
      case Stmt.Reset(block) => query(block.body)(using Set(Target.Unknown))
      case shift @ Stmt.Shift(prompt, BlockParam(r, BlockType.Resume(from, _), _), body) =>
        if StaticResumptions.Shift(shift, r, from).resumesOnly then targets(r) = tail
        query(body)(using Set(Target.Unknown))
      case Stmt.Resume(BlockVar(r, _, _), body) => query(body)(using targets.getOrElse(r, Set(Target.Unknown)))
    }
    override def block(using Set[Target]) = {
      case BlockVar(id, _, _) => targets.updateWith(id)(_.map(_ + Target.Unknown))
      case block: BlockLit => query(block.body)(using Set(Target.Unknown))
      case Block.New(impl) => all(impl.operations, operation => query(operation.body)(using Set(Target.Unknown)))
    }
    override def clause(using Set[Target]) = {
      case (tag, block) => query(block.body)
    }
  }
  collecting.query(m)(using Set(Target.Unknown))

  targets.toMap
}


// Contify
// -------

/**
 * Lets every local definition that returns to exactly one continuation `k` jump there and moves it right behind `k`,
 * unless `k` is already in scope. Such a definition waits for `k` while we rewrite the rest after it.
 * What a resumption that returns to exactly one continuation `k` is resumed with jumps there, too.
 */
def contify(m: ModuleDecl, analysis: Map[Id, Set[Target]]): ModuleDecl = {
  val continuation = analysis.flatMap { case (f, returns) =>
    returns.toList match {
      case List(Target.Known(k)) => Some(f -> k)
      case _ => None
    }
  }
  val waiting = mutable.Map.empty[Id, List[(Id, Block)]]

  object contification extends Tree.Rewrite {
    override def rewrite(b: BlockVar): BlockVar = b match {
      case BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, _), capt) if continuation.contains(f) =>
        val k = rewrite(continuation(f))
        BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, result(k)), capt ++ k.capt)
      case BlockVar(r, BlockType.Resume(_, answer), capt) if continuation.contains(r) =>
        BlockVar(r, BlockType.Resume(result(rewrite(continuation(r))), answer), capt)
      case other => super.rewrite(other)
    }

    override def rewrite(p: BlockParam): BlockParam = p match {
      case BlockParam(r, BlockType.Resume(_, answer), capt) if continuation.contains(r) =>
        BlockParam(r, BlockType.Resume(result(rewrite(continuation(r))), answer), capt)
      case other => super.rewrite(other)
    }

    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Val(ids, call @ Stmt.App(BlockVar(f, _, _), _, _, _), Stmt.App(BlockVar(k, _, _), Nil, vargs, Nil))
          if isFrame(ids, vargs) && continuation.get(f).exists(_.id == k) =>
        rewrite(call)
      case Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, body), rest) if continuation.contains(f) =>
        val k = continuation(f)
        val block = BlockLit(tparams, cparams, vparams, bparams, rewrite(go(body, rewrite(k))))
        waiting(k.id) = (f, block) :: waiting.getOrElse(k.id, Nil)
        val rewrittenRest = rewrite(rest)
        if waiting.getOrElse(k.id, Nil).exists(_._1 == f) then {
          waiting(k.id) = waiting(k.id).filterNot(_._1 == f)
          placed(f, block, rewrittenRest)
        } else rewrittenRest
      case Stmt.Def(id, block, rest) =>
        val rewrittenBlock = rewrite(block)
        placed(id, rewrittenBlock, rewrite(rest))
      case Stmt.Resume(resumption @ BlockVar(r, _, _), resumed) if continuation.contains(r) =>
        Stmt.Resume(rewrite(resumption), rewrite(go(resumed, rewrite(continuation(r)))))
      case other => super.rewrite(other)
    }

    /** The definition of [[id]], followed by the definitions waiting for it, each followed by those waiting for it. */
    def placed(id: Id, block: Block, rest: Stmt): Stmt =
      Stmt.Def(id, block, waiting.remove(id).getOrElse(Nil).foldLeft(rest) { case (rest, (f, block)) => placed(f, block, rest) })

    /**
     *   [[ return v ]]_k               =  k(v)
     *   [[ f(a) ]]_k                   =  f(a)                    if `f` returns to `k`, since it jumps there itself
     *   [[ shift(p) { {r} => b } ]]_k  =  shift(p) { {r} => b }    if `r` returns to `k`, since what it is resumed with jumps there
     *   [[ s ]]_k                      =  val r = s; k(r)         for any other exit
     */
    def go(s: Stmt, k: BlockVar): Stmt = s match {
      case Stmt.Return(exprs) => Stmt.App(k, Nil, exprs, Nil)
      case Stmt.App(BlockVar(f, _, _), _, _, _) if continuation.get(f).exists(_.id == k.id) => s
      case Stmt.Def(id, block, rest) => Stmt.Def(id, block, go(rest, k))
      case Stmt.Let(id, binding, rest) => Stmt.Let(id, binding, go(rest, k))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        Stmt.ImpureApp(id, callee, targs, vargs, bargs, go(rest, k))
      case Stmt.Val(ids, binding, rest) => Stmt.Val(ids, binding, go(rest, k))
      case Stmt.If(cond, thn, els) => Stmt.If(cond, go(thn, k), go(els, k))
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        Stmt.Match(scrutinee, result(k),
          clauses.map { case (id, block) => (id, block.copy(body = go(block.body, k))) },
          default.map(go(_, k)))
      case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, init, region, go(rest, k))
      case Stmt.Get(id, tpe, ref, capt, rest) => Stmt.Get(id, tpe, ref, capt, go(rest, k))
      case Stmt.Put(ref, capt, value, rest) => Stmt.Put(ref, capt, value, go(rest, k))
      case Stmt.Hole(tpe, span) => Stmt.Hole(result(k), span)
      case Stmt.Shift(prompt, BlockParam(r, _, _), body) if continuation.get(r).exists(_.id == k.id) => s
      case other =>
        val results = other.tpe.map { _ => Id("r") }
        Stmt.Val(results, other, Stmt.App(k, Nil, (results zip other.tpe).map(ValueVar.apply), Nil))
    }
  }

  contification.rewrite(m)
}


// Cleanup
// -------

/**
 * Removes local definitions that do not occur, noting on the way whether the others are called once and occur nowhere
 * else, and then inlines those with [[inlineOnce]].
 *
 * A definition comes after its rest, so a removed definition does not count what it uses.
 */
def cleanup(m: ModuleDecl): ModuleDecl = {
  val calledOnce = mutable.Map.empty[Id, Boolean]
  object removing extends Tree.Rewrite {
    override def rewrite(b: BlockVar): BlockVar = {
      calledOnce(b.id) = false
      b
    }
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) =>
        val rewrittenRest = rewrite(rest)
        if calledOnce.contains(id) then Stmt.Def(id, rewrite(block), rewrittenRest) else rewrittenRest
      case Stmt.App(callee @ BlockVar(f, _, _), targs, vargs, bargs) =>
        calledOnce(f) = !calledOnce.contains(f)
        Stmt.App(callee, targs, vargs.map(rewrite), bargs.map(rewrite))
      case Stmt.Get(_, _, ref, _, _) =>
        calledOnce(ref) = false
        super.rewrite(s)
      case Stmt.Put(ref, _, _, _) =>
        calledOnce(ref) = false
        super.rewrite(s)
      case other => super.rewrite(other)
    }
  }
  val removed = removing.rewrite(m)
  inlineOnce(removed, calledOnce.collect { case (f, true) => f }.toSet)
}

/**
 * Inlines the definitions in [[once]] at their only call, and replaces every name bound to a variable by that variable:
 *
 *   [[ def f(x){g} = s; C[ f(a){h} ] ]]  =  C[ def g = h; [[ let x = a; s ]] ]
 *   [[ let x = y; s ]]                   =  [[ s ]][x := y]
 */
def inlineOnce(m: ModuleDecl, once: Set[Id]): ModuleDecl = {
  val inlined = mutable.Map.empty[Id, BlockLit]
  val aliases = mutable.Map.empty[Id, Expr]
  object inlining extends Tree.Rewrite {
    override def rewrite(e: Expr): Expr = e match {
      case ValueVar(x, _) if aliases.contains(x) => aliases(x)
      case other => super.rewrite(other)
    }
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block: BlockLit, rest) if once.contains(id) =>
        inlined(id) = block
        rewrite(rest)
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) if inlined.contains(f) =>
        val block = inlined(f)
        val types = Substitution(DB.from(block.tparams zip targs), DB.from(block.cparams zip bargs.map(_.capt)), DB.empty, DB.empty)
        val withValues = (block.vparams zip vargs).foldRight(substitute(block.body)(using types)) {
          case ((param, arg), rest) => Stmt.Let(param.id, arg, rest)
        }
        (block.bparams zip bargs).foldRight(rewrite(withValues)) { case ((param, arg), rest) => Stmt.Def(param.id, rewrite(arg), rest) }
      case Stmt.Let(id, binding, rest) =>
        rewrite(binding) match {
          case alias: ValueVar =>
            aliases(id) = alias
            rewrite(rest)
          case other => Stmt.Let(id, other, rewrite(rest))
        }
      case other => super.rewrite(other)
    }
  }
  inlining.rewrite(m)
}
