package effekt
package core
package optimizer

import scala.annotation.tailrec
import scala.collection.mutable

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Turns statically known continuations into jumps:
 * [[identify]] names the body of every `val`, [[contify]] uses [[returnsTo]] to replace the returns of definitions
 * returning to a single continuation by jumps there and moves them into the scope of that continuation,
 * and [[cleanup]] removes unused definitions and inlines those used once.
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
 *   [[ val x = s; body ]]_k  =  def k'(x) = [[ body ]]_k; [[ s ]]_k'
 *   [[ return v ]]_k         =  k(v)
 *   [[ s ]]_k                =  val r = [[ s ]]; k(r)    for any other exit, such as calls or delimiters
 *
 * Afterwards, the only `val`s left are frames `val r = s; k(r)`.
 */
def identify(m: ModuleDecl): ModuleDecl = {
  object identification extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
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
      case other =>
        val results = other.tpe.map { _ => Id("r") }
        Stmt.Val(results, rewrite(other), Stmt.App(k, Nil, (results zip other.tpe).map(ValueVar.apply), Nil))
    }
  }
  identification.rewrite(m)
}

// Returns to
// ----------

enum Target {
  case Known(k: Id)
  case Unknown
}

/**
 * Where each local definition returns to, in one pass: the context is what a tail call in the current position
 * returns to.
 *
 * A call in the binding of a frame `val r = s; k(r)` returns to `k`; a tail call in the body of a local definition `g`
 * returns wherever `g` returns to; any other occurrence, a tail call under a delimiter or in a block literal,
 * or a use other than as the callee of a call, returns to somewhere unknown.
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
      case Stmt.Val(ids, binding, Stmt.App(BlockVar(k, _, _), Nil, vargs, Nil)) if isFrame(ids, vargs) =>
        query(binding)(using Set(Target.Known(k)))
        targets.updateWith(k)(_.map(_ ++ tail))
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
      case Stmt.Shift(prompt, k, body) => query(body)(using Set(Target.Unknown))
      case Stmt.Resume(k, body) => query(body)(using Set(Target.Unknown))
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
 */
def contify(m: ModuleDecl, analysis: Map[Id, Set[Target]]): ModuleDecl = {
  val chains = contified(analysis)
  val definitions = localDefinitions(m)
  val reached = chains.map { case (f, chain) => f -> chain.toSet }
  val results = chains.map { case (f, chain) => f -> result(definitions(chain.last)) }
  val captures = chains.map { case (f, chain) => f -> chain.flatMap(definitions(_).capt).toSet }
  val waiting = mutable.Map.empty[Id, List[(Id, Block)]]

  object contification extends Tree.Rewrite {
    override def rewrite(b: BlockVar): BlockVar = b match {
      case BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, _), capt) if chains.contains(f) =>
        BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, results(f)), capt ++ captures(f))
      case other => super.rewrite(other)
    }

    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Val(ids, binding, Stmt.App(BlockVar(k, _, _), Nil, vargs, Nil)) if isFrame(ids, vargs) && definitions.contains(k) =>
        go(rewrite(binding), rewrite(definitions(k)))
      case Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, body), rest) if chains.contains(f) =>
        val k = chains(f).head
        val block = BlockLit(tparams, cparams, vparams, bparams, go(rewrite(body), rewrite(definitions(k))))
        waiting(k) = (f, block) :: waiting.getOrElse(k, Nil)
        val rewrittenRest = rewrite(rest)
        if waiting.getOrElse(k, Nil).exists(_._1 == f) then {
          waiting(k) = waiting(k).filterNot(_._1 == f)
          placed(f, block, rewrittenRest)
        } else rewrittenRest
      case Stmt.Def(id, block, rest) =>
        val rewrittenBlock = rewrite(block)
        placed(id, rewrittenBlock, rewrite(rest))
      case other => super.rewrite(other)
    }

    /** The definition of [[id]], followed by the definitions waiting for it, each followed by those waiting for it. */
    def placed(id: Id, block: Block, rest: Stmt): Stmt =
      Stmt.Def(id, block, waiting.remove(id).getOrElse(Nil).foldLeft(rest) { case (rest, (f, block)) => placed(f, block, rest) })

    /**
     *   [[ return v ]]_k  =  k(v)
     *   [[ f(a) ]]_k      =  f(a)               if `f` reaches `k`, since it jumps there itself
     *   [[ s ]]_k         =  val r = s; k(r)    for any other exit
     */
    def go(s: Stmt, k: BlockVar): Stmt = s match {
      case Stmt.Return(exprs) => Stmt.App(k, Nil, exprs, Nil)
      case Stmt.App(BlockVar(f, _, _), _, _, _) if reached.get(f).exists(_.contains(k.id)) => s
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
 * Removes local definitions that do not occur and inlines those that occur exactly once, as the callee of a call.
 * Nothing else:
 *
 *   [[ def f = b; s ]]                 =  [[ s ]]                          if `f` does not occur in `s`
 *   [[ def f(x){g} = s; C[ f(a){h} ] ]]  =  C[ let x = a; def g = h; s ]    if `f` occurs exactly once
 */
def cleanup(m: ModuleDecl): ModuleDecl = {
  object cleaning extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) =>
        val rewrittenBlock = rewrite(block)
        val rewrittenRest = rewrite(rest)
        val occurring = occurrenceCounting.query(rewrittenRest)(using ()).getOrElse(id, 0)
        rewrittenBlock match {
          case _ if occurring == 0 => rewrittenRest
          case block: BlockLit if occurring == 1 && callCounting.query(rewrittenRest)(using ()).getOrElse(id, 0) == 1 && !block.free.contains(id) =>
            inlineCall(id, block, rewrittenRest)
          case _ => Stmt.Def(id, rewrittenBlock, rewrittenRest)
        }
      case other => super.rewrite(other)
    }
  }
  cleaning.rewrite(m)
}

/** [[rest]] with the call to [[id]] replaced by the body of [[block]]. */
private def inlineCall(id: Id, block: BlockLit, rest: Stmt): Stmt = {
  object replacement extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) if f == id => bind(block, targs, vargs.map(rewrite), bargs.map(rewrite))
      case other => super.rewrite(other)
    }
  }
  replacement.rewrite(rest)
}

/**
 * The body of [[block]] applied to the arguments: value arguments are bound by `let`, block arguments by `def`;
 * only type and capture parameters are substituted.
 */
private def bind(block: BlockLit, targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Stmt = {
  val types = Substitution(DB.from(block.tparams zip targs), DB.from(block.cparams zip bargs.map(_.capt)), DB.empty, DB.empty)
  val body = substitute(block.body)(using types)
  val withBlocks = (block.bparams zip bargs).foldRight(body) { case ((param, arg), rest) => Stmt.Def(param.id, arg, rest) }
  (block.vparams zip vargs).foldRight(withBlocks) { case ((param, arg), rest) => Stmt.Let(param.id, arg, rest) }
}


// Shared
// ------

/** All local definitions of block literals in [[m]], with their types and captures. */
private def localDefinitions(m: ModuleDecl): Map[Id, BlockVar] = {
  object collector extends Tree.Query[Unit, Map[Id, BlockVar]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block: BlockLit, rest) => Map[Id, BlockVar](id -> BlockVar(id, block.tpe, block.capt)) ++ query(block) ++ query(rest)
    }
  }
  collector.query(m)(using ())
}

private trait Counting extends Tree.Query[Unit, Map[Id, Int]] {
  def empty = Map.empty
  def combine = (left, right) => right.foldLeft(left) { case (counts, (id, n)) => counts.updated(id, counts.getOrElse(id, 0) + n) }
}

/** How often each block occurs as the callee of a call. */
private object callCounting extends Counting {
  override def stmt(using Unit) = {
    case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) => combine(Map(f -> 1), combine(all(vargs, query), all(bargs, query)))
  }
}

/** How often each block occurs. */
private object occurrenceCounting extends Counting {
  override def block(using Unit) = {
    case BlockVar(f, _, _) => Map(f -> 1)
  }
}

/** The result type of the continuation [[k]]. */
def result(k: BlockVar): List[ValueType] = k.annotatedTpe match {
  case BlockType.Function(_, _, _, _, result) => result
  case _ => sys error "A continuation needs a function type"
}

/** Whether `val ids = s; k(vargs)` is a frame `val r = s; k(r)`. */
def isFrame(ids: List[Id], vargs: List[Expr]): Boolean =
  vargs.size == ids.size && vargs.collect { case ValueVar(x, _) => x } == ids

/**
 * The definitions that return to exactly one continuation, each with the continuations it reaches:
 * that one, the one it in turn returns to, and so on, until one does not jump or they would repeat.
 */
def contified(analysis: Map[Id, Set[Target]]): Map[Id, List[Id]] = {
  val targets = analysis.flatMap { case (f, returns) =>
    returns.toList match {
      case List(Target.Known(k)) => Some(f -> k)
      case _ => None
    }
  }
  @tailrec def chain(f: Id, reached: List[Id]): List[Id] = targets.get(f) match {
    case Some(k) if !reached.contains(k) => chain(k, k :: reached)
    case _ => reached.reverse
  }
  targets.map { case (f, _) => f -> chain(f, Nil) }
}
