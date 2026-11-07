package effekt
package core
package optimizer

import scala.annotation.tailrec

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Turns statically known continuations into jumps:
 * [[identify]] names the body of every `val`, [[returnsTo]] computes where each local definition returns to,
 * [[sinking]] replaces the returns of those returning to a single continuation by jumps there and sinks them behind it,
 * and [[cleanup]] removes unused definitions and inlines those used once.
 */
def contification(m: ModuleDecl): ModuleDecl = cleanup(sinking(identify(m)))


// Identify
// --------

/**
 * Names the body of every `val` and lets every exit of its binding jump there:
 *
 *   [[ val x = s; body ]]  =  def k(x) = [[ body ]]; jump(k, [[ s ]])
 *
 * Afterwards, the only `val`s left are frames `val r = s; k(r)` around exits that cannot jump themselves,
 * such as calls or delimiters.
 */
def identify(m: ModuleDecl): ModuleDecl = {
  object identification extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Val(ids, binding, body) =>
        val rewrittenBinding = rewrite(binding)
        val continuation = BlockLit(Nil, Nil, (ids zip rewrittenBinding.tpe).map(ValueParam.apply), Nil, rewrite(body))
        val k = Id("k")
        Stmt.Def(k, continuation, jump(BlockVar(k, continuation.tpe, continuation.capt), _ => false, rewrittenBinding))
      case other => super.rewrite(other)
    }
  }
  identification.rewrite(m)
}

/** Whether `val ids = s; k(vargs)` is a frame `val r = s; k(r)`. */
def isFrame(ids: List[Id], vargs: List[Expr]): Boolean =
  vargs.collect { case ValueVar(x, _) => x } == ids && vargs.size == ids.size

/**
 * [[s]] where every exit jumps to [[k]]:
 *
 *   [[ return v ]]  =  k(v)
 *   [[ f(a) ]]      =  f(a)               if [[jumpsTo]](f), since `f` jumps to `k` itself
 *   [[ shift(p) { {k0} => b } ]]  =  shift(p) { {k0} => b[ resume(k0){ s } := resume(k0){ [[ s ]] } ] }
 *                                     if `k0` occurs only resumed, since the frame is part of what `k0` captures
 *                                     (TODO: maybe move this to another phase, the Normalizer already pushes frames
 *                                     into shifts that do not resume)
 *   [[ s ]]         =  val r = s; k(r)    for any other exit, including delimiters like `var` and `reset`
 */
def jump(k: BlockVar, jumpsTo: Id => Boolean, s: Stmt): Stmt = {
  val result = k.annotatedTpe match {
    case BlockType.Function(_, _, _, _, result) => result
    case _ => sys error "A continuation needs a function type"
  }
  def go(s: Stmt): Stmt = s match {
    case Stmt.Return(exprs) => Stmt.App(k, Nil, exprs, Nil)
    case Stmt.App(BlockVar(f, _, _), _, _, _) if jumpsTo(f) => s
    case Stmt.Def(id, block, rest) => Stmt.Def(id, block, go(rest))
    case Stmt.Let(id, binding, rest) => Stmt.Let(id, binding, go(rest))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
      Stmt.ImpureApp(id, callee, targs, vargs, bargs, go(rest))
    case Stmt.Val(ids, binding, rest) => Stmt.Val(ids, binding, go(rest))
    case Stmt.If(cond, thn, els) => Stmt.If(cond, go(thn), go(els))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      Stmt.Match(scrutinee, result,
        clauses.map { case (id, block) => (id, block.copy(body = go(block.body))) },
        default.map(go))
    case Stmt.Alloc(id, init, region, rest) => Stmt.Alloc(id, init, region, go(rest))
    case Stmt.Get(id, tpe, ref, capt, rest) => Stmt.Get(id, tpe, ref, capt, go(rest))
    case Stmt.Put(ref, capt, value, rest) => Stmt.Put(ref, capt, value, go(rest))
    case Stmt.Hole(tpe, span) => Stmt.Hole(result, span)
    case shift @ Stmt.Shift(prompt, BlockParam(resumption, BlockType.Resume(resumed, answer), captures), _)
        if StaticResumptions.Shift(shift, resumption, resumed).resumesOnly =>
      val retyped = BlockParam(resumption, BlockType.Resume(result, answer), captures)
      Stmt.Shift(prompt, retyped, StaticResumptions.Shift(shift, resumption, resumed).replaceResumptions { s =>
        Stmt.Resume(BlockVar(resumption, retyped.tpe, retyped.capt), go(s))
      })
    case other =>
      val results = other.tpe.map { _ => Id("r") }
      Stmt.Val(results, other, Stmt.App(k, Nil, (results zip other.tpe).map(ValueVar.apply), Nil))
  }
  go(s)
}


// Returns to
// ----------

enum Target {
  case Known(k: Id)
  case Unknown
}

/** What a call in some position returns to: a target, or wherever the enclosing definition returns to. */
private enum Source {
  case Exactly(target: Target)
  case Like(definition: Id)
}

/** Where each local definition in [[definitions]] returns to. */
case class ReturnsTo(targets: Map[Id, Set[Target]], definitions: Map[Id, BlockLit]) {

  /** The continuation [[f]] always returns to, if there is exactly one. */
  def target(f: Id): Option[Id] = targets.getOrElse(f, Set.empty).toList match {
    case List(Target.Known(k)) => Some(k)
    case _ => None
  }

  def show: String = definitions.keys.toList.sortBy(_.name.name).map { f =>
    val shown = targets.getOrElse(f, Set.empty).toList.map {
      case Target.Known(k) => k.name.name
      case Target.Unknown => "?"
    }.sorted
    s"${f.name.name} ↦ ${shown.mkString(", ")}"
  }.mkString("\n")
}

/**
 * Where each local definition returns to.
 *
 * A call in the binding of a frame `val r = s; k(r)` returns to `k`; a tail call in the body of a local definition `g`
 * returns wherever `g` returns to; any other occurrence, a tail call under a delimiter or in a block literal,
 * or a use other than as the callee of a call, returns to somewhere unknown.
 */
def returnsTo(m: ModuleDecl): ReturnsTo = {
  val definitions = localDefinitions(m)
  val allCalls = calls(m, definitions.keySet)

  @tailrec def leastFixedPoint(targets: Map[Id, Set[Target]]): Map[Id, Set[Target]] = {
    val next = allCalls.groupMapReduce(_._1) {
      case (_, Source.Exactly(target)) => Set(target)
      case (_, Source.Like(definition)) => targets.getOrElse(definition, Set.empty)
    }(_ ++ _)
    if next == targets then targets else leastFixedPoint(next)
  }

  ReturnsTo(leastFixedPoint(Map.empty).filter { case (f, _) => definitions.contains(f) }, definitions)
}


/**
 * The calls in [[m]], each with what it returns to; [[local]] are the local definitions that can be continuations.
 * The context is what a tail call in the current position returns to.
 */
private def calls(m: ModuleDecl, local: Set[Id]): List[(Id, Source)] = {
  val unknown = Source.Exactly(Target.Unknown)
  object collecting extends Tree.Query[Source, List[(Id, Source)]] {
    def empty = Nil
    def combine = _ ++ _
    override def stmt(using tail: Source) = {
      case Stmt.Def(id, block: BlockLit, rest) => query(block.body)(using Source.Like(id)) ++ query(rest)
      case Stmt.Val(ids, binding, Stmt.App(BlockVar(k, _, _), Nil, vargs, Nil)) if isFrame(ids, vargs) && local.contains(k) =>
        query(binding)(using Source.Exactly(Target.Known(k))) ++ List(k -> tail)
      case Stmt.Val(ids, binding, body) => query(binding)(using unknown) ++ query(body)
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) => (f -> tail) :: all(vargs, query) ++ all(bargs, query)
      case Stmt.Var(ref, init, capture, body) => query(init) ++ query(body)(using unknown)
      case Stmt.Region(block) => query(block.body)(using unknown)
      case Stmt.Reset(block) => query(block.body)(using unknown)
      case Stmt.Shift(prompt, k, body) => query(body)(using unknown)
      case Stmt.Resume(k, body) => query(body)(using unknown)
    }
    override def block(using Source) = {
      case BlockVar(id, _, _) => List(id -> unknown)
      case block: BlockLit => query(block.body)(using unknown)
      case Block.New(impl) => all(impl.operations, operation => query(operation.body)(using unknown))
    }
    override def clause(using Source) = {
      case (tag, block) => query(block.body)
    }
  }
  collecting.query(m)(using unknown)
}


// Contify
// -------

/** Lets each definition in [[targets]] jump to its continuation, retyping its occurrences accordingly. */
private case class Contification(targets: Map[Id, Id], definitions: Map[Id, BlockLit]) extends Tree.Rewrite {

  /** [[f]] and the continuations it jumps to, one after the other, until one does not jump or they would repeat. */
  def path(f: Id): List[Id] = {
    @tailrec def go(visited: List[Id]): List[Id] = targets.get(visited.head) match {
      case Some(k) if !visited.contains(k) => go(k :: visited)
      case _ => visited.reverse
    }
    go(List(f))
  }

  /** The result type of [[f]] after the transformation: that of the continuation it finally jumps to. */
  def result(f: Id): List[ValueType] = definitions(path(f).last).tpe match {
    case BlockType.Function(_, _, _, _, result) => result
    case _ => sys error "A local definition needs a function type"
  }

  /** The captures of [[f]] after the transformation, including those of the continuations it jumps to. */
  def captures(f: Id): Captures = path(f).flatMap(definitions(_).capt).toSet

  /** Whether [[g]] jumps to [[k]], directly or through the continuations it jumps to. */
  def reaches(g: Id, k: Id): Boolean = {
    val visited = path(g)
    visited.tail.contains(k) || targets.get(visited.last).contains(k)
  }

  def continuation(k: Id): BlockVar = rewrite(BlockVar(k, definitions(k).tpe, definitions(k).capt): BlockVar)

  override def rewrite(b: BlockVar): BlockVar = b match {
    case BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, _), capt) if targets.contains(f) =>
      BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, result(f)), capt ++ captures(f))
    case other => super.rewrite(other)
  }

  override def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Val(ids, binding, Stmt.App(BlockVar(k, _, _), Nil, vargs, Nil)) if isFrame(ids, vargs) && definitions.contains(k) =>
      jump(continuation(k), reaches(_, k), rewrite(binding))
    case Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, body), rest) if targets.contains(f) =>
      val k = targets(f)
      val jumped = jump(continuation(k), reaches(_, k), rewrite(body))
      Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, jumped), rewrite(rest))
    case other => super.rewrite(other)
  }
}


// Sink
// ----

/**
 * Contifies every local definition that returns to exactly one continuation `k`, wherever it stands, and then sinks
 * it right behind `k`; if `k` uses it, it sinks to the place of `k` and takes `k` into its body.
 */
def sinking(m: ModuleDecl): ModuleDecl = {
  val analysis = returnsTo(m)
  val targets = analysis.definitions.keySet.flatMap { f => analysis.target(f).map(f -> _) }.toMap
  sink(Contification(targets, analysis.definitions).rewrite(m), targets)
}

/** Sinks each definition in [[targets]] right behind its continuation, the rest of the statements first. */
private def sink(m: ModuleDecl, targets: Map[Id, Id]): ModuleDecl = {
  object sinking extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(f, block: BlockLit, rest) if targets.contains(f) =>
        val sunkRest = rewrite(rest)
        val sunkBlock = rewrite(block).asInstanceOf[BlockLit]
        val k = targets(f)
        if defines(sunkRest, k) then behind(k, f, sunkBlock, sunkRest) else Stmt.Def(f, sunkBlock, sunkRest)
      case other => super.rewrite(other)
    }
  }
  sinking.rewrite(m)
}

/** [[s]] with [[f]] right behind the definition of [[k]], or around it if `k` uses `f`. */
private def behind(k: Id, f: Id, block: BlockLit, s: Stmt): Stmt = {
  object inserting extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, kBlock, rest) if id == k && kBlock.free.contains(f) =>
        Stmt.Def(f, block.copy(body = Stmt.Def(k, kBlock, block.body)), rest)
      case Stmt.Def(id, kBlock, rest) if id == k => Stmt.Def(k, kBlock, Stmt.Def(f, block, rest))
      case other => super.rewrite(other)
    }
  }
  inserting.rewrite(s)
}

/** Whether [[s]] defines [[k]]. */
private def defines(s: Stmt, k: Id): Boolean = {
  object definition extends Tree.Query[Unit, Boolean] {
    def empty = false
    def combine = _ || _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block, rest) => id == k || query(block) || query(rest)
    }
  }
  definition.query(s)(using ())
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

/** All local definitions of block literals in [[m]]. */
private def localDefinitions(m: ModuleDecl): Map[Id, BlockLit] = {
  object collector extends Tree.Query[Unit, Map[Id, BlockLit]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block: BlockLit, rest) => Map(id -> block) ++ query(block) ++ query(rest)
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
