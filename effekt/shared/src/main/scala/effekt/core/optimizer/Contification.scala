package effekt
package core
package optimizer

import scala.annotation.tailrec

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Turns statically known continuations into jumps:
 * [[identify]] names the body of every `val`, [[sink]] moves definitions next to their uses,
 * [[returnsTo]] computes where each local definition returns to, [[contify]] replaces its returns by jumps there,
 * and [[cleanup]] removes unused definitions and inlines those used once.
 */
def contification(m: ModuleDecl): ModuleDecl = cleanup(contify(sink(identify(m))))


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
      case Stmt.Val(ids, binding, body) => frame(ids, body) match {
        case Some(k) => jump(k, _ => false, rewrite(binding))
        case None =>
          val rewrittenBinding = rewrite(binding)
          val continuation = BlockLit(Nil, Nil, (ids zip rewrittenBinding.tpe).map(ValueParam.apply), Nil, rewrite(body))
          val k = Id("k")
          Stmt.Def(k, continuation, jump(BlockVar(k, continuation.tpe, continuation.capt), _ => false, rewrittenBinding))
      }
      case other => super.rewrite(other)
    }
  }
  identification.rewrite(m)
}

/** The continuation `k` if [[body]] is `k(ids)`, so that `val ids = binding; body` is a frame around `binding`. */
def frame(ids: List[Id], body: Stmt): Option[BlockVar] = body match {
  case Stmt.App(k: BlockVar, Nil, vargs, Nil) if ids.size == vargs.size && (ids zip vargs).forall {
    case (id, ValueVar(x, _)) => x == id
    case _ => false
  } => Some(k)
  case _ => None
}

/**
 * [[s]] where every exit jumps to [[k]]:
 *
 *   [[ return v ]]  =  k(v)
 *   [[ f(a) ]]      =  f(a)               if [[jumpsTo]](f), since `f` jumps to `k` itself
 *   [[ shift(p) { {k0} => b } ]]  =  shift(p) { {k0} => b[ resume(k0){ s } := resume(k0){ [[ s ]] } ] }
 *                                     if `k0` occurs only resumed, since the frame is part of what `k0` captures
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
    case StaticResumptions.Shift(shift) if shift.resumesOnly =>
      val Stmt.Shift(prompt, BlockParam(resumption, BlockType.Resume(_, answer), captures), _) = shift.stmt: @unchecked
      val retyped = BlockParam(resumption, BlockType.Resume(result, answer), captures)
      Stmt.Shift(prompt, retyped, shift.replaceResumptions { resumed =>
        Stmt.Resume(BlockVar(resumption, retyped.tpe, retyped.capt), go(resumed))
      })
    case other =>
      val results = other.tpe.map { _ => Id("r") }
      Stmt.Val(results, other, Stmt.App(k, Nil, (results zip other.tpe).map(ValueVar.apply), Nil))
  }
  go(s)
}


// Sink
// ----

/**
 * Moves every local definition down to the innermost statement that contains all its occurrences,
 * including delimiters and block arguments, but never into the body of another definition nor into the jump of a frame `val r = s; k(r)`:
 *
 *   [[ def f = b; let x = e; s ]]       =  let x = e; [[ def f = b; s ]]                if `f` does not occur in `e`
 *   [[ def f = b; def g = c; s ]]       =  def g = c; [[ def f = b; s ]]                if `f` does not occur in `c`
 *   [[ def f = b; if (c) s1 else s2 ]]  =  if (c) [[ def f = b; s1 ]] else s2           if `f` occurs only in `s1`
 *   [[ def f = b; g(a) { () => s } ]]   =  g(a) { () => [[ def f = b; s ]] }            if `f` occurs only in that argument
 *
 * TODO Moving into the bodies of other definitions would let more definitions see their continuation, but makes those
 *   bodies too big to inline. With closed definitions at the toplevel, sinking would not be necessary at all.
 */
def sink(m: ModuleDecl): ModuleDecl = {
  object sinking extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) => sinkDefinition(id, rewrite(block), rewrite(rest))
      case other => super.rewrite(other)
    }
  }
  sinking.rewrite(m)
}

/** `def id = block; rest`, with the definition placed as deep in [[rest]] as possible. */
private def sinkDefinition(id: Id, block: Block, rest: Stmt): Stmt = {
  def occurs(free: Free): Boolean = free.contains(id)
  def into(s: Stmt): Stmt = sinkDefinition(id, block, s)
  def intoBlock(b: BlockLit): BlockLit = b.copy(body = into(b.body))
  lazy val here = Stmt.Def(id, block, rest)

  /** The arguments with the definition moved into the single block literal that uses it, if there is one. */
  def intoArgument(vargs: List[Expr], bargs: List[Block]): Option[List[Block]] =
    if vargs.exists(v => occurs(v.free)) then None
    else bargs.filter(b => occurs(b.free)) match {
      case List(argument: BlockLit) => Some(bargs.map { b => if b eq argument then intoBlock(argument) else b })
      case _ => None
    }

  if !occurs(rest.free) then here else rest match {
    case Stmt.Def(other, otherBlock, body) if !occurs(otherBlock.free) =>
      Stmt.Def(other, otherBlock, into(body))
    case Stmt.Let(x, binding, body) if !occurs(binding.free) =>
      Stmt.Let(x, binding, into(body))
    case Stmt.ImpureApp(x, callee, targs, vargs, bargs, body)
        if !occurs(callee.free) && !vargs.exists(v => occurs(v.free)) && !bargs.exists(b => occurs(b.free)) =>
      Stmt.ImpureApp(x, callee, targs, vargs, bargs, into(body))
    case Stmt.Val(ids, binding, body) if !occurs(binding.free) && frame(ids, body).isEmpty =>
      Stmt.Val(ids, binding, into(body))
    case Stmt.Val(ids, binding, body) if !occurs(body.free) =>
      Stmt.Val(ids, into(binding), body)
    case Stmt.If(cond, thn, els) if !occurs(cond.free) && !occurs(els.free) =>
      Stmt.If(cond, into(thn), els)
    case Stmt.If(cond, thn, els) if !occurs(cond.free) && !occurs(thn.free) =>
      Stmt.If(cond, thn, into(els))
    case Stmt.Match(scrutinee, tpe, clauses, default) if !occurs(scrutinee.free) =>
      val inClauses = clauses.count { case (_, block) => occurs(block.free) }
      val inDefault = default.count(d => occurs(d.free))
      if inClauses + inDefault != 1 then here
      else Stmt.Match(scrutinee, tpe,
        clauses.map { case (tag, block) => (tag, if occurs(block.free) then block.copy(body = into(block.body)) else block) },
        default.map(d => if occurs(d.free) then into(d) else d))
    case Stmt.Var(ref, init, capture, body) if !occurs(init.free) =>
      Stmt.Var(ref, init, capture, into(body))
    case Stmt.Alloc(x, init, region, body) if !occurs(init.free) =>
      Stmt.Alloc(x, init, region, into(body))
    case Stmt.Get(x, tpe, ref, capt, body) =>
      Stmt.Get(x, tpe, ref, capt, into(body))
    case Stmt.Put(ref, capt, value, body) if !occurs(value.free) =>
      Stmt.Put(ref, capt, value, into(body))
    case Stmt.Reset(body) => Stmt.Reset(intoBlock(body))
    case Stmt.Region(body) => Stmt.Region(intoBlock(body))
    case Stmt.Shift(prompt, k, body) if !occurs(prompt.free) => Stmt.Shift(prompt, k, into(body))
    case Stmt.Resume(k, body) if !occurs(k.free) => Stmt.Resume(k, into(body))
    case Stmt.App(callee, targs, vargs, bargs) if !occurs(callee.free) =>
      intoArgument(vargs, bargs).map(Stmt.App(callee, targs, vargs, _)).getOrElse(here)
    case Stmt.Invoke(callee, method, tpe, targs, vargs, bargs) if !occurs(callee.free) =>
      intoArgument(vargs, bargs).map(Stmt.Invoke(callee, method, tpe, targs, vargs, _)).getOrElse(here)
    case Stmt.ImpureApp(x, callee, targs, vargs, bargs, body) if !occurs(callee.free) && !occurs(body.free) =>
      intoArgument(vargs, bargs).map(Stmt.ImpureApp(x, callee, targs, vargs, _, body)).getOrElse(here)
    case _ => here
  }
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

/** All local definitions of block literals in [[m]]. */
private def localDefinitions(m: ModuleDecl): Map[Id, BlockLit] = {
  object collector extends Tree.Query[Unit, Map[Id, BlockLit]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block: BlockLit, body) => Map(id -> block) ++ query(block) ++ query(body)
    }
  }
  collector.query(m)(using ())
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
      case Stmt.Val(ids, binding, body) if frame(ids, body).exists(k => local.contains(k.id)) =>
        val k = frame(ids, body).get.id
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

/**
 * Replaces the returns of every local definition that always returns to the same continuation `k`
 * by jumps to `k`, provided `k` is in scope at the definition, and dissolves the frames around its calls:
 *
 *   [[ def f(x) = s; ... val r = f(a); k(r) ... ]]  =  def f(x) = jump(k, s); ... f(a) ...
 */
def contify(m: ModuleDecl): ModuleDecl = {
  val analysis = returnsTo(m)
  val candidates = analysis.definitions.keySet.flatMap { f => analysis.target(f).map(f -> _) }.toMap
  val targets = candidates.filter { case (f, k) => inScope(m, f, k) }
  Contification(targets, analysis.definitions).rewrite(m)
}

/** Whether the continuation [[k]] is in scope at the definition of [[f]]. */
private def inScope(m: ModuleDecl, f: Id, k: Id): Boolean = {
  object scoping extends Tree.Query[Set[Id], Boolean] {
    def empty = false
    def combine = _ || _
    override def stmt(using scope: Set[Id]) = {
      case Stmt.Def(id, block, rest) if id == f => scope.contains(k)
      case Stmt.Def(id, block, rest) => query(block)(using scope + id) || query(rest)(using scope + id)
    }
  }
  scoping.query(m)(using Set.empty)
}

/** Lets each definition in [[targets]] jump to its continuation, retyping its occurrences accordingly. */
private case class Contification(targets: Map[Id, Id], definitions: Map[Id, BlockLit]) extends Tree.Rewrite {

  /** The result type of [[f]] after the transformation: that of the continuation it finally jumps to. */
  def result(f: Id, seen: Set[Id]): List[ValueType] = targets.get(f) match {
    case Some(k) if !seen.contains(k) => result(k, seen + f)
    case _ => definitions(f).tpe match {
      case BlockType.Function(_, _, _, _, result) => result
      case _ => sys error "A local definition needs a function type"
    }
  }

  /** The captures of [[f]] after the transformation, including those of the continuations it jumps to. */
  def captures(f: Id, seen: Set[Id]): Captures = targets.get(f) match {
    case Some(k) if !seen.contains(k) => definitions(f).capt ++ captures(k, seen + f)
    case _ => definitions(f).capt
  }

  /** Whether [[g]] jumps to [[k]], directly or through the continuations it jumps to. */
  def reaches(g: Id, k: Id, seen: Set[Id]): Boolean = targets.get(g) match {
    case Some(target) if target == k => true
    case Some(target) if !seen.contains(target) => reaches(target, k, seen + g)
    case _ => false
  }

  def continuation(k: Id): BlockVar = {
    val original: BlockVar = BlockVar(k, definitions(k).tpe, definitions(k).capt)
    rewrite(original)
  }

  override def rewrite(b: BlockVar): BlockVar = b match {
    case BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, _), capt) if targets.contains(f) =>
      BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, result(f, Set(f))), capt ++ captures(f, Set(f)))
    case other => super.rewrite(other)
  }

  override def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Val(ids, binding, body) if frame(ids, body).exists(k => definitions.contains(k.id)) =>
      val k = frame(ids, body).get.id
      jump(continuation(k), g => reaches(g, k, Set.empty), rewrite(binding))
    case Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, body), rest) if targets.contains(f) =>
      val k = targets(f)
      val jumped = jump(continuation(k), g => reaches(g, k, Set.empty), rewrite(body))
      Stmt.Def(f, BlockLit(tparams, cparams, vparams, bparams, jumped), rewrite(rest))
    case other => super.rewrite(other)
  }
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
        rewrittenBlock match {
          case _ if occurrences(id, rewrittenRest) == 0 => rewrittenRest
          case block: BlockLit if calls(id, rewrittenRest) == 1 && occurrences(id, rewrittenRest) == 1 && !block.free.contains(id) =>
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

/** How often [[id]] occurs as the callee of a call in [[s]]. */
private def calls(id: Id, s: Stmt): Int = {
  object counting extends Tree.Query[Unit, Int] {
    def empty = 0
    def combine = _ + _
    override def stmt(using Unit) = {
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) if f == id => 1 + all(vargs, query) + all(bargs, query)
    }
  }
  counting.query(s)(using ())
}

/** How often [[id]] occurs in [[s]]. */
private def occurrences(id: Id, s: Stmt): Int = {
  object counting extends Tree.Query[Unit, Int] {
    def empty = 0
    def combine = _ + _
    override def block(using Unit) = {
      case BlockVar(f, _, _) if f == id => 1
    }
  }
  counting.query(s)(using ())
}
