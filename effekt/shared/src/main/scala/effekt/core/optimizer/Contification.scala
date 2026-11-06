package effekt
package core
package optimizer

import scala.annotation.tailrec

import effekt.util.DB
import effekt.core.substitutions.{ Substitution, substitute }

/**
 * Turns statically known continuations into jumps:
 * [[identify]] names the body of every `val`, [[place]] brings continuations into scope where they are returned to,
 * [[returnsTo]] computes where each local definition returns to, [[contify]] replaces its returns by jumps there,
 * [[parametrize]] passes what went out of scope as parameters,
 * and [[cleanup]] removes unused definitions and inlines those used once.
 */
def contification(m: ModuleDecl): ModuleDecl = cleanup(parametrize(contify(place(identify(m)))))


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


// Place
// -----

/**
 * Brings each continuation `k` into scope at the definitions that return to it, one continuation at a time:
 *
 *   - `k` moves up, right before the first of them, together with the definitions it uses that would be out of scope there;
 *   - unless it uses one of them or a definition around it, since then they form a loop, whose definitions must stay
 *     inside of it: those returning to `k` move down instead, right after `k` or to the start of its body.
 *
 * Values and blocks that end up out of scope become parameters in [[parametrize]], after [[contify]].
 */
def place(m: ModuleDecl): ModuleDecl = {
  @tailrec def go(m: ModuleDecl, rejected: Set[Id], fuel: Int): ModuleDecl = {
    val analysis = returnsTo(m)
    val where = positions(m)
    val order = definitionOrder(m).zipWithIndex.toMap
    val pending = analysis.definitions.keySet.toList
      .flatMap { f => analysis.target(f).filter { k => !where(f).sees(k) }.map(f -> _) }
      .sortBy { case (f, _) => order(f) }
      .map(_._2).distinct.filterNot(rejected.contains)
    pending match {
      case k :: _ if fuel > 0 => placeContinuation(m, analysis, where, order, k) match {
        case Some(placed) => go(placed, rejected, fuel - 1)
        case None => go(m, rejected + k, fuel - 1)
      }
      case _ => m
    }
  }
  go(m, Set.empty, 2 * definitionOrder(m).size)
}

/** Where a definition stands: what it sees there, which type variables are bound, and the definitions around it. */
private case class Position(visible: Set[Id], types: Set[Id], enclosing: List[Id]) {
  def sees(id: Id): Boolean = visible.contains(id) || enclosing.contains(id)
  def bind(ids: IterableOnce[Id]): Position = copy(visible = visible ++ ids)
}

/** The position of every local definition in [[m]]. */
private def positions(m: ModuleDecl): Map[Id, Position] = {
  object positioning extends Tree.Query[Position, Map[Id, Position]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using here: Position) = {
      case Stmt.Def(id, block, rest) =>
        Map(id -> here) ++ query(block)(using here.copy(enclosing = id :: here.enclosing)) ++ query(rest)(using here.bind(List(id)))
      case Stmt.Let(id, binding, rest) => query(binding) ++ query(rest)(using here.bind(List(id)))
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        all(vargs, query) ++ all(bargs, query) ++ query(rest)(using here.bind(List(id)))
      case Stmt.Val(ids, binding, rest) => query(binding) ++ query(rest)(using here.bind(ids))
      case Stmt.Var(ref, init, capture, rest) => query(init) ++ query(rest)(using here.bind(List(ref)))
      case Stmt.Alloc(id, init, region, rest) => query(init) ++ query(rest)(using here.bind(List(id)))
      case Stmt.Get(id, tpe, ref, capt, rest) => query(rest)(using here.bind(List(id)))
      case Stmt.Shift(prompt, k, rest) => query(rest)(using here.bind(List(k.id)))
    }
    override def block(using here: Position) = {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        query(body)(using here.bind(vparams.map(_.id) ++ bparams.map(_.id)).copy(types = here.types ++ tparams))
    }
    override def operation(using here: Position) = {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        query(body)(using here.bind(vparams.map(_.id) ++ bparams.map(_.id)).copy(types = here.types ++ tparams))
    }
  }
  positioning.query(m)(using Position(Set.empty, Set.empty, Nil))
}

/** The local definitions of [[m]] in the order they occur. */
private def definitionOrder(m: ModuleDecl): List[Id] = {
  object ordering extends Tree.Query[Unit, List[Id]] {
    def empty = Nil
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block, rest) => id :: query(block) ++ query(rest)
    }
  }
  ordering.query(m)(using ())
}

/** All local definitions of [[m]], of any block. */
private def allDefinitions(m: ModuleDecl): Map[Id, Block] = {
  object collector extends Tree.Query[Unit, Map[Id, Block]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block, rest) => Map(id -> block) ++ query(block) ++ query(rest)
    }
  }
  collector.query(m)(using ())
}

/** [[m]] with [[k]] in scope at the definitions returning to it, if that is possible. */
private def placeContinuation(m: ModuleDecl, analysis: ReturnsTo, where: Map[Id, Position], order: Map[Id, Int], k: Id): Option[ModuleDecl] = {
  val definitions = allDefinitions(m)
  val returning = analysis.definitions.keySet.filter { f => analysis.target(f).contains(k) }
  val outside = returning.filterNot { f => where(f).sees(k) }.toList.sortBy(order)
  val anchor = outside.head

  def outermost(ids: Set[Id]): List[Id] = ids.filterNot { id => where(id).enclosing.exists(ids.contains) }.toList.sortBy(order)

  @tailrec def needed(found: Set[Id]): Set[Id] = {
    val next = found ++ found.flatMap { d =>
      definitions(d).free.blocks.toMap.keySet.filter { b =>
        definitions.contains(b) && b != d && !where(anchor).sees(b) && !where(b).enclosing.contains(d)
      }
    }
    if next == found then found else needed(next)
  }
  val lifted = needed(Set(k))
  val loop = lifted.exists { d => returning.contains(d) || where(k).enclosing.contains(d) }

  val candidates =
    if !loop then List(() => liftBefore(m, definitions, outermost(lifted), anchor))
    else List(() => sinkAfter(m, definitions, outermost(outside.toSet), k), () => sinkInto(m, definitions, outermost(outside.toSet), k))

  candidates.iterator.map(_()).find { placed => feasible(m, placed, analysis, returning, k) }
}

/** Whether every definition returning to [[k]] sees it in [[placed]], and what is out of scope can become parameters. */
private def feasible(m: ModuleDecl, placed: ModuleDecl, analysis: ReturnsTo, returning: Set[Id], k: Id): Boolean = {
  val before = positions(m)
  val after = positions(placed)
  val jumps = analysis.definitions.keySet.flatMap { f => analysis.target(f).filter { k => after.get(f).exists(_.sees(k)) }.map(f -> _) }.toMap
  returning.forall { f => after.get(f).exists(_.sees(k)) } &&
    after.forall { case (d, position) => before.get(d).forall { original => original.types.subsetOf(position.types) } } &&
    extraParameters(placed, jumps).isDefined
}

/** [[m]] with the definitions [[moved]] right before the definition of [[anchor]]. */
private def liftBefore(m: ModuleDecl, definitions: Map[Id, Block], moved: List[Id], anchor: Id): ModuleDecl = {
  object lifting extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) if moved.contains(id) => rewrite(rest)
      case Stmt.Def(id, block, rest) if id == anchor =>
        moved.foldRight(Stmt.Def(id, rewrite(block), rewrite(rest)): Stmt) { (d, inner) => Stmt.Def(d, rewrite(definitions(d)), inner) }
      case other => super.rewrite(other)
    }
  }
  lifting.rewrite(m)
}

/** [[m]] with the definitions [[moved]] right after the definition of [[k]]. */
private def sinkAfter(m: ModuleDecl, definitions: Map[Id, Block], moved: List[Id], k: Id): ModuleDecl = {
  object sinking extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) if moved.contains(id) => rewrite(rest)
      case Stmt.Def(id, block, rest) if id == k =>
        Stmt.Def(id, rewrite(block), moved.foldRight(rewrite(rest)) { (d, inner) => Stmt.Def(d, rewrite(definitions(d)), inner) })
      case other => super.rewrite(other)
    }
  }
  sinking.rewrite(m)
}

/** [[m]] with the definitions [[moved]] at the start of the body of [[k]]. */
private def sinkInto(m: ModuleDecl, definitions: Map[Id, Block], moved: List[Id], k: Id): ModuleDecl = {
  object sinking extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) if moved.contains(id) => rewrite(rest)
      case Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, body), rest) if id == k =>
        val inside = moved.foldRight(rewrite(body)) { (d, inner) => Stmt.Def(d, rewrite(definitions(d)), inner) }
        Stmt.Def(id, BlockLit(tparams, cparams, vparams, bparams, inside), rewrite(rest))
      case other => super.rewrite(other)
    }
  }
  sinking.rewrite(m)
}

/** The values and blocks bound anywhere inside [[block]], including its parameters. */
private def binders(block: BlockLit): Set[Id] = {
  object binding extends Tree.Query[Unit, Set[Id]] {
    def empty = Set.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Let(id, binding, body) => Set(id) ++ query(body)
      case Stmt.Val(ids, binding, body) => ids.toSet ++ query(binding) ++ query(body)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Set(id) ++ all(bargs, query) ++ query(body)
      case Stmt.Get(id, tpe, ref, capt, body) => Set(id) ++ query(body)
      case Stmt.Def(id, block, body) => Set(id) ++ query(block) ++ query(body)
      case Stmt.Var(ref, init, capture, body) => Set(ref) ++ query(body)
      case Stmt.Alloc(id, init, region, body) => Set(id) ++ query(body)
      case Stmt.Shift(prompt, k, body) => Set(k.id) ++ query(body)
    }
    override def block(using Unit) = {
      case block: BlockLit => parameters(block) ++ query(block.body)
    }
    override def clause(using Unit) = {
      case (tag, block) => parameters(block) ++ query(block.body)
    }
  }
  def parameters(block: BlockLit): Set[Id] = block.vparams.map(_.id).toSet ++ block.bparams.map(_.id)
  binding.query(block)(using ())
}

/** How often each block occurs as the callee of a call in [[m]]. */
private def countCalls(m: ModuleDecl): Map[Id, Int] = {
  object counting extends Tree.Query[Unit, Map[Id, Int]] {
    def empty = Map.empty
    def combine = (left, right) => right.foldLeft(left) { case (counts, (id, n)) => counts.updated(id, counts.getOrElse(id, 0) + n) }
    override def stmt(using Unit) = {
      case Stmt.App(BlockVar(f, _, _), targs, vargs, bargs) => combine(Map(f -> 1), combine(all(vargs, query), all(bargs, query)))
    }
  }
  counting.query(m)(using ())
}

/** How often each block occurs in [[m]]. */
private def countOccurrences(m: ModuleDecl): Map[Id, Int] = {
  object counting extends Tree.Query[Unit, Map[Id, Int]] {
    def empty = Map.empty
    def combine = (left, right) => right.foldLeft(left) { case (counts, (id, n)) => counts.updated(id, counts.getOrElse(id, 0) + n) }
    override def block(using Unit) = {
      case BlockVar(f, _, _) => Map(f -> 1)
    }
  }
  counting.query(m)(using ())
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


// Parametrize
// -----------

/**
 * Makes every local definition take the values and blocks it uses but does not see as additional parameters,
 * and passes them at every call:
 *
 *   [[ def f(x){g} = s; ... f(a){h} ... ]]  =  def f(x, y){g, b} = s[y := y', b := b']; ... f(a, y){h, b} ...
 *
 * where `y` and `b` are the values and blocks out of scope at `f`, including those of the definitions `f` calls.
 * Each additional block parameter gets a fresh capture parameter, which replaces the captures of that block in `s`.
 */
def parametrize(m: ModuleDecl): ModuleDecl = {
  val parameters = extraParameters(m, Map.empty).getOrElse { sys error "Cannot pass what is out of scope as parameters" }
    .filter { case (_, extra) => extra.values.nonEmpty || extra.blocks.nonEmpty }
    .view.mapValues { case Extra(values, blocks) => (values.toList.sortBy(_._1.id), blocks.toList.sortBy(_._1.id)) }.toMap

  object passing extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.App(BlockVar(f, BlockType.Function(tparams, cparams, vparams, bparams, result), capt), targs, vargs, bargs)
          if parameters.contains(f) =>
        val (values, blocks) = parameters(f)
        val tpe = BlockType.Function(tparams, cparams ++ blocks.map(_._1), vparams ++ values.map(_._2), bparams ++ blocks.map(_._2._1), result)
        Stmt.App(BlockVar(f, tpe, capt), targs, vargs.map(rewrite) ++ values.map(ValueVar.apply),
          bargs.map(rewrite) ++ blocks.map { case (b, (tpe, capt)) => BlockVar(b, tpe, capt) })
      case Stmt.Def(f, block: BlockLit, rest) if parameters.contains(f) =>
        val (values, blocks) = parameters(f)
        val BlockLit(tparams, cparams, vparams, bparams, body) = rewrite(block): @unchecked
        val freshValues = values.map { case (id, tpe) => ValueParam(Id(id), tpe) }
        val freshCaptures = blocks.map { case (id, _) => Id(id) }
        val freshBlocks = blocks.zip(freshCaptures).map { case ((id, (tpe, _)), c) => BlockParam(Id(id), tpe, Set(c)) }
        val captures = blocks.zip(freshCaptures).flatMap { case ((_, (_, capt)), c) => capt.map(_ -> c) }
          .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
        val renaming = Substitution(DB.empty, DB.from(captures),
          DB.from(values.map(_._1) zip freshValues.map(p => ValueVar(p.id, p.tpe))),
          DB.from(blocks.map(_._1) zip freshBlocks.map(p => BlockVar(p.id, p.tpe, p.capt))))
        Stmt.Def(f, BlockLit(tparams, cparams ++ freshCaptures, vparams ++ freshValues, bparams ++ freshBlocks,
          substitute(body)(using renaming)), rewrite(rest))
      case other => super.rewrite(other)
    }
  }
  if parameters.isEmpty then m else passing.rewrite(m)
}

private case class Extra(values: Map[Id, ValueType], blocks: Map[Id, (BlockType, Captures)])

/**
 * The values and blocks each local definition of [[m]] uses without seeing them, also through the definitions it calls
 * or, by [[jumps]], will jump to; if all of them can become parameters: they are neither definitions nor of types with
 * type variables or captures, and the definitions taking them only occur as the callee of calls.
 */
private def extraParameters(m: ModuleDecl, jumps: Map[Id, Id]): Option[Map[Id, Extra]] = {
  val global = m.definitions.map(_.id).toSet ++ m.externs.collect { case extern: Extern.Def => extern.id }
  val definitions = allDefinitions(m)
  val where = positions(m)
  val callCounts = countCalls(m)
  val occurrenceCounts = countOccurrences(m)

  def unseen(d: Id, id: Id): Boolean = !global.contains(id) && id != d && !where(d).sees(id)
  def missing(d: Id): Extra = {
    val free = definitions(d).free
    Extra(free.values.toMap.filter { case (id, _) => unseen(d, id) }, free.blocks.toMap.filter { case (id, _) => unseen(d, id) })
  }
  def uses(d: Id): Set[Id] = definitions(d).free.blocks.toMap.keySet.filter(definitions.contains) ++ jumps.get(d)
  val inside = definitions.map {
    case (d, block: BlockLit) => d -> binders(block)
    case (d, _) => d -> Set.empty[Id]
  }

  @tailrec def leastFixedPoint(extra: Map[Id, Extra]): Map[Id, Extra] = {
    val next = definitions.keySet.map { d =>
      val used = uses(d).flatMap(extra.get)
      val own = missing(d)
      d -> Extra(
        (own.values ++ used.flatMap(_.values)).filter { case (id, _) => unseen(d, id) && !inside(d).contains(id) },
        (own.blocks ++ used.flatMap(_.blocks)).filter { case (id, _) => unseen(d, id) && !inside(d).contains(id) })
    }.toMap
    if next == extra then extra else leastFixedPoint(next)
  }
  val extra = leastFixedPoint(Map.empty)

  val possible = extra.forall { case (d, Extra(values, blocks)) =>
    (values.isEmpty && blocks.isEmpty) || (
      definitions(d).isInstanceOf[BlockLit] &&
      callCounts.getOrElse(d, 0) == occurrenceCounts.getOrElse(d, 0) &&
      values.values.forall(closed) &&
      blocks.forall { case (b, (tpe, _)) => !definitions.contains(b) && closedBlock(tpe) })
  }
  Option.when(possible)(extra)
}

private def closed(tpe: ValueType): Boolean = tpe match {
  case ValueType.Var(_) => false
  case ValueType.Data(_, targs) => targs.forall(closed)
  case ValueType.Boxed(_, _) => false
}

private def closedBlock(tpe: BlockType): Boolean = tpe match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
    tparams.isEmpty && vparams.forall(closed) && bparams.forall(closedBlock) && result.forall(closed)
  case BlockType.Interface(_, targs) => targs.forall(closed)
  case BlockType.Prompt(answer) => answer.forall(closed)
  case BlockType.Resume(result, answer) => result.forall(closed) && answer.forall(closed)
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
