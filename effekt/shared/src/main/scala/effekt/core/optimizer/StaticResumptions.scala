package effekt
package core
package optimizer

import scala.util.boundary
import scala.util.boundary.break

/**
 * Reduces delimited control whose context is static.
 *
 *   [[ reset { p => E[ shift(p) { {k} => b } ] } ]] ~> b[ resume(k){s} := reset { p => E[s] } ]
 *
 * Afterwards, `Deadcode` drops a `reset` once nothing shifts to its prompt.
 */
object StaticResumptions {

  def apply(m: ModuleDecl): ModuleDecl = reduce.rewrite(m)

  object reduce extends Tree.Rewrite {
    override def rewrite(stmt: Stmt): Stmt = stmt match {
      // 0) a shift whose context is unknown (inside a call or a closure):
      //    [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]   if every exit resumes
      case Handler(handler) => inPlace(handler, atDelimiter = false) match {
        case Some(reduced) => reduced
        case None => handler.withBody(rewrite(handler.body)).shift
      }

      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt @ BlockParam(_, Type.TPrompt(answer), _)), body)) =>
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt),
          underDelimiter(Delimiter(prompt, cparams.head, answer), rewrite(body))))

      case other => super.rewrite(other)
    }
  }

  case class Delimiter(prompt: BlockParam, capture: Id, answer: ValueType) {
    val id: Id = prompt.id

    /** A shift to this delimiter. */
    object Shift {
      def unapply(stmt: Stmt): Option[Handler] = stmt match {
        case Stmt.Shift(Block.BlockVar(p, _, _), _, _) if p == id => Handler.unapply(stmt)
        case _ => None
      }
    }
  }

  case class Handler(shift: Stmt.Shift, k: Id, hole: ValueType) {
    def body: Stmt = shift.body
    def withBody(body: Stmt): Handler = copy(shift = shift.copy(body = body))

    /** `resume(k){s}` */
    object Resume {
      def unapply(stmt: Stmt): Option[Stmt] = stmt match {
        case Stmt.Resume(k2, resumed) if k2.id == k => Some(resumed)
        case _ => None
      }
    }

    /** Whether a resumption in [[stmt]] can observe a segment known by [[names]]. */
    def observes(names: Set[Id], stmt: Stmt): Boolean =
      object query extends Tree.Query[Unit, Boolean] {
        def empty = false
        def combine = _ || _
        override def stmt(using Unit) = {
          case Resume(resumed) => resumed.typing.capt.exists(names)
        }
      }
      query.query(stmt)(using ())
  }

  object Handler {
    def unapply(stmt: Stmt): Option[Handler] = stmt match {
      case shift @ Stmt.Shift(_, BlockParam(k, Type.TResume(hole, _), _), _) => Some(Handler(shift, k, hole))
      case _ => None
    }
  }

  /**
   * A statement that pushes exactly one frame, which the answer pops: a `var`, a `region` or a nested `reset`.
   *
   * @param names what can observe it: a `var` by its capture, a region or prompt by its id **and** its capture
   * @param before what it evaluates before its body
   */
  case class Segment(names: Set[Id], before: Free, body: Stmt, rebuild: Stmt => Stmt, isDelimiter: Boolean)
  object Segment {
    def unapply(stmt: Stmt): Option[Segment] = stmt match {
      case Stmt.Var(ref, init, capture, body) =>
        Some(Segment(Set(capture), init.free, body, Stmt.Var(ref, init, capture, _), isDelimiter = false))
      case Stmt.Region(BlockLit(tps, cps, vps, bps @ List(region), body)) =>
        Some(Segment(cps.toSet + region.id, Free.empty, body, b => Stmt.Region(BlockLit(tps, cps, vps, bps, b)), isDelimiter = false))
      case Stmt.Reset(BlockLit(tps, cps, vps, bps @ List(prompt), body)) =>
        Some(Segment(cps.toSet + prompt.id, Free.empty, body, b => Stmt.Reset(BlockLit(tps, cps, vps, bps, b)), isDelimiter = true))
      case _ => None
    }
  }

  def underDelimiter(d: Delimiter, stmt: Stmt): Stmt = stmt match {
    // 1) a shift to `d`, with nothing but popped segments left between it and the delimiter
    case d.Shift(handler) => inPlace(handler, atDelimiter = true) match {
      // 1a) in place, since any exit is the answer already
      case Some(reduced) => reduced
      // 1b) otherwise left alone
      case None => handler.shift
    }

    // 2) a `var` or `region` is popped by the answer, so the shifts below it still stand at the delimiter
    //    [[ var x = e; s ]] ~> var x = e; [[ s ]]
    case Segment(segment) if !segment.isDelimiter =>
      segment.rebuild(underDelimiter(d, segment.body))

    // 3) anything else pushes nothing, so continue into its tail positions
    //    [[ T[s₁, …, sₙ] ]] ~> T[ [[ s₁ ]], …, [[ sₙ ]] ]
    case other => tailPositions(other) match {
      case Some(positions) => positions.rewrite(underDelimiter(d, _))
      case None => other
    }
  }

  /**
   * [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]
   *
   * Sound iff every use of `k` is a tail resumption,
   * reached only through segments no resumption observes,
   * and every exit is one too (unless the shift is [[atDelimiter]], where any exit is the answer already).
   */
  def inPlace(handler: Handler, atDelimiter: Boolean): Option[Stmt] = boundary {
    import handler.{ k, hole }

    def go(stmt: Stmt, transparent: Set[Id]): Stmt = tailPositions(stmt) match {
      case Some(positions) =>
        // `k` escapes into a binding
        if positions.before.contains(k) then break(None)
        retypeAnswer(positions.rewrite(go(_, transparent ++ positions.transparent)), hole)

      case None => stmt match {
        // [[ resume(k){s} ]] = s
        case handler.Resume(s) if !s.free.contains(k) => s

        // [[ var x = e; s ]] = var x = e; [[ s ]]   if no resumption observes it
        // ... same for a region, and a nested reset at the delimiter only, since an abort to it is an exit by a value
        case Segment(segment) if (!segment.isDelimiter || atDelimiter)
            && !segment.before.contains(k) && !handler.observes(segment.names, segment.body) =>
          segment.rebuild(go(segment.body, transparent))

        case other if other.free.contains(k) => break(None)

        // [[ f(…) ]] = f(…)   a tail call to a block only ever tail-called
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, hole), capt), targs, vargs, bargs)

        // an exit that never returns is no exit
        case h: Stmt.Hole => retypeAnswer(h, hole)
        case exit if atDelimiter || exit.tpe == Type.TBottom => retypeAnswer(exit, hole)
        case _ => break(None)
      }
    }
    Some(go(handler.body, Set.empty))
  }

  private def retypeAnswer(stmt: Stmt, tpe: ValueType): Stmt = stmt match {
    case Stmt.Match(scrutinee, _, clauses, default) => Stmt.Match(scrutinee, tpe, clauses, default)
    case Stmt.Hole(_, span) => Stmt.Hole(tpe, span)
    case other => other
  }

  /**
   * Stores a [[rewrite]] function that will get applied to all tail positions of a given (partially applied) statement.
   * (thus being somewhat `lens`-ey)
   *
   * @param before what is evaluated before them, so an occurrence there is not in tail position
   * @param transparent blocks only ever tail-called from here, whose tail positions are ours too
   */
  case class TailPositions(rewrite: (Stmt => Stmt) => Stmt, before: Free, transparent: Set[Id] = Set.empty) {
    def forall(p: Stmt => Boolean): Boolean = {
      var holds = true
      rewrite { child => holds &&= p(child); child }
      holds
    }
    def exists(p: Stmt => Boolean): Boolean = !forall(!p(_))
  }

  /**
   * A statement stands in tail position when nothing is pushed on the stack on the way to it: bindings,
   * `if` and `match` push nothing, nor does a call to a block only ever tail-called. A [[Segment]]
   * pushes, so it ends tail position; a caller that may look through one says so itself.
   */
  def tailPositions(stmt: Stmt): Option[TailPositions] = stmt match {
    // a block that is only ever tail-called adds no frame ~> its own tail positions are tail positions here (and so is every call to it)
    case Stmt.Def(id, BlockLit(tps, cps, vps, bps, inner), body)
      if tailCalledOnly(id, body) && tailCalledOnly(id, inner) =>
        Some(TailPositions(f => Stmt.Def(id, BlockLit(tps, cps, vps, bps, f(inner)), f(body)), Free.empty, Set(id)))

    case Stmt.Val(id, binding, body) =>
      Some(TailPositions(f => Stmt.Val(id, binding, f(body)), binding.free))
    case Stmt.Let(id, binding, body) =>
      Some(TailPositions(f => Stmt.Let(id, binding, f(body)), binding.free))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      val arguments = (vargs.map(_.free) ++ bargs.map(_.free)).foldLeft(callee.free)(_ ++ _)
      Some(TailPositions(f => Stmt.ImpureApp(id, callee, targs, vargs, bargs, f(body)), arguments))
    case Stmt.Def(id, block, body) =>
      Some(TailPositions(f => Stmt.Def(id, block, f(body)), block.free))
    case Stmt.Alloc(id, init, region, body) =>
      Some(TailPositions(f => Stmt.Alloc(id, init, region, f(body)), init.free))
    case Stmt.Get(id, tpe, ref, capt, body) =>
      Some(TailPositions(f => Stmt.Get(id, tpe, ref, capt, f(body)), Free.empty))
    case Stmt.Put(ref, capt, value, body) =>
      Some(TailPositions(f => Stmt.Put(ref, capt, value, f(body)), value.free))

    case Stmt.If(cond, thn, els) =>
      Some(TailPositions(f => Stmt.If(cond, f(thn), f(els)), cond.free))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      def rewrite(f: Stmt => Stmt): Stmt =
        Stmt.Match(scrutinee, tpe, clauses.map {
          case (tag, BlockLit(tps, cps, vps, bps, body)) => tag -> BlockLit(tps, cps, vps, bps, f(body))
        }, default.map(f))
      Some(TailPositions(rewrite, scrutinee.free))

    case _ => None
  }

  /** Whether every use of [[id]] in [[stmt]] is a call to it in tail position. */
  def tailCalledOnly(id: Id, stmt: Stmt): Boolean =
    tailPositions(stmt) match {
      case Some(positions) =>
        !positions.before.contains(id) && positions.forall(tailCalledOnly(id, _))

      case None => stmt match {
        // a `var` or `region` is popped by the answer, so a call under one is still a tail call
        case Segment(segment) if !segment.isDelimiter =>
          !segment.before.contains(id) && tailCalledOnly(id, segment.body)
        // the only permitted occurrence: a tail call, whose arguments must not mention it again
        case Stmt.App(Block.BlockVar(callee, _, _), _, vargs, bargs) if callee == id =>
          !vargs.exists(_.free.contains(id)) && !bargs.exists(_.free.contains(id))
        // anywhere else it must not occur
        case other => !other.free.contains(id)
      }
    }
}
