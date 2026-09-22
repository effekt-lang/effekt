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
 * When `E` is syntactically known, the continuation is known, and we perform the call.
 *   - Reuse the continuation in-place ([[inPlace]]) when the resumption is its last use
 *   - Otherwise copy it as code, renaming what it captures (as long as it captures it privately) ([[joinpoint]])
 *
 * Afterwards, `Deadcode` drops a `reset` once nothing shifts to its prompt, inliner works with the join points.
 */
object StaticResumptions {

  def apply(m: ModuleDecl): ModuleDecl = reduce.rewrite(m)

  object reduce extends Tree.Rewrite {
    override def rewrite(stmt: Stmt): Stmt = stmt match {
      // 0) a shift whose context is unknown (inside a call or a closure), inner shifts first:
      //    [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]   if every exit resumes
      case Handler(handler) =>
        val inner = handler.withBody(rewrite(handler.body))
        inPlace(inner, atDelimiter = false).getOrElse { inner.shift }

      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt @ BlockParam(_, Type.TPrompt(answer), _)), body)) =>
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt),
          underDelimiter(Delimiter(prompt, cparams.head, answer), rewrite(body), joinable = true)))

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

    /** [[ resume(k){s} ]] = by(s), everywhere in the body */
    def replaceResumptions(by: Stmt => Stmt): Stmt = {
      object replace extends Tree.Rewrite {
        override def rewrite(stmt: Stmt): Stmt = stmt match {
          case Resume(resumed) => by(rewrite(resumed))
          case other => super.rewrite(other)
        }
      }
      replace.rewrite(body)
    }

    /** Whether `k` occurs only as `resume(k){...}`. */
    def resumesOnly: Boolean = !replaceResumptions(identity).free.contains(k)

    /** Does every `resume(k){s}` resume with a value (i.e., not bidirectional). */
    def resumesWithValues: Boolean = {
      object query extends Tree.Query[Unit, Boolean] {
        def empty = true
        def combine = _ && _
        override def stmt(using Unit) = {
          // a `return` holds an expression, so it cannot be hiding a resumption of its own
          case Resume(resumed) => resumed.isInstanceOf[Stmt.Return]
        }
      }
      query.query(body)(using ())
    }

    /** Whether a resumption in [[stmt]] can observe a segment known by [[names]]. */
    def observes(names: Set[Id], stmt: Stmt): Boolean = {
      object query extends Tree.Query[Unit, Boolean] {
        def empty = false
        def combine = _ || _
        override def stmt(using Unit) = {
          case Resume(resumed) => resumed.typing.capt.exists(names)
        }
      }
      query.query(stmt)(using ())
    }
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

  /**
   * The part of a continuation a join point re-creates.
   * Has a [[delimiter]] and the `val` [[frame]] `val y = []; rest` if there is one.
   */
  case class Captured(d: Delimiter, frame: Option[(Id, Stmt)]) {
    /** What the continuation owns ~> what a copy must rename & must not escape. */
    val names: Set[Id] = Set(d.id, d.capture)

    /** [[ reset { p => val y = hole; rest } ]] */
    def fill(hole: Stmt): Stmt =
      Stmt.Reset(BlockLit(Nil, List(d.capture), Nil, List(d.prompt), frame match {
        case Some((y, rest)) => Stmt.Val(y, hole, rest)
        case None => hole
      }))
  }


  /**
   * Walks the static context of [[d]] and treats every shift to it.
   *
   * @param joinable no `var` or `region` lies between [[d]] and here, so frames can still be copied
   */
  def underDelimiter(d: Delimiter, stmt: Stmt, joinable: Boolean): Stmt = stmt match {
    // 1) a shift to `d`, with nothing left between it and the delimiter
    case d.Shift(handler) => inPlace(handler, atDelimiter = true) match {
      // 1a) in place, since any exit is the answer already
      case Some(reduced) => reduced
      // 1b) otherwise copied into a joinpoint, if no `var` or `region` lies above
      case None if joinable => join(Captured(d, frame = None), handler, orElse = handler.shift)
      // 1c) otherwise left alone
      case None /* otherwise */ => handler.shift
    }

    // 2) a `val` frame over a shift to `d`, with no `var` or `region` above:
    //    [[ val y = E[ shift(p) { {k} => b } ]; rest ]]
    //      ~> def j{s} = reset { p' => val y = s(); rest }; [[ E[ b[ resume(k){s} := j{s} ] ] ]]
    //    where every other leaf `l` of `E` becomes `j{ () => l }`
    case Stmt.Val(y, binding, rest) if joinable && shiftsTo(d, binding).isDefined =>
      joinpoint(Captured(d, Some(y -> rest)), binding.tpe, carries = !shiftsTo(d, binding).contains(true),
                orElse = Stmt.Val(y, binding, underDelimiter(d, rest, joinable))) { jump =>
        underDelimiter(d, jumpsToJoin(d, binding, jump), joinable = true)
      }

    // 3) a `var` or `region` is popped by the answer, so the shifts below it still stand at the delimiter;
    //    but it cannot be copied, so from here only 1a) applies
     //    [[ var x = e; s ]] ~> var x = e; [[ s ]]
     case Segment(segment) if !segment.isDelimiter =>
      segment.rebuild(underDelimiter(d, segment.body, joinable = false))
 
    // 4) anything else pushes nothing, so continue into its tail positions
     //    [[ T[s₁, …, sₙ] ]] ~> T[ [[ s₁ ]], …, [[ sₙ ]] ]
     case other => tailPositions(other) match {
       case Some(positions) => positions.rewrite(underDelimiter(d, _, joinable))
       case None => other
     }
   }

  /**
   * Whether a shift to [[d]] that can be joined stands in [[stmt]] under `val` frames only, and if so
   * whether all of them resume with a value: `None` when there is none to join.
   */
  private def shiftsTo(d: Delimiter, stmt: Stmt): Option[Boolean] = stmt match {
    case d.Shift(handler) if handler.resumesOnly => Some(handler.resumesWithValues)
    case d.Shift(_) => None
    case Stmt.Val(_, binding, body) => and(shiftsTo(d, binding), shiftsTo(d, body))
    case other => tailPositions(other) match {
      case Some(positions) =>
        var found: Option[Boolean] = None
        positions.rewrite { child => found = and(found, shiftsTo(d, child)); child }
        found
      case None => None
    }
  }

  private def and(left: Option[Boolean], right: Option[Boolean]): Option[Boolean] = (left, right) match {
    case (Some(x), Some(y)) => Some(x && y)
    case (found, None) => found
    case (None, found) => found
  }

  /**
   * The binding of a joined `val`, now standing at the delimiter: every exit jumps to the join point.
   *
   *   [[ shift(p) { {k} => b } ]] = b[ resume(k){s} := jump(s) ]
   *   [[ l ]]                     = jump(l)                         any other leaf, an untreatable shift included
   */
  private def jumpsToJoin(d: Delimiter, stmt: Stmt, jump: Stmt => Stmt, transparent: Set[Id] = Set.empty): Stmt =
    tailPositions(stmt) match {
      case Some(positions) =>
        retypeAnswer(positions.rewrite(jumpsToJoin(d, _, jump, transparent ++ positions.transparent)), d.answer)
      case None => stmt match {
        case d.Shift(handler) if handler.resumesOnly => handler.replaceResumptions(jump)

        // [[ f(…) ]] = f(…)   a tail call to a block only ever tail-called
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, d.answer), capt), targs, vargs, bargs)

        // a leaf produces the frame's value where it stands, so it jumps with that value: the frames
        // moved into the join point, the leaf did not
        case leaf: Stmt.Return => jump(leaf)
        case leaf =>
          val y = Id("y")
          Stmt.Val(y, leaf, jump(Stmt.Return(Expr.ValueVar(y, leaf.tpe))))
      }
    }

  /** [[ shift(p) { {k} => b } ]] ~> def j{s} = reset { p' => s() }; b[ resume(k){s} := j{s} ]   if `k` is only resumed */
  private def join(captured: Captured, handler: Handler, orElse: => Stmt): Stmt =
    if (handler.resumesOnly) {
      joinpoint(captured, handler.hole, carries = !handler.resumesWithValues, orElse) { jump =>
        handler.replaceResumptions(jump)
      }
    } else {
      orElse
    }

  /**
   * Binds `def j{s} = [[captured]].fill(s())` around `scope(s => j{ () => s })`, with the prompt renamed.
   *
   * The machine re-installs the *same* prompt on resume, a join point only a fresh one: nothing other than
   * the renamed binders may know the old name, neither inside the join point nor in what is passed to it.
   * Renaming would hide that, so it is checked on the free variables before renaming.
   */
  private def joinpoint(captured: Captured, hole: ValueType, carries: Boolean, orElse: => Stmt)
                       (scope: (Stmt => Stmt) => Stmt): Stmt = boundary {
    val fresh: Map[Id, Id] = captured.names.map { id => id -> Id(id) }.toMap
    object renaming extends Tree.Rewrite {
      override def rewrite(id: Id): Id = fresh.getOrElse(id, id)
    }
    def knowsOldName(stmt: Stmt): Boolean = {
      val free = stmt.free
      free.blocks.toMap.exists { case (id, (tpe, capt)) => !fresh.contains(id) && (capt ++ captures(tpe)).exists(fresh.contains) } ||
        free.values.toMap.exists { case (id, tpe) => !fresh.contains(id) && captures(tpe).exists(fresh.contains) }
    }
    if (captures(hole).exists(fresh.contains)) break(orElse)

    val j = Id("j")

    /** The join point's body: the continuation's frames. */
    def frames(filled: Stmt): BlockLit = {
      val code = captured.fill(filled)
      if (knowsOldName(code)) break(orElse)
      BlockLit(Nil, Nil, Nil, Nil, reduce.rewrite(renaming.rewrite(code)))
    }

    /** Nothing may enter the join point carrying a name the copy renamed. */
    def entering(stmt: Stmt): Stmt = {
      if (stmt.capt.exists(fresh.contains) || knowsOldName(stmt)) break(orElse)
      stmt
    }

    /** `def j(y) = reset { p => E[return y] }` */
    def takingAValue: Stmt = {
      val y = ValueParam(Id("y"), hole)
      val jDef = frames(Stmt.Return(Expr.ValueVar(y.id, y.tpe))).copy(vparams = List(y))
      val jVar = Block.BlockVar(j, jDef.tpe, jDef.capt)
      Stmt.Def(j, jDef, scope { stmt => entering(stmt) match {
        case Stmt.Return(e) => Stmt.App(jVar, Nil, List(e), Nil)
        case _ => break(orElse)
      }})
    }

    /** `def j{s} = reset { p => E[s()] }` */
    def takingAComputation: Stmt = {
      val s = Id("s")
      val sCapt = Id("sCapt")
      val sParam = BlockParam(s, BlockType.Function(Nil, Nil, Nil, Nil, hole), Set(sCapt))
      val jDef = frames(Stmt.App(Block.BlockVar(s, sParam.tpe, Set(sCapt)), Nil, Nil, Nil))
        .copy(cparams = List(sCapt), bparams = List(sParam))
      val jVar = Block.BlockVar(j, jDef.tpe, jDef.capt)
      Stmt.Def(j, jDef, scope { stmt =>
        Stmt.App(jVar, Nil, Nil, List(BlockLit(Nil, Nil, Nil, Nil, entering(stmt))))
      })
    }

    if (carries) takingAComputation else takingAValue
  }

  /** The captures that occur in a type. */
  private def captures(tpe: ValueType): Captures = tpe match {
    case ValueType.Var(_) => Set.empty
    case ValueType.Data(_, targs) => targs.flatMap(captures).toSet
    case ValueType.Boxed(tpe, capt) => capt ++ captures(tpe)
  }

  private def captures(tpe: BlockType): Captures = tpe match {
    case BlockType.Function(_, cparams, vparams, bparams, result) =>
      (vparams.flatMap(captures) ++ bparams.flatMap(captures) ++ captures(result)).toSet -- cparams
    case BlockType.Interface(_, targs) => targs.flatMap(captures).toSet
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
        if (positions.before.contains(k)) break(None)

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
      def rewrite(f: Stmt => Stmt): Stmt = {
        Stmt.Match(scrutinee, tpe, clauses.map {
          case (tag, BlockLit(tps, cps, vps, bps, body)) => tag -> BlockLit(tps, cps, vps, bps, f(body))
        }, default.map(f))
      }
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
