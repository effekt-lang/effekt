package effekt
package core
package optimizer

import effekt.util.DB

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

/**
 * Reduces delimited control whose context is static.
 *
 *   [[ reset { p => E[ shift(p) { {k} => b } ] } ]] ~> b[ resume(k){s} := reset { p => E[s] } ]
 *
 * When `E` is syntactically known, the continuation is known, and we perform the call.
 *   - Reuse the continuation in-place ([[reuse]]) when the shift is its last use
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
      case Shift(shift) =>
        val inner = shift.withBody(rewrite(shift.body))
        reuse(inner, atPrompt = false).getOrElse { inner.stmt }

      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt @ BlockParam(_, Type.TPrompt(answer), _)), body)) =>
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt),
          reduceReset(Prompt(prompt, cparams.head, answer), rewrite(body))))

      case other => super.rewrite(other)
    }
  }

  case class Prompt(param: BlockParam, capture: Id, answer: ValueType) {
    val id: Id = param.id

    /** The names it owns ~> what a copy must rename, and what must not escape into one. */
    val names: Set[Id] = Set(id, capture)

    object ShiftsWithin {
      def unapply(stmt: Stmt): Option[List[Shift]] = shiftsTo(Prompt.this, stmt) match {
        case Nil => None
        case shifts => Some(shifts)
      }
    }

    object Shift {
      def unapply(stmt: Stmt): Option[Shift] = stmt match {
        case Stmt.Shift(Block.BlockVar(p, _, _), _, _) if p == id => asShift(stmt)
        case _ => None
      }
    }
  }

  /** A `Stmt.Shift` + what we need here: the resumption `k` it binds, the `result` that `k` resumes with, and the body that uses it. */
  case class Shift(stmt: Stmt.Shift, k: Id, result: ValueType) {
    def body: Stmt = stmt.body
    def withBody(body: Stmt): Shift = copy(stmt = stmt.copy(body = body))

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
    def resumesOnly: Boolean = {
      object query extends Tree.Query[Unit, Boolean] {
        def empty = true
        def combine = _ && _
        override def stmt(using Unit) = {
          // the shift's own mention of `k` is the use we allow; what it resumes with is not
          case Resume(resumed) => this.query(resumed)
        }
        override def block(using Unit) = {
          case Block.BlockVar(id, _, _) => id != k
        }
      }
      query.query(body)(using ())
    }

    /** Does every `resume(k){s}` resume with a value (i.e., not bidirectional). */
    def resumesWithValues: Boolean = {
      object query extends Tree.Query[Unit, Boolean] {
        def empty = true
        def combine = _ && _
        override def stmt(using Unit) = {
          // a `return` holds an expression, so it cannot be hiding a shift of its own
          case Resume(resumed) => resumed.isInstanceOf[Stmt.Return]
        }
      }
      query.query(body)(using ())
    }

    /** Whether a shift in [[stmt]] can observe a frame known by [[names]]. */
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
  /** Reads a `Stmt.Shift` into a [[Shift]]. */
  private def asShift(stmt: Stmt): Option[Shift] = stmt match {
    case shift @ Stmt.Shift(_, BlockParam(k, Type.TResume(result, _), _), _) => Some(Shift(shift, k, result))
    case _ => None
  }

  /** Any shift. */
  object Shift {
    def unapply(stmt: Stmt): Option[Shift] = asShift(stmt)
  }

  /**
   * A statement that pushes exactly one frame, which the answer pops: a `var`, a `region` or a nested `reset`.
   *
   * @param names what can observe it: a `var` by its capture, a region or prompt by its id **and** its capture
   * @param before what it evaluates before its body
   */
  case class Frame(names: Set[Id], before: Free, body: Stmt, rebuild: Stmt => Stmt, isDelimiter: Boolean)
  object Frame {
    def unapply(stmt: Stmt): Option[Frame] = stmt match {
      case Stmt.Var(ref, init, capture, body) =>
        Some(Frame(Set(capture), init.free, body, Stmt.Var(ref, init, capture, _), isDelimiter = false))
      case Stmt.Region(BlockLit(tps, cps, vps, bps @ List(region), body)) =>
        Some(Frame(cps.toSet + region.id, Free.empty, body, b => Stmt.Region(BlockLit(tps, cps, vps, bps, b)), isDelimiter = false))
      case Stmt.Reset(BlockLit(tps, cps, vps, bps @ List(prompt), body)) =>
        Some(Frame(cps.toSet + prompt.id, Free.empty, body, b => Stmt.Reset(BlockLit(tps, cps, vps, bps, b)), isDelimiter = true))
      case _ => None
    }
  }

  /**
   * The part of a continuation a join point re-creates.
   * Has a [[prompt]] and the `val` [[frame]] `val y = []; rest` if there is one.
   */
  case class Continuation(prompt: Prompt, binder: Option[(Id, Stmt)]) {
    /** [[ reset { p => val y = hole; rest } ]] */
    def fill(filling: Stmt): Stmt =
      Stmt.Reset(BlockLit(Nil, List(prompt.capture), Nil, List(prompt.param), binder match {
        case Some((y, rest)) => Stmt.Val(y, filling, rest)
        case None => filling
      }))
  }


  /** A joinpoint `def id = block`, bound around [[scope]], where the resumptions now call it. */
  case class Joinpoint(id: Id, block: Block, scope: Stmt)

  /** The joinpoints built while one reset is reduced, checked by [[reduceReset]] once it is rewritten. */
  class Joinpoints {
    val built: mutable.ListBuffer[Joinpoint] = mutable.ListBuffer.empty
  }

  /**
   * Processes every shift to [[prompt]] in [[body]], keeping the joinpoints only if none of them can observe that
   * its prompt is not the old one; otherwise the shifts are treated in place only.
   */
  def reduceReset(prompt: Prompt, body: Stmt): Stmt = {
    given joinpoints: Joinpoints = Joinpoints()
    val joined = reduceShifts(prompt, body, joinable = true)
    if (joinpoints.built.exists(observesOldPrompt(prompt, _, joined))) reduceShifts(prompt, body, joinable = false)
    else joined
  }

  /**
   * Walks the static context of [[prompt]] and processes every shift to it.
   *
   * @param joinable no `var` or `region` lies between [[prompt]] and here, so frames can still be copied
   */
  def reduceShifts(prompt: Prompt, stmt: Stmt, joinable: Boolean)(using Joinpoints): Stmt = stmt match {
    // 1) a shift to `prompt`, with nothing left between it and the prompt
    case prompt.Shift(shift) =>
      // 1a) in place, since any exit is the answer already
      reuse(shift, atPrompt = true)
        // 1b) else copied into a joinpoint, if no `var` or `region` lies above
        .orElse(if (joinable) copy(Continuation(prompt, binder = None), shift) else None)
        // 1c) else left alone
        .getOrElse(shift.stmt)

    // 2) a `val` frame over a shift to `prompt`, with no `var` or `region` above:
    //    [[ val y = E[ shift(p) { {k} => b } ]; rest ]]
    //      ~> def j(y) = reset { p' => rest }; [[ E[ b[ resume(k){return e} := j(e) ] ] ]]
    //    where every other leaf `l` of `E` becomes `val y = l; j(y)`
    case Stmt.Val(y, binding @ prompt.ShiftsWithin(shifts), rest) if joinable =>
      joinpoint(Continuation(prompt, Some(y -> rest)), binding.tpe, shifts) { jump =>
        reduceShifts(prompt, jumpsToJoin(prompt, binding, jump), joinable = true)
      }.getOrElse(Stmt.Val(y, binding, reduceShifts(prompt, rest, joinable)))

    // 3) a `var` or `region` is popped by the answer, so the shifts below it still stand at the prompt;
    //    but it cannot be copied, so from here only 1a) applies
     //    [[ var x = e; s ]] ~> var x = e; [[ s ]]
     case Frame(frame) if !frame.isDelimiter =>
      frame.rebuild(reduceShifts(prompt, frame.body, joinable = false))
 
    // 4) anything else pushes nothing, so continue into its tail positions
     //    [[ T[s₁, …, sₙ] ]] ~> T[ [[ s₁ ]], …, [[ sₙ ]] ]
     case other => tailPositions(other) match {
       case Some(positions) => positions.rewrite(reduceShifts(prompt, _, joinable))
       case None => other
     }
   }

  /** The shifts to [[d]] that can be joined, standing in [[stmt]] under `val` frames only. */
  private def shiftsTo(prompt: Prompt, stmt: Stmt): List[Shift] = stmt match {
    case prompt.Shift(shift) => if (shift.resumesOnly) List(shift) else Nil
    case Stmt.Val(_, binding, body) => shiftsTo(prompt, binding) ++ shiftsTo(prompt, body)
    case other => tailPositions(other) match {
      case Some(positions) => positions.flatMap(shiftsTo(prompt, _))
      case None => Nil
    }
  }

  /**
   * The binding of a joined `val`, now standing at the prompt: every exit jumps to the join point.
   *
   *   [[ shift(p) { {k} => b } ]] = b[ resume(k){s} := jump(s) ]
   *   [[ l ]]                     = jump(l)                         any other leaf, an untreatable shift included
   */
  private def jumpsToJoin(prompt: Prompt, stmt: Stmt, jump: Stmt => Stmt, transparent: Set[Id] = Set.empty): Stmt =
    tailPositions(stmt) match {
      case Some(positions) =>
        retypeAnswer(positions.rewrite(jumpsToJoin(prompt, _, jump, transparent ++ positions.transparent)), prompt.answer)
      case None => stmt match {
        case prompt.Shift(shift) if shift.resumesOnly => shift.replaceResumptions(jump)

        // [[ f(…) ]] = f(…)   a tail call to a block only ever tail-called
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, prompt.answer), capt), targs, vargs, bargs)

        // an exit that never returns is no exit
        case leaf if leaf.tpe == Type.TBottom => retypeAnswer(leaf, prompt.answer)

        // a leaf produces the frame's value where it stands, so it jumps with that value: the frames
        // moved into the join point, the leaf did not
        case leaf: Stmt.Return => jump(leaf)
        case leaf =>
          val y = Id("y")
          Stmt.Val(y, leaf, jump(Stmt.Return(Expr.ValueVar(y, leaf.tpe))))
      }
    }

  /** [[ shift(p) { {k} => b } ]] ~> def j(y) = reset { p' => return y }; b[ resume(k){return e} := j(e) ]   if `k` is only resumed */
  private def copy(continuation: Continuation, shift: Shift)(using Joinpoints): Option[Stmt] =
    if (shift.resumesOnly) {
      joinpoint(continuation, shift.result, List(shift)) { jump =>
        shift.replaceResumptions(jump)
      }
    } else {
      None
    }

  /**
   * Binds `def j(y) = [[continuation]].fill(return y)` around `scope(e => j(e))`, with the prompt renamed,
   * or [[takingAComputation]] the thunked form.
   *
   * The machine re-installs the *same* prompt on resume, a join point a fresh one
   * whether anything can distinguish that is decided once the whole reset is rewritten ([[reduceReset]]).
   *
   * @param shifts the shifts whose shifts jump here, which decide the form it takes
   */
  private def joinpoint(continuation: Continuation, result: ValueType, shifts: List[Shift])
                       (scope: (Stmt => Stmt) => Stmt)(using joinpoints: Joinpoints): Option[Stmt] = boundary {
    val fresh: Map[Id, Id] = continuation.prompt.names.map { id => id -> Id(id) }.toMap
    object renaming extends Tree.Rewrite {
      override def rewrite(id: Id): Id = fresh.getOrElse(id, id)
    }

    val j = Id("j")

    /** The join point's body: the continuation's frames. */
    def frames(filled: Stmt): Stmt = reduce.rewrite(renaming.rewrite(continuation.fill(filled)))

    /** `def j(y) = reset { p => E[return y] }` */
    def takingAValue: Joinpoint = {
      val y = ValueParam(Id("y"), result)
      val jDef = BlockLit(Nil, Nil, List(y), Nil, frames(Stmt.Return(Expr.ValueVar(y.id, y.tpe))))
      val jVar = Block.BlockVar(j, jDef.tpe, jDef.capt)
      Joinpoint(j, jDef, scope {
        case Stmt.Return(e) => Stmt.App(jVar, Nil, List(e), Nil)
        case _ => break(None)
      })
    }

    /** `def j{s} = reset { p => E[s()] }` */
    def takingAComputation: Joinpoint = {
      val s = Id("s")
      val sCapt = Id("sCapt")
      val sParam = BlockParam(s, BlockType.Function(Nil, Nil, Nil, Nil, result), Set(sCapt))
      val jDef = BlockLit(Nil, List(sCapt), Nil, List(sParam),
        frames(Stmt.App(Block.BlockVar(s, sParam.tpe, Set(sCapt)), Nil, Nil, Nil)))
      val jVar = Block.BlockVar(j, jDef.tpe, jDef.capt)
      Joinpoint(j, jDef, scope { stmt =>
        Stmt.App(jVar, Nil, Nil, List(BlockLit(Nil, Nil, Nil, Nil, stmt)))
      })
    }

    val built = if (shifts.forall(_.resumesWithValues)) takingAValue else takingAComputation
    joinpoints.built += built
    Some(Stmt.Def(built.id, built.block, built.scope))
  }

  /**
   * Whether [[joinpoint]] can observe that its prompt is a fresh copy of [[prompt]]'s, in [[body]].
   *
   * It cannot when it is only ever tail-called, since its prompt is then installed right where the old one is.
   * Otherwise it can when anything running in it, or passed to it, reaches the old prompt through what [[body]] binds.
   * This follows identifiers, not captures: the uses of a handler's capability carry its own capture, not the prompt's.
   */
  private def observesOldPrompt(prompt: Prompt, joinpoint: Joinpoint, body: Stmt): Boolean = {
    lazy val mentioned = mentions(prompt, body)

    @tailrec
    def reaches(todo: List[Id], seen: Set[Id]): Boolean = todo match {
      case Nil => false
      case id :: _ if id == prompt.id => true
      case id :: rest if seen.contains(id) => reaches(rest, seen)
      case id :: rest => reaches(mentioned.getOrElse(id, Set.empty).toList ++ rest, seen + id)
    }

    !tailCalledOnly(joinpoint.id, joinpoint.scope) &&
      reaches(joinpoint.block.free.freeIds.toList ++ arguments(joinpoint), Set.empty)
  }

  /** What is passed at the calls of [[joinpoint]], and thus runs inside of it. */
  private def arguments(joinpoint: Joinpoint): List[Id] = {
    object calls extends Tree.Query[Unit, List[Id]] {
      def empty = Nil
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.App(Block.BlockVar(id, _, _), _, vargs, bargs) if id == joinpoint.id =>
          vargs.flatMap(_.free.freeIds) ++ bargs.flatMap(_.free.freeIds)
      }
    }
    calls.query(joinpoint.scope)(using ())
  }

  /** For every binder in [[body]], what does its definition mention. */
  private def mentions(prompt: Prompt, body: Stmt): DB[Set[Id]] = {
    def unknown(vparams: List[ValueParam], bparams: List[BlockParam]): DB[Set[Id]] =
      DB.from((bparams.map(_.id) ++ vparams.filter(p => mayHoldBlock(p.tpe)).map(_.id)).map(_ -> Set(prompt.id)))
    def fresh(bparams: List[BlockParam]): DB[Set[Id]] =
      DB.from(bparams.map(_.id -> Set.empty[Id]))

    object binders extends Tree.Query[Unit, DB[Set[Id]]] {
      def empty = DB.empty
      def combine = _ ++ _

      override def stmt(using Unit) = {
        case Stmt.Def(id, block, rest) => query(block) ++ query(rest) + (id -> block.free.freeIds)
        case Stmt.Let(id, binding, rest) => query(binding) ++ query(rest) + (id -> binding.free.freeIds)
        case Stmt.Val(id, binding, rest) => query(binding) ++ query(rest) + (id -> binding.free.freeIds)
        case Stmt.ImpureApp(id, callee, _, vargs, bargs, rest) =>
          val arguments = vargs.flatMap(_.free.freeIds) ++ bargs.flatMap(_.free.freeIds)
          all(vargs, query) ++ all(bargs, query) ++ query(rest) + (id -> (callee.free.freeIds ++ arguments))
        // a cell may have been written anywhere
        case Stmt.Get(id, tpe, _, _, rest) if mayHoldBlock(tpe) => query(rest) + (id -> Set(prompt.id))
        case Stmt.Reset(block) => query(block) ++ fresh(block.bparams)
        case Stmt.Region(block) => query(block) ++ fresh(block.bparams)
      }
      override def block(using Unit) = {
        case BlockLit(_, _, vparams, bparams, body) => query(body) ++ unknown(vparams, bparams)
      }
      override def operation(using Unit) = {
        case Operation(_, _, _, vparams, bparams, body) => query(body) ++ unknown(vparams, bparams)
      }
    }
    binders.query(body)(using ())
  }

  /** Whether a value of this type can hold a block, and so a closure over a prompt. */
  private def mayHoldBlock(tpe: ValueType): Boolean = tpe match {
    case ValueType.Var(_) => true
    case ValueType.Data(_, targs) => targs.exists(mayHoldBlock)
    case ValueType.Boxed(_, _) => true
  }

  /**
   * [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]
   *
   * Sound iff every use of `k` is a tail shift,
   * reached only through segments no shift observes,
   * and every exit is one too (unless the shift is [[atPrompt]], where any exit is the answer already).
   */
  def reuse(shift: Shift, atPrompt: Boolean): Option[Stmt] = boundary {
    import shift.{ k, result }

    def go(stmt: Stmt, transparent: Set[Id]): Stmt = tailPositions(stmt) match {
      case Some(positions) =>
        // `k` escapes into a binding
        if (positions.before.contains(k)) break(None)

        retypeAnswer(positions.rewrite(go(_, transparent ++ positions.transparent)), result)

      case None => stmt match {
        // [[ resume(k){s} ]] = s
        case shift.Resume(s) if !s.free.contains(k) => s

        // [[ var x = e; s ]] = var x = e; [[ s ]]   if no shift observes it
        // ... same for a region, and a nested reset at the prompt only, since an abort to it is an exit by a value
        case Frame(frame) if (!frame.isDelimiter || atPrompt)
            && !frame.before.contains(k) && !shift.observes(frame.names, frame.body) =>
          frame.rebuild(go(frame.body, transparent))

        case other if other.free.contains(k) => break(None)

        // [[ f(…) ]] = f(…)   a tail call to a block only ever tail-called
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, result), capt), targs, vargs, bargs)

        // an exit that never returns is no exit
        case h: Stmt.Hole => retypeAnswer(h, result)
        case exit if atPrompt || exit.tpe == Type.TBottom => retypeAnswer(exit, result)
        case _ => break(None)
      }
    }
    Some(go(shift.body, Set.empty))
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
    private def tails: List[Stmt] = {
      var found: List[Stmt] = Nil
      rewrite { child => found = child :: found; child }
      found.reverse
    }
    def forall(p: Stmt => Boolean): Boolean = tails.forall(p)
    def flatMap[A](f: Stmt => List[A]): List[A] = tails.flatMap(f)
  }

  /**
   * A statement stands in tail position when nothing is pushed on the stack on the way to it: bindings,
   * `if` and `match` push nothing, nor does a call to a block only ever tail-called. A [[Frame]]
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
        case Frame(frame) if !frame.isDelimiter =>
          !frame.before.contains(id) && tailCalledOnly(id, frame.body)
        // the only permitted occurrence: a tail call, whose arguments must not mention it again
        case Stmt.App(Block.BlockVar(callee, _, _), _, vargs, bargs) if callee == id =>
          !vargs.exists(_.free.contains(id)) && !bargs.exists(_.free.contains(id))
        // anywhere else it must not occur
        case other => !other.free.contains(id)
      }
    }
}
