package effekt
package core
package optimizer

import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

/**
 * Reduces delimited control whose context is static.
 *
 *   [[ reset { p => E[ shift(p) { {k} => b } ] } ]] ~> b[ resume(k){s} := reset { p => E[s] } ]
 *
 *   - A shift whose every exit resumes in tail position leaves nothing for the handler to do ([[reuse]]).
 *   - A reset whose every shift stands in tail position leaves nothing for the continuation to do ([[atPrompt]]):
 *     contification has moved the frames into the resumptions, and control is the stack.
 *
 * Afterwards, `Deadcode` drops a `reset` once nothing shifts to its prompt.
 */
object StaticResumptions {

  def apply(m: ModuleDecl): ModuleDecl = reduce(blocked(m)).rewrite(m)

  /**
   * The rules below run the body of a shift where the shift stood, so its prompt stays installed.
   * That is only sound if the body cannot resume another continuation to the same prompt: the machine
   * does not allow a prompt twice on the stack (#1510). So we leave alone every prompt
   *   - some shift to which lets its continuation escape, also through a block that closes over it,
   *   - that occurs other than as the prompt of a shift, or
   *   - that is a parameter, since we do not know which reset it stands for.
   */
  def blocked(m: ModuleDecl): Set[Id] = {
    object blocking extends Tree.Query[Unit, Set[Id]] {
      def empty = Set.empty
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case shift @ Stmt.Shift(Block.BlockVar(prompt, _, _), BlockParam(k, BlockType.Resume(result, _), _), body) =>
          (if (Shift(shift, k, result).resumesOnly && !escapes(k, body)) Set.empty else Set(prompt)) ++ query(body)
        case Stmt.Reset(BlockLit(_, _, _, _, body)) => query(body)
      }
      override def block(using Unit) = {
        case Block.BlockVar(id, BlockType.Prompt(_), _) => Set(id)
        case BlockLit(_, _, _, bparams, body) =>
          bparams.collect { case BlockParam(id, BlockType.Prompt(_), _) => id }.toSet ++ query(body)
      }
    }
    blocking.query(m)(using ())
  }

  /**
   * Whether [[k]] escapes in [[stmt]]: it, or a local definition closing over it, occurs other than as
   * the callee of a call, or in a block that is not bound by a definition.
   */
  private def escapes(k: Id, stmt: Stmt): Boolean = {
    object definitions extends Tree.Query[Unit, Map[Id, Block]] {
      def empty = Map.empty
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.Def(id, block, rest) => Map(id -> block) ++ query(block) ++ query(rest)
      }
    }
    val defined = definitions.query(stmt)(using ())

    @tailrec def closing(over: Set[Id]): Set[Id] = {
      val next = over ++ defined.collect { case (id, block) if block.free.freeIds.exists(over) => id }
      if next == over then over else closing(next)
    }
    val closed = closing(Set(k))

    object escaping extends Tree.Query[Unit, Boolean] {
      def empty = false
      def combine = _ || _
      override def stmt(using Unit) = {
        case Stmt.Def(id, BlockLit(_, _, _, _, body), rest) => query(body) || query(rest)
        case Stmt.App(Block.BlockVar(f, _, _), _, vargs, bargs) => all(vargs, query) || all(bargs, query)
        case Stmt.Resume(Block.BlockVar(_, _, _), resumed) => query(resumed)
        case Stmt.Reset(BlockLit(_, _, _, _, body)) => query(body)
        case Stmt.Region(BlockLit(_, _, _, _, body)) => query(body)
      }
      override def block(using Unit) = {
        case Block.BlockVar(id, _, _) => closed.contains(id)
        case block: BlockLit => block.free.freeIds.exists(closed)
        case block: Block.New => block.free.freeIds.exists(closed)
      }
      override def clause(using Unit) = {
        case (tag, block) => query(block.body)
      }
    }
    escaping.query(stmt)(using ())
  }

  class reduce(blocked: Set[Id]) extends Tree.Rewrite {
    override def rewrite(stmt: Stmt): Stmt = stmt match {
      // 0) a shift whose context is unknown (inside a call or a closure), inner shifts first:
      //    [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]   if every exit resumes
      case Shift(shift) if !blocked.contains(shift.stmt.prompt.id) =>
        val inner = shift.withBody(rewrite(shift.body))
        reuse(inner).getOrElse { inner.stmt }

      case Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt), body)) =>
        val inner = rewrite(body)
        Stmt.Reset(BlockLit(tparams, cparams, vparams, List(prompt),
          if (atPrompt(prompt.id, inner, Set(prompt.id))) erase(prompt.id, Set.empty).rewrite(inner) else inner))

      case other => super.rewrite(other)
    }
  }

  /**
   * Whether every shift to [[prompt]] in [[stmt]] stands in tail position of its reset, as does what it resumes with.
   * Then the continuation of every shift is empty, and control is the stack:
   *
   *   [[ shift(p) { {k} => b } ]] = [[ b ]]
   *   [[ resume(k) { s } ]]       = [[ s ]]
   *
   * @param names the prompt and the local definitions that shift to it, which may only be called in tail position
   */
  def atPrompt(prompt: Id, stmt: Stmt, names: Set[Id]): Boolean = {
    def clean(free: Free): Boolean = !names.exists(free.contains)
    def here(stmt: Stmt): Boolean = atPrompt(prompt, stmt, names)
    stmt match {
      case shift @ Stmt.Shift(Block.BlockVar(p, _, _), BlockParam(k, BlockType.Resume(result, _), _), body) if p == prompt =>
        val handler = Shift(shift, k, result)
        object resumed extends Tree.Query[Unit, Boolean] {
          def empty = true
          def combine = _ && _
          override def stmt(using Unit) = {
            case handler.Resume(resumed) => here(resumed)
          }
        }
        handler.resumesOnly && clean(handler.replaceResumptions { resumed => Stmt.Return(Nil) }.free) && resumed.query(body)(using ())
      case Stmt.App(Block.BlockVar(f, _, _), _, vargs, bargs) if names.contains(f) =>
        vargs.forall { arg => clean(arg.free) } && bargs.forall { arg => clean(arg.free) }
      case Stmt.Def(f, block: BlockLit, rest) if !clean(block.free) =>
        atPrompt(prompt, block.body, names + f) && atPrompt(prompt, rest, names + f)
      case Stmt.Def(f, block, rest) => clean(block.free) && here(rest)
      case Stmt.Let(id, binding, rest) => clean(binding.free) && here(rest)
      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, rest) =>
        vargs.forall { arg => clean(arg.free) } && bargs.forall { arg => clean(arg.free) } && here(rest)
      case Stmt.Val(ids, binding, rest) => clean(binding.free) && here(rest)
      case Stmt.Alloc(id, init, region, rest) => clean(init.free) && here(rest)
      case Stmt.Get(id, tpe, ref, capt, rest) => here(rest)
      case Stmt.Put(ref, capt, value, rest) => clean(value.free) && here(rest)
      case Stmt.If(cond, thn, els) => clean(cond.free) && here(thn) && here(els)
      case Stmt.Match(scrutinee, tpe, clauses, default) =>
        clean(scrutinee.free) && clauses.forall { case (tag, clause) => here(clause.body) } && default.forall(here)
      case other => clean(other.free)
    }
  }

  class erase(prompt: Id, resumptions: Set[Id]) extends Tree.Rewrite {
    override def rewrite(stmt: Stmt): Stmt = stmt match {
      case Stmt.Shift(Block.BlockVar(p, _, _), BlockParam(k, _, _), body) if p == prompt => erase(prompt, resumptions + k).rewrite(body)
      case Stmt.Resume(Block.BlockVar(k, _, _), resumed) if resumptions.contains(k) => rewrite(resumed)
      case other => super.rewrite(other)
    }
  }

  /** A `Stmt.Shift` + what we need here: the resumption `k` it binds, the `result` values that `k` resumes with, and the body that uses it. */
  case class Shift(stmt: Stmt.Shift, k: Id, result: List[ValueType]) {
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
    case shift @ Stmt.Shift(_, BlockParam(k, BlockType.Resume(result, _), _), _) => Some(Shift(shift, k, result))
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
  case class Frame(names: Set[Id], before: Free, body: Stmt, rebuild: Stmt => Stmt)
  object Frame {
    def unapply(stmt: Stmt): Option[Frame] = stmt match {
      case Stmt.Var(ref, init, capture, body) =>
        Some(Frame(Set(capture), init.free, body, Stmt.Var(ref, init, capture, _)))
      case Stmt.Region(BlockLit(tps, cps, vps, bps @ List(region), body)) =>
        Some(Frame(cps.toSet + region.id, Free.empty, body, b => Stmt.Region(BlockLit(tps, cps, vps, bps, b))))
      case _ => None
    }
  }

  /**
   * [[ shift(p) { {k} => B[resume(k){s}] } ]] ~> B[s]
   *
   * Sound iff every use of `k` is a tail shift,
   * reached only through segments no shift observes,
   * and every exit is one too (unless the shift is [[atPrompt]], where any exit is the answer already).
   */
  def reuse(shift: Shift): Option[Stmt] = boundary {
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
        case Frame(frame) if !frame.before.contains(k) && !shift.observes(frame.names, frame.body) =>
          frame.rebuild(go(frame.body, transparent))

        case other if other.free.contains(k) => break(None)

        // [[ f(…) ]] = f(…)   a tail call to a block only ever tail-called
        case Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, _), capt), targs, vargs, bargs) if transparent.contains(f) =>
          Stmt.App(Block.BlockVar(f, BlockType.Function(tps, cps, vps, bps, result), capt), targs, vargs, bargs)

        // an exit that never returns is no exit
        case h: Stmt.Hole => retypeAnswer(h, result)
        case exit if exit.tpe.contains(Type.TBottom) => retypeAnswer(exit, result)
        case _ => break(None)
      }
    }
    Some(go(shift.body, Set.empty))
  }

  private def retypeAnswer(stmt: Stmt, tpe: List[ValueType]): Stmt = stmt match {
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

    case Stmt.Val(ids, binding, body) =>
      Some(TailPositions(f => Stmt.Val(ids, binding, f(body)), binding.free))
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
        case Frame(frame) =>
          !frame.before.contains(id) && tailCalledOnly(id, frame.body)
        // the only permitted occurrence: a tail call, whose arguments must not mention it again
        case Stmt.App(Block.BlockVar(callee, _, _), _, vargs, bargs) if callee == id =>
          !vargs.exists(_.free.contains(id)) && !bargs.exists(_.free.contains(id))
        // anywhere else it must not occur
        case other => !other.free.contains(id)
      }
    }
}
