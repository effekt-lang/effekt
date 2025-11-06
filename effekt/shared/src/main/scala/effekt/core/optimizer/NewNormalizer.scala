package effekt
package core
package optimizer

import effekt.core.ValueType.Boxed
import effekt.source.Span
import effekt.core.optimizer.semantics.{Computation, NeutralStmt, Value}
import effekt.symbols.builtins
import effekt.util.messages.{ErrorReporter, INTERNAL_ERROR}
import effekt.symbols.builtins.AsyncCapability
import kiama.output.ParenPrettyPrinter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap

// TODO
// - change story of how inlining is implemented. We need to also support toplevel functions that potentially
//   inline each other. Do we need to sort them topologically? How do we deal with (mutually) recursive definitions?
//
//
// plan: only introduce parameters for free things inside a block that are bound in the **stack**
// that is in
//
// only abstract over p, but not n:
//
//   def outer(n: Int) =
//     def foo(p) = shift(p) { ... n ... }
//     reset { p =>
//       ...
//     }
//
// Same actually for stack allocated mutable state, we should abstract over those (but only those)
// and keep the function in its original location.
// This means we only need to abstract over blocks, no values, no types.
//
// TODO Region desugaring
// region r {
//   reset { p =>
//    var x in r = 42
//    x = !x + 1
//    println(!x)
//   }
// }
//
// reset { r =>
//   reset { p =>
//    //var x in r = 42
//    shift(r) { k =>
//      var x = 42
//      resume(k) {
//        x = !x + 1
//        println(!x)
//      }
//    }
//   }
// }
//
// - Typeability preservation: {r: Region} becomes {r: Prompt[T]}
//    [[ def f() {r: Region} = s ]] = def f[T]() {r: Prompt[T]} = ...
// - Continuation capture is _not_ constant time in JS backend, so we expect a (drastic) slowdown when desugaring

object semantics {

  // Values
  // ------

  type Addr = Id
  type Label = Id
  type Prompt = Id

  // this could not only compute free variables, but also usage information to guide the inliner (see "secrets of the ghc inliner")
  type Variables = Set[Id]
  def all[A](ts: List[A], f: A => Variables): Variables = ts.flatMap(f).toSet

  enum Value {
    // Stuck
    case Var(id: Id, annotatedType: ValueType)
    case Extern(f: BlockVar, targs: List[ValueType], vargs: List[Addr])

    // Actual Values
    case Literal(value: Any, annotatedType: ValueType)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], vargs: List[Addr])

    // TODO use dynamic captures
    case Box(body: Computation, annotatedCaptures: Set[effekt.symbols.Symbol])

    val free: Variables = this match {
      case Value.Var(id, annotatedType) => Set.empty
      case Value.Extern(id, targs, vargs) => vargs.toSet
      case Value.Literal(value, annotatedType) => Set.empty
      case Value.Make(data, tag, targs, vargs) => vargs.toSet
      case Value.Box(body, tpe) => body.free
    }
  }

  // TODO find better name for this
  enum Binding {
    case Let(value: Value)
    case Def(block: Block)
    case Rec(block: Block, tpe: BlockType, capt: Captures)
    case Val(stmt: NeutralStmt)
    case Run(f: BlockVar, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation])
    case Unbox(addr: Addr, tpe: BlockType, capt: Captures)
    case Get(ref: Id, tpe: ValueType, cap: Captures)

    val free: Variables = this match {
      case Binding.Let(value) => value.free
      case Binding.Def(block) => block.free
      case Binding.Rec(block, tpe, capt) => block.free
      case Binding.Val(stmt) => stmt.free
      case Binding.Run(f, targs, vargs, bargs) => vargs.toSet ++ all(bargs, _.free)
      case Binding.Unbox(addr: Addr, tpe: BlockType, capt: Captures) => Set(addr)
      case Binding.Get(ref, tpe, cap) => Set(ref)
    }
  }

  type Bindings = List[(Id, Binding)]
  object Bindings {
    def empty: Bindings = Nil
  }

  /**
   * A Scope is a bit like a basic block, but without the terminator
   */
  class Scope(
    var bindings: ListMap[Id, Binding],
    var inverse: Map[Value, Addr],
    val outer: Option[Scope]
  ) {
    // Backtrack the internal state of Scope after running `prog`
    def local[A](prog: => A): A = {
      val scopeBefore = Scope(this.bindings, this.inverse, this.outer)
      val res = prog
      this.bindings = scopeBefore.bindings
      this.inverse = scopeBefore.inverse
      res
    }

    // floating values to the top is not always beneficial. For example
    //   def foo() = COMPUTATION
    // vs
    //   let x = COMPUTATION
    //   def foo() = x
    def getDefinition(value: Value): Option[Addr] =
      inverse.get(value) orElse outer.flatMap(_.getDefinition(value))

    def allocate(hint: String, value: Value): Addr =
      getDefinition(value) match {
        case Some(value) => value
        case None =>
          val addr = Id(hint)
          bindings = bindings.updated(addr, Binding.Let(value))
          inverse = inverse.updated(value, addr)
          addr
      }

    def allocateGet(ref: Id, tpe: ValueType, cap: Captures): Addr = {
      val addr = Id("get")
      bindings = bindings.updated(addr, Binding.Get(ref, tpe, cap))
      addr
    }

    def run(hint: String, callee: BlockVar, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation]): Addr = {
      val addr = Id(hint)
      bindings = bindings.updated(addr, Binding.Run(callee, targs, vargs, bargs))
      addr
    }

    def unbox(innerAddr: Addr, tpe: BlockType, capt: Captures): Addr = {
      val unboxAddr = Id("unbox")
      bindings = bindings.updated(unboxAddr, Binding.Unbox(innerAddr, tpe, capt))
      unboxAddr
    }

    // TODO Option[Value] or Var(id) in Value?
    def lookupValue(addr: Addr): Option[Value] = bindings.get(addr) match {
      case Some(Binding.Let(value)) => Some(value)
      case _ => outer.flatMap(_.lookupValue(addr))
    }

    def define(label: Label, block: Block): Unit =
      bindings = bindings.updated(label, Binding.Def(block))

    def defineRecursive(label: Label, block: Block, tpe: BlockType, capt: Captures): Unit =
      bindings = bindings.updated(label, Binding.Rec(block, tpe, capt))

    def push(id: Id, stmt: NeutralStmt): Unit =
      bindings = bindings.updated(id, Binding.Val(stmt))
  }
  object Scope {
    def empty: Scope = new Scope(ListMap.empty, Map.empty, None)
  }

  def reifyBindings(scope: Scope, body: NeutralStmt): BasicBlock = {
    var used = body.free
    var filtered = Bindings.empty
    // TODO implement properly
    scope.bindings.toSeq.reverse.foreach {
      // TODO for now we keep ALL definitions
      case (addr, b: Binding.Def) =>
        used = used ++ b.free
        filtered = (addr, b) :: filtered
      case (addr, b: Binding.Rec) =>
        used = used ++ b.free
        filtered = (addr, b) :: filtered
      case (addr, s: Binding.Val) =>
        used = used ++ s.free
        filtered = (addr, s) :: filtered
      case (addr, v: Binding.Run) =>
        used = used ++ v.free
        filtered = (addr, v) :: filtered

      // TODO if type is unit like, we can potentially drop this binding (but then we need to make up a "fresh" unit at use site)
      case (addr, v: Binding.Let) if used.contains(addr) =>
        used = used ++ v.free
        filtered = (addr, v) :: filtered
      case (addr, v: Binding.Let) => ()
      case (addr, b: Binding.Unbox) =>
        used = used ++ b.free
        filtered = (addr, b):: filtered
      case (addr, g: Binding.Get) =>
        used = used ++ g.free
        filtered = (addr, g) :: filtered
    }

    // we want to avoid turning tailcalls into non tail calls like
    //
    //   val x = app(x)
    //   return x
    //
    // so we eta-reduce here. Can we achieve this by construction?
    // TODO lastOption will go through the list AGAIN, let's see whether this causes performance problems
    (filtered.lastOption, body) match {
      case (Some((id1, Binding.Val(stmt))), NeutralStmt.Return(id2)) if id1 == id2 =>
        BasicBlock(filtered.init, stmt)
      case (_, _) =>
        BasicBlock(filtered, body)
    }
  }

  def nested(prog: Scope ?=> NeutralStmt)(using scope: Scope): BasicBlock = {
    // TODO parent code and parent store
    val local = Scope(ListMap.empty, Map.empty, Some(scope))
    val result = prog(using local)
    reifyBindings(local, result)
  }

  case class Env(values: Map[Id, Addr], computations: Map[Id, Computation]) {
    def lookupValue(id: Id): Addr = values(id)
    def bindValue(id: Id, value: Addr): Env = Env(values + (id -> value), computations)
    def bindValue(newValues: List[(Id, Addr)]): Env = Env(values ++ newValues, computations)

    def lookupComputation(id: Id): Computation = computations.getOrElse(id, sys error s"Unknown computation: ${util.show(id)} -- env: ${computations.map { case (id, comp) => s"${util.show(id)}: $comp" }.mkString("\n") }")
    def bindComputation(id: Id, computation: Computation): Env = Env(values, computations + (id -> computation))
    def bindComputation(newComputations: List[(Id, Computation)]): Env = Env(values, computations ++ newComputations)
  }
  object Env {
    def empty: Env = Env(Map.empty, Map.empty)
  }
  // "handlers"
  def bind[R](id: Id, addr: Addr)(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindValue(id, addr))

  def bind[R](id: Id, computation: Computation)(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindComputation(id, computation))

  def bind[R](values: List[(Id, Addr)])(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindValue(values))

  case class Block(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: BasicBlock) {
    val free: Variables = body.free -- vparams.map(_.id) -- bparams.map(_.id)
  }

  case class BasicBlock(bindings: Bindings, body: NeutralStmt) {
    val free: Variables = {
      var free = body.free
      bindings.reverse.foreach {
        case (id, b: Binding.Let) => free = (free - id) ++ b.free
        case (id, b: Binding.Def) => free = (free - id) ++ b.free
        case (id, b: Binding.Rec) => free = (free - id) ++ (b.free - id)
        case (id, b: Binding.Val) => free = (free - id) ++ b.free
        case (id, b: Binding.Run) => free = (free - id) ++ b.free
        case (id, b: Binding.Unbox) => free = (free - id) ++ b.free
        case (id, b: Binding.Get) => free = (free - id) ++ b.free
      }
      free
    }
  }

  enum Computation {
    // Unknown
    case Var(id: Id)
    // Known function
    case Def(closure: Closure)

    case Continuation(k: Cont)

    // TODO ? distinguish?
    //case Region(prompt: Id) ???
    //case Prompt(prompt: Id) ???
    //case Reference(prompt: Id) ???

    // Known object
    case New(interface: BlockType.Interface, operations: List[(Id, Closure)])

    lazy val free: Variables = this match {
      case Computation.Var(id) => Set(id)
      case Computation.Def(closure) => closure.free
      case Computation.Continuation(k) => Set.empty // TODO ???
      case Computation.New(interface, operations) => operations.flatMap(_._2.free).toSet
    }
  }

  // TODO add escaping mutable variables
  case class Closure(label: Label, environment: List[Computation.Var]) {
    val free: Variables = Set(label) ++ environment.flatMap(_.free).toSet
  }

  // Statements
  // ----------
  enum NeutralStmt {
    // context (continuation) is unknown
    case Return(result: Id)
    // callee is unknown
    case App(callee: Id, targs: List[ValueType], vargs: List[Id], bargs: List[Computation])
    // Known jump, but we do not want to inline
    case Jump(label: Id, targs: List[ValueType], vargs: List[Id], bargs: List[Computation])
    // callee is unknown
    case Invoke(id: Id, method: Id, methodTpe: BlockType, targs: List[ValueType], vargs: List[Id], bargs: List[Computation])
    // cond is unknown
    case If(cond: Id, thn: BasicBlock, els: BasicBlock)
    // scrutinee is unknown
    case Match(scrutinee: Id, clauses: List[(Id, Block)], default: Option[BasicBlock])

    // body is stuck
    case Reset(prompt: BlockParam, body: BasicBlock)
    // prompt / context is unknown
    case Shift(prompt: Prompt, kCapt: Capture, k: BlockParam, body: BasicBlock)
    // continuation is unknown
    case Resume(k: Id, body: BasicBlock)

    case Var(id: BlockParam, init: Addr, body: BasicBlock)
    case Put(ref: Id, tpe: ValueType, cap: Captures, value: Addr, body: BasicBlock)

    case Region(id: BlockParam, body: BasicBlock)
    case Alloc(id: BlockParam, init: Addr, region: Id, body: BasicBlock)

    // aborts at runtime
    case Hole(span: Span)

    val free: Variables = this match {
      case NeutralStmt.Jump(label, targs, vargs, bargs) => Set(label) ++ vargs.toSet ++ all(bargs, _.free)
      case NeutralStmt.App(label, targs, vargs, bargs) => Set(label) ++ vargs.toSet ++ all(bargs, _.free)
      case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) => Set(label) ++ vargs.toSet ++ all(bargs, _.free)
      case NeutralStmt.If(cond, thn, els) => Set(cond) ++ thn.free ++ els.free
      case NeutralStmt.Match(scrutinee, clauses, default) => Set(scrutinee) ++ clauses.flatMap(_._2.free).toSet ++ default.map(_.free).getOrElse(Set.empty)
      case NeutralStmt.Return(result) => Set(result)
      case NeutralStmt.Reset(prompt, body) => body.free - prompt.id
      case NeutralStmt.Shift(prompt, capt, k, body) => (body.free - k.id) + prompt
      case NeutralStmt.Resume(k, body) => Set(k) ++ body.free
      case NeutralStmt.Var(id, init, body) => Set(init) ++ body.free - id.id
      case NeutralStmt.Put(ref, tpe, cap, value, body) => Set(ref, value) ++ body.free
      case NeutralStmt.Region(id, body) => body.free - id.id
      case NeutralStmt.Alloc(id, init, region, body) => Set(init, region) ++ body.free - id.id
      case NeutralStmt.Hole(span) => Set.empty
    }
  }

  // Stacks
  // ------
  enum Frame {
    case Return
    case Static(tpe: ValueType, apply: Scope => Addr => Stack => NeutralStmt)
    case Dynamic(closure: Closure)

    /* Return an argument `arg` through this frame and the rest of the stack `ks`
     */
    def ret(ks: Stack, arg: Addr)(using scope: Scope): NeutralStmt = this match {
      case Frame.Return => ks match {
        case Stack.Empty => NeutralStmt.Return(arg)
        case Stack.Unknown => NeutralStmt.Return(arg)
        case Stack.Reset(p, k, ks) => k.ret(ks, arg)
        case Stack.Var(id, curr, k, ks) => k.ret(ks, arg)
        case Stack.Region(id, bindings, k, ks) => k.ret(ks, arg)
      }
      case Frame.Static(tpe, apply) => apply(scope)(arg)(ks)
      case Frame.Dynamic(Closure(label, environment)) => reify(ks) { NeutralStmt.Jump(label, Nil, List(arg), environment) }
    }

    // pushing purposefully does not abstract over env (it closes over it!)
    def push(tpe: ValueType)(f: Scope => Addr => Frame => Stack => NeutralStmt): Frame =
      Frame.Static(tpe, scope => arg => ks => f(scope)(arg)(this)(ks))
  }

  // maybe, for once it is simpler to decompose stacks like
  //
  //  f, (p, f) :: (p, f) :: Nil
  //
  // where the frame on the reset is the one AFTER the prompt NOT BEFORE!
  enum Stack {
    /**
     * Statically known to be empty
     * This only occurs at the entrypoint of normalization.
     * In other cases, where the stack is not known, you should use Unknown instead.
     */
    case Empty
    /** Dynamic tail (we do not know the shape of the remaining stack)
     */
    case Unknown
    case Reset(prompt: BlockParam, frame: Frame, next: Stack)
    case Var(id: BlockParam, curr: Addr, frame: Frame, next: Stack)
    // TODO desugar regions into var?
    case Region(id: BlockParam, bindings: Map[BlockParam, Addr], frame: Frame, next: Stack)

    lazy val bound: List[BlockParam] = this match {
      case Stack.Empty => Nil
      case Stack.Unknown => Nil
      case Stack.Reset(prompt, frame, next) => prompt :: next.bound
      case Stack.Var(id, curr, frame, next) => id :: next.bound
      case Stack.Region(id, bindings, frame, next) => id :: next.bound ++ bindings.keys
    }
  }

  @tailrec
  def get(ref: Id, ks: Stack): Option[Addr] = ks match {
    case Stack.Empty => sys error s"Should not happen: trying to lookup ${util.show(ref)} in empty stack"
    // We have reached the end of the known stack, so the variable must be in the unknown part.
    case Stack.Unknown => None
    case Stack.Reset(prompt, frame, next) => get(ref, next)
    case Stack.Var(id1, curr, frame, next) if ref == id1.id => Some(curr)
    case Stack.Var(id1, curr, frame, next) => get(ref, next)
    case Stack.Region(id, bindings, frame, next) =>
      val containsRef = bindings.keys.find(bp => bp.id == ref)
      containsRef match {
        case Some(bparam) => Some(bindings(bparam))
        case None => get(ref, next)
      }
  }

  def put(ref: Id, value: Addr, ks: Stack): Option[Stack] = ks match {
    case Stack.Empty => sys error s"Should not happen: trying to put ${util.show(ref)} in empty stack"
    // We have reached the end of the known stack, so the variable must be in the unknown part.
    case Stack.Unknown => None
    case Stack.Reset(prompt, frame, next) => put(ref, value, next).map(Stack.Reset(prompt, frame, _))
    case Stack.Var(id, curr, frame, next) if ref == id.id => Some(Stack.Var(id, value, frame, next))
    case Stack.Var(id, curr, frame, next) => put(ref, value, next).map(Stack.Var(id, curr, frame, _))
    case Stack.Region(id, bindings, frame, next) =>
      val containsRef = bindings.keys.find(bp => bp.id == ref)
      containsRef match {
        case Some(bparam) => Some(Stack.Region(id, bindings.updated(bparam, value), frame, next))
        case None => put(ref, value, next).map(Stack.Region(id, bindings, frame, _))
      }
  }

  def alloc(ref: BlockParam, reg: Id, value: Addr, ks: Stack): Option[Stack] = ks match {
    // This case can occur if we normalize a function that abstracts over a region as a parameter
    // We return None and force the reification of the allocation
    case Stack.Empty => None
    // We have reached the end of the known stack, so the variable must be in the unknown part.
    case Stack.Unknown => None
    case Stack.Reset(prompt, frame, next) =>
      alloc(ref, reg, value, next).map(Stack.Reset(prompt, frame, _))
    case Stack.Var(id, curr, frame, next) =>
      alloc(ref, reg, value, next).map(Stack.Var(id, curr, frame, _))
    case Stack.Region(id, bindings, frame, next) =>
      if (reg == id.id){
        Some(Stack.Region(id, bindings.updated(ref, value), frame, next))
      } else {
        alloc(ref, reg, value, next).map(Stack.Region(id, bindings, frame, _))
      }
  }

  enum Cont {
    case Empty
    case Reset(frame: Frame, prompt: BlockParam, rest: Cont)
<<<<<<< HEAD
    case Var(frame: Frame, id: BlockParam, curr: Addr, rest: Cont)
    case Region(frame: Frame, id: BlockParam, bindings: Map[Id, Addr], rest: Cont)
=======
    case Var(frame: Frame, id: BlockParam, curr: Addr, init: Addr, rest: Cont)
    case Region(frame: Frame, id: BlockParam, bindings: Map[BlockParam, Addr], rest: Cont)
>>>>>>> 9039d884 (fix region reification)
  }

  def shift(p: Id, k: Frame, ks: Stack): (Cont, Frame, Stack) = ks match {
    case Stack.Empty => sys error s"Should not happen: cannot find prompt ${util.show(p)}"
    case Stack.Unknown => sys error s"Cannot find prompt ${util.show(p)} in unknown stack"
    case Stack.Reset(prompt, frame, next) if prompt.id == p =>
      (Cont.Reset(k, prompt, Cont.Empty), frame, next)
    case Stack.Reset(prompt, frame, next) =>
      val (c, frame2, stack) = shift(p, frame, next)
      (Cont.Reset(k, prompt, c), frame2, stack)
    case Stack.Var(id, curr, frame, next) =>
      val (c, frame2, stack) = shift(p, frame, next)
      (Cont.Var(k, id, curr, c), frame2, stack)
    case Stack.Region(id, bindings, frame, next) =>
      val (c, frame2, stack) = shift(p, frame, next)
      (Cont.Region(k, id, bindings, c), frame2, stack)
  }

  def resume(c: Cont, k: Frame, ks: Stack): (Frame, Stack) = c match {
    case Cont.Empty => (k, ks)
    case Cont.Reset(frame, prompt, rest) =>
      val (k1, ks1) = resume(rest, frame, ks)
      val stack = Stack.Reset(prompt, k1, ks1)
      (frame, stack)
    case Cont.Var(frame, id, curr, rest) =>
      val (k1, ks1) = resume(rest, frame, ks)
      (frame, Stack.Var(id, curr, k1, ks1))
    case Cont.Region(frame, id, bindings, rest) =>
      val (k1, ks1) = resume(rest, frame, ks)
      (frame, Stack.Region(id, bindings, k1, ks1))
  }

  def joinpoint(k: Frame, ks: Stack)(f: (Frame, Stack) => NeutralStmt)(using scope: Scope): NeutralStmt = {
    def reifyFrame(k: Frame, escaping: Stack)(using scope: Scope): Frame = k match {
      case Frame.Static(tpe, apply) =>
        val x = Id("x")
        nested { scope ?=> apply(scope)(x)(Stack.Unknown) } match {
          // Avoid trivial continuations like
          //   def k_6268 = (x_6267: Int_3) {
          //     return x_6267
          //   }
          case BasicBlock(Nil, _: (NeutralStmt.Return | NeutralStmt.App | NeutralStmt.Jump)) =>
            k
          case body =>
            val k = Id("k")
            val closureParams = escaping.bound.collect { case p if body.free contains p.id => p }
            scope.define(k, Block(Nil, ValueParam(x, tpe) :: Nil, closureParams, body))
            Frame.Dynamic(Closure(k, closureParams.map { p => Computation.Var(p.id) }))
        }
      case Frame.Return => k
      case Frame.Dynamic(label) => k
    }

    def reifyStack(ks: Stack): Stack = ks match {
      case Stack.Empty => Stack.Empty
      case Stack.Unknown => Stack.Unknown
      case Stack.Reset(prompt, frame, next) =>
        Stack.Reset(prompt, reifyFrame(frame, next), reifyStack(next))
      case Stack.Var(id, curr, frame, next) =>
        Stack.Var(id, curr, reifyFrame(frame, next), reifyStack(next))
      case Stack.Region(id, bindings, frame, next) =>
        Stack.Region(id, bindings, reifyFrame(frame, next), reifyStack(next))
    }
    f(reifyFrame(k, ks), reifyStack(ks))
  }

  def reify(k: Frame, ks: Stack)(stmt: Scope ?=> NeutralStmt)(using Scope): NeutralStmt =
    reify(ks) { reify(k) { stmt } }

  def reify(k: Frame)(stmt: Scope ?=> NeutralStmt)(using scope: Scope): NeutralStmt =
    k match {
      case Frame.Return => stmt
      case Frame.Static(tpe, apply) =>
        val tmp = Id("tmp")
        scope.push(tmp, stmt)
        // TODO Over-approximation
        // Don't pass Stack.Unknown but rather the stack until the next reset?
        /*
          |----------|               |----------|               |---------|
          |          | ---> ... ---> |          | ---> ... ---> |         | ---> ...
          |----------|               |----------|               |---------|
              r1                          r2                 first next prompt

        Pass r1 :: ... :: r2 :: ... :: prompt :: UNKNOWN
         */
        apply(scope)(tmp)(Stack.Unknown)
      case Frame.Dynamic(Closure(label, closure)) =>
        val tmp = Id("tmp")
        scope.push(tmp, stmt)
        NeutralStmt.Jump(label, Nil, List(tmp), closure)
    }

  def reifyKnown(k: Frame, ks: Stack)(stmt: Scope ?=> NeutralStmt)(using scope: Scope): NeutralStmt =
    k match {
      case Frame.Return => reify(ks) { stmt }
      case Frame.Static(tpe, apply) =>
        val tmp = Id("tmp")
        scope.push(tmp, stmt)
        apply(scope)(tmp)(ks)
      case Frame.Dynamic(Closure(label, closure)) => reify(ks) { sc ?=>
        val tmp = Id("tmp")
        sc.push(tmp, stmt)
        NeutralStmt.Jump(label, Nil, List(tmp), closure)
      }
    }

  @tailrec
  final def reify(ks: Stack)(stmt: Scope ?=> NeutralStmt)(using scope: Scope): NeutralStmt = {
    ks match {
      case Stack.Empty => stmt
      case Stack.Unknown => stmt
      case Stack.Reset(prompt, frame, next) =>
        reify(next) { reify(frame) {
          val body = nested { stmt }
          if (body.free contains prompt.id) NeutralStmt.Reset(prompt, body)
          else stmt // TODO this runs normalization a second time in the outer scope!
        }}
      case Stack.Var(id, curr, frame, next) =>
        reify(next) { reify(frame) {
          val body = nested { stmt }
          if (body.free contains id.id) NeutralStmt.Var(id, curr, body)
          else stmt
        }}
      case Stack.Region(id, bindings, frame, next) =>
        reify(next) { reify(frame) {
          val body = nested { stmt }
          val bodyUsesBinding = body.free.exists(bindings.map { b => b._1.id }.toSet.contains(_))
          if (body.free.contains(id.id) || bodyUsesBinding) {
            // we need to reify all bindings in this region as allocs using their current value
            val reifiedAllocs = bindings.foldLeft(body) { case (acc, (bp, addr)) =>
              nested { NeutralStmt.Alloc(bp, addr, id.id, acc) }
            }
            NeutralStmt.Region(id, reifiedAllocs)
          }
          else stmt
        }}
    }
  }

  object PrettyPrinter extends ParenPrettyPrinter {

    override val defaultIndent = 2

    def toDoc(s: NeutralStmt): Doc = s match {
      case NeutralStmt.Return(result) =>
        "return" <+> toDoc(result)
      case NeutralStmt.If(cond, thn, els) =>
        "if" <+> parens(toDoc(cond)) <+> toDoc(thn) <+> "else" <+> toDoc(els)
      case NeutralStmt.Match(scrutinee, clauses, default) =>
        "match" <+> parens(toDoc(scrutinee)) <+> braces(hcat(clauses.map { case (id, block) => toDoc(id) <> ":" <+> toDoc(block) })) <>
          (if (default.isDefined) "else" <+> toDoc(default.get) else emptyDoc)
      case NeutralStmt.Jump(label, targs, vargs, bargs) =>
        // Format as: l1[T1, T2](r1, r2)
        "jump" <+> toDoc(label) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma)) <> hsep(bargs.map(b => braces(toDoc(b))))
      case NeutralStmt.App(label, targs, vargs, bargs) =>
        // Format as: l1[T1, T2](r1, r2)
        toDoc(label) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma)) <> hsep(bargs.map(b => braces(toDoc(b))))

      case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) =>
        // Format as: l1[T1, T2](r1, r2)
        toDoc(label) <> "." <> toDoc(method) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma)) <> hsep(bargs.map(b => braces(toDoc(b))))

      case NeutralStmt.Reset(prompt, body) =>
        "reset" <+> braces(toDoc(prompt) <+> "=>" <+> nest(line <> toDoc(body.bindings) <> toDoc(body.body)) <> line)

      case NeutralStmt.Shift(prompt, capt, k, body) =>
        "shift" <> parens(toDoc(prompt)) <+> braces(toDoc(k) <+> "=>" <+> nest(line <> toDoc(body.bindings) <> toDoc(body.body)) <> line)

      case NeutralStmt.Resume(k, body) =>
        "resume" <> parens(toDoc(k)) <+> toDoc(body)

      case NeutralStmt.Var(id, init, body) =>
        "var" <+> toDoc(id.id) <+> "=" <+> toDoc(init) <> line <> toDoc(body.bindings) <> toDoc(body.body)

      case NeutralStmt.Put(ref, tpe, cap, value, body) =>
        toDoc(ref) <+> ":=" <+> toDoc(value) <> line <> toDoc(body.bindings) <>  toDoc(body.body)

      case NeutralStmt.Region(id, body) =>
        "region" <+> toDoc(id) <+> toDoc(body)

      case NeutralStmt.Alloc(id, init, region, body) =>
        "var" <+> toDoc(id) <+> "in" <+> toDoc(region) <+> "=" <+> toDoc(init) <> line <> toDoc(body.bindings) <> toDoc(body.body)

      case NeutralStmt.Hole(span) => "hole()"
    }

    def toDoc(id: Id): Doc = id.show

    def toDoc(value: Value): Doc = value match {
      // case Value.Var(id, tpe) => toDoc(id)

      case Value.Extern(callee, targs, vargs) =>
        toDoc(callee.id) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma))

      case Value.Literal(value, _) => util.show(value)

      case Value.Make(data, tag, targs, vargs) =>
        "make" <+> toDoc(data) <+> toDoc(tag) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma))

      case Value.Box(body, tpe) =>
        "box" <+> braces(nest(line <> toDoc(body) <> line))

      case Value.Var(id, tpe) => toDoc(id)
    }

    def toDoc(block: Block): Doc = block match {
      case Block(tparams, vparams, bparams, body) =>
        (if (tparams.isEmpty) emptyDoc else brackets(hsep(tparams.map(toDoc), comma))) <>
          parens(hsep(vparams.map(toDoc), comma)) <> hsep(bparams.map(toDoc)) <+> toDoc(body)
    }

    def toDoc(comp: Computation): Doc = comp match {
      case Computation.Var(id) => toDoc(id)
      case Computation.Def(closure) => toDoc(closure)
      case Computation.Continuation(k) => ???
      case Computation.New(interface, operations) => "new" <+> toDoc(interface) <+> braces {
        hsep(operations.map { case (id, impl) => "def" <+> toDoc(id) <+> "=" <+> toDoc(impl) }, ",")
      }
    }
    def toDoc(closure: Closure): Doc = closure match {
      case Closure(label, env) => toDoc(label) <+> "@" <+> brackets(hsep(env.map(toDoc), comma))
    }

    def toDoc(bindings: Bindings): Doc =
      hcat(bindings.map {
        case (addr, Binding.Let(value)) => "let" <+> toDoc(addr) <+> "=" <+> toDoc(value) <> line
        case (addr, Binding.Def(block)) => "def" <+> toDoc(addr) <+> "=" <+> toDoc(block) <> line
        case (addr, Binding.Rec(block, tpe, capt)) => "def" <+> toDoc(addr) <+> "=" <+> toDoc(block) <> line
        case (addr, Binding.Val(stmt))  => "val" <+> toDoc(addr) <+> "=" <+> toDoc(stmt) <> line
        case (addr, Binding.Run(callee, targs, vargs, bargs)) => "let !" <+> toDoc(addr) <+> "=" <+> toDoc(callee.id) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma)) <> hcat(bargs.map(b => braces { toDoc(b) })) <> line
        case (addr, Binding.Unbox(innerAddr, tpe, capt)) => "def" <+> toDoc(addr) <+> "=" <+> "unbox" <+> toDoc(innerAddr) <> line
        case (addr, Binding.Get(ref, tpe, cap)) => "let" <+> toDoc(addr) <+> "=" <+> "!" <> toDoc(ref) <> line
      })

    def toDoc(block: BasicBlock): Doc =
      braces(nest(line <> toDoc(block.bindings) <> toDoc(block.body)) <> line)

    def toDoc(p: ValueParam): Doc = toDoc(p.id) <> ":" <+> toDoc(p.tpe)
    def toDoc(p: BlockParam): Doc = braces(toDoc(p.id))

    def toDoc(t: ValueType): Doc = util.show(t)
    def toDoc(t: BlockType): Doc = util.show(t)

    def show(stmt: NeutralStmt): String = pretty(toDoc(stmt), 80).layout
    def show(value: Value): String = pretty(toDoc(value), 80).layout
    def show(block: Block): String = pretty(toDoc(block), 80).layout
    def show(bindings: Bindings): String = pretty(toDoc(bindings), 80).layout
  }

}

/**
 * A new normalizer that is conservative (avoids code bloat)
 */
class NewNormalizer {

  import semantics.*

  // used for potentially recursive definitions
  def evaluateRecursive(id: Id, block: core.BlockLit, escaping: Stack)(using env: Env, scope: Scope): Computation =
    block match {
      case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        val freshened = Id(id)

        // we keep the params as they are for now...
        given localEnv: Env = env
          .bindValue(vparams.map(p => p.id -> p.id))
          .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))
          // Assume that we capture nothing
          .bindComputation(id, Computation.Def(Closure(freshened, Nil)))

        val normalizedBlock = scope.local {
          Block(tparams, vparams, bparams, nested {
            evaluate(body, Frame.Return, Stack.Unknown)(using localEnv)
          })
        }

        val closureParams = escaping.bound.filter { p => normalizedBlock.free contains p.id }

        // Only normalize again if we actually we wrong in our assumption that we capture nothing
        // We might run into exponential complexity for nested recursive functions
        if (closureParams.isEmpty) {
          scope.defineRecursive(freshened, normalizedBlock.copy(bparams = normalizedBlock.bparams), block.tpe, block.capt)
          Computation.Def(Closure(freshened, Nil))
        } else {
          val captures = closureParams.map { p => Computation.Var(p.id): Computation.Var }
          given localEnv1: Env = env
            .bindValue(vparams.map(p => p.id -> p.id))
            .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))
            .bindComputation(id, Computation.Def(Closure(freshened, captures)))

          val normalizedBlock1 = Block(tparams, vparams, bparams, nested {
            evaluate(body, Frame.Return, Stack.Unknown)(using localEnv1)
          })

          val tpe: BlockType.Function = block.tpe match {
            case _: BlockType.Interface => ???
            case ftpe: BlockType.Function => ftpe
          }
          scope.defineRecursive(
            freshened,
            normalizedBlock1.copy(bparams = normalizedBlock1.bparams ++ closureParams),
            tpe.copy(cparams = tpe.cparams ++ closureParams.map { p => p.id }),
            block.capt
          )
          Computation.Def(Closure(freshened, captures))
        }

    }

  // the stack here is not the one this is run in, but the one the definition potentially escapes
  def evaluate(block: core.Block, hint: String, escaping: Stack)(using env: Env, scope: Scope): Computation = block match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      env.lookupComputation(id)
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      // we keep the params as they are for now...
      given localEnv: Env = env
        .bindValue(vparams.map(p => p.id -> p.id))
        .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))

      val normalizedBlock = Block(tparams, vparams, bparams, nested {
        evaluate(body, Frame.Return, Stack.Unknown)
      })

      val closureParams = escaping.bound.filter { p => normalizedBlock.free contains p.id }

      val f = Id(hint)
      scope.define(f, normalizedBlock.copy(bparams = normalizedBlock.bparams ++ closureParams))
      Computation.Def(Closure(f, closureParams.map(p => Computation.Var(p.id))))

    case core.Block.Unbox(pure) =>
      val addr = evaluate(pure, escaping)
      scope.lookupValue(addr) match {
        case Some(Value.Box(body, _)) => body
        case Some(_) | None => {
          val (tpe, capt) = pure.tpe match {
            case Boxed(tpe, capt) => (tpe, capt)
            case _ => sys error "should not happen"
          }
          // TODO translate static capture set capt to a dynamic capture set (e.g. {exc} -> {@p_17})
          val unboxAddr = scope.unbox(addr, tpe, capt)
          Computation.Var(unboxAddr)
        }
      }

    case core.Block.New(Implementation(interface, operations)) =>
      val ops = operations.map {
        case Operation(name, tparams, cparams, vparams, bparams, body) =>
          // Check whether the operation is already "just" an eta expansion and then use the identifier...
          //   no need to create a fresh block literal
          val eta: Option[Closure] =
            body match {
              case Stmt.App(BlockVar(id, _, _), targs, vargs, bargs) =>
                def sameTargs = targs == tparams.map(t => ValueType.Var(t))
                def sameVargs = vargs == vparams.map(p => ValueVar(p.id, p.tpe))
                def sameBargs = bargs == bparams.map(p => BlockVar(p.id, p.tpe, p.capt))
                def isEta = sameTargs && sameVargs && sameBargs

                env.lookupComputation(id) match {
                  // TODO what to do with closure environment
                  case Computation.Def(closure) if isEta => Some(closure)
                  case _ => None
                }
              case _ => None
            }

          val closure = eta.getOrElse {
            evaluate(core.Block.BlockLit(tparams, cparams, vparams, bparams, body), name.name.name, escaping) match {
              case Computation.Def(closure) => closure
              case _ => sys error "Should not happen"
            }
          }
          (name, closure)
      }
      Computation.New(interface, ops)
  }

  def evaluate(expr: Expr, escaping: Stack)(using env: Env, scope: Scope): Addr = expr match {
    case Expr.ValueVar(id, annotatedType) =>
      env.lookupValue(id)

    case core.Expr.Literal(value, annotatedType) =>
      scope.allocate("x", Value.Literal(value, annotatedType))

    // right now everything is stuck... no constant folding ...
    case core.Expr.PureApp(f, targs, vargs) =>
      scope.allocate("x", Value.Extern(f, targs, vargs.map(evaluate(_, escaping))))

    case core.Expr.Make(data, tag, targs, vargs) =>
      scope.allocate("x", Value.Make(data, tag, targs, vargs.map(evaluate(_, escaping))))

    case core.Expr.Box(b, annotatedCapture) =>
      /*
      var counter = 22;
      val p : Borrowed[Int] at counter = box new Borrowed[Int] {
        def dereference() = counter
       };
       counter = counter + 1;
       println(p.dereference)
      */
      // should capture `counter` but does not since the stack is Stack.Unknown
      // (effekt.JavaScriptTests.examples/pos/capture/borrows.effekt (js))
      // TLDR we need to pass an escaping stack to do a proper escape analysis. Stack.Unkown is insufficient
      val comp = evaluate(b, "x", escaping)
      scope.allocate("x", Value.Box(comp, annotatedCapture))
  }

  // TODO make evaluate(stmt) return BasicBlock (won't work for shift or reset, though)
  def evaluate(stmt: Stmt, k: Frame, ks: Stack)(using env: Env, scope: Scope): NeutralStmt = stmt match {

    case Stmt.Return(expr) =>
      k.ret(ks, evaluate(expr, ks))

    case Stmt.Val(id, annotatedTpe, binding, body) =>
      evaluate(binding, k.push(annotatedTpe) { scope => res => k => ks =>
        given Scope = scope
        bind(id, res) { evaluate(body, k, ks) }
      }, ks)

    case Stmt.ImpureApp(id, f, targs, vargs, bargs, body) =>
      assert(bargs.isEmpty)
      val addr = scope.run("x", f, targs, vargs.map(evaluate(_, ks)), bargs.map(evaluate(_, "f", Stack.Unknown)))
      evaluate(body, k, ks)(using env.bindValue(id, addr), scope)

    case Stmt.Let(id, annotatedTpe, binding, body) =>
      bind(id, evaluate(binding, ks)) { evaluate(body, k, ks) }

    // can be recursive
    case Stmt.Def(id, block: core.BlockLit, body) =>
      bind(id, evaluateRecursive(id, block, ks)) { evaluate(body, k, ks) }

    case Stmt.Def(id, block, body) =>
      bind(id, evaluate(block, id.name.name, ks)) { evaluate(body, k, ks) }

    case Stmt.App(core.Block.BlockLit(tparams, cparams, vparams, bparams, body), targs, vargs, bargs) =>
      // TODO also bind type arguments in environment
      // TODO substitute cparams???
      val newEnv = env
        .bindValue(vparams.zip(vargs).map { case (p, a) => p.id -> evaluate(a, ks) })
        .bindComputation(bparams.zip(bargs).map { case (p, a) => p.id -> evaluate(a, "f", ks) })

      evaluate(body, k, ks)(using newEnv, scope)

    case Stmt.App(callee, targs, vargs, bargs) =>
      evaluate(callee, "f", ks) match {
        case Computation.Var(id) =>
          reify(k, ks) { NeutralStmt.App(id, targs, vargs.map(evaluate(_, ks)), bargs.map(evaluate(_, "f", ks))) }
        case Computation.Def(Closure(label, environment)) =>
          val args = vargs.map(evaluate(_, ks))
          /*
          try {
            prog {
              do Eff()
            }
          } with Eff { ... }
          ---
          val captures = stack.bound.filter { block.free }
          is incorrect as the result is always the empty capture set since Stack.Unkown.bound = Set()
           */
          val blockargs = bargs.map(evaluate(_, "f", ks))
          // if stmt doesn't capture anything, it can not make any changes to the stack (ks) and we don't have to pretend it is unknown as an over-approximation
          // TODO examples/pos/lambdas/localstate.effekt fails if we only check stmt.capt
          // TODO capture {io, global} is also fine
          if (stmt.capt.isEmpty && environment.isEmpty) {
            reifyKnown(k, ks) {
              NeutralStmt.Jump(label, targs, args, blockargs)
            }
          } else {
            reify(k, ks) {
              NeutralStmt.Jump(label, targs, args, blockargs ++ environment)
            }
          }
        case _: (Computation.New | Computation.Continuation) => sys error "Should not happen"
      }

    // case Stmt.Invoke(New)

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      val escapingStack = Stack.Unknown
      evaluate(callee, "o", escapingStack) match {
        case Computation.Var(id) =>
          reify(k, ks) { NeutralStmt.Invoke(id, method, methodTpe, targs, vargs.map(evaluate(_, ks)), bargs.map(evaluate(_, "f", escapingStack))) }
        case Computation.New(interface, operations) =>
          operations.collectFirst { case (id, Closure(label, environment)) if id == method =>
            reify(k, ks) { NeutralStmt.Jump(label, targs, vargs.map(evaluate(_, ks)), bargs.map(evaluate(_, "f", escapingStack)) ++ environment) }
          }.get
        case _: (Computation.Def | Computation.Continuation) => sys error s"Should not happen"
      }

    case Stmt.If(cond, thn, els) =>
      val sc = evaluate(cond, ks)
      scope.lookupValue(sc) match {
        case Some(Value.Literal(true, _)) => evaluate(thn, k, ks)
        case Some(Value.Literal(false, _)) => evaluate(els, k, ks)
        case _ =>
          joinpoint(k, ks) { (k, ks) =>
            NeutralStmt.If(sc, nested {
              evaluate(thn, k, ks)
            }, nested {
              evaluate(els, k, ks)
            })
          }
      }

    case Stmt.Match(scrutinee, clauses, default) =>
      val sc = evaluate(scrutinee, ks)
      scope.lookupValue(sc) match {
        case Some(Value.Make(data, tag, targs, vargs)) =>
          // TODO substitute types (or bind them in the env)!
          clauses.collectFirst {
            case (tpe, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) if tpe == tag =>
              bind(vparams.map(_.id).zip(vargs)) { evaluate(body, k, ks) }
          }.getOrElse {
            evaluate(default.getOrElse { sys.error("Non-exhaustive pattern match.") }, k, ks)
          }
        // linear usage of the continuation
        //        case _ if (clauses.size + default.size) <= 1 =>
        //          NeutralStmt.Match(sc,
        //            clauses.map { case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        //              given localEnv: Env = env.bindValue(vparams.map(p => p.id -> p.id))
        //              val block = Block(tparams, vparams, bparams, nested {
        //                evaluate(body, k, ks)
        //              })
        //              (id, block)
        //            },
        //            default.map { stmt => nested { evaluate(stmt, k, ks) } })
        case _ =>
          joinpoint(k, ks) { (k, ks) =>
            NeutralStmt.Match(sc,
              // This is ALMOST like evaluate(BlockLit), but keeps the current continuation
              clauses.map { case (id, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) =>
                given localEnv: Env = env.bindValue(vparams.map(p => p.id -> p.id))
                val block = Block(tparams, vparams, bparams, nested {
                  evaluate(body, k, ks)
                })
                (id, block)
              },
              default.map { stmt => nested { evaluate(stmt, k, ks) } })
          }
      }

    case Stmt.Hole(span) => NeutralStmt.Hole(span)

    // State
    case Stmt.Region(BlockLit(Nil, List(capture), Nil, List(cap), body)) =>
      given Env = env.bindComputation(cap.id, Computation.Var(cap.id))
      evaluate(body, Frame.Return, Stack.Region(cap, Map.empty, k, ks))
    case Stmt.Region(_) => ???
    case Stmt.Alloc(id, init, region, body) =>
      val addr = evaluate(init, ks)
      val bp = BlockParam(id, Type.TState(init.tpe), Set(region))
      alloc(bp, region, addr, ks) match {
        case Some(ks1) => evaluate(body, k, ks1)
        case None => NeutralStmt.Alloc(bp, addr, region, nested { evaluate(body, k, ks) })
      }

    case Stmt.Var(ref, init, capture, body) =>
      val addr = evaluate(init, ks)
      evaluate(body, Frame.Return, Stack.Var(BlockParam(ref, Type.TState(init.tpe), Set(capture)), addr, k, ks))
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      get(ref, ks) match {
        case Some(addr) => bind(id, addr) { evaluate(body, k, ks) }
        case None => bind(id, scope.allocateGet(ref, annotatedTpe, annotatedCapt)) { evaluate(body, k, ks) }
      }
    case Stmt.Put(ref, annotatedCapt, value, body) =>
      val addr = evaluate(value, ks)
      put(ref, addr, ks) match {
        case Some(stack) => evaluate(body, k, stack)
        case None =>
          NeutralStmt.Put(ref, value.tpe, annotatedCapt, addr, nested { evaluate(body, k, ks) })
      }

    // Control Effects
    case Stmt.Shift(prompt, core.Block.BlockLit(Nil, cparam :: Nil, Nil, k2 :: Nil, body)) =>
      val p = env.lookupComputation(prompt.id) match {
        case Computation.Var(id) => id
        case _ => ???
      }

      if (ks.bound.exists { other => other.id == p }) {
        val (cont, frame, stack) = shift(p, k, ks)
        given Env = env.bindComputation(k2.id -> Computation.Continuation(cont) :: Nil)
        evaluate(body, frame, stack)
      } else {
        val neutralBody = {
          given Env = env.bindComputation(k2.id -> Computation.Var(k2.id) :: Nil)
          nested {
            evaluate(body, Frame.Return, Stack.Unknown)
          }
        }
        assert(Set(cparam) == k2.capt, "At least for now these need to be the same")
        reify(k, ks) { NeutralStmt.Shift(p, cparam, k2, neutralBody) }
      }
    case Stmt.Shift(_, _) => ???
    //case Stmt.Reset(BlockLit(Nil, cparams, Nil, prompt :: Nil, body)) =>
    //      // TODO is Var correct here?? Probably needs to be a new computation value...
    //      //   but shouldn't it be a fresh prompt each time?
    //      val p = Id(prompt.id)
    //      val neutralBody = {
    //        given Env = env.bindComputation(prompt.id -> Computation.Var(p) :: Nil)
    //        nested {
    //          evaluate(body, MetaStack.Empty)
    //        }
    //      }
    //      // TODO implement properly
    //      k.reify(NeutralStmt.Reset(BlockParam(p, prompt.tpe, prompt.capt), neutralBody))


    case Stmt.Reset(core.Block.BlockLit(Nil, cparams, Nil, prompt :: Nil, body)) =>
      val p = Id(prompt.id)
      // TODO is Var correct here?? Probably needs to be a new computation value...
      given Env = env.bindComputation(prompt.id -> Computation.Var(p) :: Nil)
      evaluate(body, Frame.Return, Stack.Reset(BlockParam(p, prompt.tpe, prompt.capt), k, ks))

    case Stmt.Reset(_) => ???
    case Stmt.Resume(k2, body) =>
      env.lookupComputation(k2.id) match {
        case Computation.Var(r) =>
          reify(k, ks) {
            NeutralStmt.Resume(r, nested {
              evaluate(body, Frame.Return, Stack.Unknown)
            })
          }
        case Computation.Continuation(k3) =>
          val (k4, ks4) = resume(k3, k, ks)
          evaluate(body, k4, ks4)
        case _ => ???
      }
  }

  def run(mod: ModuleDecl): ModuleDecl = {
    //util.trace(mod)
    // TODO deal with async externs properly (see examples/benchmarks/input_output/dyck_one.effekt)
    val asyncExterns = mod.externs.collect { case defn: Extern.Def if defn.annotatedCapture.contains(AsyncCapability.capture) => defn }
    val asyncTypes = asyncExterns.map { d =>
      d.id -> (BlockType.Function(d.tparams, d.cparams, d.vparams.map { _.tpe }, d.bparams.map { bp => bp.tpe }, d.ret), d.annotatedCapture)
    }

    val toplevelEnv = Env.empty
      // user-defined functions
      .bindComputation(mod.definitions.collect {
        case Toplevel.Def(id, b) => id -> (b match {
          case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) => Computation.Def(Closure(id, Nil))
          case core.Block.BlockVar(idd, annotatedTpe, annotatedCapt) => Computation.Var(id)
          case core.Block.Unbox(pure) => Computation.Var(id)
          case core.Block.New(impl) => Computation.Var(id)
        })
      })
      // user-defined values
      .bindValue(mod.definitions.collect {
        case Toplevel.Val(id, _, _) => id -> id
      })
      // async extern functions
      .bindComputation(asyncExterns.map(defn => defn.id -> Computation.Var(defn.id)))

    val typingContext = TypingContext(
      mod.definitions.collect {
        case Toplevel.Val(id, tpe, _) => id -> tpe
      }.toMap,
      mod.definitions.collect {
        case Toplevel.Def(id, b) => id -> (b.tpe, b.capt)
      }.toMap ++ asyncTypes
    )

    val newDefinitions = mod.definitions.map(d => run(d)(using toplevelEnv, typingContext))
    mod.copy(definitions = newDefinitions)
  }

  val showDebugInfo = false
  inline def debug(inline msg: => Any) = if (showDebugInfo) println(msg) else ()

  def run(defn: Toplevel)(using env: Env, G: TypingContext): Toplevel = defn match {
    case Toplevel.Def(id, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) =>
      debug(s"------- ${util.show(id)} -------")
      debug(util.show(body))

      given localEnv: Env = env
        .bindValue(vparams.map(p => p.id -> p.id))
        .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))

      given scope: Scope = Scope.empty
      val result = evaluate(body, Frame.Return, Stack.Empty)

      debug(s"----------normalized-----------")
      val block = Block(tparams, vparams, bparams, reifyBindings(scope, result))
      debug(PrettyPrinter.show(block))

      debug(s"----------embedded-----------")
      val embedded = embedBlockLit(block)
      debug(util.show(embedded))

      Toplevel.Def(id, embedded)
    case other => other
  }

  case class TypingContext(values: Map[Addr, ValueType], blocks: Map[Label, (BlockType, Captures)]) {
    def bind(id: Id, tpe: ValueType): TypingContext = this.copy(values = values + (id -> tpe))
    def bind(id: Id, tpe: BlockType, capt: Captures): TypingContext = this.copy(blocks = blocks + (id -> (tpe, capt)))
    def bindValues(vparams: List[ValueParam]): TypingContext = this.copy(values = values ++ vparams.map(p => p.id -> p.tpe))
    def lookupValue(id: Id): ValueType = values.getOrElse(id, sys.error(s"Unknown value: ${util.show(id)}"))
    def bindComputations(bparams: List[BlockParam]): TypingContext = this.copy(blocks = blocks ++ bparams.map(p => p.id -> (p.tpe, p.capt)))
    def bindComputation(bparam: BlockParam): TypingContext = this.copy(blocks = blocks + (bparam.id -> (bparam.tpe, bparam.capt)))
  }

  def embedStmt(neutral: NeutralStmt)(using G: TypingContext): core.Stmt = neutral match {
    case NeutralStmt.Return(result) =>
      Stmt.Return(embedExpr(result))
    case NeutralStmt.Jump(label, targs, vargs, bargs) =>
      Stmt.App(embedBlockVar(label), targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.App(label, targs, vargs, bargs) =>
      Stmt.App(embedBlockVar(label), targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) =>
      Stmt.Invoke(embedBlockVar(label), method, tpe, targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.If(cond, thn, els) =>
      Stmt.If(embedExpr(cond), embedStmt(thn), embedStmt(els))
    case NeutralStmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(embedExpr(scrutinee),
        clauses.map { case (id, block) => id -> embedBlockLit(block) },
        default.map(embedStmt))
    case NeutralStmt.Reset(prompt, body) =>
      val capture = prompt.capt match {
        case set if set.size == 1 => set.head
        case _ => sys error "Prompt needs to have a single capture"
      }
      Stmt.Reset(core.BlockLit(Nil, capture :: Nil, Nil, prompt :: Nil, embedStmt(body)(using G.bindComputation(prompt))))
    case NeutralStmt.Shift(prompt, capt, k, body) =>
      Stmt.Shift(embedBlockVar(prompt), core.BlockLit(Nil, capt :: Nil, Nil, k :: Nil, embedStmt(body)(using G.bindComputation(k))))
    case NeutralStmt.Resume(k, body) =>
      Stmt.Resume(embedBlockVar(k), embedStmt(body))
    case NeutralStmt.Var(blockParam, init, body) =>
      val capt = blockParam.capt match {
        case cs if cs.size == 1 => cs.head
        case _ => sys error "Variable needs to have a single capture"
      }
      Stmt.Var(blockParam.id, embedExpr(init), capt, embedStmt(body)(using G.bind(blockParam.id, blockParam.tpe, blockParam.capt)))
    case NeutralStmt.Put(ref, annotatedTpe, annotatedCapt, value, body) =>
      Stmt.Put(ref, annotatedCapt, embedExpr(value), embedStmt(body))
    case NeutralStmt.Region(id, body) =>
      Stmt.Region(BlockLit(Nil, List(id.id), Nil, List(id), embedStmt(body)(using G.bindComputation(id))))
    case NeutralStmt.Alloc(blockparam, init, region, body) =>
      Stmt.Alloc(blockparam.id, embedExpr(init), region, embedStmt(body)(using G.bind(blockparam.id, blockparam.tpe, blockparam.capt)))
    case NeutralStmt.Hole(span) =>
      Stmt.Hole(span)
  }

  def embedStmt(basicBlock: BasicBlock)(using G: TypingContext): core.Stmt = basicBlock match {
    case BasicBlock(bindings, stmt) =>
      bindings.foldRight((G: TypingContext) => embedStmt(stmt)(using G)) {
        case ((id, Binding.Let(value)), rest) => G =>
          val coreExpr = embedExpr(value)(using G)
          // TODO why do we even have this type in core, if we always infer it?
          Stmt.Let(id, coreExpr.tpe, coreExpr, rest(G.bind(id, coreExpr.tpe)))
        case ((id, Binding.Def(block)), rest) => G =>
          val coreBlock = embedBlock(block)(using G)
          Stmt.Def(id, coreBlock, rest(G.bind(id, coreBlock.tpe, coreBlock.capt)))
        case ((id, Binding.Rec(block, tpe, capt)), rest) => G =>
          val coreBlock = embedBlock(block)(using G.bind(id, tpe, capt))
          Stmt.Def(id, coreBlock, rest(G.bind(id, coreBlock.tpe, coreBlock.capt)))
        case ((id, Binding.Val(stmt)), rest) => G =>
          val coreStmt = embedStmt(stmt)(using G)
          Stmt.Val(id, coreStmt.tpe, coreStmt, rest(G.bind(id, coreStmt.tpe)))
        case ((id, Binding.Run(callee, targs, vargs, bargs)), rest) => G =>
          val vargs1 = vargs.map(arg => embedExpr(arg)(using G))
          val bargs1 = bargs.map(arg => embedBlock(arg)(using G))
          val tpe = Type.bindingType(callee, targs, vargs1, bargs1)
          core.ImpureApp(id, callee, targs, vargs1, bargs1, rest(G.bind(id, tpe)))
        case ((id, Binding.Unbox(addr, tpe, capt)), rest) => G =>
          val pureValue = embedExpr(addr)(using G)
          Stmt.Def(id, core.Block.Unbox(pureValue), rest(G.bind(id, tpe, capt)))
        case ((id, Binding.Get(ref, tpe, cap)), rest) => G =>
          Stmt.Get(id, tpe, ref, cap, rest(G.bind(id, tpe)) )
      }(G)
  }

  def embedExpr(value: Value)(using TypingContext): core.Expr = value match {
    case Value.Extern(callee, targs, vargs) => Expr.PureApp(callee, targs, vargs.map(embedExpr))
    case Value.Literal(value, annotatedType) => Expr.Literal(value, annotatedType)
    case Value.Make(data, tag, targs, vargs) => Expr.Make(data, tag, targs, vargs.map(embedExpr))
    case Value.Box(body, annotatedCapture) => Expr.Box(embedBlock(body), annotatedCapture)
    case Value.Var(id, annotatedType) => Expr.ValueVar(id, annotatedType)
  }

  def embedExpr(addr: Addr)(using G: TypingContext): core.Expr = Expr.ValueVar(addr, G.lookupValue(addr))

  def embedBlock(comp: Computation)(using G: TypingContext): core.Block = comp match {
    case Computation.Var(id) =>
      embedBlockVar(id)
    case Computation.Def(Closure(label, Nil)) =>
      embedBlockVar(label)
    case Computation.Def(closure) =>
      etaExpandToBlockLit(closure)
    case Computation.Continuation(k) => ???
    case Computation.New(interface, operations) =>
      val ops = operations.map { etaExpandToOperation.tupled }
      core.Block.New(Implementation(interface, ops))
  }

  /**
   * Embed `Computation.Def` to a `core.BlockLit`
   * This eta-expands the block var that stands for the `Computation.Def` to a full block literal
   * so that we can supply the correct capture arguments from the environment.
   */
  def etaExpandToBlockLit(closure: Closure)(using G: TypingContext): core.BlockLit = {
    val Closure(label, environment) = closure
    val blockvar = embedBlockVar(label)
    G.blocks(label) match {
      // TODO why is `captures` unused?
      case (BlockType.Function(tparams, cparams, vparams, bparams, result), captures) =>
        val vps = vparams.map { p => core.ValueParam(Id("x"), p) }
        val vargs = vps.map { vp => core.Expr.ValueVar(vp.id, vp.tpe) }

        // this uses the invariant that we _append_ all environment captures to the bparams
        val (origCapts, synthCapts) = cparams.splitAt(bparams.length - environment.length)
        val (origBparams, synthBparams) = bparams.splitAt(bparams.length - environment.length)
        val origBps = origBparams.zip(origCapts).map { case (bp, c) => core.BlockParam(Id("f"), bp, Set(c)) }
        val origBargs = origBps.map { bp => core.BlockVar(bp.id, bp.tpe, bp.capt) }
        val synthBargs = environment.zip(synthBparams).zip(synthCapts).map {
          case ((Computation.Var(id), bp), c) => core.BlockVar(id, bp, Set(c))
        }
        val bargs = origBargs ++ synthBargs

        val targs = tparams.map { core.ValueType.Var.apply }

        core.Block.BlockLit(
          tparams,
          origCapts,
          vps,
          origBps,
          Stmt.App(
            blockvar,
            targs,
            vargs,
            bargs
          )
        )
      case _ => sys.error("Unexpected block type for a closure")
    }
  }

  /**
   * Embed an operation as part of a `Computation.New`.
   * This eta-expands the block var that stands for the operation body to a full operation
   * so that we can supply the correct capture arguments from the environment.
   */
  def etaExpandToOperation(id: Id, closure: Closure)(using G: TypingContext): core.Operation = {
   val Closure(label, environment) = closure
    G.blocks(label) match {
      case (BlockType.Function(tparams, cparams, vparams, bparams, result), captures) =>
        val tparams2 = tparams.map(t => Id(t))
        // TODO if we freshen cparams, then we also need to substitute them in the result AND the parameters
        val cparams2 = cparams //.map(c => Id(c))
        val vparams2 = vparams.map(t => ValueParam(Id("x"), t))
        val bparams2 = (bparams zip cparams).map { case (t, c) => BlockParam(Id("f"), t, Set(c)) }
        // In the following section, we create a new instance of the interface.
        // All operation bodies were lifted to block literals in an earlier stage.
        // While doing so, their block parameters (bparams) were concatenated with their capture parameters (cparams).
        // When we embed back to core, we need to "eta-expand" the operation body to supply the correct captures from the environment.
        // To see why this "eta-expansion" is necessary to achieve this, consider the following example:
        // ```scala
        // effect Eff(): Unit
        // def use = { do Eff() }
        // def main() = {
        //     val r = try {
        //         use()
        //     } with Eff {
        //         resume(())
        //     }
        // }
        // ```
        // the handler body normalizes to the following:
        // ```scala
        // reset {{p} =>
        //     jump use(){new Eff {def Eff = Eff @ [p]}}
        // }
        // ```
        // where
        // ```
        //  def Eff = (){p} { ... }
        // ```
        // In particular, the prompt `p` needs to be passed to the lifted operation body.
        // ```
        val (origBparams, synthBparams) = bparams2.splitAt(bparams2.length - environment.length)
        val bargs =
          // TODO: Fix captures
          origBparams.map { case bp => BlockVar(bp.id, bp.tpe, Set()) } ++
            synthBparams.zip(environment).map {
              // TODO: Fix captures
              case (bp, Computation.Var(id)) => BlockVar(id, bp.tpe, Set())
            }

        core.Operation(
          id,
          tparams2,
          cparams.take(cparams.length - environment.length),
          vparams2,
          origBparams,
          Stmt.App(
            embedBlockVar(label),
            tparams2.map(ValueType.Var.apply),
            vparams2.map(p => ValueVar(p.id, p.tpe)),
            bargs
          )
        )
      case _ => sys error "Unexpected block type"
    }
  }

  def embedBlock(block: Block)(using G: TypingContext): core.Block = block match {
    case Block(tparams, vparams, bparams, b) =>
      val cparams = bparams.map {
        case BlockParam(id, tpe, captures) =>
          assert(captures.size == 1)
          captures.head
      }
      core.Block.BlockLit(tparams, cparams, vparams, bparams,
        embedStmt(b)(using G.bindValues(vparams).bindComputations(bparams)))
  }

  def embedBlockLit(block: Block)(using G: TypingContext): core.BlockLit = embedBlock(block).asInstanceOf[core.BlockLit]

  def embedBlockVar(label: Label)(using G: TypingContext): core.BlockVar =
    val (tpe, capt) = G.blocks.getOrElse(label, sys error s"Unknown block: ${util.show(label)}. ${G.blocks.keys.map(util.show).mkString(", ")}")
    core.BlockVar(label, tpe, capt)
}
