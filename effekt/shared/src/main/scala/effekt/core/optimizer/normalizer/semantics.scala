package effekt
package core
package optimizer
package normalizer

import effekt.core.optimizer.normalizer.semantics.PrettyPrinter.{braces, brackets, comma, emptyDoc, hcat, hsep, line, nest, parens, pretty}
import effekt.core.{BlockVar, Capture, Captures, Id}
import effekt.source.Span
import kiama.output.ParenPrettyPrinter

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object semantics {

  // Values
  // ------

  type Addr = Id
  type Label = Id
  type Prompt = Id

  // this could not only compute free variables, but also usage information to guide the inliner (see "secrets of the ghc inliner")
  type Variables = Set[Id]
  def all[A](ts: List[A], f: A => Variables): Variables = ts.flatMap(f).toSet

  type Neutral = Value.Var | Value.PureExtern

  enum Value {
    // Stuck (neutrals)
    case Var(id: Id, annotatedType: ValueType)
    case PureExtern(f: BlockVar, targs: List[ValueType], vargs: List[Addr])

    // Values with specialized representation for algebraic simplification
    case Integer(value: theories.integers.IntegerRep)
    case String(value: theories.strings.StringRep)

    // Fallback literal for other values types without special representation
    case Literal(value: Any, annotatedType: ValueType)

    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], vargs: List[Addr])

    // TODO use dynamic captures
    case Box(body: Computation, annotatedCaptures: Set[effekt.symbols.Symbol])

    val dynamicCapture: Variables = Set.empty

    val free: Variables = this match {
      case Value.Var(id, annotatedType) => Set(id)
      case Value.PureExtern(id, targs, vargs) => vargs.toSet
      case Value.Literal(value, annotatedType) => Set.empty
      case Value.Integer(value) => value.free
      case Value.String(value) => value.free
      case Value.Make(data, tag, targs, vargs) => vargs.toSet
      // Box abstracts over all free computation variables, only when unboxing, they occur free again
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
      // TODO block args for externs are not supported (for now?)
      case Binding.Run(f, targs, vargs, bargs) => vargs.toSet
      case Binding.Unbox(addr: Addr, tpe: BlockType, capt: Captures) => Set(addr)
      case Binding.Get(ref, tpe, cap) => Set(ref)
    }

    val dynamicCapture: Variables = this match {
      case Binding.Let(value) => value.dynamicCapture
      case Binding.Def(block) => block.dynamicCapture
      case Binding.Rec(block, tpe, capt) => block.dynamicCapture
      case Binding.Val(stmt) => stmt.dynamicCapture
      // TODO block args for externs are not supported (for now?)
      case Binding.Run(f, targs, vargs, bargs) => Set.empty
      case Binding.Unbox(addr: Addr, tpe: BlockType, capt: Captures) => capt // TODO these are the static not dynamic captures
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
    def subst(ids: List[Id]): List[Id] = ids.map(subst)
    def subst(id: Id): Id = computations.get(id) match {
      case Some(Computation.Known(inner)) => inner.id
      case Some(Computation.Unknown(id)) => id
      case _ => id
    }
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
    val dynamicCapture: Variables = body.dynamicCapture -- bparams.map(_.id)
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

    val dynamicCapture: Variables = {
      body.dynamicCapture ++ bindings.flatMap(_._2.dynamicCapture)
    }
  }

  enum Computation {
    // Unknown identifiers -- stuck
    case Unknown(id: Id)
    // Known function
    case Def(closure: Closure)

    // known identifiers introduced by reset, var and region
    case Known(inner: Static[Id])

    case Continuation(k: Cont)

    case BuiltinExtern(id: Id, builtinName: String)

    // Known object
    case New(interface: BlockType.Interface, operations: List[(Id, Closure)])

    val free: Variables = this match {
      case Computation.Unknown(id) => Set(id)
      case Computation.Known(inner) => Set(inner.id)
      case Computation.Def(closure) => closure.free
      case Computation.Continuation(k) => Set.empty // TODO ???
      case Computation.New(interface, operations) => operations.flatMap(_._2.free).toSet
      case Computation.BuiltinExtern(id, vmSymbol) => Set.empty
    }

    val dynamicCapture: Variables = this match {
      case Computation.Unknown(id) => Set(id)
      case Computation.Known(inner) => Set(inner.id)
      case Computation.Def(closure) => closure.dynamicCapture
      case Computation.Continuation(k) => Set.empty // TODO ???
      case Computation.New(interface, operations) => operations.flatMap(_._2.dynamicCapture).toSet
      case Computation.BuiltinExtern(id, vmSymbol) => Set.empty
    }
  }

  enum Static[I] {
    case Prompt(id: I)
    case Reference(id: I)
    case Region(id: I)
    val id: I

    def map[A](f: I => A): Static[A] = this match {
      case Static.Prompt(id) => Static.Prompt(f(id))
      case Static.Reference(id) => Static.Reference(f(id))
      case Static.Region(id) => Static.Region(f(id))
    }
  }

  // TODO add escaping mutable variables
  case class Closure(label: Label, environment: List[Computation.Known]) {
    val free: Variables = Set(label) ++ environment.flatMap(_.free).toSet
    val dynamicCapture: Variables = environment.map(_.inner.id).toSet
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

    val dynamicCapture: Variables = this match {
      case NeutralStmt.Return(result) => Set.empty
      case NeutralStmt.Hole(span) => Set.empty

      case NeutralStmt.Jump(label, targs, vargs, bargs) => all(bargs, _.dynamicCapture)
      case NeutralStmt.App(label, targs, vargs, bargs) => all(bargs, _.dynamicCapture)
      case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) => all(bargs, _.dynamicCapture)
      case NeutralStmt.If(cond, thn, els) => thn.dynamicCapture ++ els.dynamicCapture
      case NeutralStmt.Match(scrutinee, clauses, default) => clauses.flatMap(_._2.dynamicCapture).toSet ++ default.map(_.dynamicCapture).getOrElse(Set.empty)
      case NeutralStmt.Reset(prompt, body) => body.dynamicCapture - prompt.id
      case NeutralStmt.Shift(prompt, capt, k, body) => (body.dynamicCapture - k.id) + prompt
      case NeutralStmt.Resume(k, body) => Set(k) ++ body.dynamicCapture
      case NeutralStmt.Var(id, init, body) => body.dynamicCapture - id.id
      case NeutralStmt.Put(ref, tpe, cap, value, body) => Set(ref) ++ body.dynamicCapture
      case NeutralStmt.Region(id, body) => body.dynamicCapture - id.id
      case NeutralStmt.Alloc(id, init, region, body) => Set(region) ++ body.dynamicCapture - id.id
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

    lazy val bound: List[Static[BlockParam]] = this match {
      case Stack.Empty => List.empty
      case Stack.Unknown => List.empty
      case Stack.Reset(prompt, frame, next) => Static.Prompt(prompt) :: next.bound
      case Stack.Var(id, curr, frame, next) => Static.Reference(id) :: next.bound
      case Stack.Region(id, bindings, frame, next) => Static.Region(id) :: (bindings.keys.map { k => Static.Reference(k) }.toList ++ next.bound)
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
    case Var(frame: Frame, id: BlockParam, curr: Addr, rest: Cont)
    case Region(frame: Frame, id: BlockParam, bindings: Map[BlockParam, Addr], rest: Cont)
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
    case Cont.Empty =>
      (k, ks)
    case Cont.Reset(frame, prompt, rest) =>
      val (k1, ks1) = resume(rest, k, ks)
      (frame, Stack.Reset(prompt, k1, ks1))
    case Cont.Var(frame, id, curr, rest) =>
      val (k1, ks1) = resume(rest, k, ks)
      (frame, Stack.Var(id, curr, k1, ks1))
    case Cont.Region(frame, id, bindings, rest) =>
      val (k1, ks1) = resume(rest, k, ks)
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
            val closureParams = escaping.bound.collect { case bp if body.dynamicCapture contains bp.id.id => bp }.toList
            scope.define(k, Block(Nil, ValueParam(x, tpe) :: Nil, closureParams.map(_.id), body))
            Frame.Dynamic(Closure(k, closureParams.map { bp => Computation.Known(bp.map(_.id)) }))
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
          if (body.dynamicCapture contains prompt.id) NeutralStmt.Reset(prompt, body)
          else stmt // TODO this runs normalization a second time in the outer scope!
        }}
      case Stack.Var(id, curr, frame, next) =>
        reify(next) { reify(frame) {
          val body = nested { stmt }
          if (body.dynamicCapture contains id.id) NeutralStmt.Var(id, curr, body)
          else stmt
        }}
      case Stack.Region(id, bindings, frame, next) =>
        reify(next) { reify(frame) {
          val body = nested { stmt }
          val bodyUsesBinding = body.dynamicCapture.exists(bindings.map { b => b._1.id }.toSet.contains(_))
          if (body.dynamicCapture.contains(id.id) || bodyUsesBinding) {
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

      case Value.PureExtern(callee, targs, vargs) =>
        toDoc(callee.id) <>
          (if (targs.isEmpty) emptyDoc else brackets(hsep(targs.map(toDoc), comma))) <>
          parens(hsep(vargs.map(toDoc), comma))

      case Value.Literal(value, _) => util.show(value)
      case Value.Integer(value) => value.show
      case Value.String(value) => value.show

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
      case Computation.Unknown(id) => toDoc(id)
      case Computation.Def(closure) => toDoc(closure)
      case Computation.Known(Static.Reference(id)) => "ref@" <> toDoc(id)
      case Computation.Known(Static.Region(id)) => "reg@" <> toDoc(id)
      case Computation.Known(Static.Prompt(id)) => "p@" <> toDoc(id)
      case Computation.Continuation(k) => ???
      case Computation.New(interface, operations) => "new" <+> toDoc(interface) <+> braces {
        hsep(operations.map { case (id, impl) => "def" <+> toDoc(id) <+> "=" <+> toDoc(impl) }, ",")
      }
      case Computation.BuiltinExtern(id, vmSymbol) => "extern" <+> toDoc(id) <+> "=" <+> vmSymbol
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
