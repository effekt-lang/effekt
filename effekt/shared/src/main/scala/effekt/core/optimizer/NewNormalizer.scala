package effekt
package core
package optimizer

import effekt.source.Span
import effekt.core.optimizer.semantics.NeutralStmt
import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR }
import effekt.symbols.builtins.AsyncCapability
import kiama.output.ParenPrettyPrinter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap

// TODO
//   while linearity is difficult to track bottom up, variable usage is possible
//   this way deadcode can be eliminated on the way up.
//
// plan: don't inline... this is a separate pass after normalization
object semantics {

  // Values
  // ------

  type Addr = Id
  type Label = Id
  type Prompt = Id

  enum Value {
    // Stuck
    //case Var(id: Id, annotatedType: ValueType)
    case Extern(f: BlockVar, targs: List[ValueType], vargs: List[Addr])

    // Actual Values
    case Literal(value: Any, annotatedType: ValueType)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], vargs: List[Addr])

    val free: Set[Addr] = this match {
      // case Value.Var(id, annotatedType) => Set.empty
      case Value.Extern(id, targs, vargs) => vargs.toSet
      case Value.Literal(value, annotatedType) => Set.empty
      case Value.Make(data, tag, targs, vargs) => vargs.toSet
    }
  }

  // TODO find better name for this
  enum Binding {
    case Let(value: Value)
    case Def(block: Block)
    case Rec(block: Block, tpe: BlockType, capt: Captures)
    case Val(stmt: NeutralStmt)
    case Run(f: BlockVar, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation])

    val free: Set[Addr] = this match {
      case Binding.Let(value) => value.free
      case Binding.Def(block) => block.free
      case Binding.Rec(block, tpe, capt) => block.free
      case Binding.Val(stmt) => stmt.free
      case Binding.Run(f, targs, vargs, bargs) => vargs.toSet
    }
  }

  type Bindings = List[(Id, Binding)]
  object Bindings {
    def empty: Bindings = Nil
  }
  extension (bindings: Bindings) {
    def free: Set[Id] = {
      val bound = bindings.map(_._1).toSet
      val free = bindings.flatMap { b => b._2.free }.toSet
      free -- bound
    }
  }

  /**
   * A Scope is a bit like a basic block, but without the terminator
   */
  class Scope(
    var bindings: ListMap[Id, Binding],
    var inverse: Map[Value, Id],
    outer: Option[Scope]
  ) {
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

    def run(hint: String, callee: BlockVar, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation]): Addr = {
      val addr = Id(hint)
      bindings = bindings.updated(addr, Binding.Run(callee, targs, vargs, bargs))
      addr
    }

    // TODO Option[Value] or Var(id) in Value?
    def lookupValue(addr: Addr): Option[Value] = bindings.get(addr) match {
      case Some(Binding.Let(value)) => Some(value)
      case _ => outer.flatMap(_.lookupValue(addr))
    }

    def define(hint: String, block: Block): Label =
      val label = Id(hint)
      bindings = bindings.updated(label, Binding.Def(block))
      label

    def defineRecursive(label: Label, block: Block, tpe: BlockType, capt: Captures): Label =
      bindings = bindings.updated(label, Binding.Rec(block, tpe, capt))
      label

    def push(id: Id, stmt: NeutralStmt): Unit =
      bindings = bindings.updated(id, Binding.Val(stmt))
  }
  object Scope {
    def empty: Scope = new Scope(ListMap.empty, Map.empty, None)
  }

  case class Block(tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: BasicBlock) {
    def free: Set[Addr] = body.free -- vparams.map(_.id)
  }

  case class BasicBlock(bindings: Bindings, body: NeutralStmt) {
    def free: Set[Addr] = bindings.free ++ body.free
  }

  enum Computation {
    // Unknown
    case Var(id: Id)
    // Known function
    case Def(label: Label)
    // Known object
    case New(interface: BlockType.Interface, operations: List[(Id, Label)])
  }

  // Statements
  // ----------
  enum NeutralStmt {
    // continuation is unknown
    case Return(result: Addr)
    // callee is unknown or we do not want to inline (TODO no block arguments for now)
    case App(label: Label, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation])
    // callee is unknown
    case Invoke(id: Id, method: Id, methodTpe: BlockType, targs: List[ValueType], vargs: List[Addr], bargs: List[Computation])
    // cond is unknown
    case If(cond: Addr, thn: BasicBlock, els: BasicBlock)
    // scrutinee is unknown
    case Match(scrutinee: Addr, clauses: List[(Id, Block)], default: Option[BasicBlock])

    // what's actually unknown here?
    case Reset(prompt: BlockParam, body: BasicBlock)
    // prompt / context is unknown
    case Shift(prompt: Prompt, kCapt: Capture, k: BlockParam, body: BasicBlock)
    // continuation is unknown
    case Resume(k: Id, body: BasicBlock)

    // aborts at runtime
    case Hole(span: Span)

    val free: Set[Addr] = this match {
      case NeutralStmt.App(label, targs, vargs, bargs) => vargs.toSet
      case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) => vargs.toSet
      case NeutralStmt.If(cond, thn, els) => Set(cond) ++ thn.free ++ els.free
      case NeutralStmt.Match(scrutinee, clauses, default) => Set(scrutinee) ++ clauses.flatMap(_._2.free).toSet ++ default.map(_.free).getOrElse(Set.empty)
      case NeutralStmt.Return(result) => Set(result)
      case NeutralStmt.Reset(prompt, body) => body.free
      case NeutralStmt.Shift(prompt, capt, k, body) => body.free
      case NeutralStmt.Resume(k, body) => body.free
      case NeutralStmt.Hole(span) => Set.empty
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
    }

    def toDoc(block: Block): Doc = block match {
      case Block(tparams, cparams, vparams, bparams, body) =>
        (if (tparams.isEmpty) emptyDoc else brackets(hsep(tparams.map(toDoc), comma))) <>
        parens(hsep(vparams.map(toDoc), comma)) <> hsep(bparams.map(toDoc)) <+> toDoc(body)
    }

    def toDoc(comp: Computation): Doc = comp match {
      case Computation.Var(id) => toDoc(id)
      case Computation.Def(label) => toDoc(label)
      case Computation.New(interface, operations) => "new" <+> toDoc(interface) <+> braces {
        hsep(operations.map { case (id, impl) => toDoc(id) <> ":" <+> toDoc(impl) }, ",")
      }
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
object NewNormalizer { normal =>

  import semantics.*

  // "effects"
  case class Env(values: Map[Id, Addr], computations: Map[Id, Computation]) {
    def lookupValue(id: Id): Addr = values(id)
    def bindValue(id: Id, value: Addr): Env = Env(values + (id -> value), computations)
    def bindValue(newValues: List[(Id, Addr)]): Env = Env(values ++ newValues, computations)

    def lookupComputation(id: Id): Computation = computations(id)
    def bindComputation(id: Id, computation: Computation): Env = Env(values, computations + (id -> computation))
    def bindComputation(newComputations: List[(Id, Computation)]): Env = Env(values, computations ++ newComputations)
  }
  object Env {
    def empty: Env = Env(Map.empty, Map.empty)
  }

  def reify(scope: Scope, body: NeutralStmt): BasicBlock = {
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
    reify(local, result)
  }

  // "handlers"
  def bind[R](id: Id, addr: Addr)(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindValue(id, addr))

  def bind[R](id: Id, computation: Computation)(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindComputation(id, computation))

  def bind[R](values: List[(Id, Addr)])(prog: Env ?=> R)(using env: Env): R =
    prog(using env.bindValue(values))

  // Stacks
  // ------
  enum Stack {
    case Empty
    case Static(tpe: ValueType, apply: Env => Scope => Addr => NeutralStmt)
    case Dynamic(label: Label)
  }
  enum MetaStack {
    case Last(stack: Stack)
    // case Segment(prompt: Prompt, stack: Stack, next: MetaStack)

    def ret(arg: Addr)(using env: Env, scope: Scope): NeutralStmt = this match {
      case MetaStack.Last(stack) => stack match {
        case Stack.Empty => NeutralStmt.Return(arg)
        case Stack.Static(tpe, apply) => apply(env)(scope)(arg)
        case Stack.Dynamic(label) => NeutralStmt.App(label, List.empty, List(arg), Nil)
      }
    }
    def push(tpe: ValueType)(f: Env ?=> Scope ?=> Addr => MetaStack => NeutralStmt): MetaStack = this match {
      case MetaStack.Last(stack) => MetaStack.Last(Stack.Static(tpe,
        env => scope => arg => f(using env)(using scope)(arg)(this)
      ))
    }
    def reify(stmt: NeutralStmt)(using env: Env, scope: Scope): NeutralStmt = this match {
      case MetaStack.Last(stack) => stack match {
        case Stack.Empty => stmt
        // [[ val x = { val y = stmt1; stmt2 }; stmt3 ]] = [[ val y = stmt1; val x = stmt2; stmt3 ]]
        case Stack.Static(tpe, apply) =>
          val tmp = Id("tmp")
          scope.push(tmp, stmt)
          apply(env)(scope)(tmp)
        // stack is already reified
        case Stack.Dynamic(label) =>
          stmt
      }
    }
    def joinpoint(f: MetaStack => NeutralStmt)(using env: Env, scope: Scope): NeutralStmt = this match {
      case MetaStack.Last(stack) => stack match {
        case Stack.Static(tpe, apply) =>
          val x = Id("x")
          nested { scope ?=> apply(env)(scope)(x) } match {
            // Avoid trivial continuations like
            //   def k_6268 = (x_6267: Int_3) {
            //     return x_6267
            //   }
            case BasicBlock(Nil, _: (NeutralStmt.Return | NeutralStmt.App)) => f(this)
            case body =>
              val k = scope.define("k", Block(Nil, Nil, ValueParam(x, tpe) :: Nil, Nil, body))
              f(MetaStack.Last(Stack.Dynamic(k)))
          }
        case Stack.Empty => f(this)
        case Stack.Dynamic(label) => f(this)
      }
    }
  }
  object MetaStack {
    def empty: MetaStack = MetaStack.Last(Stack.Empty)
  }

  def evaluate(block: core.Block)(using env: Env, scope: Scope): Computation = block match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      env.lookupComputation(id)
    case b @ core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Computation.Def(scope.define("f", evaluate(b)))
    case core.Block.Unbox(pure) =>
      ???

    case core.Block.New(Implementation(interface, operations)) =>
      val ops = operations.map {
        case Operation(name, tparams, cparams, vparams, bparams, body) =>
          // Check whether the operation is already "just" an eta expansion and then use the identifier...
          //   no need to create a fresh block literal
          val eta: Option[Label] =
            body match {
              case Stmt.App(BlockVar(id, _, _), targs, vargs, bargs) =>
                def sameTargs = targs == tparams.map(t => ValueType.Var(t))
                def sameVargs = vargs == vparams.map(p => ValueVar(p.id, p.tpe))
                def sameBargs = bargs == bparams.map(p => BlockVar(p.id, p.tpe, p.capt))
                def isEta = sameTargs && sameVargs && sameBargs

                env.lookupComputation(id) match {
                  case Computation.Def(label) if isEta => Some(label)
                  case _ => None
                }
              case _ => None
            }

          val label = eta.getOrElse {
            scope.define(name.name.name,
              evaluate(core.Block.BlockLit(tparams, cparams, vparams, bparams, body): core.Block.BlockLit))
          }
          (name, label)
      }
      Computation.New(interface, ops)
  }

  def evaluate(block: core.Block.BlockLit)(using env: Env, scope: Scope): Block =
    block match {
      case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        // we keep the params as they are for now...
        given localEnv: Env = env
          .bindValue(vparams.map(p => p.id -> p.id))
          .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))
        Block(tparams, cparams, vparams, bparams, nested {
          evaluate(body, MetaStack.empty)
        })
    }

  def evaluate(expr: Expr)(using env: Env, scope: Scope): Addr = expr match {
    case Pure.ValueVar(id, annotatedType) =>
      env.lookupValue(id)

    case Pure.Literal(value, annotatedType) =>
      scope.allocate("x", Value.Literal(value, annotatedType))

    // right now everything is stuck... no constant folding ...
    case Pure.PureApp(f, targs, vargs) =>
      scope.allocate("x", Value.Extern(f, targs, vargs.map(evaluate)))

    case DirectApp(f, targs, vargs, bargs) =>
      assert(bargs.isEmpty)
      scope.run("x", f, targs, vargs.map(evaluate), bargs.map(evaluate))

    case Pure.Make(data, tag, targs, vargs) =>
      scope.allocate("x", Value.Make(data, tag, targs, vargs.map(evaluate)))

    case Pure.Box(b, annotatedCapture) =>
      ???
  }

  // TODO make evaluate(stmt) return BasicBlock (won't work for shift or reset, though)
  def evaluate(stmt: Stmt, k: MetaStack)(using env: Env, scope: Scope): NeutralStmt = stmt match {

    case Stmt.Return(expr) =>
      k.ret(evaluate(expr))

    case Stmt.Val(id, annotatedTpe, binding, body) =>
      // This push can lead to an eta-redex (a superfluous push...)
      evaluate(binding, k.push(annotatedTpe) { res => k =>
        bind(id, res) { evaluate(body, k) }
      })

    case Stmt.Let(id, annotatedTpe, binding, body) =>
      bind(id, evaluate(binding)) { evaluate(body, k) }

    // can be recursive
    case Stmt.Def(id, block: core.BlockLit, body) =>
      given Env = env.bindComputation(id, Computation.Def(id))
      scope.defineRecursive(id, evaluate(block), block.tpe, block.capt)
      bind(id, Computation.Def(id)) {
        evaluate(body, k)
      }

    case Stmt.Def(id, block, body) =>
      bind(id, evaluate(block)) { evaluate(body, k) }

    case Stmt.App(callee, targs, vargs, bargs) =>
      evaluate(callee) match {
        case Computation.Var(id) =>
          k.reify(NeutralStmt.App(id, targs, vargs.map(evaluate), bargs.map(evaluate)))
        case Computation.Def(label) =>
          // TODO this should be "jump"
          k.reify(NeutralStmt.App(label, targs, vargs.map(evaluate), bargs.map(evaluate)))
        case Computation.New(interface, operations) => sys error "Should not happen: app on new"
      }

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      evaluate(callee) match {
        case Computation.Var(id) =>
          k.reify(NeutralStmt.Invoke(id, method, methodTpe, targs, vargs.map(evaluate), bargs.map(evaluate)))
        case Computation.Def(label) => sys error s"Should not happen: invoke on def ${label}"
        case Computation.New(interface, operations) =>
          val op = operations.collectFirst { case (id, label) if id == method => label }.get
          k.reify(NeutralStmt.App(op, targs, vargs.map(evaluate), bargs.map(evaluate)))
      }

    case Stmt.If(cond, thn, els) =>
      val sc = evaluate(cond)
      scope.lookupValue(sc) match {
        case Some(Value.Literal(true, _)) => evaluate(thn, k)
        case Some(Value.Literal(false, _)) => evaluate(els, k)
        case _ =>
          k.joinpoint { k =>
            NeutralStmt.If(sc, nested {
              evaluate(thn, k)
            }, nested {
              evaluate(els, k)
            })
          }
      }

    case Stmt.Match(scrutinee, clauses, default) =>
      val sc = evaluate(scrutinee)
      scope.lookupValue(sc) match {
        case Some(Value.Make(data, tag, targs, vargs)) =>
          // TODO substitute types (or bind them in the env)!
          clauses.collectFirst {
            case (tpe, BlockLit(tparams, cparams, vparams, bparams, body)) if tpe == tag =>
              bind(vparams.map(_.id).zip(vargs)) { evaluate(body, k) }
          }.getOrElse {
            evaluate(default.getOrElse { sys.error("Non-exhaustive pattern match.") }, k)
          }
        case _ =>
          k.joinpoint { k =>
            NeutralStmt.Match(sc,
              // This is ALMOST like evaluate(BlockLit), but keeps the current continuation
              clauses.map { case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
                given localEnv: Env = env.bindValue(vparams.map(p => p.id -> p.id))
                val block = Block(tparams, cparams, vparams, bparams, nested {
                  evaluate(body, k)
                })
                (id, block)
              },
              default.map { stmt => nested { evaluate(stmt, k) } })
          }
      }

    case Stmt.Hole(span) => NeutralStmt.Hole(span)

    // State
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???

    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???

    // Control Effects
    case Stmt.Shift(prompt, BlockLit(Nil, cparam :: Nil, Nil, k2 :: Nil, body)) =>
      val p = env.lookupComputation(prompt.id) match {
        case Computation.Var(id) => id
        case _ => ???
      }
      // TODO implement correctly...
      val neutralBody = {
        given Env = env.bindComputation(k2.id -> Computation.Var(k2.id) :: Nil)
        nested {
          evaluate(body, MetaStack.empty)
        }
      }
      assert(Set(cparam) == k2.capt, "At least for now these need to be the same")
      k.reify(NeutralStmt.Shift(p, cparam, k2, neutralBody))
    case Stmt.Shift(_, _) => ???
    case Stmt.Reset(BlockLit(Nil, cparams, Nil, prompt :: Nil, body)) =>
      // TODO is Var correct here?? Probably needs to be a new computation value...
      //   but shouldn't it be a fresh prompt each time?
      val p = Id(prompt.id)
      val neutralBody = {
        given Env = env.bindComputation(prompt.id -> Computation.Var(p) :: Nil)
        nested {
          evaluate(body, MetaStack.empty)
        }
      }
      // TODO implement properly
      k.reify(NeutralStmt.Reset(BlockParam(p, prompt.tpe, prompt.capt), neutralBody))
    case Stmt.Reset(_) => ???
    case Stmt.Resume(k2, body) =>
      val r = env.lookupComputation(k2.id) match {
        case Computation.Var(id) => id
        case _ => ???
      }
      // TODO implement properly
      k.reify(NeutralStmt.Resume(r, nested {
        evaluate(body, MetaStack.empty)
      }))
  }

  def run(mod: ModuleDecl): ModuleDecl = {

    // TODO deal with async externs properly (see examples/benchmarks/input_output/dyck_one.effekt)
    val asyncExterns = mod.externs.collect { case defn: Extern.Def if defn.annotatedCapture.contains(AsyncCapability.capture) => defn }
    val toplevelEnv = Env.empty
      // user defined functions
      .bindComputation(mod.definitions.map(defn => defn.id -> Computation.Def(defn.id)))
      // async extern functions
      .bindComputation(asyncExterns.map(defn => defn.id -> Computation.Def(defn.id)))

    val typingContext = TypingContext(Map.empty, mod.definitions.collect {
      case Toplevel.Def(id, b) => id -> (b.tpe, b.capt)
    }.toMap) // ++ asyncExterns.map { d => d.id -> null })

    val newDefinitions = mod.definitions.map(d => run(d)(using toplevelEnv, typingContext))
    mod.copy(definitions = newDefinitions)
  }

  inline def debug(inline msg: => Any) = println(msg)

  def run(defn: Toplevel)(using env: Env, G: TypingContext): Toplevel = defn match {
    case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
      debug(s"------- ${util.show(id)} -------")
      debug(util.show(body))

      given localEnv: Env = env
        .bindValue(vparams.map(p => p.id -> p.id))
        .bindComputation(bparams.map(p => p.id -> Computation.Var(p.id)))

      given scope: Scope = Scope.empty
      val result = evaluate(body, MetaStack.empty)

      debug(s"---------------------")
      val block = Block(tparams, cparams, vparams, bparams, reify(scope, result))
      debug(PrettyPrinter.show(block))

      debug(s"---------------------")
      val embedded = embedBlockLit(block)
      debug(util.show(embedded))

      Toplevel.Def(id, embedded)
    case other => other
  }

  case class TypingContext(values: Map[Addr, ValueType], blocks: Map[Label, (BlockType, Captures)]) {
    def bind(id: Id, tpe: ValueType): TypingContext = this.copy(values = values + (id -> tpe))
    def bind(id: Id, tpe: BlockType, capt: Captures): TypingContext = this.copy(blocks = blocks + (id -> (tpe, capt)))
    def bindValues(vparams: List[ValueParam]): TypingContext = this.copy(values = values ++ vparams.map(p => p.id -> p.tpe))
    def bindComputations(bparams: List[BlockParam]): TypingContext = this.copy(blocks = blocks ++ bparams.map(p => p.id -> (p.tpe, p.capt)))
    def lookupValue(id: Id): ValueType = values.getOrElse(id, sys.error(s"Unknown value: ${util.show(id)}"))
  }

  def embedStmt(neutral: NeutralStmt)(using G: TypingContext): core.Stmt = neutral match {
    case NeutralStmt.Return(result) =>
      Stmt.Return(embedPure(result))
    case NeutralStmt.App(label, targs, vargs, bargs) =>
      Stmt.App(embedBlockVar(label), targs, vargs.map(embedPure), bargs.map(embedBlock))
    case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) =>
      Stmt.Invoke(embedBlockVar(label), method, tpe, targs, vargs.map(embedPure), bargs.map(embedBlock))
    case NeutralStmt.If(cond, thn, els) =>
      Stmt.If(embedPure(cond), embedStmt(thn), embedStmt(els))
    case NeutralStmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(embedPure(scrutinee),
        clauses.map { case (id, block) => id -> embedBlockLit(block) },
        default.map(embedStmt))
    case NeutralStmt.Reset(prompt, body) =>
      val capture = prompt.capt match {
        case set if set.size == 1 => set.head
        case _ => sys error "Prompt needs to have a single capture"
      }
      Stmt.Reset(core.BlockLit(Nil, capture :: Nil, Nil, prompt :: Nil, embedStmt(body)(using G.bindComputations(prompt :: Nil))))
    case NeutralStmt.Shift(prompt, capt, k, body) =>
      Stmt.Shift(embedBlockVar(prompt), core.BlockLit(Nil, capt :: Nil, Nil, k :: Nil, embedStmt(body)(using G.bindComputations(k :: Nil))))
    case NeutralStmt.Resume(k, body) =>
      Stmt.Resume(embedBlockVar(k), embedStmt(body))
    case NeutralStmt.Hole(span) =>
      Stmt.Hole(span)
  }

  def embedStmt(basicBlock: BasicBlock)(using G: TypingContext): core.Stmt = basicBlock match {
    case BasicBlock(bindings, stmt) =>
      bindings.foldRight((G: TypingContext) => embedStmt(stmt)(using G)) {
        case ((id, Binding.Let(value)), rest) => G =>
          val coreExpr = embedPure(value)(using G)
          // TODO why do we even have this type in core, if we always infer it?
          Stmt.Let(id, coreExpr.tpe, coreExpr, rest(G.bind(id, coreExpr.tpe)))
        case ((id, Binding.Def(block)), rest) => G =>
          val coreBlock = embedBlockLit(block)(using G)
          Stmt.Def(id, coreBlock, rest(G.bind(id, coreBlock.tpe, coreBlock.capt)))
        case ((id, Binding.Rec(block, tpe, capt)), rest) => G =>
          val coreBlock = embedBlockLit(block)(using G.bind(id, tpe, capt))
          Stmt.Def(id, coreBlock, rest(G.bind(id, tpe, capt)))
        case ((id, Binding.Val(stmt)), rest) => G =>
          val coreStmt = embedStmt(stmt)(using G)
          Stmt.Val(id, coreStmt.tpe, coreStmt, rest(G.bind(id, coreStmt.tpe)))
        case ((id, Binding.Run(callee, targs, vargs, bargs)), rest) => G =>
          val coreExpr = DirectApp(callee, targs, vargs.map(arg => embedPure(arg)(using G)), bargs.map(arg => embedBlock(arg)(using G)))
          Stmt.Let(id, coreExpr.tpe, coreExpr, rest(G.bind(id, coreExpr.tpe)))
      }(G)
  }

  def embedPure(value: Value)(using TypingContext): core.Pure = value match {
    case Value.Extern(callee, targs, vargs) => Pure.PureApp(callee, targs, vargs.map(embedPure))
    case Value.Literal(value, annotatedType) => Pure.Literal(value, annotatedType)
    case Value.Make(data, tag, targs, vargs) => Pure.Make(data, tag, targs, vargs.map(embedPure))
  }
  def embedPure(addr: Addr)(using G: TypingContext): core.Pure = Pure.ValueVar(addr, G.lookupValue(addr))

  def embedBlock(comp: Computation)(using G: TypingContext): core.Block = comp match {
    case Computation.Var(id) => embedBlockVar(id)
    case Computation.Def(label) => embedBlockVar(label)
    case Computation.New(interface, operations) =>
      val ops = operations.map { case (id, label) =>
        G.blocks(label) match {
          case (BlockType.Function(tparams, cparams, vparams, bparams, result), captures) =>
            val tparams2 = tparams.map(t => Id(t))
            // TODO if we freshen cparams, then we also need to substitute them in the result AND
            val cparams2 = cparams //.map(c => Id(c))
            val vparams2 = vparams.map(t => ValueParam(Id("x"), t))
            val bparams2 = (bparams zip cparams).map { case (t, c) => BlockParam(Id("f"), t, Set(c)) }

            core.Operation(id, tparams2, cparams, vparams2, bparams2,
              Stmt.App(embedBlockVar(label), tparams2.map(ValueType.Var.apply), vparams2.map(p => ValueVar(p.id, p.tpe)), bparams2.map(p => BlockVar(p.id, p.tpe, p.capt))))
          case _ => sys error "Unexpected block type"
        }
      }
      core.Block.New(Implementation(interface, ops))
  }

  def embedBlockLit(block: Block)(using G: TypingContext): core.BlockLit = block match {
    case Block(tparams, cparams, vparams, bparams, body) =>
      core.Block.BlockLit(tparams, cparams, vparams, bparams,
        embedStmt(body)(using G.bindValues(vparams).bindComputations(bparams)))
  }
  def embedBlockVar(label: Label)(using G: TypingContext): core.BlockVar =
    val (tpe, capt) = G.blocks.getOrElse(label, sys error s"Unknown block: ${util.show(label)}. ${G.blocks.keys.map(util.show).mkString(", ")}")
    core.BlockVar(label, tpe, capt)
}
