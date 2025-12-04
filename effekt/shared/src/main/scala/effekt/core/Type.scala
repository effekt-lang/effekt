package effekt
package core

import symbols.{ Symbol, builtins }
import effekt.util.messages.ErrorReporter

/**
 * In core, all names, including those in capture sets are just symbols.
 */
type Capture = symbols.Symbol
type Captures = Set[Capture]

/**
 * Design Decisions:
 * - TypeConstructor vs. flat ValueType (right now the latter)
 * - Locally Nameless vs. Named (right now the latter)
 *
 * ----------[[ effekt.core.Type ]]----------
 *
 *   ─ [[ Type ]]
 *     │─ [[ ValueType ]]
 *     │  │─ [[ Var ]]
 *     │  │─ [[ Data ]]
 *     │  │─ [[ Boxed ]]
 *     │
 *     │─ [[ BlockType ]]
 *     │  │─ [[ Function ]]
 *     │  │─ [[ Interface ]]
 *     │
 *
 * -------------------------------------------
 */
sealed trait Type {
  def show: String = util.show(this)
}

enum ValueType extends Type {
  case Var(name: Id)
  case Data(name: Id, targs: List[ValueType])
  case Boxed(tpe: BlockType, capt: Captures)
}

enum BlockType extends Type {

  // [A, B, C] (X, Y, Z)   {  f  :   S }    =>    T
  //  ^^^^^^^   ^^^^^^^     ^^^^^^^^^^^          ^^^
  //  tparams   vparams   cparams zip bparams   result
  case Function(tparams: List[Id], cparams: List[Id], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(name: Id, targs: List[ValueType])
}

case class TypeError(msg: String) extends Throwable(msg)
def typeError(msg: String) = throw TypeError(msg)

// We could ALSO just save the Make to decrease pressure on the GC
// this could even improve the error messages, since we can point at the term that's wrong...
// so `type Later = Expr.Make | Implementation`
//
// How do we check the dual method calls and pattern matches then against declarations?
enum Constraints {
  case Make(ctor: Id, result: ValueType.Data, existentialTypeArgs: List[ValueType], arguments: List[ValueType])
  case Implementation(interface: BlockType.Interface, operations: Map[Id, BlockType.Function])

  // These are always never traversed (accept when checking on the module level, so we use the free structure)
  case Empty
  case Append(lhs: Constraints, rhs: Constraints)

  def ++(other: Constraints) = (this, other) match {
    case (Empty, other) => other
    case (other, Empty) => other
    case (lhs, rhs) => Constraints.Append(lhs, rhs)
  }
}
object Constraints {
  def empty: Constraints = Constraints.Empty
}

// only used for type checking
case class Free(values: Map[Id, ValueType], blocks: Map[Id, (BlockType, Captures)], constraints: Constraints) {
  // throws type error if they are not compatible
  def ++(other: Free): Free =
    if !Free.valuesCompatible(values, other.values) then typeError("incompatible free variables")
    if !Free.blocksCompatible(blocks, other.blocks) then typeError("incompatible free variables")
    Free(values ++ other.values, blocks ++ other.blocks, constraints ++ other.constraints)

  def withoutValue(id: Id, tpe: ValueType): Free =
    values.get(id).foreach { otherTpe =>
      if !Type.equals(tpe, otherTpe) then
        typeError(s"free variable ${id} has two different types (${util.show(tpe)} vs. ${util.show(otherTpe)})")
    }
    Free(values - id, blocks, constraints)

  def withoutValues(bindings: List[ValueParam]): Free = bindings.foldLeft(this) {
    case (free, ValueParam(id, tpe)) => free.withoutValue(id, tpe)
  }

  def withoutBlock(id: Id, tpe: BlockType, capt: Captures): Free =
    blocks.get(id).foreach { case (otherTpe, otherCapt) =>
      if !Type.equals(tpe, otherTpe) then
        typeError(s"free variable ${id} has two different types (${util.show(tpe)} vs. ${util.show(otherTpe)})")
      if !otherCapt.subsetOf(capt) then
        typeError(s"free variable assume a wrong capture set (${util.show(capt)} vs. ${util.show(otherCapt)})")
    }
    Free(values, blocks - id, constraints)

  def withoutBlocks(bindings: List[BlockParam]): Free = bindings.foldLeft(this) {
    case (free, BlockParam(id, tpe, capt)) => free.withoutBlock(id, tpe, capt)
  }

  def isEmpty: Boolean = values.isEmpty && blocks.isEmpty
}
object Free {
  def empty = Free(Map.empty, Map.empty, Constraints.empty)
  def value(id: Id, tpe: ValueType) = Free(Map(id -> tpe), Map.empty, Constraints.empty)
  def block(id: Id, tpe: BlockType, capt: Captures) = Free(Map.empty, Map(id -> (tpe, capt)), Constraints.empty)


  def make(ctor: Id, result: ValueType.Data, existentialTypeArgs: List[ValueType], arguments: List[ValueType]) =
    Free(Map.empty, Map.empty, Constraints.Make(ctor, result, existentialTypeArgs, arguments))

  def valuesCompatible(free1: Map[Id, ValueType], free2: Map[Id, ValueType]): Boolean =
    val same = free1.keySet intersect free2.keySet
    same.forall { id => Type.equals(free1(id), free2(id)) }

  def blocksCompatible(free1: Map[Id, (BlockType, Captures)], free2: Map[Id, (BlockType, Captures)]): Boolean =
    val same = free1.keySet intersect free2.keySet
    same.forall { id =>
      val (tpe1, capt1) = free1(id)
      val (tpe2, capt2) = free2(id)
      Type.equals(tpe1, tpe2) && Type.equals(capt1, capt2)
    }
}



case class Typing[+T](tpe: T, capt: Captures, free: Free)

object Type {

  // The subtyping lattice
  val TBottom = ValueType.Data(builtins.BottomSymbol, Nil)
  val TUnit   = ValueType.Data(builtins.UnitSymbol, Nil)

  val TInt = ValueType.Data(builtins.IntSymbol, Nil)
  val TChar = ValueType.Data(builtins.CharSymbol, Nil)
  val TByte = ValueType.Data(builtins.ByteSymbol, Nil)
  val TBoolean = ValueType.Data(builtins.BooleanSymbol, Nil)
  val TString = ValueType.Data(builtins.StringSymbol, Nil)
  val TDouble = ValueType.Data(builtins.DoubleSymbol, Nil)

  val TRegion = BlockType.Interface(builtins.RegionSymbol, Nil)

  val PromptSymbol = Id("Prompt")
  val ResumeSymbol = Id("Resume")

  def equals(tpe1: ValueType, tpe2: ValueType): Boolean = (tpe1, tpe2) match {
    case (ValueType.Var(name1), ValueType.Var(name2)) => name1 == name2
    case (ValueType.Data(name1, args1), ValueType.Data(name2, args2)) => name1 == name2 && all(args1, args2, equals)
    case (ValueType.Boxed(btpe1, capt1), ValueType.Boxed(btpe2, capt2)) => equals(btpe1, btpe2) && equals(capt1, capt2)
    case _ => false
  }

  private final def all[T](tpes1: List[T], tpes2: List[T], pred: (T, T) => Boolean): Boolean =
    tpes1.size == tpes2.size && tpes1.zip(tpes2).forall { case (t1, t2) => pred(t1, t2) }

  def equals(tpe1: BlockType, tpe2: BlockType): Boolean = (tpe1, tpe2) match {
    case (
      // [A, f](Option[A]) { f: () => A }: () => A at {f, exc}
      BlockType.Function(tparams1, cparams1, vparams1, bparams1, result1),
      BlockType.Function(tparams2, cparams2, vparams2, bparams2, result2)
    ) =>
      val tparamSubst = tparams1.zip(tparams2).map { case (to, from) => from -> ValueType.Var(to) }.toMap
      val cparamSubst = cparams1.zip(cparams2).map { case (to, from) => from -> Set(to) }.toMap
      def typeParamArity = tparams1.size == tparams2.size
      def equalVparams = all(vparams1, vparams2.map(t => substitute(t, tparamSubst, cparamSubst)), equals)
      def equalBparams = all(bparams1, bparams2.map(t => substitute(t, tparamSubst, cparamSubst)), equals)
      def equalResult = equals(result1, substitute(result2, tparamSubst, cparamSubst))
      typeParamArity && equalVparams && equalBparams && equalResult

    case (BlockType.Interface(name1, args1), BlockType.Interface(name2, args2)) =>
      name1 == name2 && all(args1, args2, equals)
    case _ => false
  }

  def equals(capt1: Captures, capt2: Captures): Boolean = capt1 == capt2


  object TResume {
    def apply(result: ValueType, answer: ValueType) = BlockType.Interface(ResumeSymbol, List(result, answer))
    def unapply(tpe: BlockType): Option[(ValueType, ValueType)] = tpe match {
      case BlockType.Interface(ResumeSymbol, List(result, answer)) => Some((result, answer))
      case _ => None
    }
  }

  object TPrompt {
    def apply(answer: ValueType): BlockType.Interface = BlockType.Interface(PromptSymbol, List(answer))
    def unapply(tpe: BlockType): Option[ValueType] =
      tpe match {
        case BlockType.Interface(PromptSymbol, List(answer)) => Some(answer)
        case _ => None
      }
  }

  object TState {
    def apply(tpe: ValueType) = BlockType.Interface(builtins.TState.interface, List(tpe))
    def unapply(tpe: BlockType): Option[ValueType] =
      tpe match {
        case BlockType.Interface(builtins.TState.interface, List(tpe)) => Some(tpe)
        case tpe => None
      }
  }

  def instantiate(f: BlockType.Function, targs: List[ValueType], cargs: List[Captures]): BlockType.Function = f match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      assert(targs.size == tparams.size, "Wrong number of type arguments")
      assert(cargs.size == cparams.size, "Wrong number of capture arguments")

      val tsubst = (tparams zip targs).toMap
      val csubst = (cparams zip cargs).toMap
      BlockType.Function(Nil, Nil,
        vparams.map { tpe => substitute(tpe, tsubst, Map.empty) },
        bparams.map { tpe => substitute(tpe, tsubst, Map.empty) },
        substitute(result, tsubst, csubst))
  }

  def substitute(capt: Captures, csubst: Map[Id, Captures]): Captures = capt.flatMap {
    case id if csubst.isDefinedAt(id) => csubst(id)
    case c => Set(c)
  }

  def substitute(tpe: BlockType, vsubst: Map[Id, ValueType], csubst: Map[Id, Captures]): BlockType = tpe match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      // names are unique symbols so shadowing should NOT take place; we still subtract to be safe.
      val vsubstLocal = vsubst -- tparams
      val csubstLocal = csubst -- cparams

      BlockType.Function(tparams, cparams,
        vparams.map { tpe => substitute(tpe, vsubstLocal, csubst) }, // technically in source, cparams are not bound in value arguments
        bparams.map { tpe => substitute(tpe, vsubstLocal, csubstLocal) },
        substitute(result, vsubstLocal, csubstLocal))

    case BlockType.Interface(sym, targs) =>
      BlockType.Interface(sym, targs map { tpe => substitute(tpe, vsubst, csubst) })
  }

  def substitute(tpe: ValueType, vsubst: Map[Id, ValueType], csubst: Map[Id, Captures]): ValueType = tpe match {
    case ValueType.Var(id) if vsubst.isDefinedAt(id) => vsubst(id)
    case ValueType.Var(id) => tpe

    case ValueType.Data(sym, targs) => ValueType.Data(sym, targs.map(t => substitute(t, vsubst, csubst)))

    case ValueType.Boxed(tpe, capt) =>
      ValueType.Boxed(substitute(tpe, vsubst, csubst), substitute(capt, csubst))
  }

  def bindingType(stmt: Stmt.ImpureApp): ValueType = stmt match {
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      Type.instantiate(callee.tpe.asInstanceOf[core.BlockType.Function], targs, bargs.map(_.capt)).result
  }

  def bindingType(bind: Binding.ImpureApp): ValueType = bind match {
    case Binding.ImpureApp(id, callee, targs, vargs, bargs) =>
      Type.instantiate(callee.tpe.asInstanceOf[core.BlockType.Function], targs, bargs.map(_.capt)).result
  }

  extension (block: Block) {
    def returnType: ValueType = block.functionType.result
    def functionType: BlockType.Function = block.tpe.asInstanceOf
    def answerType: ValueType = block.tpe match {
      case BlockType.Interface(name, List(result, answer)) => answer
      case _ => ???
    }
    def resultType: ValueType = block.tpe match {
      case BlockType.Interface(name, List(result, answer)) => result
      case _ => ???
    }
  }

  def typecheck(expr: Expr): Typing[ValueType] = expr match {
    case Expr.ValueVar(id, annotatedType) => Typing(annotatedType, Set.empty, Free.value(id, annotatedType))
    case Expr.Literal(value, annotatedType) => Typing(annotatedType, Set.empty, Free.empty)
    case Expr.PureApp(callee, targs, vargs) =>
       val BlockType.Function(tparams, cparams, vparams, bparams, result) = instantiate(callee.functionType, targs, Nil)
       if bparams.nonEmpty then typeError("Pure apps cannot have block params")
       val Typing(argTypes, _, argFrees) = all(vargs, e => e.typing)
       valuesShouldEqual(vparams, argTypes)
       Typing(result, Set.empty, argFrees ++ Free.block(callee.id, callee.annotatedTpe, callee.annotatedCapt))

    // targs here are the existential type arguments
    // Make Coalg[Bool].State[Int](0, box { n => n + 1 }, box { n => n > 0 })
    case Expr.Make(data, tag, targs, vargs) =>
      //      // type Coalg[T] { case State[S](state: S, next: S => S at {}, get: S => T at {}) }
      //      val decl = D.getData(data.name)
      //      // case State[S](state: S, next: S => S at {}, get: S => T at {})
      //      val ctor = D.getConstructor(tag)
      //      // [T]
      //      val universalParams = decl.tparams
      //      // [S]
      //      val existentialParams = ctor.tparams
      //      // [T, S](S, S => S at {}, S => T at {}) => Coalg[T]
      //      val sig: BlockType.Function = BlockType.Function(universalParams ++ existentialParams, Nil, ctor.fields.map(_.tpe), Nil,
      //        ValueType.Data(data.name, universalParams.map(ValueType.Var.apply)))
      //      // (Int, Int => Int at {}, Int => Bool at {}) => Coalg[Bool]
      //      val BlockType.Function(_, _, paramTypes, _, retType) = instantiate(sig, data.targs ++ targs, Nil)
      //      // TODO factor out callLike things
      //      shouldEqual(data, retType)
      //      val Typing(argTypes, argCapt, argFree) = all(vargs, typecheck)
      //      valuesShouldEqual(paramTypes, argTypes)

      val Typing(argTypes, argCapt, argFree) = all(vargs, arg => arg.typing)
      // we assume that the annotated type is correct and check later...
      Typing(data, argCapt, argFree ++ Free.make(tag, data, targs, argTypes))

    case Expr.Box(b, annotatedCapture) =>
      val Typing(bTpe, bCapt, bFree) = b.typing
      // Here we actually allow "subcapturing"
      if !bCapt.subsetOf(annotatedCapture) then typeError(s"Inferred capture ${bCapt} is not allowed by annotation: ${annotatedCapture}")
      Typing(ValueType.Boxed(bTpe, annotatedCapture), Set.empty, bFree)
  }

  def typecheck(block: Block): Typing[BlockType] = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => Typing(annotatedTpe, annotatedCapt, Free.block(id, annotatedTpe, annotatedCapt))
    case Block.Unbox(pure) =>
      val Typing(tpe, capt, free) = pure.typing
      tpe match {
        case ValueType.Boxed(tpe2, capt2) => Typing(tpe2, capt2, free)
        case other => typeError(s"Expected a boxed type, but got: ${util.show(other)}")
      }
    case b : Block.BlockLit => typecheck(b)
    case Block.New(impl) => impl.typing
  }

  def typecheck(blocklit: BlockLit): Typing[BlockType.Function] = blocklit match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), bodyTpe), bodyCapt -- cparams,
        bodyFree.withoutBlocks(bparams).withoutValues(vparams))
  }

  def typecheck(impl: Implementation): Typing[BlockType.Interface] = ???

  def typecheck(impl: Operation): Typing[BlockType.Function] = ???

  def valuesShouldEqual(tpes1: List[ValueType], tpes2: List[ValueType]): Unit =
    if tpes1.size != tpes2.size then typeError(s"Different number of types: ${tpes1} vs. ${tpes2}")
    tpes1.zip(tpes2).foreach(shouldEqual)

  def shouldEqual(tpe1: ValueType, tpe2: ValueType): Unit =
    if !Type.equals(tpe1, tpe2) then typeError(s"Type mismatch: ${util.show(tpe1)} vs ${util.show(tpe2)}")

  def all[T, R](terms: List[T], check: T => Typing[R]): Typing[List[R]] =
    terms.foldRight(Typing[List[R]](Nil, Set.empty, Free.empty)) {
      case (term, Typing(tpes, capts, frees)) =>
        val Typing(tpe, capt, free) = check(term)
        Typing(tpe :: tpes, capts ++ capt, frees ++ free)
    }

  // TODO
  // - [ ] get rid of `DeclarationContext` AND `ErrorReport` (transitively)
  // - [ ] Rename `Result` -> `Typing`
  // - [ ] make `val typing: Typing[ValueType]` a field on `Stmt`
  // - [ ] define `def tpe = typing.result`, similar for `capt`
  // - [ ] define `def free: Free = typing.free` and use it in the phases that require free variable computation
  // - [ ] delete `core.Variables` (defined in core/Tree.scala) and replace all usages by `stmt.free`

  def typecheck(stmt: Stmt): Typing[ValueType] = stmt match {
    case Stmt.Def(id, block, body) =>
      val canBeRecursive = block match {
        case Block.BlockLit(tparams, cparams, vparams, bparams, body) => true
        case Block.New(impl) => true
        case _ => false
      }
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      val Typing(blockTpe, blockCapt, blockFree) = block.typing

      Typing(bodyTpe, bodyCapt, bodyFree.withoutBlock(id, blockTpe, blockCapt) ++
        (if canBeRecursive then blockFree.withoutBlock(id, blockTpe, blockCapt) else blockFree))

    case Stmt.Let(id, binding, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      val Typing(bindTpe, bindCapt, bindFree) = binding.typing
      Typing(bodyTpe, bodyCapt ++ bindCapt, bodyFree.withoutValue(id, bindTpe) ++ bindFree)

    case Stmt.Return(expr) => expr.typing

    case Stmt.Val(id, binding, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      val Typing(bindTpe, bindCapt, bindFree) = binding.typing
      Typing(bodyTpe, bodyCapt ++ bindCapt, bodyFree.withoutValue(id, bindTpe) ++ bindFree)

    case Stmt.If(cond, thn, els) =>
      val Typing(condTpe, condCapt, condFree) = cond.typing
      val Typing(thnTpe, thnCapt, thnFree) = thn.typing
      val Typing(elsTpe, elsCapt, elsFree) = els.typing
      shouldEqual(condTpe, TBoolean)
      shouldEqual(thnTpe, elsTpe)
      Typing(thnTpe, condCapt ++ thnCapt ++ elsCapt, condFree ++ thnFree ++ elsFree)

    case Stmt.Match(sc, annotatedTpe, clauses, default) =>
      val Typing(scType, scCapt, scFree) = sc.typing
      val clauseTypings = clauses.map { case (id, arm) =>
        val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), armCapt, armFree) = arm.typing.asInstanceOf[Typing[BlockType.Function]]
        Typing(result, armCapt, armFree ++ Free.make(id, sc.tpe.asInstanceOf[ValueType.Data], tparams.map(id => ValueType.Var(id)), vparams))
      }

      def join(typings: List[Typing[ValueType]], annotated: ValueType): Typing[ValueType] =
        typings.foldLeft(Typing(annotated, Set.empty, Free.empty)) {
          case (Typing(tpe1, capt1, free1), Typing(tpe2, capt2, free2)) =>
            shouldEqual(tpe1, tpe2)
            Typing(tpe1, capt1 ++ capt2, free1 ++ free2)
        }

      join(clauseTypings ++ default.toList.map { stmt => stmt.typing }, annotatedTpe)

    case Stmt.Region(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyFree) = body.typing.asInstanceOf[Typing[BlockType.Function]]
      // TODO we should check that cparams do not occur in result!
      Typing(result, bodyCapt, bodyFree)

    case Stmt.Alloc(id, init, region, body) =>
      val Typing(initTpe, initCapt, initFree) = init.typing
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(bodyTpe, bodyCapt ++ Set(region),
        initFree ++ Free.block(region, TRegion, Set(region)) ++ bodyFree.withoutBlock(id, TState(init.tpe), Set(region)))

    case Stmt.Var(ref, init, capture, body) =>
      val Typing(initTpe, initCapt, initFree) = init.typing
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(bodyTpe, bodyCapt -- Set(capture), initFree ++ bodyFree.withoutValue(ref, init.tpe))

    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, Free.block(ref, core.Type.TState(annotatedTpe), annotatedCapt) ++ bodyFree.withoutValue(id, annotatedTpe))

    case Stmt.Put(ref, annotatedCapt, value, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      val Typing(valueTpe, valueCapt, valueFree) = value.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, Free.block(ref, core.Type.TState(valueTpe), annotatedCapt) ++ valueFree ++ bodyFree)

    case Stmt.Reset(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyFree) = body.typing.asInstanceOf[Typing[BlockType.Function]]
      // TODO we should check that cparams do not occur in result!
      Typing(result, bodyCapt, bodyFree)

    case Stmt.Shift(prompt, body) => ???
    case Stmt.Resume(k, body) => ???

    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???

    case Stmt.App(callee, targs, vargs, bargs) => ???

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???

    case Stmt.Hole(annotatedTpe, span) => Typing(annotatedTpe, Set.empty, Free.empty)
  }
}
