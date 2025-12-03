package effekt
package core

import symbols.{ Symbol, builtins }

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

  def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(id, tpe, capt) => tpe
    case Block.BlockLit(tparams, cparams, vps, bps, body) =>
      val vparams = vps.map { p => p.tpe }
      val bparams = bps.map { p => p.tpe }

      BlockType.Function(tparams, cparams, vparams, bparams, body.tpe)
    case Block.Unbox(pure) => pure.tpe match {
      case ValueType.Boxed(tpe, capt) => tpe
      case tpe => println(util.show(block)); sys.error(s"Got ${tpe}, which is not a boxed block type.")
    }
    case Block.New(impl) => impl.tpe
  }
  def inferCapt(block: Block): Captures = block match {
    case Block.BlockVar(id, tpe, capt) => capt
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      body.capt -- cparams
    case Block.Unbox(pure) => pure.tpe.asInstanceOf[ValueType.Boxed].capt
    case Block.New(impl) => impl.capt
  }

  def bindingType(stmt: Stmt.ImpureApp): ValueType = stmt match {
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      Type.instantiate(callee.tpe.asInstanceOf[core.BlockType.Function], targs, bargs.map(_.capt)).result
  }

  def bindingType(bind: Binding.ImpureApp): ValueType = bind match {
    case Binding.ImpureApp(id, callee, targs, vargs, bargs) =>
      Type.instantiate(callee.tpe.asInstanceOf[core.BlockType.Function], targs, bargs.map(_.capt)).result
  }

  private inline def debug(inline assertion: Boolean, inline msg: => String): Unit = () // assert(assertion, msg)

  def inferType(stmt: Stmt): ValueType = stmt match {
    case Stmt.Def(id, block, body) => body.tpe
    case Stmt.Let(id, binding, body) => body.tpe
    case Stmt.ImpureApp(id, calle, targs, vargs, bargs, body) => body.tpe
    case Stmt.Return(expr) => expr.tpe
    case Stmt.Val(id, binding, body) => body.tpe
    case Stmt.App(callee, targs, vargs, bargs) =>
      instantiate(callee.functionType, targs, bargs.map(_.capt)).result
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      instantiate(methodTpe.asInstanceOf, targs, bargs.map(_.capt)).result
    case Stmt.If(cond, thn, els) => assert(Type.equals(cond.tpe, TBoolean)); debug(Type.equals(thn.tpe, els.tpe), s"Different types: ${util.show(thn)} and ${util.show(els)}"); thn.tpe
    case Stmt.Match(scrutinee, tpe, clauses, default) => clauses.foreach { case (_,
      BlockLit(tparams, cparams, vparams, bparams, body)) => debug(Type.equals(tpe, body.tpe), s"Different types in match: ${util.show(body.tpe)} vs. at match ${util.show(tpe)}")
    }; tpe

    case Stmt.Alloc(id, init, region, body) => body.tpe
    case Stmt.Var(ref, init, cap, body) => body.tpe
    case Stmt.Get(ref, capt, tpe, id, body) => body.tpe
    case Stmt.Put(ref, capt, value, body) => body.tpe
    case Stmt.Reset(BlockLit(_, _, _, prompt :: Nil, body)) => prompt.tpe match {
      case TPrompt(tpe) => tpe
      case _ => ???
    }
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, body) => body.bparams match {
      case core.BlockParam(id, BlockType.Interface(ResumeSymbol, List(result, answer)), captures) :: Nil => result
      case _ => ???
    }
    case Stmt.Resume(k, body) => k.tpe match {
      case BlockType.Interface(ResumeSymbol, List(result, answer)) => answer
      case _ => ???
    }
    case Stmt.Region(body) => body.returnType

    case Stmt.Hole(tpe, span) => tpe
  }

  def inferCapt(stmt: Stmt): Captures = stmt match {
    case Stmt.Def(id, block, body) => block.capt ++ body.capt
    case Stmt.Let(id, binding, body) => body.capt
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => callee.capt ++ bargs.flatMap(_.capt).toSet ++ body.capt
    case Stmt.Return(expr) => Set.empty
    case Stmt.Val(id, binding, body) => binding.capt ++ body.capt
    case Stmt.App(callee, targs, vargs, bargs) => callee.capt ++ bargs.flatMap(_.capt).toSet
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => callee.capt ++ bargs.flatMap(_.capt).toSet
    case Stmt.If(cond, thn, els) => thn.capt ++ els.capt
    case Stmt.Match(scrutinee, tpe, clauses, default) => clauses.flatMap { (_, cl) => cl.capt }.toSet ++ default.toSet.flatMap(s => s.capt)
    case Stmt.Alloc(id, init, region, body) => Set(region) ++ body.capt
    case Stmt.Var(ref, init, cap, body) => body.capt -- Set(cap)
    case Stmt.Get(id, tpe, ref, capt, body) => capt
    case Stmt.Put(ref, capt, value, body) => capt
    case Stmt.Reset(body) => body.capt
    case Stmt.Shift(prompt, body) => prompt.capt ++ body.capt
    case Stmt.Resume(k, body) => k.capt ++ body.capt
    case Stmt.Region(body) => body.capt
    case Stmt.Hole(tpe, span) => Set.empty
  }

  def inferType(expr: Expr): ValueType = expr match {
    case Expr.ValueVar(id, tpe) => tpe
    case Expr.Literal(value, tpe) => tpe
    case Expr.PureApp(callee, targs, args) => instantiate(callee.functionType, targs, Nil).result
    case Expr.Make(tpe, tag, targs, args) => tpe // TODO instantiate?
    case Expr.Box(block, capt) => ValueType.Boxed(block.tpe, capt)
  }

  /**
   * Invariant: can only be {} or {io}
   */
  def inferCapt(expr: Expr): Captures = expr match {
    case pure: Expr => Set.empty
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
}
