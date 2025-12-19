package effekt
package core

import symbols.{ Symbol, builtins }
import effekt.util.messages.ErrorReporter
import scala.collection.immutable.HashMap

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

case class TypeError(msg: String, context: List[core.Tree]) extends Throwable(msg) {
  override def toString = msg + "\n" + context.map(util.show).mkString("\n----- Context -----\n", "\n------\n", "\n----------\n")
}
def typeError(msg: String) = throw TypeError(msg, Nil)

def checking[T <: Tree, R](t: T)(f: T => R): R =
  try f(t) catch {
    case TypeError(msg, context) => throw TypeError(msg, t :: context)
  }

case class MatchClause(ctor: Id, result: ValueType.Data, existentialTypeArgs: List[ValueType], arguments: List[ValueType])

type Constraint = Expr.Make | Implementation | MatchClause

sealed trait Free {
  def without(values: List[ValueParam], blocks: List[BlockParam]): Free = Free.Without(values, blocks, this)
  def withoutValues(bindings: List[ValueParam]): Free = without(bindings, Nil)
  def withoutBlocks(bindings: List[BlockParam]): Free = without(Nil, bindings)
  def withoutBlock(id: Id, tpe: BlockType, capt: Captures): Free = without(Nil, BlockParam(id, tpe, capt) :: Nil)
  def withoutValue(id: Id, tpe: ValueType): Free = withoutValues(ValueParam(id, tpe) :: Nil)
  def ++(other: Free): Free = other match {
    case Free.Empty => this
    case other => Free.Join(this, other)
  }

  // for debugging: checks whether all variables are compatible
  def wellformed(): Unit

  def checkConstraints()(using DeclarationContext, ErrorReporter): Unit

  // these are cached
  lazy val freeValues: HashMap[Id, ValueType] = _freeValues
  lazy val freeBlocks: HashMap[Id, (BlockType, Captures)] = _freeBlocks
  lazy val freeIds: Set[Id] = _freeIds

  protected def _freeValues: HashMap[Id, ValueType]
  protected def _freeBlocks: HashMap[Id, (BlockType, Captures)]
  protected def _freeIds: Set[Id]
}

object Free {

  object Empty extends Free {
    override def withoutValue(id: Id, tpe: ValueType): Free = this
    override def ++(other: Free): Free = other
    override def withoutValues(bindings: List[ValueParam]): Free = this
    override def withoutBlock(id: Id, tpe: BlockType, capt: Captures): Free = this

    override def wellformed(): Unit = ()
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = ()
    override protected def _freeValues: HashMap[Id, ValueType] = HashMap.empty
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = HashMap.empty
    override protected def _freeIds: Set[Id] = Set.empty
  }
  case class Join(left: Free, right: Free) extends Free {
    override def wellformed(): Unit = {
      left.wellformed()
      right.wellformed()
      // check compatiblity of free variables
      mergeValues(left.freeValues, right.freeValues)
      mergeBlocks(left.freeBlocks, right.freeBlocks)
    }
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = { left.checkConstraints(); right.checkConstraints() }
    override protected def _freeValues: HashMap[Id, ValueType] = left.freeValues ++ right.freeValues
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = left.freeBlocks ++ right.freeBlocks
    override protected def _freeIds: Set[Id] = left.freeIds ++ right.freeIds
  }
  case class Value(id: Id, tpe: ValueType) extends Free {
    override def wellformed(): Unit = ()
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = ()
    override protected def _freeValues: HashMap[Id, ValueType] = HashMap(id -> tpe)
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = HashMap.empty
    override protected def _freeIds: Set[Id] = Set(id)
  }
  case class Block(id: Id, tpe: BlockType, capt: Captures) extends Free {
    override def wellformed(): Unit = ()
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = ()
    override protected def _freeValues: HashMap[Id, ValueType] = HashMap.empty
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = HashMap(id -> (tpe, capt))
    override protected def _freeIds: Set[Id] = Set(id)
  }
  case class Without(values: List[ValueParam], blocks: List[BlockParam], underlying: Free) extends Free {
    private val valueIds = values.map(_.id)
    private val blockIds = blocks.map(_.id)
    override def wellformed(): Unit = {
      underlying.wellformed()
      values.foreach {
        case ValueParam(id, tpe) => underlying.freeValues.get(id).foreach { otherTpe =>
          if !Type.equals(tpe, otherTpe) then
            typeError(s"free variable ${util.show(id)} has two different types (${util.show(tpe)} vs. ${util.show(otherTpe)})")
        }
      }
      blocks.foreach {
        case BlockParam(id, tpe, capt) => underlying.freeBlocks.get(id).foreach { case (otherTpe, otherCapt) =>
          if !Type.equals(tpe, otherTpe) then
            typeError(s"free variable ${util.show(id)} has two different types (${util.show(tpe)} vs. ${util.show(otherTpe)})")
          // for now ignore captures
          // if !otherCapt.subsetOf(capt) then
            // typeError(s"free variable ${util.show(id)} assumes a wrong capture set (${capt.map(core.PrettyPrinter.show)} vs. ${otherCapt.map(core.PrettyPrinter.show)})")
        }
      }
    }
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = underlying.checkConstraints()
    override protected def _freeValues: HashMap[Id, ValueType] = underlying.freeValues.removedAll(valueIds)
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = underlying.freeBlocks.removedAll(blockIds)
    override protected def _freeIds: Set[Id] = underlying.freeIds.removedAll(valueIds).removedAll(blockIds)
  }
  case class Defer(constraint: Constraint) extends Free {
    override def wellformed(): Unit = ()
    override def checkConstraints()(using DeclarationContext, ErrorReporter): Unit = constraint match {
      case impl: Implementation =>
        checking(impl) { Type.checkAgainstDeclaration }
      case make @ Expr.Make(data, tag, targs, vargs) =>
        checking(make) { make => Type.checkAgainstDeclaration(data, tag, targs, vargs.map(_.tpe)) }
      case MatchClause(tag, result, targs, arguments) =>
        Type.checkAgainstDeclaration(result, tag, targs, arguments)
    }
    override protected def _freeValues: HashMap[Id, ValueType] = HashMap.empty
    override protected def _freeBlocks: HashMap[Id, (BlockType, Captures)] = HashMap.empty
    override protected def _freeIds: Set[Id] = Set.empty
  }

  def empty: Free = Free.Empty
  def value(id: Id, tpe: ValueType): Free = Free.Value(id, tpe)
  def block(id: Id, tpe: BlockType, capt: Captures): Free = Free.Block(id, tpe, capt)
  def defer(c: Constraint): Free = Free.Defer(c)

  def mergeValues(free1: HashMap[Id, ValueType], free2: HashMap[Id, ValueType]): HashMap[Id, ValueType] =
    free1.merged(free2) {
      case ((id1, tpe1), (id2, tpe2)) =>
        Type.valueShouldEqual(tpe1, tpe2)
        id1 -> tpe1
    }

  def mergeBlocks(free1: HashMap[Id, (BlockType, Captures)], free2: HashMap[Id, (BlockType, Captures)]): HashMap[Id, (BlockType, Captures)] =
    free1.merged(free2) {
      case ((id1, (tpe1, capt1)), (id2, (tpe2, capt2))) =>
        Type.blockShouldEqual(tpe1, tpe2)
        id1 -> (tpe1, capt1)
    }
}

case class Typing[+T](tpe: T, capt: Captures, free: Free) {
  def map[S](f: T => S): Typing[S] = Typing(f(tpe), capt, free)
}

object Type {

  val TTop = ValueType.Data(builtins.TopSymbol, Nil)
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
    // ignore captures for now :(
    case (ValueType.Boxed(btpe1, capt1), ValueType.Boxed(btpe2, capt2)) => equals(btpe1, btpe2) // && equals(capt1, capt2)
    case _ => false
  }

  private final inline def all[T](tpes1: List[T], tpes2: List[T], inline pred: (T, T) => Boolean): Boolean =
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
      assert(targs.size == tparams.size, s"Wrong number of type arguments\n  targs: ${targs}\n  tparams: ${tparams}")
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

  def typecheck(module: ModuleDecl)(using ErrorReporter): Unit = module match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      given DeclarationContext(declarations, externs)

      // bound on the toplevel
      val boundBlocks = definitions.collect {
        case Toplevel.Def(id, block) => BlockParam(id, block.tpe, block.capt)
      } ++ externs.collect {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, capt, _) =>
          BlockParam(id, BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), ret), capt)
      }

      val boundValues = definitions.collect {
        case Toplevel.Val(id, expr) => ValueParam(id, expr.tpe)
      }

      def wellformed(free: Free): Unit =
        val toplevel = free.without(boundValues, boundBlocks)
        toplevel.wellformed()
        toplevel.checkConstraints()

        assert(toplevel.freeIds.isEmpty, {
          val freeBlocks = toplevel.freeBlocks.map { case (id, (tpe, capt)) => s"${util.show(id)}: ${util.show(tpe)} @ ${capt.map(util.show).mkString("{", ", ", "}")}" }
          val freeValues = toplevel.freeValues.map { case (id, tpe) => s"${util.show(id)}: ${util.show(tpe)}" }
          s"Toplevel program should be closed, but got:\n${ freeBlocks.mkString("\n") }\n\n${freeValues.mkString("\n")}"
        })

      definitions.foreach {
        case Toplevel.Def(id, block) => wellformed(block.free)
        case Toplevel.Val(id, binding) => wellformed(binding.free)
      }

      externs.foreach {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
          val splices = body match {
            case ExternBody.StringExternBody(featureFlag, contents) => contents.args
            case ExternBody.Unsupported(err) => Nil
          }
          val free = all(splices, splice => splice.typing).free.without(vparams, bparams)
          wellformed(free)

        case Extern.Include(featureFlag, contents) => ()
      }
  }

  // Make Coalg[Bool].State[Int](0, box { n => n + 1 }, box { n => n > 0 })
  def checkAgainstDeclaration(result: ValueType.Data, tag: Id, existentialTypeArgs: List[ValueType], arguments: List[ValueType])(using DC: DeclarationContext, E: ErrorReporter): Unit = {
      // type Coalg[T] { case State[S](state: S, next: S => S at {}, get: S => T at {}) }
      val decl = DC.getData(result.name)
      // case State[S](state: S, next: S => S at {}, get: S => T at {})
      val ctor = DC.getConstructor(tag)
      // [T]
      val universalParams = decl.tparams
      // [S]
      val existentialParams = ctor.tparams
      // [T, S](S, S => S at {}, S => T at {}) => Coalg[T]
      val sig: BlockType.Function = BlockType.Function(universalParams ++ existentialParams, Nil, ctor.fields.map(_.tpe), Nil,
        ValueType.Data(result.name, universalParams.map(ValueType.Var.apply)))
      // (Int, Int => Int at {}, Int => Bool at {}) => Coalg[Bool]
      val BlockType.Function(_, _, paramTypes, _, retType) = instantiate(sig, result.targs ++ existentialTypeArgs, Nil)
      valueShouldEqual(result, retType)
      valuesShouldEqual(paramTypes, arguments)
  }

  def checkAgainstDeclaration(impl: Implementation)(using DC: DeclarationContext, E: ErrorReporter): Unit = impl match {
    // interface is the _applied_ interface type, for instance
    // Implementation Callback[Int] { def register: [S](arg: S, fun: S => Int at {}) => Unit }
    case Implementation(interface, operations) =>
      val definitions = operations.map { op => op.name -> op.tpe }

      // interface Callback[A] { def register: [B](arg: B, fun: B => A at {}) => Unit }
      val decl = DC.getInterface(interface.name)

      // check all are defined
      val declared = decl.properties.map(_.id).toSet
      val implemented = definitions.map(_._1).toSet
      assert(declared == implemented, s"Interface ${interface.name} declares ${declared}, but implemented: ${implemented}")

      // [A]
      val universalParams = decl.tparams
      // [Int]
      val universalArgs = interface.targs

      definitions.foreach {
        case (id, tpe) => (DC.getProperty(id).tpe, tpe) match {
          case (
            //                 [B]
            BlockType.Function(declTparams, declCparams, declVparams, declBparams, declRet),
            //                 [S]
            BlockType.Function(implTparams, implCparams, implVparams, implBparams, implRet)
          ) =>
            // [A, B](B, B => A at {}) => Unit
            val sig: BlockType.Function = BlockType.Function(universalParams ++ declTparams, declCparams, declVparams, declBparams, declRet)
            // (S, S => Int at {}) => Unit
            val BlockType.Function(_, _, vparams, bparams, result) = instantiate(sig, universalArgs ++ implTparams.map(ValueType.Var.apply),
              implCparams.map(id => Set(id)))

            valueShouldEqual(result, implRet)
            valuesShouldEqual(vparams, implVparams)
            blocksShouldEqual(bparams, implBparams)

          case (other1, other2) =>
            throw new AssertionError(s"Both are required to be function types: ${util.show(other1)} and ${util.show(other2)}")
        }
      }
  }

  def typecheck(expr: Expr): Typing[ValueType] = checking(expr) {
    case Expr.ValueVar(id, annotatedType) => Typing(annotatedType, Set.empty, Free.value(id, annotatedType))
    case Expr.Literal(value, annotatedType) => Typing(annotatedType, Set.empty, Free.empty)
    case Expr.PureApp(callee, targs, vargs) =>
       val BlockType.Function(tparams, cparams, vparams, bparams, result) = instantiate(callee.functionType, targs, Nil)
       if bparams.nonEmpty then typeError("Pure apps cannot have block params")
       val Typing(argTypes, _, argFrees) = all(vargs, e => e.typing)
       valuesShouldEqual(vparams, argTypes)
       Typing(result, Set.empty, argFrees ++ Free.block(callee.id, callee.annotatedTpe, callee.annotatedCapt))

    case make @ Expr.Make(data, tag, targs, vargs) =>
      val Typing(argTypes, argCapt, argFree) = all(vargs, arg => arg.typing)
      // we assume that the annotated type is correct and check later...
      Typing(data, argCapt, argFree ++ Free.defer(make))

    case Expr.Box(b, annotatedCapture) =>
      val Typing(bTpe, bCapt, bFree) = b.typing
      // Here we actually allow "subcapturing"
      // if !bCapt.subsetOf(annotatedCapture) then typeError(s"Inferred capture ${bCapt} is not allowed by annotation: ${annotatedCapture}")
      Typing(ValueType.Boxed(bTpe, annotatedCapture), Set.empty, bFree)
  }

  def typecheck(block: Block): Typing[BlockType] = checking(block) {
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

  def typecheck(blocklit: BlockLit): Typing[BlockType.Function] = checking(blocklit) {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), bodyTpe), bodyCapt -- cparams,
        bodyFree.withoutBlocks(vparams, bparams))
  }

  def typecheck(impl: Implementation): Typing[BlockType.Interface] = checking(impl) {
    case Implementation(interface, operations) =>
      val Typing(ops, capts, free) = fold(operations.map { op => op.typing.map { tpe => Map(op.name -> tpe) }}, Map.empty) { _ ++ _ }
      Typing(interface, capts, free ++ Free.defer(impl))
  }

  def typecheck(op: Operation): Typing[BlockType.Function] = checking(op) {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), bodyTpe), bodyCapt -- cparams,
        bodyFree.withoutBlocks(vparams, bparams))
  }

  def typecheck(stmt: Stmt): Typing[ValueType] = checking(stmt) {
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
      valueShouldEqual(condTpe, TBoolean)
      valueShouldEqual(thnTpe, elsTpe)
      Typing(thnTpe, condCapt ++ thnCapt ++ elsCapt, condFree ++ thnFree ++ elsFree)

    case Stmt.Match(sc, annotatedTpe, clauses, default) =>
      val Typing(scType, scCapt, scFree) = sc.typing
      val clauseTypings = clauses.map { case (id, arm) =>
        val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), armCapt, armFree) = asFunctionTyping(arm.typing)
        Typing(result, armCapt, armFree ++ Free.defer(MatchClause(id, sc.tpe.asInstanceOf[ValueType.Data], tparams.map(id => ValueType.Var(id)), vparams)))
      }

      // TODO assert that scrutinee actually has type being matched on
      def join(typings: List[Typing[ValueType]], annotated: ValueType): Typing[ValueType] =
        fold(typings, annotated) { (tpe1, tpe2) => valueShouldEqual(tpe1, tpe2); tpe1 }

      val Typing(tpe, capt, free) = join(clauseTypings ++ default.toList.map { stmt => stmt.typing }, annotatedTpe)
      Typing(tpe, scCapt ++ capt, scFree ++ free)


    case Stmt.Region(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyFree) = asFunctionTyping(body.typing)
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
      Typing(bodyTpe, bodyCapt -- Set(capture), initFree ++ bodyFree.withoutBlock(ref, TState(init.tpe), Set(capture)))

    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, Free.block(ref, core.Type.TState(annotatedTpe), annotatedCapt) ++ bodyFree.withoutValue(id, annotatedTpe))

    case Stmt.Put(ref, annotatedCapt, value, body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      val Typing(valueTpe, valueCapt, valueFree) = value.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, Free.block(ref, core.Type.TState(valueTpe), annotatedCapt) ++ valueFree ++ bodyFree)

    case Stmt.Reset(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyFree) = asFunctionTyping(body.typing)
      // TODO we should check that cparams do not occur in result!
      Typing(result, bodyCapt, bodyFree)

    // shift(p) { k: Resume[from, to] => body }
    case Stmt.Shift(BlockVar(id, ptpe@TPrompt(delimiter), annotatedCapt), BlockParam(k, tpe@BlockType.Interface(ResumeSymbol, List(from, to)), capt), body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      valueShouldEqual(to, bodyTpe)
      valueShouldEqual(to, delimiter)
      Typing(from, bodyCapt ++ Set(id), bodyFree.withoutBlock(k, tpe, capt) ++ Free.block(id, ptpe, annotatedCapt))

    case Stmt.Shift(prompt, BlockParam(k, tpe, capt), body) =>
      typeError(s"Block parameter of shift have wrong type: ${k}: ${tpe}")

    // resume(k) { stmt }
    case Stmt.Resume(BlockVar(id, tpe@BlockType.Interface(ResumeSymbol, List(result, answer)), annotatedCapt), body) =>
      val Typing(bodyTpe, bodyCapt, bodyFree) = body.typing
      valueShouldEqual(result, bodyTpe)
      Typing(answer, annotatedCapt ++ bodyCapt, bodyFree ++ Free.block(id, tpe, annotatedCapt))

    case Stmt.Resume(k, body) => typeError(s"Continuation has wrong type: ${k}")

    case Stmt.ImpureApp(id, BlockVar(f, tpe: BlockType.Function, annotatedCapt), targs, vargs, bargs, body) =>
      val Typing(retType, callCapts, callFree) = typecheckFunctionLike(Typing(tpe, annotatedCapt, Free.block(f, tpe, annotatedCapt)), targs, vargs, bargs)
      val Typing(bodyType, bodyCapts, bodyFree) = body.typing
      Typing(bodyType, callCapts ++ bodyCapts, callFree ++ bodyFree.withoutValue(id, retType))

    case s: Stmt.ImpureApp => typeError("Impure app should have a function type")

    case Stmt.App(callee, targs, vargs, bargs) => typecheckFunctionLike(asFunctionTyping(callee.typing), targs, vargs, bargs)

    case Stmt.Invoke(callee, method, methodTpe: BlockType.Function, targs, vargs, bargs) =>
      val Typing(calleeTpe, calleeCapts, calleeFree) = callee.typing
      typecheckFunctionLike(Typing(methodTpe, calleeCapts, calleeFree), targs, vargs, bargs)

    case s: Stmt.Invoke => typeError("Method type should be a function")

    case Stmt.Hole(annotatedTpe, span) => Typing(annotatedTpe, Set.empty, Free.empty)
  }

  private def typecheckFunctionLike(calleeTyping: Typing[BlockType.Function], targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Typing[ValueType] = {
    val Typing(calleeTpe, calleeCapt, calleeFree) = calleeTyping
    val Typing(vargTypes, vargCapt, vargFree) = all(vargs, arg => arg.typing)
    val Typing(bargTypes, bargCapt, bargFree) = all(bargs, arg => arg.typing)

    val BlockType.Function(_, _, vparams, bparams, retType) = instantiate(calleeTpe, targs, bargs.map(_.capt))

    valuesShouldEqual(vparams, vargTypes)
    blocksShouldEqual(bparams, bargTypes)

    Typing(retType, calleeCapt ++ vargCapt ++ bargCapt, calleeFree ++ vargFree ++ bargFree)
  }

  private def valuesShouldEqual(tpes1: List[ValueType], tpes2: List[ValueType]): Unit =
    if tpes1.size != tpes2.size then typeError(s"Different number of types: ${tpes1} vs. ${tpes2}")
    tpes1.zip(tpes2).foreach(valueShouldEqual)

  private def blocksShouldEqual(tpes1: List[BlockType], tpes2: List[BlockType]): Unit =
    if tpes1.size != tpes2.size then typeError(s"Different number of types: ${tpes1} vs. ${tpes2}")
    tpes1.zip(tpes2).foreach(blockShouldEqual)

  def valueShouldEqual(tpe1: ValueType, tpe2: ValueType): Unit =
    if !Type.equals(tpe1, tpe2) then typeError(s"Value type mismatch:\n  ${util.show(tpe1)}\n  ${util.show(tpe2)}")

  def blockShouldEqual(tpe1: BlockType, tpe2: BlockType): Unit =
    if !Type.equals(tpe1, tpe2) then typeError(s"Block type mismatch:\n  ${util.show(tpe1)}\n  ${util.show(tpe2)}")

  private inline def all[T, R](terms: List[T], inline check: T => Typing[R]): Typing[List[R]] =
    terms.foldRight(Typing[List[R]](Nil, Set.empty, Free.empty)) {
      case (term, Typing(tpes, capts, frees)) =>
        val Typing(tpe, capt, free) = check(term)
        Typing(tpe :: tpes, capts ++ capt, frees ++ free)
    }

  private def fold[T](typings: List[Typing[T]], empty: T)(combine: (T, T) => T): Typing[T] =
    typings.foldLeft(Typing(empty, Set.empty, Free.empty)) {
      case (Typing(tpe1, capt1, free1), Typing(tpe2, capt2, free2)) =>
        Typing(combine(tpe1, tpe2), capt1 ++ capt2, free1 ++ free2)
    }

  private def asFunctionTyping(t: Typing[BlockType]): Typing[BlockType.Function] = t.asInstanceOf
}
