package effekt
package core

import symbols.{ Symbol, builtins }
import effekt.util.messages.ErrorReporter
import effekt.util.{ DB, toDB }

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

case class Typing[+T](tpe: T, capt: Captures, constraints: Type.Constraints) {
  def map[S](f: T => S): Typing[S] = Typing(f(tpe), capt, constraints)

  def check()(using DeclarationContext, ErrorReporter): Unit =
    constraints.foreach(Type.checkConstraint)
}

object Type {

  val TTop = ValueType.Data(symbols.builtins.TopSymbol, Nil)
  val TBottom = ValueType.Data(symbols.builtins.BottomSymbol, Nil)
  val TUnit   = ValueType.Data(symbols.builtins.UnitSymbol, Nil)
  val TInt = ValueType.Data(symbols.builtins.IntSymbol, Nil)
  val TChar = ValueType.Data(symbols.builtins.CharSymbol, Nil)
  val TByte = ValueType.Data(symbols.builtins.ByteSymbol, Nil)
  val TBoolean = ValueType.Data(symbols.builtins.BooleanSymbol, Nil)
  val TString = ValueType.Data(symbols.builtins.StringSymbol, Nil)
  val TDouble = ValueType.Data(symbols.builtins.DoubleSymbol, Nil)


  val TRegion = BlockType.Interface(symbols.builtins.RegionSymbol, Nil)

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
      val tparamSubst = tparams1.zip(tparams2).map { case (to, from) => from -> ValueType.Var(to) }.toDB
      val cparamSubst = cparams1.zip(cparams2).map { case (to, from) => from -> Set(to) }.toDB
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
    def apply(tpe: ValueType) = BlockType.Interface(symbols.builtins.TState.interface, List(tpe))
    def unapply(tpe: BlockType): Option[ValueType] =
      tpe match {
        case BlockType.Interface(symbols.builtins.TState.interface, List(tpe)) => Some(tpe)
        case tpe => None
      }
  }

  def instantiate(f: BlockType.Function, targs: List[ValueType], cargs: List[Captures]): BlockType.Function = f match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      assert(targs.size == tparams.size, s"Wrong number of type arguments\n  targs: ${targs}\n  tparams: ${tparams}")
      assert(cargs.size == cparams.size, "Wrong number of capture arguments")

      val tsubst = (tparams zip targs).toDB
      val csubst = (cparams zip cargs).toDB
      BlockType.Function(Nil, Nil,
        vparams.map { tpe => substitute(tpe, tsubst, DB.empty) },
        bparams.map { tpe => substitute(tpe, tsubst, DB.empty) },
        substitute(result, tsubst, csubst))
  }

  def substitute(capt: Captures, csubst: DB[Captures]): Captures = capt.flatMap {
    case id if csubst.isDefinedAt(id) => csubst(id)
    case c => Set(c)
  }

  def substitute(tpe: BlockType, vsubst: DB[ValueType], csubst: DB[Captures]): BlockType = tpe match {
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

  def substitute(tpe: ValueType, vsubst: DB[ValueType], csubst: DB[Captures]): ValueType = tpe match {
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


  // Type Checking
  // -------------
  // This is only used in debug mode of the compiler.
  // To infer the type of a statement, use `tpe` or `inferType`, which is faster.
  private def typeError(msg: String) = throw TypeError(msg, Nil)

  private inline def checking[T <: Tree, R](t: T)(inline f: T => R): R =
    try f(t) catch {
      case TypeError(msg, context) => throw TypeError(msg, t :: context)
    }

  case class MatchClause(ctor: Id, result: ValueType.Data, existentialTypeArgs: List[ValueType], arguments: List[ValueType])

  type Constraint = Expr.Make | Implementation | MatchClause

  type Constraints = Vector[Constraint]
  object Constraints {
    val empty = Vector.empty[Constraint]
  }

  def checkConstraint(c: Constraint)(using DeclarationContext, ErrorReporter): Unit = c match {
    case impl: Implementation =>
      checking(impl) { impl => checkAgainstDeclaration(impl) }
    case make @ Expr.Make(data, tag, targs, vargs) =>
      checking(make) { make => checkAgainstDeclaration(data, tag, targs, vargs.map(_.tpe)) }
    case Type.MatchClause(tag, result, targs, arguments) =>
      checkAgainstDeclaration(result, tag, targs, arguments)
  }

  def typecheck(module: ModuleDecl)(using ErrorReporter): Unit = module match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      given DeclarationContext(declarations, externs)

      definitions.foreach {
        case Toplevel.Def(id, block) => block.typing.check()
        case Toplevel.Val(id, binding) => binding.typing.check()
      }

      externs.foreach {
        case Extern.Def(id, qualifiedSignature, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
          val splices = body match {
            case ExternBody.StringExternBody(featureFlag, contents) => contents.args
            case ExternBody.Unsupported(err) => Nil
          }
          all(splices, splice => splice.typing).check()

        case Extern.Include(featureFlag, contents) => ()
        case Extern.Data(id, tparams, body) => ()
        case Extern.Interface(id, tparams, body) => ()
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

  inline def typecheck(expr: Expr): Typing[ValueType] = checking(expr) {
    case Expr.ValueVar(id, annotatedType) => Typing(annotatedType, Set.empty, Constraints.empty)
    case Expr.Literal(value, annotatedType) => Typing(annotatedType, Set.empty, Constraints.empty)
    case Expr.PureApp(callee, targs, vargs) =>
       val BlockType.Function(tparams, cparams, vparams, bparams, result) = instantiate(callee.functionType, targs, Nil)
       val Typing(argTypes, _, cs) = all(vargs, e => e.typing)
       if bparams.nonEmpty then typeError("Pure apps cannot have block params")
       valuesShouldEqual(vparams, argTypes)
       Typing(result, Set.empty, cs)

    case make @ Expr.Make(data, tag, targs, vargs) =>
      val Typing(argTypes, argCapt, argsCs) = all(vargs, arg => arg.typing)
      // we assume that the annotated type is correct and check later...
      Typing(data, Set.empty, argsCs :+ make)

    case Expr.Box(b, annotatedCapture) =>
      val Typing(bTpe, bCapt, bCs) = b.typing
      // Here we actually allow "subcapturing"
      // if !bCapt.subsetOf(annotatedCapture) then typeError(s"Inferred capture ${bCapt} is not allowed by annotation: ${annotatedCapture}")
      Typing(ValueType.Boxed(bTpe, annotatedCapture), Set.empty, bCs)
  }

  inline def typecheck(block: Block): Typing[BlockType] = checking(block) {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Typing(annotatedTpe, annotatedCapt, Constraints.empty)
    case Block.Unbox(pure) =>
      val Typing(tpe, capt, cs) = pure.typing
      tpe match {
        case ValueType.Boxed(tpe2, capt2) => Typing(tpe2, capt2, cs)
        case other => typeError(s"Expected a boxed type, but got: ${util.show(other)}")
      }
    case b : Block.BlockLit => typecheck(b)
    case Block.New(impl) => impl.typing
  }

  inline def typecheck(blocklit: BlockLit): Typing[BlockType.Function] = checking(blocklit) {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      Typing(BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), bodyTpe),
        bodyCapt -- cparams,
        bodyCs)
  }

  inline def typecheck(impl: Implementation): Typing[BlockType.Interface] = checking(impl) {
    case Implementation(interface, operations) =>
      val Typing(ops, capts, cs) = all(operations, _.typing)
      Typing(interface, capts, cs :+ impl)
  }

  inline def typecheck(op: Operation): Typing[BlockType.Function] = checking(op) {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      Typing(BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), bodyTpe),
        bodyCapt -- cparams,
        bodyCs)
  }

  inline def typecheck(stmt: Stmt): Typing[ValueType] = checking(stmt) {
    case Stmt.Def(id, block, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      val Typing(blockTpe, blockCapt, blockCs) = block.typing
      Typing(bodyTpe, bodyCapt, bodyCs ++ blockCs)

    case Stmt.Let(id, binding, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      val Typing(bindTpe, bindCapt, bindCs) = binding.typing
      Typing(bodyTpe, bodyCapt ++ bindCapt, bodyCs ++ bindCs)

    case Stmt.Return(expr) => expr.typing

    case Stmt.Val(id, binding, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      val Typing(bindTpe, bindCapt, bindCs) = binding.typing
      Typing(bodyTpe, bodyCapt ++ bindCapt, bodyCs ++ bindCs)

    case Stmt.If(cond, thn, els) =>
      val Typing(condTpe, condCapt, condCs) = cond.typing
      val Typing(thnTpe, thnCapt, thnCs) = thn.typing
      val Typing(elsTpe, elsCapt, elsCs) = els.typing
      valueShouldEqual(condTpe, TBoolean)
      valueShouldEqual(thnTpe, elsTpe)
      Typing(thnTpe, condCapt ++ thnCapt ++ elsCapt, condCs ++ thnCs ++ elsCs)

    case Stmt.Match(sc, annotatedTpe, clauses, default) =>
      val Typing(scType, scCapt, scCs) = sc.typing

      val clauseTypings = clauses.map { case (id, arm) =>
        val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), armCapt, armCs) = asFunctionTyping(arm.typing)
        Typing(result, armCapt, armCs :+ MatchClause(id, sc.tpe.asInstanceOf[ValueType.Data], tparams.map(id => ValueType.Var(id)), vparams))
      }

      def join(typings: List[Typing[ValueType]], annotated: ValueType): Typing[ValueType] =
        fold(typings, annotated) { (tpe1, tpe2) => valueShouldEqual(tpe1, tpe2); tpe1 }

      val Typing(tpe, capt, cs) = join(clauseTypings ++ default.toList.map { stmt => stmt.typing }, annotatedTpe)
      Typing(tpe, scCapt ++ capt, scCs ++ cs)


    case Stmt.Region(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyCs) = asFunctionTyping(body.typing)
      // TODO we should check that cparams do not occur in result!
      Typing(result, bodyCapt, bodyCs)

    case Stmt.Alloc(id, init, region, body) =>
      val Typing(initTpe, initCapt, initCs) = init.typing
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      Typing(bodyTpe, bodyCapt ++ Set(region), initCs ++ bodyCs)

    case Stmt.Var(ref, init, capture, body) =>
      val Typing(initTpe, initCapt, initCs) = init.typing
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      Typing(bodyTpe, bodyCapt -- Set(capture), initCs ++ bodyCs)

    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, bodyCs)

    case Stmt.Put(ref, annotatedCapt, value, body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      val Typing(valueTpe, valueCapt, valueCs) = value.typing
      Typing(bodyTpe, bodyCapt ++ annotatedCapt, valueCs ++ bodyCs)

    case Stmt.Reset(body) =>
      val Typing(BlockType.Function(tparams, cparams, vparams, bparams, result), bodyCapt, bodyCs) = asFunctionTyping(body.typing)
      // TODO we should check that cparams do not occur in result!
      Typing(result, bodyCapt, bodyCs)

    // shift(p) { k: Resume[from, to] => body }
    case Stmt.Shift(BlockVar(id, ptpe@TPrompt(delimiter), annotatedCapt), BlockParam(k, tpe@BlockType.Interface(ResumeSymbol, List(from, to)), capt), body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      valueShouldEqual(to, bodyTpe)
      valueShouldEqual(to, delimiter)
      Typing(from, bodyCapt ++ Set(id), bodyCs)

    case Stmt.Shift(prompt, BlockParam(k, tpe, capt), body) =>
      typeError(s"Block parameter of shift have wrong type: ${k}: ${tpe}")

    // resume(k) { stmt }
    case Stmt.Resume(BlockVar(id, tpe@BlockType.Interface(ResumeSymbol, List(result, answer)), annotatedCapt), body) =>
      val Typing(bodyTpe, bodyCapt, bodyCs) = body.typing
      valueShouldEqual(result, bodyTpe)
      Typing(answer, annotatedCapt ++ bodyCapt, bodyCs)

    case Stmt.Resume(k, body) => typeError(s"Continuation has wrong type: ${k}")

    case Stmt.ImpureApp(id, BlockVar(f, tpe: BlockType.Function, annotatedCapt), targs, vargs, bargs, body) =>
      val Typing(retType, callCapts, callCs) = typecheckFunctionLike(Typing(tpe, annotatedCapt, Constraints.empty), targs, vargs, bargs)
      val Typing(bodyType, bodyCapts, bodyCs) = body.typing
      Typing(bodyType, callCapts ++ bodyCapts, callCs ++ bodyCs)

    case s: Stmt.ImpureApp => typeError("Impure app should have a function type")

    case Stmt.App(callee, targs, vargs, bargs) => typecheckFunctionLike(asFunctionTyping(callee.typing), targs, vargs, bargs)

    case Stmt.Invoke(callee, method, methodTpe: BlockType.Function, targs, vargs, bargs) =>
      val Typing(calleeTpe, calleeCapts, calleeCs) = callee.typing
      typecheckFunctionLike(Typing(methodTpe, calleeCapts, calleeCs), targs, vargs, bargs)

    case s: Stmt.Invoke => typeError("Method type should be a function")

    case Stmt.Hole(annotatedTpe, span) => Typing(annotatedTpe, Set.empty, Constraints.empty)
  }

  private def typecheckFunctionLike(calleeTyping: Typing[BlockType.Function], targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Typing[ValueType] = {
    val Typing(calleeTpe, calleeCapt, calleeCs) = calleeTyping
    val Typing(vargTypes, vargCapt, vargCs) = all(vargs, arg => arg.typing)
    val Typing(bargTypes, bargCapt, bargCs) = all(bargs, arg => arg.typing)

    val BlockType.Function(_, _, vparams, bparams, retType) = instantiate(calleeTpe, targs, bargs.map(_.capt))
    valuesShouldEqual(vparams, vargTypes)
    blocksShouldEqual(bparams, bargTypes)

    Typing(retType, calleeCapt ++ vargCapt ++ bargCapt, calleeCs ++ vargCs ++ bargCs)
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
    terms.foldRight(Typing[List[R]](Nil, Set.empty, Constraints.empty)) {
      case (term, Typing(tpes, capts, css)) =>
        val Typing(tpe, capt, cs) = check(term)
        Typing(tpe :: tpes, capts ++ capt, css ++ cs)
    }

  private def fold[T](typings: List[Typing[T]], empty: T)(combine: (T, T) => T): Typing[T] =
    typings.foldLeft(Typing(empty, Set.empty, Constraints.empty)) {
      case (Typing(tpe1, capt1, cs1), Typing(tpe2, capt2, cs2)) =>
        Typing(combine(tpe1, tpe2), capt1 ++ capt2, cs1 ++ cs2)
    }

  private def asFunctionTyping(t: Typing[BlockType]): Typing[BlockType.Function] = t.asInstanceOf


  // Inference
  inline def inferType(stmt: Stmt): ValueType = stmt match {
    case Stmt.Def(id, block, body) => body.tpe
    case Stmt.Let(id, binding, body) => body.tpe
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => body.tpe
    case Stmt.Return(expr) => expr.tpe
    case Stmt.Val(id, binding, body) => body.tpe
    case Stmt.App(callee, targs, vargs, bargs) =>
      Type.instantiate(callee.tpe.asInstanceOf[BlockType.Function], targs, bargs.map(_.capt)).result
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Type.instantiate(methodTpe.asInstanceOf[BlockType.Function], targs, bargs.map(_.capt)).result
    case Stmt.If(cond, thn, els) => thn.tpe
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => annotatedTpe
    case Stmt.Region(body) => body.returnType
    case Stmt.Alloc(id, init, region, body) => body.tpe
    case Stmt.Var(ref, init, capture, body) => body.tpe
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => body.tpe
    case Stmt.Put(ref, annotatedCapt, value, body) => body.tpe
    case Stmt.Reset(body) => body.returnType
    case Stmt.Shift(prompt, k, body) =>
      k.tpe match {
        case BlockType.Interface(_, List(from, _)) => from
        case _ => typeError(s"Shift continuation must have Resume[from, to] type, got: ${util.show(k.tpe)}")
      }
    case Stmt.Resume(k, body) =>
      k.annotatedTpe match {
        case BlockType.Interface(_, List(_, answer)) => answer
        case _ => typeError(s"Resume continuation must have Resume[result, answer] type, got: ${util.show(k.annotatedTpe)}")
      }
    case Stmt.Hole(annotatedTpe, span) => annotatedTpe
  }

  inline def inferType(expr: Expr): ValueType = expr match {
    case Expr.ValueVar(id, annotatedType) => annotatedType
    case Expr.Literal(value, annotatedType) => annotatedType
    case Expr.PureApp(b, targs, vargs) =>
      Type.instantiate(b.functionType, targs, Nil).result
    case Expr.Make(data, tag, targs, vargs) => data
    case Expr.Box(b, annotatedCapture) =>
      ValueType.Boxed(b.tpe, annotatedCapture)
  }

  inline def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => annotatedTpe
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), body.tpe)
    case Block.Unbox(pure) =>
      pure.tpe match {
        case ValueType.Boxed(tpe, _) => tpe
        case other => typeError(s"Expected a boxed type, but got: ${util.show(other)}")
      }
    case Block.New(impl) => impl.interface
  }

  private inline def allCapt[T](t: IterableOnce[T], inline f: T => Captures): Captures =
    t.iterator.foldLeft[Captures](Set.empty) { case (c, t) => f(t) ++ c }

  inline def inferCapt(stmt: Stmt): Captures = stmt match {
    case Stmt.Def(id, block, body) => body.capt
    case Stmt.Let(id, binding, body) => body.capt
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      callee.capt ++ bargs.flatMap(_.capt) ++ body.capt
    case Stmt.Return(expr) => Set.empty
    case Stmt.Val(id, binding, body) => body.capt ++ binding.capt
    case Stmt.App(callee, targs, vargs, bargs) =>
      callee.capt ++ bargs.flatMap(_.capt)
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      callee.capt ++ bargs.flatMap(_.capt)
    case Stmt.If(cond, thn, els) => thn.capt ++ els.capt
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) =>
      allCapt(clauses, { case (id, cl) => cl.capt }) ++
      allCapt(default, _.capt)
    case Stmt.Region(body) => body.capt
    case Stmt.Alloc(id, init, region, body) =>
      body.capt ++ Set(region)
    case Stmt.Var(ref, init, capture, body) =>
      body.capt -- Set(capture)
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      body.capt ++ annotatedCapt
    case Stmt.Put(ref, annotatedCapt, value, body) =>
      body.capt ++ annotatedCapt
    case Stmt.Reset(body) => body.capt
    case Stmt.Shift(prompt, k, body) =>
      body.capt ++ Set(prompt.id)
    case Stmt.Resume(k, body) =>
      body.capt ++ k.annotatedCapt
    case Stmt.Hole(annotatedTpe, span) => Set.empty
  }

  inline def inferCapt(block: Block): Captures = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => annotatedCapt
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      body.capt -- cparams
    case Block.Unbox(pure) =>
      pure.tpe match {
        case ValueType.Boxed(_, capt) => capt
        case other => typeError(s"Expected a boxed type, but got: ${util.show(other)}")
      }
    case Block.New(impl) => impl.capt
  }

  inline def inferType(impl: Implementation): BlockType.Interface = impl.interface

  inline def inferCapt(impl: Implementation): Captures =
    allCapt(impl.operations, _.capt)

  inline def inferType(op: Operation): BlockType.Function = op match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), body.tpe)
  }

  inline def inferCapt(op: Operation): Captures = op match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      body.capt -- cparams
  }
}
