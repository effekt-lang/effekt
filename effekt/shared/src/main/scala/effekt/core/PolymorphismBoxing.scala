package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.core.PolymorphismBoxing.ValueCoercer.IdentityCoercer
import effekt.symbols
import effekt.symbols.{ TmpBlock, TmpValue }
import effekt.{ CoreTransformed, Phase }
import effekt.symbols.builtins.{ TBoolean, TByte, TChar, TDouble, TInt, TState, TUnit }
import effekt.symbols.ErrorMessageInterpolator

import scala.util.boundary
import scala.annotation.targetName

/**
 * [[Phase]] on [[CoreTransformed]] to automatically box values when used polymorphically
 * (i.e., their types are type parameters).
 *
 * By default, it will box all primitive types (i.e. all types of the form [[ValueType.Data]]`(T,List())`
 * with `T` in [[symbols.builtins.rootTypes]]) using the data type `BoxedT` found in the prelude
 * (see [[symbols.Module.findPrelude]]).
 * This behavior can be changed by overriding [[box]].
 */
object PolymorphismBoxing extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "polymorphism boxing"

  /**
   * Describes how to box/unbox values using extern functions
   * @param tpe The type of the boxed values
   * @param boxFn The extern function to call to box
   * @param unboxFn The extern function to call to unbox
   */
  case class Boxer(val tpe: ValueType, other: ValueType, boxFn: String, unboxFn: String) {
    def box(p: Pure) = Pure.PureApp(core.BlockVar(Id(boxFn), coercerType(other, tpe), Set()), Nil, List(p))
    def unbox(p: Pure) = Pure.PureApp(core.BlockVar(Id(unboxFn), coercerType(tpe, other), Set()), Nil, List(p))
  }

  def coercerType(from: core.ValueType, into: core.ValueType): core.BlockType.Function =
    core.BlockType.Function(List(), List(), List(from), List(), into)

  val TBoxedInt = ValueType.Data(Id("BoxedInt"), Nil)
  val TBoxedByte = ValueType.Data(Id("BoxedByte"), Nil)
  val TBoxedDouble = ValueType.Data(Id("BoxedDouble"), Nil)

  /**
   * Partial function to describe which values to box and how.
   * Is defined iff values of the given type should be boxed.
   *
   * @param tpe The [[ValueType]] of the value
   * @return a [[Boxer]] that describes how to box values of that type
   */
  def boxer: PartialFunction[ValueType, Boxer] = {
    case core.Type.TInt =>
      Boxer(TBoxedInt, core.Type.TInt, "@coerceIntPos", "@coercePosInt")
    case core.Type.TChar =>
      Boxer(TBoxedInt, core.Type.TChar, "@coerceIntPos", "@coercePosInt")
    case core.Type.TByte =>
      Boxer(TBoxedByte, core.Type.TByte, "@coerceBytePos", "@coercePosByte")
    case core.Type.TDouble =>
      Boxer(TBoxedDouble, core.Type.TDouble, "@coerceDoublePos", "@coercePosDouble")
  }

  def DeclarationContext(using ctx: DeclarationContext): DeclarationContext = ctx

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      implicit val pctx: DeclarationContext = new DeclarationContext(core.declarations, core.externs)
      Context.module = mod
      val transformed = Context.timed(phaseName, source.name) { transform(core) }

      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations map transform, externs map transform, definitions map transform, exports)
  }

  def transform(declaration: Declaration)(using Context, DeclarationContext): Declaration = declaration match {
    case Declaration.Data(id, tparams, constructors) =>
      Declaration.Data(id, tparams, constructors map transform)
    case Declaration.Interface(id, tparams, properties) =>
      Declaration.Interface(id, tparams, properties map transform)
  }

  def transform(constructor: Constructor)(using Context, DeclarationContext): Constructor = constructor match {
    case Constructor(id, fields) => Constructor(id, fields map transform)
  }

  def transform(property: Property)(using Context, DeclarationContext): Property = property match {
    case Property(id, tpe) => Property(id, transform(tpe))
  }

  def transform(field: Field)(using Context, DeclarationContext): Field = field match {
    case Field(id, tpe) => Field(id, transform(tpe))
  }

  def transform(extern: Extern)(using Context, DeclarationContext): Extern = extern match {
    case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
      Extern.Def(id, tparams, cparams, vparams map transform, bparams map transform, transform(ret),
        annotatedCapture, body match {
          case ExternBody.StringExternBody(ff, bbody) => ExternBody.StringExternBody(ff, Template(bbody.strings, bbody.args map transform))
          case e @ ExternBody.Unsupported(_) => e
        } )
    case Extern.Include(ff, contents) => Extern.Include(ff, contents)
  }

  def transform(valueParam: ValueParam)(using Context, DeclarationContext): ValueParam = valueParam match {
    case ValueParam(id, tpe) => ValueParam(id, transform(tpe))
  }

  def transform(blockParam: BlockParam)(using Context, DeclarationContext): BlockParam = blockParam match {
    case BlockParam(id, tpe, capt) => BlockParam(id, transform(tpe), capt)
  }

  def transform(toplevel: Toplevel)(using Context, DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, transform(tpe), coerce(transform(binding), transform(tpe)))
  }

  def transform(block: Block.BlockLit)(using Context, DeclarationContext): Block.BlockLit = block match {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Block.BlockLit(tparams, cparams, vparams map transform, bparams map transform,
        transform(body))
  }
  def transform(block: Block)(using Context, DeclarationContext): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Block.BlockVar(id, transform(annotatedTpe), annotatedCapt)
    case b: Block.BlockLit => transform(b)
    case Block.Unbox(pure) =>
      Block.Unbox(transform(pure))
    case Block.New(impl) =>
      Block.New(transform(impl))
  }
  def transform(blockVar: BlockVar)(using Context, DeclarationContext): BlockVar = blockVar match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Block.BlockVar(id, transform(annotatedTpe), annotatedCapt)
  }

  def transform(implementation: Implementation)(using Context, DeclarationContext): Implementation = implementation match {
    case Implementation(BlockType.Interface(symbol, targs), operations) =>
      val ifce = DeclarationContext.findInterface(symbol).getOrElse { Context.abort(s"No declaration for interface ${symbol}") }
      Implementation(BlockType.Interface(symbol, targs map transformArg), operations map transform(ifce, targs))
  }

  def transform(ifce: Interface, targs: List[ValueType])(operation: Operation)(using Context, DeclarationContext): Operation = operation match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      val prop = ifce.properties.find { p => p.id == name }.getOrElse { Context.abort(s"Interface ${ifce} declares no operation ${name}.") }
      val propTpe = prop.tpe.asInstanceOf[BlockType.Function]

      val blockTpe = BlockType.Function(tparams, propTpe.cparams, propTpe.vparams.map(transform), propTpe.bparams.map(transform), transform(propTpe.result))
      val implBlock: Block.BlockLit = Block.BlockLit(tparams, cparams, vparams, bparams, transform(body))
      val transformed: Block.BlockLit = coerce(implBlock, blockTpe)
      Operation(name, transformed.tparams, transformed.cparams, transformed.vparams, transformed.bparams,
        transformed.body)
  }

  def transform(stmt: Stmt)(using Context, DeclarationContext): Stmt = stmt match {
    case Stmt.Def(id, block, rest) =>
      Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, tpe, binding, rest) =>
      transform(binding).flatMap { e => coerce(e, transform(tpe)) }.run { e =>
        Stmt.Let(id, transform(tpe), e, transform(rest))
      }
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, tpe, binding, body) =>
      Stmt.Val(id, transform(tpe), coerce(transform(binding), transform(tpe)), transform(body))
    case Stmt.App(callee, targs, vargs, bargs) =>
      val calleeT = transform(callee)
      val tpe: BlockType.Function = calleeT.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))

      val vCoerced = (vargs zip tpe.vparams).map { (v, tpe) => coerce(transform(v), tpe) } // coerce(coerce(transform(v), itpe), itpe, tpe) }
      val bCoerced = (bargs zip tpe.bparams).map { (b, tpe) => coerce(transform(b), tpe) } // coerce(coerce(transform(b), itpe), itpe, tpe) }

      coerce(App(calleeT, targs.map(transformArg), vCoerced, bCoerced), itpe.result)

    //                               [S](S) => (Int, S)
    case Stmt.Invoke(callee, method, methodTpe: BlockType.Function, targs, vargs, bargs) =>
      //                                        Double

      val calleeT = transform(callee)

      // [S](S) => (T, S)
      val (tpe: BlockType.Function, interfaceParams, interfaceArgs) = calleeT.tpe match {
        //                             [Int]
        case BlockType.Interface(name, targs) =>
          DeclarationContext.findInterface(name) match {
            //                      [T]
            case Some(Interface(id, tparams, properties)) =>
              // op: [S](S) => (T, S)
              val prop = properties.find { p => p.id == method }.getOrElse { Context.panic(pp"Cannot find field ${method} in declaration of ${id}") }

              (prop.tpe.asInstanceOf[BlockType.Function], tparams, targs)
            case _ =>
              Context.panic(pp"Should not happen. Method call on extern interface: ${stmt}")
          }
        case _ =>
          Context.panic("Should not happen. Method call on non-interface")
      }

      // [S](S) => (BoxedInt, S)
      val boxedTpe = Type.substitute(tpe, (interfaceParams zip interfaceArgs.map(transformArg)).toMap, Map.empty).asInstanceOf[BlockType.Function]

      // duplicated from App
      val itpe = Type.instantiate(methodTpe, targs, methodTpe.cparams.map(Set(_)))
      val vCoerced = (vargs zip boxedTpe.vparams).map { (a, tpe) => coerce(transform(a), tpe) }
      val bCoerced = (bargs zip boxedTpe.bparams).map { (a, tpe) => coerce(transform(a), tpe) }

      //                     (T, S)      (Int, Double)
      coerce(Invoke(calleeT, method, boxedTpe, targs.map(transformArg), vCoerced, bCoerced), itpe.result)

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???

    case Stmt.Get(id, tpe, ref, capt, body) => Stmt.Get(id, transform(tpe), ref, capt, transform(body))
    case Stmt.Put(ref, capt, value, body) => Stmt.Put(ref, capt, transform(value), transform(body))
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.tpe match {
        case ValueType.Data(symbol, targs) =>
          val Declaration.Data(tpeId, tparams, constructors) = DeclarationContext.getData(symbol)
          Stmt.Match(transform(scrutinee), clauses.map {
            case (id, clause: Block.BlockLit) =>
              val constructor = constructors.find(_.id == id).get
              val casetpe: BlockType.Function = BlockType.Function(tparams, List(),
                constructor.fields.map(_.tpe), List(), Type.inferType(clause.body)
              )
              (id, coerce(transform(clause), Type.instantiate(casetpe, targs map transformArg, List())))
          }, default map transform)
        case t => Context.abort(pp"Match on value of type ${t}")
      }
    case Stmt.Alloc(id, init, region, body) =>
      Stmt.Alloc(id, transform(init), region, transform(body))
    case Stmt.Var(id, init, cap, body) =>
      Stmt.Var(id, transform(init), cap, transform(body))
    case Stmt.Reset(BlockLit(tps, cps, vps, prompt :: Nil, body)) =>
      Stmt.Reset(BlockLit(tps, cps, vps, prompt :: Nil, coerce(transform(body), stmt.tpe)))
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, body) =>
      Stmt.Shift(prompt, transform(body))
    case Stmt.Resume(k, body) =>
      val expected = k.tpe match {
        case core.Type.TResume(result, answer) => result
        case _ => ???
      }
      Stmt.Resume(k, coerce(transform(body), expected))

    case Stmt.Region(body) =>
      transform(body) match {
        case BlockLit(tparams, cparams, vparams, bparams, body) =>
          val originalResult = body.tpe
          val boxedResult = transformArg(originalResult)
          coerce(Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, coerce(body, boxedResult))), originalResult)
      }
    case Stmt.Hole() => Stmt.Hole()
  }

  def transform(expr: Expr)(using Context, DeclarationContext): Bind[Expr] = expr match {
    case DirectApp(b, targs, vargs, bargs) =>
      val callee = transform(b)
      val tpe: BlockType.Function = callee.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))
      val vCoerced = (vargs zip tpe.vparams).map { case (a, tpe) => coerce(transform(a), tpe) } // this was "a.tpe -> itpe -> tpe"
      val bCoerced = (bargs zip tpe.bparams).map { case (a, tpe) => coerce(transform(a), tpe) }

      coerce(DirectApp(callee, targs.map(transformArg), vCoerced, bCoerced), itpe.result)

    case pure: Pure => Bind.pure(transform(pure))
  }

  def transform(pure: Pure)(using Context, DeclarationContext): Pure = pure match {
    case Pure.ValueVar(id, annotatedType) => Pure.ValueVar(id, transform(annotatedType))
    case Pure.Literal(value, annotatedType) => Pure.Literal(value, transform(annotatedType))
    case Pure.PureApp(b, targs, vargs) =>
      val callee = transform(b)
      val tpe: BlockType.Function = callee.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))
      val vCoerced = (vargs zip tpe.vparams).map { (a, tpe) => coerce(transform(a), tpe) }
      coerce(PureApp(callee, targs.map(transformArg), vCoerced), itpe.result)

    case Pure.Make(data, tag, targs, vargs) =>
      val dataDecl = DeclarationContext.getData(data.name)
      val ctorDecl = dataDecl.constructors.find(_.id == tag).getOrElse {
        Context.panic(pp"No constructor found for tag ${tag} in data type: ${data}")
      }
      val paramTypes = ctorDecl.fields.map(_.tpe)

      val coercedArgs = (vargs zip paramTypes).map { case (arg, paramTpe) => coerce(transform(arg), paramTpe) }
      Pure.Make(transform(data), tag, targs.map(transformArg), coercedArgs)

    case Pure.Box(b, annotatedCapture) =>
      Pure.Box(transform(b), annotatedCapture)
  }

  def transform(valueType: ValueType)(using Context, DeclarationContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name)
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }

  def transform(valueType: ValueType.Data)(using Context, DeclarationContext): ValueType.Data = valueType match {
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
  }

  def transform(blockType: BlockType)(using Context, DeclarationContext): BlockType = blockType match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      BlockType.Function(tparams, cparams, vparams map transform, bparams map transform, transform(result))

    // Special case some types to not introduce boxing
    case i @ BlockType.Interface(TState.interface, _) => i
    case i @ BlockType.Interface(core.Type.ResumeSymbol, _) => i
    case i @ BlockType.Interface(core.Type.PromptSymbol, _) => i

    case BlockType.Interface(symbol, targs) => BlockType.Interface(symbol, targs map transformArg)
  }

  def transformArg(valueType: ValueType)(using Context, DeclarationContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name) // assume vars are always OK
    case t if boxer.isDefinedAt(t) => boxer(t).tpe
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }


  // Coercions
  // ---------
  def coerce(stmt: Stmt, to: ValueType)(using Context, DeclarationContext): Stmt =
    val from = stmt.tpe
    val coerce = ValueCoercer(from, to)
    if (coerce.isIdentity) { stmt }
    else {
      val orig = TmpValue("coe")
      Stmt.Val(orig, coerce.from, stmt, Stmt.Return(coerce(ValueVar(orig, coerce.from))))
    }

  def coerce(expr: Expr, to: ValueType)(using Context, DeclarationContext): Bind[Expr] =
    val from = expr.tpe
    val coerce = ValueCoercer(from, to)
    if (coerce.isIdentity) { Bind.pure(expr) }
    else { Bind.bind(expr).map { x => coerce(x) } }

  def coerce(pure: Pure, to: ValueType)(using Context, DeclarationContext): Pure = ValueCoercer(pure.tpe, to)(pure)

  def coerce(block: Block, to: BlockType)(using Context, DeclarationContext): Block = BlockCoercer(block.tpe, to)(block)

  def coerce(block: BlockLit, to: BlockType)(using Context, DeclarationContext): BlockLit = BlockCoercer(block.tpe, to)(block)


  sealed  trait ValueCoercer {
    def from: ValueType
    def to: ValueType
    def apply(t: Pure): Pure
    def isIdentity: Boolean = false
  }
  object ValueCoercer {

    def apply(from: ValueType, to: ValueType)(using Context, DeclarationContext): ValueCoercer = (from, to) match {
      case (f, t) if f == t => IdentityCoercer(f, t)
      case (_: ValueType.Var, _: ValueType.Var) => IdentityCoercer(from, to) // are always boxed
      case (unboxed, boxed) if boxer.isDefinedAt(unboxed) && boxer(unboxed).tpe == boxed => BoxCoercer(unboxed)
      case (unboxed, _: ValueType.Var) if boxer.isDefinedAt(unboxed) => BoxCoercer(unboxed)
      case (boxed, unboxed) if boxer.isDefinedAt(unboxed) && boxer(unboxed).tpe == boxed => UnboxCoercer(unboxed)
      case (_: ValueType.Var, unboxed) if boxer.isDefinedAt(unboxed) => UnboxCoercer(unboxed)
      case (unboxed, core.Type.TTop) if boxer.isDefinedAt(unboxed) => BoxCoercer(unboxed)
      case (core.Type.TBottom, unboxed) if boxer.isDefinedAt(unboxed) => BottomCoercer(unboxed)

      // assert(cs1 == cs2) // FIXME this seems to fail, what would be the correct check for subcapturing (or similar) here?
      case (f @ core.ValueType.Boxed(bt1, cs1), t @ core.ValueType.Boxed(bt2, cs2)) =>
        new ValueCoercer {
          val from: ValueType = f
          val to: ValueType = t
          private val bcoercer = BlockCoercer(bt1, bt2)
          override def isIdentity: Boolean = bcoercer.isIdentity
          override def apply(t: Pure): Pure = if isIdentity then t else t match {
            case Pure.Box(b, annotatedCapture) => Pure.Box(bcoercer(b), annotatedCapture)
            case other => Pure.Box(bcoercer(Block.Unbox(t)), cs2)
          }
        }
      case _ =>
        //Context.warning(s"Coercing ${PrettyPrinter.format(from)} to ${PrettyPrinter.format(to)}")
        IdentityCoercer(from, to)
    }

    class IdentityCoercer(val from: ValueType, val to: ValueType) extends ValueCoercer {
      override def apply(t: Pure): Pure = t
      override def isIdentity: Boolean = true
    }
    case class BoxCoercer(tpe: ValueType)(using Context, DeclarationContext) extends ValueCoercer {
      override def from = tpe
      override def to = boxer(tpe).tpe
      override def apply(t: Pure): Pure = boxer(tpe).box(t)
    }
    case class UnboxCoercer(tpe: ValueType)(using Context, DeclarationContext) extends ValueCoercer {
      override def from = boxer(tpe).tpe
      override def to = tpe
      override def apply(t: Pure): Pure = boxer(tpe).unbox(t)
    }
    case class BottomCoercer(tpe: ValueType)(using Context, DeclarationContext) extends ValueCoercer {
      override def from = core.Type.TBottom
      override def to = tpe

      override def apply(t: Pure): Pure = to match {
        case core.Type.TInt     => Pure.Literal(1337L, core.Type.TInt)
        case core.Type.TDouble  => Pure.Literal(13.37, core.Type.TDouble)
        // Do strings need to be boxed? Really?
        case core.Type.TString  => Pure.Literal("<?nothing>", core.Type.TString)
        case core.Type.TByte    => Pure.Literal(1337, core.Type.TByte)
        case core.Type.TChar    => Pure.Literal('a', core.Type.TChar)
        case t if boxer.isDefinedAt(t) => sys error s"No default value defined for ${t}"
        case _ => sys error s"Trying to unbox Nothing to ${t}"
      }
    }
  }

  sealed trait BlockCoercer {
    def from: BlockType
    def to: BlockType

    def apply[Te >: Block.BlockLit <: Block](t: Te): Te
    def isIdentity: Boolean
  }
  object BlockCoercer {
    def apply(from: BlockType, to: BlockType, targs: List[ValueType] = Nil)(using Context, DeclarationContext): BlockCoercer =
      (from, to) match {
        case (f, t) if f == t => IdentityCoercer(f, t)
        case (f: BlockType.Function, t: BlockType.Function) => FunctionCoercer(f, t, targs)
        case (f: BlockType.Interface, t: BlockType.Interface) => IdentityCoercer(f, t)
        case _ => Context.abort(pp"Unsupported coercion from ${from} to ${to}")
      }

    class IdentityCoercer(val from: BlockType, val to: BlockType) extends BlockCoercer {
      override def apply[Te >: Block.BlockLit <: Block](t: Te): Te = t
      override def isIdentity: Boolean = true
    }

    class FunctionCoercer(
      val from: BlockType.Function,
      val to: BlockType.Function,
      targs: List[ValueType]
    )(using Context, DeclarationContext) extends BlockCoercer {

      private val BlockType.Function(ftparams, fcparams, fvparams, fbparams, fresult) = from
      private val BlockType.Function(ttparams, tcparams, tvparams, tbparams, tresult) = to

      val vcoercers = (fvparams zip tvparams).map { case (t, f) => ValueCoercer(f, t) }
      val bcoercers = (fbparams zip tbparams).map { case (t, f) => BlockCoercer(f,t) }
      val rcoercer = ValueCoercer(fresult, tresult)

      override def isIdentity = (rcoercer :: vcoercers).forall(_.isIdentity) && bcoercers.forall(_.isIdentity)

      override def apply[Te >: Block.BlockLit <: Block](block: Te): Te = if (isIdentity) block else {
        val vparams = vcoercers.map { c => ValueParam(TmpValue("coe"), transform(c.from)) }
        val bparams = bcoercers.map { c => val id = TmpBlock("coe"); BlockParam(id, transform(c.from), Set(id)) }

        val vargs = (vcoercers zip vparams).map { case (c, p) => c(Pure.ValueVar(p.id, p.tpe)) }
        val bargs = (bcoercers zip bparams).map { case (c, p) => c(Block.BlockVar(p.id, p.tpe, Set.empty)) }
        Block.BlockLit(ftparams, bparams.map(_.id), vparams, bparams,
            coerce(Stmt.App(
              block,
              (targs map transformArg) ++ (ftparams map core.ValueType.Var.apply),
              vargs,
              bargs), tresult))
      }
    }
  }
}
