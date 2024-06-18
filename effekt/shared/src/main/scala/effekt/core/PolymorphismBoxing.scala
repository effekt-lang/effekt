package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.symbols
import effekt.symbols.{ TmpBlock, TmpValue }
import effekt.{ CoreTransformed, Phase }
import effekt.symbols.builtins.{ TBoolean, TDouble, TInt, TChar, TByte, TState, TUnit }
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
   * Describes how to box/unbox values
   */
  trait Boxer {
    /** The type of the boxed values */
    def tpe: ValueType
    /** Generates a pure expression boxing the parameter */
    def box(p: Pure): Pure
    /** Generates a pure expression unboxing the parameter */
    def unbox(p: Pure): Pure
  }
  /**
   * Describes how to box/unbox values using extern functions
   * @param tpe The type of the boxed values
   * @param boxFn The extern function to call to box
   * @param unboxFn The extern function to call to unbox
   */
  case class ExternFnBoxer(val tpe: ValueType, boxFn: Block.BlockVar, unboxFn: Block.BlockVar) extends Boxer {
    def box(p: Pure) = Pure.PureApp(boxFn, Nil, List(p))
    def unbox(p: Pure) = Pure.PureApp(unboxFn, Nil, List(p))
  }

  /**
   * Describes how to box/unbox values using records
   *
   * @param boxTpe       The type BoxedT
   * @param constructor  The constructor to use for boxing
   * @param field        The field to access for unboxing
   */
  case class RecordBoxer(boxTpe: ValueType.Data, constructor: Constructor, field: Field) extends Boxer {
    def tpe = boxTpe
    def box(p: Pure) = Pure.Make(boxTpe, constructor.id, List(p))
    def unbox(p: Pure) = Pure.Select(p, field.id, field.tpe)
  }

  /**
   * Partial function to describe which values to box and how.
   * Is defined iff values of the given type should be boxed.
   *
   * @param tpe The [[ValueType]] of the value
   * @return a [[Boxer]] that describes how to box values of that type
   */
  def box(using PContext): PartialFunction[ValueType, Boxer] = {
    case core.Type.TInt     => PContext.boxer("Int")
    case core.Type.TChar    => PContext.boxer("Char")
    case core.Type.TByte    => PContext.boxer("Byte")
    case core.Type.TDouble  => PContext.boxer("Double")
  }

  class PContext(declarations: List[Declaration], externs: List[Extern])(using val Context: Context) extends DeclarationContext(declarations, externs){
    /**
     * Find a pure extern def with one value parameter named [[name]] in the prelude (or some namespace in the prelude).
     */
    def findPureExternFn(name: String): Option[Block.BlockVar] = {
       this.externs.collectFirst {
        case d@effekt.core.Extern.Def(id, List(), List(), List(vparam), List(), ret, capts, body)
          if id.name.name == name && capts.isEmpty => d
      }.map { definition =>
         val id = definition.id
         val tpe = core.BlockType.Function(Nil, Nil, definition.vparams.map(_.tpe), Nil, definition.ret)
         Block.BlockVar(id, tpe, Set.empty)
       }
    }

    /**
     * Find a record declaration named [[name]] with one field in the prelude (or some namespace in the prelude).
     */
    def findRecord(name: String): Option[Declaration.Data] = {
      this.declarations.collectFirst {
        case d@effekt.core.Declaration.Data(tpe, List(), List(Constructor(cns, List(fld))))
          if tpe.name.name == name => d
      }
    }

    /**
     * Finds the corresponding boxer for a primitive type.
     *
     * @param name The name of the [[ValueType]]
     * @return a [[Boxer]] that describes how to box values of that type
     */
    def boxer(name: String): Boxer = {
      /** Try to find `boxT` and `unboxT` externs */
      def findExternFnBoxer() = boundary {
        val box = findPureExternFn("box" ++ name).getOrElse { boundary.break(None) }
        val unbox = findPureExternFn("unbox" ++ name).getOrElse { boundary.break(None) }
        val boxRet = box.annotatedTpe match {
          case BlockType.Function(_, _, _, _, result) => result
          case _ => Context.abort(pp"${box} is not of function type.")
        }
        unbox.annotatedTpe match {
          case BlockType.Function(_, _, List(unboxArg), List(), _) =>
            if (unboxArg != boxRet) {
              Context.abort(pp"Argument type of ${unbox} and return type of ${box} do not match up.")
            }
          case _ => Context.abort(pp"${unbox} is not of function type.")
        }
        Some(ExternFnBoxer(boxRet, box, unbox))
      }
      /** Try to find a `BoxedT` type */
      def findRecordBoxer() = boundary {
        findRecord("Boxed" ++ name) match {
          case Some(Declaration.Data(tpe, List(), List(cns@Constructor(id, List(field))))) =>
            Some(RecordBoxer(ValueType.Data(tpe, Nil), cns, field))
          case _ => None
        }
      }
      findExternFnBoxer() orElse findRecordBoxer() getOrElse {
        Context.abort(s"Type ${name}, which needs to be boxed, is used as a type argument but no " +
          s"corresponding pure externs box${name} and unbox${name} were defined in the prelude, " +
          s"and also no record type Boxed${name}.")
      }
    }
  }
  def PContext(using ctx: PContext): PContext = ctx
  implicit def Context(using ctx: PContext): Context = ctx.Context

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      implicit val pctx: PContext = new PContext(core.declarations, core.externs)
      Context.module = mod
      val transformed = Context.timed(phaseName, source.name) { transform(core) }
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using PContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations map transform, externs map transform, definitions flatMap transform, exports)
  }

  def transform(declaration: Declaration)(using PContext): Declaration = declaration match {
    case Declaration.Data(id, tparams, constructors) =>
      Declaration.Data(id, tparams, constructors map transform)
    case Declaration.Interface(id, tparams, properties) =>
      Declaration.Interface(id, tparams, properties map transform)
  }

  def transform(constructor: Constructor)(using PContext): Constructor = constructor match {
    case Constructor(id, fields) => Constructor(id, fields map transform)
  }

  def transform(property: Property)(using PContext): Property = property match {
    case Property(id, tpe) => Property(id, transform(tpe))
  }

  def transform(field: Field)(using PContext): Field = field match {
    case Field(id, tpe) => Field(id, transform(tpe))
  }

  def transform(extern: Extern)(using PContext): Extern = extern match {
    case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
      Extern.Def(id, tparams, cparams, vparams map transform, bparams map transform, transform(ret),
        annotatedCapture, body match {
          case ExternBody.StringExternBody(ff, bbody) => ExternBody.StringExternBody(ff, Template(bbody.strings, bbody.args map transform))
          case e @ ExternBody.Unsupported(_) => e
        } )
    case Extern.Include(ff, contents) => Extern.Include(ff, contents)
  }

  def transform(valueParam: Param.ValueParam)(using PContext): Param.ValueParam = valueParam match {
    case Param.ValueParam(id, tpe) => Param.ValueParam(id, transform(tpe))
  }
  def transform(blockParam: Param.BlockParam)(using PContext): Param.BlockParam = blockParam match {
    case Param.BlockParam(id, tpe, capt) => Param.BlockParam(id, transform(tpe), capt)
  }

  def transform(definition: Definition)(using PContext): List[Definition] = definition match {
    case Definition.Def(id, block) => List(Definition.Def(id, transform(block)))
    case Definition.Let(id, tpe, binding) =>
      val coerce = coercer(binding.tpe, transform(tpe))
      if (coerce.isIdentity) {
        List(Definition.Let(id, transform(tpe), transform(binding)))
      } else {
        val orig = TmpValue()
        val origTpe = binding.tpe
        List(
          Definition.Let(orig, origTpe, transform(binding)),
          Definition.Let(id, transform(tpe), coerce(ValueVar(orig, origTpe))))
      }
  }

  def transform(block: Block.BlockLit)(using PContext): Block.BlockLit = block match {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Block.BlockLit(tparams, cparams, vparams map transform, bparams map transform,
        transform(body))
  }
  def transform(block: Block)(using PContext): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Block.BlockVar(id, transform(annotatedTpe), annotatedCapt)
    case b: Block.BlockLit => transform(b)
    case Block.Member(block, field, annotatedTpe) =>
      Block.Member(transform(block), field, transform(annotatedTpe))
    case Block.Unbox(pure) =>
      Block.Unbox(transform(pure))
    case Block.New(impl) =>
      Block.New(transform(impl))
  }

  def transform(implementation: Implementation)(using PContext): Implementation = implementation match {
    case Implementation(BlockType.Interface(symbol, targs), operations) =>
      Implementation(BlockType.Interface(symbol, targs map transform), operations map transform(targs))

  }

  def transform(targs: List[ValueType])(operation: Operation)(using PContext): Operation = operation match {
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      val blockTpe = BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), transform(body.tpe))
      val implBlock: Block.BlockLit = Block.BlockLit(tparams, cparams, vparams, bparams, transform(body))
      val transformed: Block.BlockLit = coercer(implBlock.tpe, blockTpe)(implBlock)
      Operation(name, transformed.tparams, transformed.cparams, transformed.vparams, transformed.bparams,
        resume map transform,
        transformed.body)
  }

  def transform(stmt: Stmt)(using PContext): Stmt = stmt match {
    case Stmt.Scope(definitions, body) =>
      Stmt.Scope(definitions flatMap transform, transform(body))
    case Stmt.Return(expr) => Stmt.Return(transform(expr))
    case Stmt.Val(id, tpe, binding, body) =>
      val coerce = coercer(binding.tpe, transform(tpe))
      if (coerce.isIdentity) {
        Stmt.Val(id, transform(tpe), transform(binding), transform(body))
      } else {
        val orig = TmpValue()
        Stmt.Val(orig, binding.tpe, transform(binding),
          Let(id, transform(binding.tpe), coerce(Pure.ValueVar(orig, binding.tpe)),
            transform(body)))
      }
    case Stmt.App(callee, targs, vargs, bargs) =>
      val calleeT = transform(callee)
      val tpe: BlockType.Function = calleeT.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))
      val tVargs = vargs map transform
      val tBargs = bargs map transform
      val vcoercers = (tVargs zip itpe.vparams).map { (a, p) => coercer(a.tpe, p) }
      val bcoercers = (tBargs zip itpe.bparams).map { (a, p) => coercer[Block](a.tpe, p) }
      val fcoercer = coercer[Block](tpe, itpe, targs)
      fcoercer.call(calleeT, (vcoercers zip tVargs).map(_(_)), (bcoercers zip tBargs).map(_(_)))
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, transform(tpe))
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, transform(value))
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.tpe match {
        case ValueType.Data(symbol, targs) =>
          val Declaration.Data(tpeId, tparams, constructors) = PContext.getData(symbol)
          Stmt.Match(transform(scrutinee), clauses.map {
            case (id, clause: Block.BlockLit) =>
              val constructor = constructors.find(_.id == id).get
              val casetpe: BlockType.Function = BlockType.Function(tparams, List(),
                constructor.fields.map(_.tpe), List(), Type.inferType(clause.body)
              )
              (id, coercer(clause.tpe, Type.instantiate(casetpe, targs map transformArg, List()))(transform(clause)))
          }, default map transform)
        case t => Context.abort(pp"Match on value of type ${t}")
      }
    case Stmt.Alloc(id, init, region, body) =>
      Stmt.Alloc(id, transform(init), region, transform(body))
    case Stmt.Var(id, init, cap, body) =>
      Stmt.Var(id, transform(init), cap, transform(body))
    case Stmt.Try(body, handlers) =>
      Stmt.Try(transform(body), handlers map transform)
    case Stmt.Region(body) => Stmt.Region(transform(body))
    case Stmt.Hole() => Stmt.Hole()
  }

  def transform(expr: Expr)(using PContext): Expr = expr match {
    case DirectApp(b, targs, vargs, bargs) =>
      val callee = transform(b)
      val tpe: BlockType.Function = callee.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))
      val tVargs = vargs map transform
      val tBargs = bargs map transform
      val vcoercers = (tVargs zip itpe.vparams).map { (a, p) => coercer(a.tpe, p) }
      val bcoercers = (tBargs zip itpe.bparams).map { (a, p) => coercer[Block](a.tpe, p) }
      val fcoercer = coercer[Block](tpe, itpe, targs)
      fcoercer.callDirect(callee, (vcoercers zip tVargs).map(_(_)), (bcoercers zip tBargs).map(_(_)))
    case Run(s) => Run(transform(s))
    case pure: Pure => transform(pure)
  }

  def transform(pure: Pure)(using PContext): Pure = pure match {
    case Pure.ValueVar(id, annotatedType) => Pure.ValueVar(id, transform(annotatedType))
    case Pure.Literal(value, annotatedType) => Pure.Literal(value, transform(annotatedType))
    case Pure.PureApp(b, targs, vargs) =>
      val callee = transform(b)
      val tpe: BlockType.Function = callee.tpe match {
        case tpe: BlockType.Function => tpe
        case _ => sys error "Callee does not have function type"
      }
      val itpe = Type.instantiate(tpe, targs, tpe.cparams.map(Set(_)))
      val tVargs = vargs map transform
      val vcoercers = (tVargs zip itpe.vparams).map { (a, p) => coercer(a.tpe, p) }
      val fcoercer = coercer[Block](tpe, itpe, targs)
      fcoercer.callPure(b, (vcoercers zip tVargs).map(_(_)))
    case Pure.Make(data, tag, vargs) =>
      val dataDecl = PContext.getData(data.name)
      val ctorDecl = dataDecl.constructors.find(_.id == tag).getOrElse {
        Context.panic(pp"No constructor found for tag ${tag} in data type: ${data}")
      }

      val argTypes   = vargs.map(_.tpe)
      val paramTypes = ctorDecl.fields.map(_.tpe)

      val coercedArgs = (paramTypes zip (argTypes zip vargs)).map { case (param, (targ, arg)) =>
        coercer(targ, param)(transform(arg))
      }
      Pure.Make(transform(data), tag, coercedArgs)

    case Pure.Select(target, field, annotatedType) => {
      val (symbol, targs) = target.tpe match {
        case ValueType.Data(symbol, targs) => (symbol, targs)
        case t => Context.abort(s"Select on value of type ${PrettyPrinter.format(t)} is not supported.")
      }
      PContext.getData(symbol) match {
        case Declaration.Data(id, tparams, List(Constructor(cns, fields))) =>
          val f = fields.find(_.id == field).getOrElse{
            Context.abort(s"${id} has no field ${field}.")
          }
          coercer(f.tpe, Type.substitute(f.tpe, (tparams zip targs).toMap, Map()))(Pure.Select(target, field, transform(annotatedType)))
        case t => Context.abort(s"Select on data type ${t.id} is not supported.")
      }
    }
    case Pure.Box(b, annotatedCapture) =>
      Pure.Box(transform(b), annotatedCapture)
  }

  def transform(valueType: ValueType)(using PContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name)
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }

  def transform(valueType: ValueType.Data)(using PContext): ValueType.Data = valueType match {
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
  }

  def transform(blockType: BlockType)(using PContext): BlockType = blockType match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      BlockType.Function(tparams, cparams, vparams map transform, bparams map transform, transform(result))
    case i @ BlockType.Interface(TState.interface, _) => i
    case BlockType.Interface(symbol, targs) => BlockType.Interface(symbol, targs map transformArg)
  }

  def transformArg(valueType: ValueType)(using PContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name) // assume vars are always OK
    case t if box.isDefinedAt(t) => box(t).tpe
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }

  def instantiate[B >: BlockLit <: Block](block: B, targs: List[ValueType])(using PContext): FunctionCoercer[BlockType, B] = {
    block.tpe match {
      case tpe: BlockType.Function =>
        coercer(tpe, Type.instantiate(tpe, targs, tpe.cparams.map(Set(_))), targs)
      case tpe: BlockType.Interface =>
        Context.abort(s"Using interface of type ${tpe} in function position.")
    }
  }

  trait Coercer[Ty <: Type, Te <: Tree] {
    def from: Ty
    def to: Ty

    @targetName("applyType")
    def apply(tpe: Ty): Ty = if tpe == from then to else tpe
    def apply(t: Te): Te
    def isIdentity: Boolean = false
  }
  class IdentityCoercer[Ty <: Type, Te <: Tree](
      override val from: Ty,
      override val to: Ty) extends Coercer[Ty, Te] {
    override def apply(t: Te): Te = t
    override def isIdentity: Boolean = true
  }
  case class BoxCoercer(valueType: ValueType)(using PContext) extends Coercer[ValueType, Pure] {
    override def from = valueType
    override def to = box(valueType).tpe

    override def apply(t: Pure): Pure = {
      val boxer = box(valueType)
      boxer.box(t)
    }
  }
  case class UnboxCoercer(valueType: ValueType)(using PContext) extends Coercer[ValueType, Pure] {
    override def from = box(valueType).tpe
    override def to = valueType

    override def apply(t: Pure): Pure = {
      val boxer = box(valueType)
      boxer.unbox(t)
    }
  }
  case class BottomCoercer(valueType: ValueType)(using PContext) extends Coercer[ValueType, Pure] {
    override def from = core.Type.TBottom
    override def to = valueType

    override def apply(t: Pure): Pure = {
      to match {
        case core.Type.TInt => Pure.Literal(1337L, core.Type.TInt)
        case core.Type.TDouble  => Pure.Literal(13.37, core.Type.TDouble)
        // Do strings need to be boxed? Really?
        case core.Type.TString  => Pure.Literal("<?nothing>", core.Type.TString)
        case t if box.isDefinedAt(t) => sys error s"No default value defined for ${t}"
        case _ => sys error s"Trying to unbox Nothing to ${t}"
      }
    }
  }

  def coercer(from: ValueType, to: ValueType)(using PContext): Coercer[ValueType, Pure] = (from, to) match {
    case (f,t) if f==t => new IdentityCoercer(f,t)
    case (_: ValueType.Var, _: ValueType.Var) => new IdentityCoercer(from, to) // are always boxed
    case (unboxed, boxed) if box.isDefinedAt(unboxed) && box(unboxed).tpe == boxed => BoxCoercer(unboxed)
    case (unboxed, _: ValueType.Var) if box.isDefinedAt(unboxed) => BoxCoercer(unboxed)
    case (boxed, unboxed) if box.isDefinedAt(unboxed) && box(unboxed).tpe == boxed => UnboxCoercer(unboxed)
    case (_: ValueType.Var, unboxed) if box.isDefinedAt(unboxed) => UnboxCoercer(unboxed)
    case (unboxed, core.Type.TTop) if box.isDefinedAt(unboxed) => BoxCoercer(unboxed)
    case (core.Type.TBottom, unboxed) if box.isDefinedAt(unboxed) => BottomCoercer(unboxed)
    case (core.ValueType.Boxed(bt1,cs1), core.ValueType.Boxed(bt2, cs2)) =>
      // assert(cs1 == cs2) // FIXME this seems to fail, what would be the correct check for subcapturing (or similar) here?
      val bcoercer = coercer[Block](bt1, bt2)
      if (bcoercer.isIdentity) then { IdentityCoercer(from, to) } else {
        val _fr = from
        val _to = to
        new Coercer[ValueType, Pure] {
          val from: ValueType = _fr
          val to: ValueType = _to
          override def isIdentity: Boolean = false
          override def apply(t: Pure): Pure = {
            Pure.Box(bcoercer(Block.Unbox(t)), cs2)
          }
        }
      }
    case _ =>
      //Context.warning(s"Coercing ${PrettyPrinter.format(from)} to ${PrettyPrinter.format(to)}")
      new IdentityCoercer(from, to)
  }

  trait FunctionCoercer[Ty <: BlockType, Te <: Block] extends Coercer[Ty, Te] {
    def callPure(block: Te, vargs: List[Pure])(using PContext): Pure
    def callDirect(block: Te, vargs: List[Pure], bargs: List[Block])(using PContext): Expr
    def call(block: Te, vargs: List[Pure], bargs: List[Block])(using PContext): Stmt
  }
  class FunctionIdentityCoercer[Ty <: BlockType, Te <: Block](
      from: Ty, to: Ty, targs: List[ValueType]) extends IdentityCoercer[Ty, Te](from, to) with FunctionCoercer[Ty, Te] {
    override def call(block: Te, vargs: List[Pure], bargs: List[Block])(using PContext): Stmt =
      Stmt.App(block, targs map transformArg, vargs.map(transform), bargs map transform)
    override def callPure(block: Te, vargs: List[Pure])(using PContext): Pure =
      Pure.PureApp(block, targs map transformArg, vargs map transform)
    override def callDirect(block: Te, vargs: List[Pure], bargs: List[Block])(using PContext): Expr =
      DirectApp(block, targs map transformArg, vargs map transform, bargs map transform)
  }
  def coercer[B >: Block.BlockLit <: Block](fromtpe: BlockType, totpe: BlockType, targs: List[ValueType] = List())(using PContext): FunctionCoercer[BlockType, B] =
    (fromtpe, totpe) match {
    case (f,t) if f == t => new FunctionIdentityCoercer(fromtpe, totpe, targs)
    case (BlockType.Function(ftparams, fcparams, fvparams, fbparams, fresult),
          BlockType.Function(ttparams, tcparams, tvparams, tbparams, tresult)) =>

      val vcoercers = (fvparams zip tvparams).map {
        case (t,f) => // note: Order inversed as contravariant in arguments
          coercer(f,t)
      }
      val bcoercers: List[Coercer[BlockType, Block]] = (fbparams zip tbparams).map {
        case (t,f) => // note: Order inversed as contravariant in arguments
          coercer(f,t)
      }
      val rcoercer = coercer(fresult, tresult)

      if((rcoercer +: (vcoercers ++ bcoercers)).forall(_.isIdentity)) {
        return new FunctionIdentityCoercer(fromtpe, totpe, targs) // nothing to do here
      }

      new FunctionCoercer[BlockType, B] {
        override def from = fromtpe
        override def to = totpe

        override def apply(block: B): B = {
          val vparams: List[Param.ValueParam] = vcoercers.map { c => Param.ValueParam(TmpValue(), transform(c.from)) }
          val bparams: List[Param.BlockParam] = bcoercers.map { c => val id = TmpBlock(); Param.BlockParam(id, transform(c.from), Set(id)) }
          val result = TmpValue()
          val inner = TmpBlock()
          val vargs = (vcoercers zip vparams).map { case (c, p) => c(Pure.ValueVar(p.id, p.tpe)) }
          val bargs = (bcoercers zip bparams).map { case (c, p) => c(Block.BlockVar(p.id, p.tpe, Set.empty)) }
          Block.BlockLit(ftparams, bparams.map(_.id), vparams, bparams,
            Def(inner, block,
              Stmt.Val(result, rcoercer.from, Stmt.App(Block.BlockVar(inner, block.tpe, block.capt), targs map transformArg, vargs, bargs),
                Stmt.Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))))
        }

        override def callPure(block: B, vargs: List[Pure])(using PContext): Pure = {
          rcoercer(Pure.PureApp(block, targs map transformArg, (vcoercers zip vargs).map { case (c,v) => c(v) }))
        }

        override def callDirect(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Expr = {
          val result = TmpValue()
          Run(Let(result, rcoercer.from, DirectApp(block, targs map transformArg,
            (vcoercers zip vargs).map {case (c,v) => c(v)},
            (bcoercers zip bargs).map {case (c,b) => c(b)}),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from)))))
        }

        override def call(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Stmt = {
          val result = TmpValue()
          Stmt.Val(result, rcoercer.from, Stmt.App(block, targs map transformArg,
            (vcoercers zip vargs).map { case (c, v) => c(v) },
            (bcoercers zip bargs).map { case (c, b) => c(b) }),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))
        }

      }
    case (BlockType.Interface(n1,targs), BlockType.Interface(n2,_)) =>
      FunctionIdentityCoercer(fromtpe, totpe, targs)
    case _ => Context.abort(pp"Unsupported coercion from ${fromtpe} to ${totpe}")
  }

}
