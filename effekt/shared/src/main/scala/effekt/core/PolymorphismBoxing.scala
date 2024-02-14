package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.symbols
import effekt.symbols.{ TmpBlock, TmpValue }
import effekt.{ CoreTransformed, Phase }
import effekt.symbols.builtins.{ TBoolean, TDouble, TInt, TState, TUnit }

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
   * @param tpe The type of the values to be boxed
   * @param constructor The constructor to box the values with
   * @param field The field to access for unboxing
   */
  case class Boxer(tpe: ValueType.Data, constructor: Id, field: Id)

  /**
   * Partial function to describe which values to box and how.
   * Is defined iff values of the given type should be boxed.
   *
   * @param tpe The [[ValueType]] of the value
   * @return a [[Boxer]] that describes how to box values of that type
   */
  def box(using PContext): PartialFunction[ValueType, Boxer] = {
    case core.Type.TInt     => PContext.boxer("Int")
    case core.Type.TBoolean => PContext.boxer("Boolean")
    case core.Type.TDouble  => PContext.boxer("Double")
    case core.Type.TUnit    => PContext.boxer("Unit")
    // Do strings need to be boxed? Really?
    case core.Type.TString  => PContext.boxer("String")
  }

  class PContext(val declarations: List[Declaration])(using val Context: Context){
    def findDeclarations(id: Id): List[Declaration] = {
      declarations.filter(_.id == id)
    }
    def getDataLikeDeclaration(id: Id): Declaration.Data = {
      val decls: List[Declaration.Data] = declarations.flatMap{
        case d : Declaration.Data if d.id == id =>
          Some(d)
        case _ => None
      }
      if (decls.length != 1) {
        Context.abort(s"No unique declaration for ${id}. Options: \n" +
          (decls.map(d => PrettyPrinter.pretty(PrettyPrinter.indent(PrettyPrinter.toDoc(d)), 60).layout)).mkString("\n\n"))
      } else {
        decls.head
      }
    }

    lazy val prelude = Context.module.findPrelude

    /**
     * Finds the corresponding boxer for a primitive type.
     *
     * @param name The name of the [[ValueType]]
     * @return a [[Boxer]] that describes how to box values of that type
     */
    def boxer(name: String): Boxer = prelude.types.get("Boxed" + name) match {
      case Some(value) => value match {
        case tpeCns @ symbols.TypeConstructor.Record(_, List(), cns @ symbols.Constructor(_, List(), List(field), _)) =>
          Boxer(ValueType.Data(tpeCns, List()), cns, field)
        case tpeCns @ symbols.TypeConstructor.DataType(_, List(), List(cns @ symbols.Constructor(_, List(), List(field), _))) =>
          Boxer(ValueType.Data(tpeCns, List()), cns, field)
        case _ =>
          Context.abort(s"No appropriate Boxed${name} type, but ${name} used as type parameter.")
      }
      case None =>
        Context.abort(s"Primitive ${name} is used as type parameter but Boxed${name} is not defined.")
    }
  }
  def PContext(using ctx: PContext): PContext = ctx
  implicit def Context(using ctx: PContext): Context = ctx.Context

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      implicit val pctx: PContext = new PContext(core.declarations)
      Context.module = mod
      val transformed = transform(core)
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using PContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations map transform, externs map transform, definitions map transform, exports)
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
        annotatedCapture, Template(body.strings, body.args map transform))
    case Extern.Include(contents) => Extern.Include(contents)
  }

  def transform(valueParam: Param.ValueParam)(using PContext): Param.ValueParam = valueParam match {
    case Param.ValueParam(id, tpe) => Param.ValueParam(id, transform(tpe))
  }
  def transform(blockParam: Param.BlockParam)(using PContext): Param.BlockParam = blockParam match {
    case Param.BlockParam(id, tpe, capt) => Param.BlockParam(id, transform(tpe), capt)
  }

  def transform(definition: Definition)(using PContext): Definition = definition match {
    case Definition.Def(id, block) => Definition.Def(id, transform(block))
    case Definition.Let(id, binding) => Definition.Let(id, transform(binding))
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
      Stmt.Scope(definitions map transform, transform(body))
    case Stmt.Return(expr) => Stmt.Return(transform(expr))
    case Stmt.Val(id, binding, body) => Stmt.Val(id, transform(binding), transform(body))
    case Stmt.App(callee, targs, vargs, bargs) =>
      val calleeT = transform(callee)
      instantiate(calleeT, targs).call(calleeT, vargs map transform, bargs map transform)
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, transform(tpe))
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, transform(value))
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.tpe match {
        case ValueType.Data(symbol, targs) =>
          val Declaration.Data(tpeId, tparams, constructors) = PContext.getDataLikeDeclaration(symbol)
          Stmt.Match(transform(scrutinee), clauses.map {
            case (id, clause: Block.BlockLit) =>
              val constructor = constructors.find(_.id == id).get
              val casetpe: BlockType.Function = BlockType.Function(tparams, List(),
                constructor.fields.map(_.tpe), List(), Type.inferType(clause.body)
              )
              (id, coercer(clause.tpe, Type.instantiate(casetpe, targs map transformArg, List()))(transform(clause)))
          }, default map transform)
        case t => Context.abort(s"Match on value of type ${PrettyPrinter.format(t)}")
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
      instantiate(b, targs).callDirect(b, vargs map transform, bargs map transform)
    case Run(s) => Run(transform(s))
    case pure: Pure => transform(pure)
  }

  def transform(pure: Pure)(using PContext): Pure = pure match {
    case Pure.ValueVar(id, annotatedType) => Pure.ValueVar(id, transform(annotatedType))
    case Pure.Literal(value, annotatedType) => Pure.Literal(value, transform(annotatedType))
    case Pure.PureApp(b, targs, vargs) => instantiate(b, targs).callPure(b, vargs map transform)
    case Pure.Make(data, tag, vargs) =>
      val dataDecl = PContext.getDataLikeDeclaration(data.name)
      val ctorDecl = dataDecl.constructors.find(_.id == tag).getOrElse {
        Context.panic(s"No constructor found for tag ${tag} in data type: ${data}")
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
      PContext.getDataLikeDeclaration(symbol) match {
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
      Pure.Make(boxer.tpe, boxer.constructor, List(t))
    }
  }
  case class UnboxCoercer(valueType: ValueType)(using PContext) extends Coercer[ValueType, Pure] {
    override def from = box(valueType).tpe
    override def to = valueType

    override def apply(t: Pure): Pure = {
      val boxer = box(valueType)
      Pure.Select(t, boxer.field, to)
    }
  }

  def coercer(from: ValueType, to: ValueType)(using PContext): Coercer[ValueType, Pure] = (from, to) match {
    case (f,t) if f==t => new IdentityCoercer(f,t)
    case (_: ValueType.Var, _: ValueType.Var) => new IdentityCoercer(from, to) // are always boxed
    case (unboxed, boxed) if box.isDefinedAt(unboxed) && box(unboxed).tpe == boxed => BoxCoercer(unboxed)
    case (unboxed, _: ValueType.Var) if box.isDefinedAt(unboxed) => BoxCoercer(unboxed)
    case (boxed, unboxed) if box.isDefinedAt(unboxed) && box(unboxed).tpe == boxed => UnboxCoercer(unboxed)
    case (_: ValueType.Var, unboxed) if box.isDefinedAt(unboxed) => UnboxCoercer(unboxed)
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
              Stmt.Val(result, Stmt.App(Block.BlockVar(inner, block.tpe, block.capt), targs map transformArg, vargs, bargs),
                Stmt.Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))))
        }

        override def callPure(block: B, vargs: List[Pure])(using PContext): Pure = {
          rcoercer(Pure.PureApp(block, targs map transformArg, (vcoercers zip vargs).map { case (c,v) => c(v) }))
        }

        override def callDirect(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Expr = {
          val result = TmpValue()
          Run(Let(result, DirectApp(block, targs map transformArg,
            (vcoercers zip vargs).map {case (c,v) => c(v)},
            (bcoercers zip bargs).map {case (c,b) => c(b)}),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from)))))
        }

        override def call(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Stmt = {
          val result = TmpValue()
          Stmt.Val(result, Stmt.App(block, targs map transformArg,
            (vcoercers zip vargs).map {case (c,v) => c(v)},
            (bcoercers zip bargs).map {case (c,b) => c(b)}),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))
        }

      }
    case _ => Context.abort(s"Unsupported coercion from ${PrettyPrinter.format(fromtpe)} to ${PrettyPrinter.format(totpe)}")
  }

}
