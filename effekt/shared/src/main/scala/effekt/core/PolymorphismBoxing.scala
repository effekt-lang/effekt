package effekt.core

import effekt.context.Context
import effekt.symbols
import effekt.symbols.{TmpBlock, TmpValue}
import effekt.{CoreTransformed, Phase}

import scala.annotation.targetName

object PolymorphismBoxing extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "polymorphism boxing"

  case class Boxer(tpe: ValueType, constructor: Id, field: Id)
  def box(using PContext): PartialFunction[ValueType, Boxer] = {
    case ValueType.Data(name, List()) if symbols.builtins.rootTypes.values.exists(_ == name) =>
      Context.module.findPrelude.types("Boxed" + name) match {
        case tpeCns @ symbols.TypeConstructor.Record(_, List(), cns @ symbols.Constructor(_, List(), List(field), _)) =>
          Boxer(ValueType.Data(tpeCns,List()), cns, field)
        case tpeCns @ symbols.TypeConstructor.DataType(_, List(), List(cns @ symbols.Constructor(_, List(), List(field), _))) =>
          Boxer(ValueType.Data(tpeCns,List()), cns, field)
        case _ =>
          Context.abort(s"No appropriate Boxed${name} type, but ${name} used as type parameter.")
      }
  }

  class PContext(val declarations: List[Declaration])(using val context: Context){
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
        context.abort(s"No unique declaration for ${id}. Options: \n" +
          (decls.map(d => PrettyPrinter.pretty(PrettyPrinter.indent(PrettyPrinter.toDoc(d)), 60).layout)).mkString("\n\n")) // TODO nicer message
      } else {
        decls.head
      }
    }
  }
  def PContext(using ctx: PContext): PContext = ctx
  implicit def Context(using ctx: PContext): Context = ctx.context

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      implicit val pctx: PContext = new PContext(core.declarations)
      Context.module = mod
      val transformed = transform(core)
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using PContext): ModuleDecl = decl match {
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations map transform, externs map transform, definitions map transform, exports)
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
    // TODO: Should we transform externs?
    case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
      Extern.Def(id, tparams, cparams, vparams map transform, bparams map transform, transform(ret),
        annotatedCapture, body)
    case Extern.Include(contents) => Extern.Include(contents)
  }

  def transform(valueParam: Param.ValueParam)(using PContext): Param.ValueParam = valueParam match {
    case Param.ValueParam(id, tpe) => Param.ValueParam(id, transform(tpe))
  }
  def transform(blockParam: Param.BlockParam)(using PContext): Param.BlockParam = blockParam match {
    case Param.BlockParam(id, tpe) => Param.BlockParam(id, transform(tpe))
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
      // TODO: Should we potentially coerce field?
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
      val blockTpe = BlockType.Function(tparams, cparams, vparams.map(_.tpe), bparams.map(_.tpe), body.tpe)
      val implBlock: Block.BlockLit = Block.BlockLit(tparams, cparams, vparams, bparams, body)
      val transformed: Block.BlockLit = coercer(implBlock.tpe, blockTpe)(implBlock) // instantiate(implBlock, targs)(implBlock) // TODO wrong
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
      instantiate(callee, targs).call(callee, vargs map transform, bargs map transform)
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.tpe match {
        case ValueType.Data(symbol, targs) =>
          val Declaration.Data(tpeId, tparams, constructors) = PContext.getDataLikeDeclaration(symbol)
          Stmt.Match(transform(scrutinee), clauses.map{
            case (id, clause: Block.BlockLit) =>
              val constructor = constructors.find(_.id == id).get
              val casetpe: BlockType.Function = BlockType.Function(tparams, List(),
                constructor.fields.map(_.tpe), List(), Type.inferType(clause.body)
              )
              (id, coercer(clause.tpe, Type.instantiate(casetpe, targs map transformArg, List()))(transform(clause)))
          }, default map transform)
        case t => Context.abort(s"Match on value of type ${PrettyPrinter.format(t)}")
      }
    case Stmt.State(id, init, region, body) =>
      Stmt.State(id, transform(init), region, transform(body))
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
    case Pure.PureApp(b, targs, vargs) =>
      instantiate(b, targs).callPure(b, vargs map transform)
    case Pure.Select(target, field, annotatedType) => ???
    case Pure.Box(b, annotatedCapture) => ???
  }

  def transform(valueType: ValueType)(using PContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name)
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }

  def transform(blockType: BlockType)(using PContext): BlockType = blockType match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      BlockType.Function(tparams, cparams, vparams map transform, bparams map transform, transform(result))
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
      case BlockType.Interface(symbol, targs) => ???
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
      val blockTpe = BlockType.Function(List(), List(), List(from), List(), to)
      Pure.PureApp(Block.BlockVar(boxer.constructor, blockTpe, Set()), List(), List(t))
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
      Stmt.App(block, targs map transformArg, vargs.map(transform(_)), bargs map transform)
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

      val vcoercers = (fvparams zip tvparams).map{
        case (t,f) => // note: Order inversed as contravariant in arguments
          coercer(f,t)
      }
      val bcoercers: List[Coercer[BlockType, Block]] = (fbparams zip tbparams).map{
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
          val vparams: List[Param.ValueParam] = vcoercers.map{ c => Param.ValueParam(TmpValue(), transform(c.from)) }
          val bparams: List[Param.BlockParam] = bcoercers.map{ c => Param.BlockParam(TmpBlock(), transform(c.from)) }
          val result = TmpValue()
          val inner = TmpBlock()
          val vargs = (vcoercers zip vparams).map{ case (c, p) => c(Pure.ValueVar(p.id, p.tpe)) }
          val bargs = (bcoercers zip bparams).map{ case (c, p) => c(Block.BlockVar(p.id, p.tpe, Set.empty)) }
          Block.BlockLit(ftparams, fcparams, vparams, bparams,
            Def(inner, block,
              Stmt.Val(result, Stmt.App(Block.BlockVar(inner, block.tpe, block.capt), targs map transformArg, vargs, bargs),
                Stmt.Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))))
        }

        override def callPure(block: B, vargs: List[Pure])(using PContext): Pure = {
          rcoercer(Pure.PureApp(block, targs map transformArg, (vcoercers zip vargs).map{case (c,v)=> c(v)}))
        }

        override def callDirect(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Expr = {
          val result = TmpValue()
          Run(Let(result, DirectApp(block, targs map transformArg,
            (vcoercers zip vargs).map{case (c,v) => c(v)},
            (bcoercers zip bargs).map{case (c,b) => c(b)}),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from)))))
        }

        override def call(block: B, vargs: List[Pure], bargs: List[Block])(using PContext): Stmt = {
          val result = TmpValue()
          Stmt.Val(result, Stmt.App(block, targs map transformArg,
            (vcoercers zip vargs).map{case (c,v) => c(v)},
            (bcoercers zip bargs).map{case (c,b) => c(b)}),
            Return(rcoercer(Pure.ValueVar(result, rcoercer.from))))
        }

      }
    case _ => Context.abort(s"Unsupported coercion from ${PrettyPrinter.format(fromtpe)} to ${PrettyPrinter.format(totpe)}")
  }

}
