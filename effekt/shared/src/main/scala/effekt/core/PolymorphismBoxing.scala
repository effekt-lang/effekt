package effekt
package core

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.symbols
import effekt.symbols.{TmpBlock, TmpValue}
import effekt.{CoreTransformed, Phase}
import effekt.symbols.builtins.{TBoolean, TByte, TChar, TDouble, TInt, TState, TUnit}
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
        annotatedCapture, body match {
          case ExternBody.StringExternBody(ff, bbody) => ExternBody.StringExternBody(ff, Template(bbody.strings, bbody.args map transform))
          case e @ ExternBody.Unsupported(_) => e
        } )
    case Extern.Include(ff, contents) => Extern.Include(ff, contents)
  }

  def transform(valueParam: ValueParam)(using PContext): ValueParam = valueParam match {
    case ValueParam(id, tpe) => ValueParam(id, transform(tpe))
  }

  def transform(blockParam: BlockParam)(using PContext): BlockParam = blockParam match {
    case BlockParam(id, tpe, capt) => BlockParam(id, transform(tpe), capt)
  }

  def transform(toplevel: Toplevel)(using PContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, transform(tpe), coerce(transform(binding), transform(tpe)))
  }

  def coerce(stmt: Stmt, from: ValueType, to: ValueType)(using PContext): Stmt =
    val coerce = ValueCoercer(from, to)
    if (coerce.isIdentity) { stmt }
    else {
      val orig = TmpValue("coe")
      Stmt.Val(orig, coerce.from, stmt, Stmt.Return(coerce(ValueVar(orig, coerce.from))))
    }

  def coerce(stmt: Stmt, to: ValueType)(using PContext): Stmt = coerce(stmt, stmt.tpe, to)

  def coerce(expr: Expr, from: ValueType, to: ValueType)(using PContext): Bind[Expr] =
    val coerce = ValueCoercer(from, to)
    if (coerce.isIdentity) { Bind.pure(expr) }
    else { Bind.bind(expr).map { x => coerce(x) } }

  def coerce(expr: Expr, to: ValueType)(using PContext): Bind[Expr] = coerce(expr, expr.tpe, to)

  def coerce(pure: Pure, from: ValueType, to: ValueType)(using PContext): Pure = ValueCoercer(from, to)(pure)

  def coerce(pure: Pure, to: ValueType)(using PContext): Pure = coerce(pure, pure.tpe, to)

  def coerce(block: Block, from: BlockType, to: BlockType)(using PContext): Block = BlockCoercer(from, to)(block)

  def coerce(block: Block, to: BlockType)(using PContext): Block = coerce(block, block.tpe, to)

  def coerce(block: BlockLit, to: BlockType)(using PContext): BlockLit = BlockCoercer(block.tpe, to)(block)

  def transform(block: Block.BlockLit)(using PContext): Block.BlockLit = block match {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Block.BlockLit(tparams, cparams, vparams map transform, bparams map transform,
        transform(body))
  }
  def transform(block: Block)(using PContext): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Block.BlockVar(id, transform(annotatedTpe), annotatedCapt)
    case b: Block.BlockLit => transform(b)
    case Block.Unbox(pure) =>
      Block.Unbox(transform(pure))
    case Block.New(impl) =>
      Block.New(transform(impl))
  }
  def transform(blockVar: BlockVar)(using PContext): BlockVar = blockVar match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      Block.BlockVar(id, transform(annotatedTpe), annotatedCapt)
  }

  def transform(implementation: Implementation)(using PContext): Implementation = implementation match {
    case Implementation(BlockType.Interface(symbol, targs), operations) =>
      val ifce = PContext.findInterface(symbol).getOrElse { Context.abort(s"No declaration for interface ${symbol}") }
      Implementation(BlockType.Interface(symbol, targs map transformArg), operations map transform(ifce, targs))
  }

  def transform(ifce: Interface, targs: List[ValueType])(operation: Operation)(using PContext): Operation = operation match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      val prop = ifce.properties.find { p => p.id == name }.getOrElse { Context.abort(s"Interface ${ifce} declares no operation ${name}.") }
      val propTpe = prop.tpe.asInstanceOf[BlockType.Function]

      val blockTpe = BlockType.Function(tparams, propTpe.cparams, propTpe.vparams.map(transform), propTpe.bparams.map(transform), transform(propTpe.result))
      val implBlock: Block.BlockLit = Block.BlockLit(tparams, cparams, vparams, bparams, transform(body))
      val transformed: Block.BlockLit = coerce(implBlock, blockTpe)
      Operation(name, transformed.tparams, transformed.cparams, transformed.vparams, transformed.bparams,
        transformed.body)
  }

  def transform(stmt: Stmt)(using PContext): Stmt = stmt match {
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
          PContext.findInterface(name) match {
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
      coerce(Invoke(calleeT, method, boxedTpe, targs.map(transformArg), vCoerced, bCoerced), tpe.result, itpe.result)

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???

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
              (id, coerce(transform(clause),Type.instantiate(casetpe, targs map transformArg, List())))
          }, default map transform)
        case t => Context.abort(pp"Match on value of type ${t}")
      }
    case Stmt.Alloc(id, init, region, body) =>
      Stmt.Alloc(id, transform(init), region, transform(body))
    case Stmt.Var(id, init, cap, body) =>
      Stmt.Var(id, transform(init), cap, transform(body))
    case Stmt.Reset(body) =>
      Stmt.Reset(transform(body))
    case Stmt.Shift(prompt, body) =>
      Stmt.Shift(prompt, transform(body))
    case Stmt.Resume(k, body) =>
      val expected = k.tpe match {
        case core.Type.TResume(result, answer) => result
        case _ => ???
      }
      Stmt.Resume(k, coerce(transform(body), expected))

    case Stmt.Region(body) =>
      val tBody = transform(body)
      // make sure the result type is a boxed one
      val (expectedBodyTpe, actualReturnType, expectedReturnType) = tBody.tpe match {
        case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          val boxedResult = transformArg(result)
          (BlockType.Function(tparams, cparams, vparams, bparams, boxedResult), boxedResult, result)
        case _ => Context.abort("Body of a region cannot have interface type")
      }
      coerce(Stmt.Region(coerce(tBody, expectedBodyTpe)), actualReturnType, expectedReturnType)
    case Stmt.Hole() => Stmt.Hole()
  }

  def transform(expr: Expr)(using PContext): Bind[Expr] = expr match {
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
      val vCoerced = (vargs zip tpe.vparams).map { (a, tpe) => coerce(transform(a), tpe) }
      coerce(PureApp(callee, targs.map(transformArg), vCoerced), itpe.result)

    case Pure.Make(data, tag, vargs) =>
      val dataDecl = PContext.getData(data.name)
      val ctorDecl = dataDecl.constructors.find(_.id == tag).getOrElse {
        Context.panic(pp"No constructor found for tag ${tag} in data type: ${data}")
      }
      val paramTypes = ctorDecl.fields.map(_.tpe)

      val coercedArgs = (vargs zip paramTypes).map { case (arg, paramTpe) => coerce(transform(arg), paramTpe) }
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
          coerce(Pure.Select(target, field, transform(annotatedType)), f.tpe, Type.substitute(f.tpe, (tparams zip targs).toMap, Map()))
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

    // Special case some types to not introduce boxing
    case i @ BlockType.Interface(TState.interface, _) => i
    case i @ BlockType.Interface(core.Type.ResumeSymbol, _) => i
    case i @ BlockType.Interface(core.Type.PromptSymbol, _) => i

    case BlockType.Interface(symbol, targs) => BlockType.Interface(symbol, targs map transformArg)
  }

  def transformArg(valueType: ValueType)(using PContext): ValueType = valueType match {
    case ValueType.Var(name) => ValueType.Var(name) // assume vars are always OK
    case t if box.isDefinedAt(t) => box(t).tpe
    case ValueType.Data(symbol, targs) => ValueType.Data(symbol, targs map transformArg)
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), capt)
  }

  sealed  trait ValueCoercer {
    def from: ValueType
    def to: ValueType
    def apply(t: Pure): Pure
    def isIdentity: Boolean = false
  }
  object ValueCoercer {
    class IdentityCoercer(val from: ValueType, val to: ValueType) extends ValueCoercer {
      override def apply(t: Pure): Pure = t
      override def isIdentity: Boolean = true
    }
    case class BoxCoercer(tpe: ValueType)(using PContext) extends ValueCoercer {
      override def from = tpe
      override def to = box(tpe).tpe

      override def apply(t: Pure): Pure = {
        val boxer = box(tpe)
        boxer.box(t)
      }
    }
    case class UnboxCoercer(tpe: ValueType)(using PContext) extends ValueCoercer {
      override def from = box(tpe).tpe
      override def to = tpe

      override def apply(t: Pure): Pure = {
        val boxer = box(tpe)
        boxer.unbox(t)
      }
    }
    case class BottomCoercer(tpe: ValueType)(using PContext) extends ValueCoercer {
      override def from = core.Type.TBottom
      override def to = tpe

      override def apply(t: Pure): Pure = to match {
        case core.Type.TInt => Pure.Literal(1337L, core.Type.TInt)
        case core.Type.TDouble  => Pure.Literal(13.37, core.Type.TDouble)
        // Do strings need to be boxed? Really?
        case core.Type.TString  => Pure.Literal("<?nothing>", core.Type.TString)
        case core.Type.TByte    => Pure.Literal(1337, core.Type.TByte)
        case t if box.isDefinedAt(t) => sys error s"No default value defined for ${t}"
        case _ => sys error s"Trying to unbox Nothing to ${t}"
      }
    }

    def apply(from: ValueType, to: ValueType)(using PContext): ValueCoercer = (from, to) match {
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
        val bcoercer = BlockCoercer[Block](bt1, bt2)
        if (bcoercer.isIdentity) then { IdentityCoercer(from, to) } else {
          val _fr = from
          val _to = to
          new ValueCoercer {
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
  }

  sealed trait BlockCoercer[Te <: Block] {
    def from: BlockType
    def to: BlockType

    def apply(t: Te): Te
    def isIdentity: Boolean = false
  }
  object BlockCoercer {
    class IdentityCoercer[Te <: Block](val from: BlockType, val to: BlockType, targs: List[ValueType]) extends BlockCoercer[Te] {
      override def apply(t: Te): Te = t
      override def isIdentity: Boolean = true
    }

    def apply[Te >: Block.BlockLit <: Block](fromtpe: BlockType, totpe: BlockType, targs: List[ValueType] = Nil)(using PContext): BlockCoercer[Te] =
      (fromtpe, totpe) match {
      case (f, t) if f == t => new IdentityCoercer(fromtpe, totpe, targs)
      case (BlockType.Function(ftparams, fcparams, fvparams, fbparams, fresult),
            BlockType.Function(ttparams, tcparams, tvparams, tbparams, tresult)) =>

        // note: Order inversed as contravariant in arguments
        val vcoercers = (fvparams zip tvparams).map { case (t, f) => ValueCoercer(f, t) }
        val bcoercers = (fbparams zip tbparams).map { case (t, f) => BlockCoercer[Block](f,t) }
        val rcoercer = ValueCoercer(fresult, tresult)

        if ((rcoercer :: vcoercers).forall(_.isIdentity) && bcoercers.forall(_.isIdentity)) {
          return new IdentityCoercer(fromtpe, totpe, targs) // nothing to do here
        }

        new BlockCoercer[Te] {
          override def from = fromtpe
          override def to = totpe

          override def apply(block: Te): Te = {
            val vparams = vcoercers.map { c => ValueParam(TmpValue("coe"), transform(c.from)) }
            val bparams = bcoercers.map { c => val id = TmpBlock("coe"); BlockParam(id, transform(c.from), Set(id)) }
            val result = TmpValue("coe")
            val inner = TmpBlock()
            val vargs = (vcoercers zip vparams).map { case (c, p) => c(Pure.ValueVar(p.id, p.tpe)) }
            val bargs = (bcoercers zip bparams).map { case (c, p) => c(Block.BlockVar(p.id, p.tpe, Set.empty)) }
            Block.BlockLit(ftparams, bparams.map(_.id), vparams, bparams,
              Def(inner, block,
                coerce(Stmt.App(
                  Block.BlockVar(inner, block.tpe, block.capt),
                  (targs map transformArg) ++ (ftparams map core.ValueType.Var.apply),
                  vargs,
                  bargs), fresult, tresult)))
          }
        }
      case (BlockType.Interface(n1, targs), BlockType.Interface(n2, _)) =>
        IdentityCoercer(fromtpe, totpe, targs)
      case _ => Context.abort(pp"Unsupported coercion from ${fromtpe} to ${totpe}")
    }
  }
}
