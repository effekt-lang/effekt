package effekt
package core

import scala.collection.mutable

import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.util.messages.ErrorMessageReifier

import effekt.core.Type.TString

object Show extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "show"

  private final val FUNCTION_NAME: String = "show"

  case class ShowContext(showNames: collection.mutable.Map[ValueType, Id], showDefns: collection.mutable.Map[ValueType, Toplevel.Def], tparamLookup: collection.mutable.Map[Id, ValueType]) {

    def getAllShowDef(using ShowContext)(using DeclarationContext): List[Toplevel.Def] =
      showDefns.values.toList

    var bindings = mutable.ListBuffer.empty[Binding]

    def withBindings(block: => Stmt): Stmt =
      val outer = bindings
      bindings = mutable.ListBuffer.empty[Binding]
      Binding(outer.toList, block)

    def emit(id: Id, stmt: Stmt): Unit =
      bindings.append(Binding.Val(id, stmt))
  }

  // This will check if we have already generated a Show instance for the given type and generate it if we didn't
  def getShowBlockVar(vt: ValueType)(using ctx: ShowContext)(using Context, DeclarationContext): Block.BlockVar =

    val showId = ctx.showNames.getOrElse(vt, {
      generateShowInstance(vt) match {
        case None => ctx.showNames.getOrElse(vt, {
          Context.abort(pretty"Could not generate show instance for '${vt}'")
        })
        case Some(value) => value.id
      }
    })
    Block.BlockVar(showId, BlockType.Function(List.empty, List.empty, List(vt), List.empty, TString), Set.empty)


  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      // Synthesize `show` definitions, create (ValueType -> (show) Id) map for context

      given ctx: ShowContext = ShowContext(collection.mutable.Map.empty, collection.mutable.Map.empty, collection.mutable.Map.empty)
      given dctx: DeclarationContext = DeclarationContext(core.declarations, core.externs)

      Some(CoreTransformed(source, tree, mod, transform(core)))
    }
  }

  def transform(decl: ModuleDecl)(using ctx: ShowContext)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      val transformedDefs = definitions map transform
      ModuleDecl(path, includes, declarations, externs, transformedDefs ++ ctx.getAllShowDef, exports)
  }

  def transform(toplevel: Toplevel)(using Context, ShowContext, DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, transform(binding))
  }

  def transform(block: Block)(using Context, ShowContext, DeclarationContext): Block = block match {
    case b: BlockLit => transform(b)
    case b: BlockVar => transform(b)
    case New(impl) => New(transform(impl))
    case Unbox(pure) => Unbox(transform(pure))
  }

  def transform(implementation: Implementation)(using Context, ShowContext, DeclarationContext): Implementation = implementation match {
    case Implementation(interface, operations) => Implementation(interface, operations map transform)
  }

  def transform(operation: Operation)(using Context, ShowContext, DeclarationContext): Operation = operation match {
    case Operation(name, tparams, cparams, vparams, bparams, body) => Operation(name, tparams, cparams, vparams, bparams, transform(body))
  }

  def transform(blockLit: BlockLit)(using Context, ShowContext, DeclarationContext): BlockLit = blockLit match {
    case BlockLit(tparams, cparams, vparams, bparams, body) => BlockLit(tparams, cparams, vparams, bparams, transform(body))
  }

  def transform(blockLit: BlockVar)(using Context, ShowContext, DeclarationContext): BlockVar = blockLit match {
    case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, annotatedTpe, annotatedCapt)
  }

  def transform(default: Option[Stmt])(using Context, ShowContext, DeclarationContext): Option[Stmt] = default match {
    case None => None
    case Some(value) => Some(transform(value))
  }

  def transform(stmt: Stmt)(using ctx: ShowContext)(using Context, DeclarationContext): Stmt =
    stmt match {
      case Let(id, PureApp(BlockVar(bid, bannotatedTpe, bannotatedCapt), targs, vargs), body) if bid.name.name == FUNCTION_NAME =>
        val targ = targs match {
          case targ :: Nil => targ
          case _ => Context.abort(pretty"Expected targs for '${FUNCTION_NAME}' to have exactly one argument")
        }
        // We need to wrap here, because vargs might have been extern show calls that are now not
        // this might happen everywhere we directly transform an expression
        val vargs_ = vargs map transform
        ctx.withBindings {
          Stmt.Val(id, Stmt.App(getShowBlockVar(targ), List.empty, vargs_, List.empty), transform(body))
        }
      case Let(id, binding, body) =>
        val binding_ = transform(binding)
        ctx.withBindings {
          Let(id, binding_, transform(body))
        }
      case Return(expr) =>
        val expr_ = transform(expr)
        ctx.withBindings {
          Return(expr_)
        }
      case ImpureApp(id, BlockVar(bid, annotatedTpe, annotatedCapt), targs, vargs, bargs, body) if bid.name.name == FUNCTION_NAME =>
        val targ = targs match {
          case targ :: Nil => targ
          case _ => Context.abort(pretty"Expected targs for '${FUNCTION_NAME}' to have exactly one argument")
        }
        val vargs_ = vargs map transform
        val bargs_ = bargs map transform
        ctx.withBindings {
          Stmt.Val(id, Stmt.App(getShowBlockVar(targ), List.empty, vargs_, bargs_), transform(body))
        }
      case ImpureApp(id, callee, targs, vargs, bargs, body) =>
        val vargs_ = vargs map transform
        val bargs_ = bargs map transform
        ctx.withBindings {
          ImpureApp(id, callee, targs, vargs_, bargs_, transform(body))
        }
      case Def(id, block, body) => Def(id, transform(block), transform(body))
      case Val(id, binding, body) => Val(id, transform(binding), transform(body))
      case App(callee, targs, vargs, bargs) =>
        val vargs_ = vargs map transform
        val bargs_ = bargs map transform
        ctx.withBindings {
          App(transform(callee), targs, vargs_, bargs_)
        }
      case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        val vargs_ = vargs map transform
        val bargs_ = bargs map transform
        ctx.withBindings {
          Invoke(transform(callee), method, methodTpe, targs, vargs_, bargs_)
        }
      case If(cond, thn, els) =>
        val cond_ = transform(cond)
        ctx.withBindings {
          If(transform(cond), transform(thn), transform(els))
        }
      case Match(scrutinee, matchTpe, clauses, default) =>
        val scrutinee_ = transform(scrutinee)
        ctx.withBindings {
          Match(scrutinee_, matchTpe, clauses map ((id, blockLit) => (id, transform(blockLit))), transform(default))
        }
      case Region(body) => Region(transform(body))
      case Alloc(id, init, region, body) =>
        val init_ = transform(init)
        ctx.withBindings {
          Alloc(id, init_, region, transform(body))
        }
      case Var(ref, init, capture, body) =>
        val init_ = transform(init)
        ctx.withBindings {
          Var(ref, init_, capture, transform(body))
        }
      case Get(id, annotatedTpe, ref, annotatedCapt, body) => Get(id, annotatedTpe, ref, annotatedCapt, transform(body))
      case Put(ref, annotatedCapt, value, body) =>
        val value_ = transform(value)
        ctx.withBindings {
          Put(ref, annotatedCapt, value_, transform(body))
        }
      case Reset(body) => Reset(transform(body))
      case Shift(prompt, k, body) => Shift(transform(prompt), k, transform(body))
      case Resume(k, body) => Resume(transform(k), transform(body))
      case Hole(tpe, span) => Hole(tpe, span)
    }

  def transform(expr: Expr)(using ctx: ShowContext)(using Context, DeclarationContext): Expr = expr match {
    case Make(data, tag, targs, vargs) => Make(data, tag, targs, vargs map transform)
    case PureApp(BlockVar(id, annotatedTpe, annotatedCapt), targs, vargs) if id.name.name == FUNCTION_NAME =>
      val targ = targs match {
        case targ :: Nil => targ
        case _ => Context.abort(pretty"Expected targs for '${FUNCTION_NAME}' to have exactly one argument")
      }
      // We are switching from Extern call to normal Call,
      // so we need to wrap the new call into a Val, store it and finally emit it before the statement that called this
      // therefore we return the name of the Val that we store this call into
      val stmt = Stmt.App(getShowBlockVar(targ), List.empty, vargs map transform, List.empty)
      val letId = Id("showApp")
      ctx.emit(letId, stmt)
      Expr.ValueVar(letId, TString)
    case PureApp(b, targs, vargs) => PureApp(transform(b), targs, vargs map transform)
    case Literal(value, annotatedType) => Literal(value, annotatedType)
    case ValueVar(id, annotatedType) => ValueVar(id, annotatedType)
    case Box(b, annotatedCapture) => Box(transform(b), annotatedCapture)
  }

  def generateShowInstancesBases(baseTypes: List[ValueType])(using Context, ShowContext, DeclarationContext): List[Toplevel.Def] =
    baseTypes flatMap generateShowInstance

  def generateShowInstance(vt: ValueType)(using ctx: ShowContext, dctx: DeclarationContext)(using Context): Option[Toplevel.Def] =
    val showId = freshShowId
    ctx.showNames += (vt -> showId)
    val paramId = Id("value")
    val vparam = ValueParam(paramId, vt)
    val paramValueVar = Expr.ValueVar(paramId, vt)

    def generateBlockLit(stmt: Stmt) = BlockLit(List.empty, List.empty, List(vparam), List.empty, stmt)
    def generateValAppRet(valueType: ValueType): Stmt =
      val blockVar = findExternShowDef(valueType)
      val retId = Id("ret")
      Stmt.ImpureApp(retId, blockVar, List.empty, List(paramValueVar), List.empty, Stmt.Return(Expr.ValueVar(retId, TString)))
    def generateDef(stmt: Stmt): Toplevel.Def =
      val block = generateBlockLit(stmt)
      val defn: Toplevel.Def = Toplevel.Def(showId, block)
      ctx.showDefns += (vt -> defn)
      defn
    def generateShow(valueType: ValueType): Toplevel.Def =
      generateDef(generateValAppRet(valueType))

    vt match {
      case Type.TString =>
        val stmt = Stmt.Return(paramValueVar)
        Some(generateDef(stmt))
      case Type.TUnit =>
        val stmt = Stmt.Return(Expr.Literal("()", TString))
        Some(generateDef(stmt))
      case Type.TInt =>
        Some(generateShow(Type.TInt))
      case Type.TChar =>
        Some(generateShow(Type.TChar))
      case Type.TByte =>
        Some(generateShow(Type.TByte))
      case Type.TDouble =>
        Some(generateShow(Type.TDouble))
      case Type.TBoolean =>
        val stmt = Stmt.If(paramValueVar, Stmt.Return(Expr.Literal("true", TString)), Stmt.Return(Expr.Literal("false", TString)))
        Some(generateDef(stmt))
      case ValueType.Data(name, targs) =>
        val data = dctx.datas.getOrElse(name, {
          Context.abort(pretty"Extern type '${name}', does not have a manual show implementation")
        })
        generateShowInstance(data, targs)
      case ValueType.Var(name) =>
        val lookup = ctx.tparamLookup.getOrElse(name, {
          Context.abort(pretty"Too much type indirection over show, cannot lookup '${name}'")
        })
        val showName = ctx.showNames.get(lookup)
        showName match {
          case None =>
            val varShowInstance = generateShowInstance(lookup)
            varShowInstance match {
              case None => ()
              case Some(value) => ctx.showDefns += (vt -> value)
            }
            varShowInstance
          case Some(value) => None
        }
      case ValueType.Boxed(tpe, capt) =>
        // TODO: Handle this better, but honestly your fault for using this
        Context.warning("We not properly handling Boxed types when generating show instances")
        val stmt = Stmt.Return(Expr.Literal("BOXED", TString))
        Some(generateDef(stmt))
    }

  def findExternShowDef(valueType: ValueType)(using dctx: DeclarationContext)(using Context): Block.BlockVar =
    findExternDef("showBuiltin", List(valueType))

  def findExternDef(name: String, vts: List[ValueType])(using dctx: DeclarationContext)(using Context): Block.BlockVar =
    dctx.findExternDef(name, vts) match
      case None => Context.abort(pretty"Could not find show definition for ${vts}")
      case Some(Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body)) =>
        Block.BlockVar(id, BlockType.Function(tparams, cparams, vparams map (_.tpe), bparams map (_.tpe), ret), annotatedCapture)

  def generateShowInstance(decl: Declaration, targs: List[ValueType])(using ctx: ShowContext, dctx: DeclarationContext)(using Context): Option[Toplevel.Def] = decl match {
    case dataDecl: Declaration.Data if dataDecl.constructors.nonEmpty =>
      val freshId = freshShowId
      val dataType = ValueType.Data(decl.id, targs)
      ctx.showNames += (dataType -> freshId)
      val defn = generateShowInstance(dataDecl, freshId, targs)
      ctx.showDefns += (dataType -> defn)
      Some(defn)
    case _ => None
  }

  def generateShowInstance(decl: Declaration.Data, id: Id, targs: List[ValueType])(using ctx: ShowContext)(using Context, DeclarationContext): Toplevel.Def =
    val valueId = Id("value")

    val tparamLookup = decl.tparams.zip(targs map lookupType).toMap
    ctx.tparamLookup ++= tparamLookup

    val valueTpe = ValueType.Data(decl.id, targs map lookupType)
    val vparam = ValueParam(valueId, valueTpe)

    val constructorVparams = decl.constructors.map(constructorVparam)

    val clauses = decl.constructors.zip(constructorVparams).map(constructorClauses)
    val matchStmt = Stmt.Match(
      ValueVar(valueId, valueTpe),
      TString,
      clauses,
      None
    )

    Toplevel.Def(id, Block.BlockLit(List.empty, List.empty, List(vparam), List.empty, matchStmt))

  def constructorClauses(constructor: Constructor, vparams: List[ValueParam])(using Context, ShowContext, DeclarationContext): (Id, BlockLit) =
    (constructor.id, BlockLit(List.empty, List.empty, vparams, List.empty, constructorStmt(constructor)))

  def constructorVparam(constr: Constructor)(using Context)(using ctx: ShowContext): List[ValueParam] = constr match
    case Constructor(id, tparams, fields) => fields.map({
      case Field(id, tpe) =>
        val lookup = lookupType(tpe)
        lookup match {
          case ValueType.Data(name, targs) => ValueParam(id, lookup)
          case ValueType.Var(name) =>
            Context.warning(pretty"Was not able to lookup var: ${name}")
            ValueParam(id, ctx.tparamLookup(name))
          case ValueType.Boxed(tpe, capt) => ValueParam(id, lookup)
        }
    })

  def lookupType(vt: ValueType)(using ctx: ShowContext): ValueType = vt match {
    case ValueType.Data(name, targs) => ValueType.Data(name, targs map lookupType)
    case ValueType.Var(name) => ctx.tparamLookup(name)
    case ValueType.Boxed(tpe, capt) => vt
  }

  def constructorStmt(constr: Constructor)(using ctx: ShowContext, dctx: DeclarationContext)(using Context): Stmt = constr match
    case Constructor(id, tparams, fields) =>
      val infixConcatBlockVar: Block.BlockVar = findExternDef("infixConcat", List(TString, TString))
      val pureFields = fields map fieldPure
      val concatenated = PureApp(infixConcatBlockVar, List.empty, List(Literal(id.name.name ++ "(", TString), concatPure(pureFields)))

      def fieldValStmts(idapps: List[(Id, Stmt.App)]): Stmt =
        idapps match {
          case (id, app) :: next => Stmt.Val(id, app, fieldValStmts(next))
          case Nil => Return(concatenated)
        }

      fieldValStmts(pureFields)

  // Convert a list of pure statements to comma-separated concatenated version
  // Literal("Just("), PureApp(show, x), Literal(", "), PureApp(show, y), Literal(")")
  //  =>
  // PureApp(concat, List(Literal("Just("), PureApp(concat, List(PureApp(show, x), PureApp(concat, List(Literal(", "), ...))))
  def concatPure(pures: List[(Id, Stmt.App)])(using ctx: ShowContext)(using Context, DeclarationContext): Expr =
    val infixConcatDef = findExternDef("infixConcat", List(TString, TString))
    pures match
      case (fieldId, _) :: next :: rest => PureApp(infixConcatDef, List.empty, List(Expr.ValueVar(fieldId, TString), PureApp(infixConcatDef, List.empty, List(Literal(", ", TString), concatPure(next :: rest)))))
      case (fieldId, _) :: Nil => PureApp(infixConcatDef, List.empty, List(Expr.ValueVar(fieldId, TString), Literal(")", TString)))
      case Nil => Literal(")", TString)

  def fieldValueVar(field: Field): Expr = field match
    case Field(id, tpe) => ValueVar(id, tpe)

  def fieldPure(field: Field)(using ctx: ShowContext)(using Context, DeclarationContext): (Id, Stmt.App) = field match
    case Field(id, tpe) =>
      val paramTpe = lookupType(tpe)
      val app: Stmt.App = App(getShowBlockVar(paramTpe), List.empty, List(ValueVar(id, paramTpe)), List.empty)
      (Id("field"), app)

  var freshShowCounter = 0
  def freshShowId: Id =
    val freshId = Id(FUNCTION_NAME ++ freshShowCounter.toString())
    freshShowCounter += 1
    freshId
}
