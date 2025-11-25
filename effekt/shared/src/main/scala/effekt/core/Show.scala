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
import effekt.core.Type.TString
import effekt.source.FeatureFlag
import effekt.generator.llvm.Transformer.llvmFeatureFlags
import effekt.core.ExternBody.StringExternBody
import effekt.core.ExternBody.Unsupported

object Show extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "show"

  private final val FUNCTION_NAME: String = "show"

  case class ShowContext(showNames: collection.mutable.Map[ValueType, Id], showDefns: collection.mutable.Map[ValueType, Toplevel.Def], tparamLookup: collection.mutable.Map[Id, ValueType], backend: String) {

    def getAllShowDef(using ShowContext)(using DeclarationContext): List[Toplevel.Def] =
      showDefns.map(_._2).toList
  }

  // This will check if we have already generated a Show instance for the given type and generate it if we didn't
  def getShowBlockVar(vt: ValueType)(using ctx: ShowContext)(using DeclarationContext): Block.BlockVar =
    
    val showId = ctx.showNames.getOrElse(vt, {
      generateShowInstance(vt) match {
        case None => ctx.showNames.getOrElse(vt, {
          sys error s"Could not generate show instance for '${vt}'"
        })
        case Some(value) => value.id
      }
    })
    Block.BlockVar(showId, BlockType.Function(List.empty, List.empty, List(vt), List.empty, TString), Set.empty)
  

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      // 3. Synthesize `show` definitions, create (ValueType -> (show) Id) map for context
      // 3.1 figure out which backend we are generating for
      val backend: String = core.externs.collect {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, 
          StringExternBody(featureFlag: FeatureFlag.NamedFeatureFlag, contents)) => featureFlag.id
      }(0)

      implicit val ctx: ShowContext = ShowContext(collection.mutable.Map.empty, collection.mutable.Map.empty, collection.mutable.Map.empty, backend)
      implicit val dctx: DeclarationContext = DeclarationContext(core.declarations, core.externs)

      var transformed = transform(core)
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using ctx: ShowContext)(using DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) => 
      val transformedDefns = definitions map transform
      ModuleDecl(path, includes, declarations, externs, transformedDefns ++ ctx.getAllShowDef, exports)
  }

  def transform(toplevel: Toplevel)(using ShowContext)(using DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, tpe, transform(binding))
  }

  def transform(block: Block)(using ShowContext)(using DeclarationContext): Block = block match {
    case b: BlockLit => transform(b)
    case b: BlockVar => transform(b)
    case New(impl) => New(transform(impl))
    case Unbox(pure) => Unbox(transform(pure))
  }

  def transform(implementation: Implementation)(using ShowContext)(using DeclarationContext): Implementation = implementation match {
    case Implementation(interface, operations) => Implementation(interface, operations map transform)
  }

  def transform(operation: Operation)(using ShowContext)(using DeclarationContext): Operation = operation match {
    // Maybe need to pass tparams here to be able to lookup while transforming body
    case Operation(name, tparams, cparams, vparams, bparams, body) => Operation(name, tparams, cparams, vparams, bparams, transform(body))
  }

  def transform(blockLit: BlockLit)(using ShowContext)(using DeclarationContext): BlockLit = blockLit match {
    case BlockLit(tparams, cparams, vparams, bparams, body) => BlockLit(tparams, cparams, vparams, bparams, transform(body))
  } 

  def transform(blockLit: BlockVar)(using ShowContext)(using DeclarationContext): BlockVar = blockLit match {
    case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, annotatedTpe, annotatedCapt)
  } 

  def transform(default: Option[Stmt])(using ShowContext)(using DeclarationContext): Option[Stmt] = default match {
    case None => None
    case Some(value) => Some(transform(value))
  }

  def transform(stmt: Stmt)(using ctx: ShowContext)(using DeclarationContext): Stmt = 
    stmt match {
      case Let(id, annotatedTpe, PureApp(BlockVar(bid, bannotatedTpe, bannotatedCapt), targs, vargs), body) if bid.name.name == FUNCTION_NAME => 
        if (targs.length != 1) sys error "expected targs to have exactly one argument"
        Stmt.Val(id, annotatedTpe, Stmt.App(getShowBlockVar(targs(0)), List.empty, vargs, List.empty), transform(body))
      case Let(id, annotatedTpe, binding, body) => 
        Let(id, annotatedTpe, transform(binding), transform(body))
      case Return(expr) => Return(transform(expr))
      case ImpureApp(id, BlockVar(bid, annotatedTpe, annotatedCapt), targs, vargs, bargs, body) if bid.name.name == FUNCTION_NAME =>
        if (targs.length != 1) sys error "expected targs to have exactly one argument"
        Stmt.Val(id, TString, Stmt.App(getShowBlockVar(targs(0)), List.empty, vargs map transform, bargs map transform), transform(body))
      case ImpureApp(id, callee, targs, vargs, bargs, body) => ImpureApp(id, callee, targs, vargs, bargs, transform(body))
      case Def(id, block, body) => Def(id, transform(block), transform(body))
      case Val(id, annotatedTpe, binding, body) => Val(id, annotatedTpe, transform(binding), transform(body))
      case App(callee, targs, vargs, bargs) => App(transform(callee), targs, vargs map transform, bargs map transform)
      case Invoke(callee, method, methodTpe, targs, vargs, bargs) => Invoke(transform(callee), method, methodTpe, targs, vargs map transform, bargs map transform)
      case If(cond, thn, els) => If(transform(cond), transform(thn), transform(els))
      case Match(scrutinee, clauses, default) => Match(transform(scrutinee), clauses map ((id, blockLit) => (id, transform(blockLit))), transform(default))
      case Region(body) => Region(transform(body))
      case Alloc(id, init, region, body) => Alloc(id, transform(init), region, transform(body))
      case Var(ref, init, capture, body) => Var(ref, transform(init), capture, transform(body))
      case Get(id, annotatedTpe, ref, annotatedCapt, body) => Get(id, annotatedTpe, ref, annotatedCapt, transform(body))
      case Put(ref, annotatedCapt, value, body) => Put(ref, annotatedCapt, transform(value), transform(body))
      case Reset(body) => Reset(transform(body))
      case Shift(prompt, body) => Shift(transform(prompt), transform(body))
      case Resume(k, body) => Resume(transform(k), transform(body))
      case Hole(span) => Hole(span)
    }

  def transform(expr: Expr)(using ctx: ShowContext)(using DeclarationContext): Expr = expr match {
    case Make(data, tag, targs, vargs) => Make(data, tag, targs, vargs)
    case PureApp(BlockVar(id, annotatedTpe, annotatedCapt), targs, vargs) if id.name.name == FUNCTION_NAME => 
      if (targs.length != 1) sys error "expected targs to have exactly one argument"
      Expr.PureApp(getShowBlockVar(targs(0)), List.empty, vargs)
    case PureApp(b, targs, vargs) => PureApp(transform(b), targs, vargs)
    case Literal(value, annotatedType) => Literal(value, annotatedType)
    case ValueVar(id, annotatedType) => ValueVar(id, annotatedType)
    case Box(b, annotatedCapture) => Box(transform(b), annotatedCapture)
  }

  def generateShowInstancesBases(baseTypes: List[ValueType])(using ShowContext)(using DeclarationContext): List[Toplevel.Def] =
    baseTypes flatMap generateShowInstance

  def generateShowInstance(vt: ValueType)(using ctx: ShowContext)(using DeclarationContext): Option[Toplevel.Def] = 
    ctx.backend match {
      case "llvm" => generateShowInstanceLLVM(vt) 
      case "vm" => generateShowInstanceLLVM(vt)
    }

  def generateShowInstanceLLVM(vt: ValueType)(using ctx: ShowContext)(using dctx: DeclarationContext): Option[Toplevel.Def] =
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
        // (value: String) { return value }
        val stmt = Stmt.Return(paramValueVar)
        Some(generateDef(stmt))
      case Type.TUnit =>
        /* 
        (value: Unit) {
          "()"
        }
        */
        val stmt = Stmt.Return(Expr.Literal("()", TString))
        Some(generateDef(stmt))
      case Type.TInt =>
        /* 
        (value: Int) {
          %z = call %Pos @c_bytearray_show_Int(%Int ${value})
          ret %Pos %z
        }
        */
        Some(generateShow(Type.TInt))
      case Type.TChar =>
        Some(generateShow(Type.TChar))
      case Type.TByte =>
        /* 
        (value: Byte) {
          %z = call %Pos @c_bytearray_show_Byte(%Byte ${value})
          ret %Pos %z
        }
        */
        Some(generateShow(Type.TByte))
      case Type.TDouble =>
        /* 
        (value: Double) {
          %z = call %Pos @c_bytearray_show_Double(%Double ${value})
          ret %Pos %z
        }
        */
        Some(generateShow(Type.TDouble))
      case Type.TBoolean =>
        // if (value) "true" else "false"
        val stmt = Stmt.If(paramValueVar, Stmt.Return(Expr.Literal("true", TString)), Stmt.Return(Expr.Literal("false", TString)))
        Some(generateDef(stmt))
      case ValueType.Data(name, targs) => 
        val data = dctx.datas(name)
        generateShowInstance(data, targs)
      case ValueType.Var(name) => 
        val lookup = ctx.tparamLookup(name)
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
      case _ => println(s"Missing case for vt: ${vt}"); None

    }

  def findExternShowDef(valueType: ValueType)(using dctx: DeclarationContext): Block.BlockVar = 
    findExternDef("showBuiltin", List(valueType))
  
  def findExternDef(name: String, vts: List[ValueType])(using dctx: DeclarationContext): Block.BlockVar =
    dctx.findExternDef(name, vts) match
      case None => throw new Exception(s"Could not find show definition for ${vts}")
      case Some(Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body)) => 
        Block.BlockVar(id, BlockType.Function(tparams, cparams, vparams map (_.tpe), bparams map (_.tpe), ret), annotatedCapture)

  def generateShowInstance(decl: Declaration, targs: List[ValueType])(using ctx: ShowContext)(using dctx: DeclarationContext): Option[Toplevel.Def] = decl match {
    case dataDecl: Declaration.Data if dataDecl.constructors.size > 0 => 
      val freshId = freshShowId
      val dataType = ValueType.Data(decl.id, targs)
      ctx.showNames += (dataType -> freshId)
      val defn = generateShowInstance(dataDecl, freshId, targs)
      ctx.showDefns += (dataType -> defn)
      Some(defn)
    case _ => None
  }

  def generateShowInstance(decl: Declaration.Data, id: Id, targs: List[ValueType])(using ctx: ShowContext)(using DeclarationContext): Toplevel.Def =
    val valueId = Id("value")
    
    val tparamLookup = decl.tparams.zip(targs map lookupType).toMap
    ctx.tparamLookup ++= tparamLookup

    val valueTpe = ValueType.Data(decl.id, targs map lookupType)
    val vparam = ValueParam(valueId, valueTpe)

    val constructorVparams = decl.constructors.map(constructorVparam)
    
    val clauses = decl.constructors.zip(constructorVparams).map(constructorClauses)
    val matchStmt = Stmt.Match(
      ValueVar(valueId, valueTpe),
      clauses,
      None
    )

    Toplevel.Def(id, Block.BlockLit(List.empty, List.empty, List(vparam), List.empty, matchStmt))

  def constructorClauses(constructor: Constructor, vparams: List[ValueParam])(using ShowContext)(using DeclarationContext): (Id, BlockLit) =
    (constructor.id, BlockLit(List.empty, List.empty, vparams, List.empty, constructorStmt(constructor)))

  def constructorVparam(constr: Constructor)(using ctx: ShowContext): List[ValueParam] = constr match 
    case Constructor(id, tparams, fields) => fields.map({
      case Field(id, tpe) => 
        val lookup = lookupType(tpe)
        lookup match {
          case ValueType.Data(name, targs) => ValueParam(id, lookup)
          case ValueType.Var(name) => 
            println(s"Warn: wasn't able to lookup var: ${name}")
            ValueParam(id, ctx.tparamLookup(name))
          case ValueType.Boxed(tpe, capt) => ValueParam(id, lookup)
        }
    })

  def lookupType(vt: ValueType)(using ctx: ShowContext): ValueType = vt match {
    case ValueType.Data(name, targs) => ValueType.Data(name, targs map lookupType)
    case ValueType.Var(name) => ctx.tparamLookup(name)
    case ValueType.Boxed(tpe, capt) => vt
  }

  def constructorStmt(constr: Constructor)(using ctx: ShowContext)(using dctx: DeclarationContext): Stmt = constr match
    case Constructor(id, tparams, fields) => 
      val infixConcatBlockVar: Block.BlockVar = findExternDef("infixConcat", List(TString, TString))
      val pureFields = fields map fieldPure
      val concatenated = PureApp(infixConcatBlockVar, List.empty, List(Literal(id.name.name ++ "(", TString), concatPure(pureFields)))
      
      def fieldValStmts(idapps: List[(Id, Stmt.App)]): Stmt = 
        idapps match {
          case (id, app) :: next => Stmt.Val(id, TString, app, fieldValStmts(next))
          case Nil => Return(concatenated)
        }
      
      fieldValStmts(pureFields)

  // Convert a list of pure statements to comma-separated concatenated version
  // Literal("Just("), PureApp(show, x), Literal(", "), PureApp(show, y), Literal(")")
  //  =>
  // PureApp(concat, List(Literal("Just("), PureApp(concat, List(PureApp(show, x), PureApp(concat, List(Literal(", "), ...))))
  def concatPure(pures: List[(Id, Stmt.App)])(using ctx: ShowContext)(using DeclarationContext): Expr =
    val infixConcatDef = findExternDef("infixConcat", List(TString, TString))
    pures match 
      case (fieldId, _) :: next :: rest => PureApp(infixConcatDef, List.empty, List(Expr.ValueVar(fieldId, TString), PureApp(infixConcatDef, List.empty, List(Literal(", ", TString), concatPure(next :: rest)))))
      case (fieldId, _) :: Nil => PureApp(infixConcatDef, List.empty, List(Expr.ValueVar(fieldId, TString), Literal(")", TString)))
      case Nil => Literal(")", TString)

  def fieldValueVar(field: Field): Expr = field match
    case Field(id, tpe) => ValueVar(id, tpe)

  def fieldPure(field: Field)(using ctx: ShowContext)(using DeclarationContext): (Id, Stmt.App) = field match
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
