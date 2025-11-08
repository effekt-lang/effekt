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

  case class ShowContext(showDefns: collection.mutable.Map[ValueType, Toplevel.Def], backend: String) {

    def getAllShowDef(using ShowContext)(using DeclarationContext): List[Toplevel.Def] =
      showDefns.map(_._2).toList
  }

  // This will check if we have already generated a Show instance for the given type and generate it if we didn't
  def getShowBlockVar(vt: ValueType)(using ctx: ShowContext)(using DeclarationContext): Block.BlockVar =
    val showId = ctx.showDefns.getOrElse(vt, {
      println(s"DEBUG: Generating show for '${vt}'")
      generateShowInstance(vt).getOrElse({
        sys error s"Could not generate show instance for '${vt}'"
      })
    }).id
    Block.BlockVar(showId, BlockType.Function(List.empty, List.empty, List(vt), List.empty, TString), Set.empty)
  

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      // 3. Synthesize `show` definitions, create (ValueType -> (show) Id) map for context
      // 3.1 figure out which backend we are generating for
      val backend: String = core.externs.collect {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => 
          body match {
            case StringExternBody(featureFlag: FeatureFlag.NamedFeatureFlag, contents) => featureFlag.id
            case _ => sys error "Should never happen"
          }
      }(0)

      implicit val ctx: ShowContext = ShowContext(collection.mutable.Map.empty, backend)
      implicit val dctx: DeclarationContext = DeclarationContext(core.declarations, core.externs)

      var transformed = transform(core)
      // println(util.show(transformed))
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
    case Toplevel.Val(id, tpe, binding) => ???
  }

  def transform(block: Block)(using ShowContext)(using DeclarationContext): Block = block match {
    case BlockLit(tparams, cparams, vparams, bparams, body) => BlockLit(tparams, cparams, vparams, bparams, transform(body))
    case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, annotatedTpe, annotatedCapt)
    case New(impl) => ???
    case Unbox(pure) => ???
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
      case o => println(o); ???
    }

  def transform(expr: Expr)(using ctx: ShowContext)(using DeclarationContext): Expr = expr match {
    case Make(data, tag, targs, vargs) => Make(data, tag, targs, vargs)
    case PureApp(BlockVar(id, annotatedTpe, annotatedCapt), targs, vargs) if id.name.name == FUNCTION_NAME => 
      if (targs.length != 1) sys error "expected targs to have exactly one argument"
      Expr.PureApp(getShowBlockVar(targs(0)), List.empty, vargs)
    case PureApp(b, targs, vargs) => PureApp(b, targs, vargs)
    case Literal(value, annotatedType) => Literal(value, annotatedType)
    case ValueVar(id, annotatedType) => ValueVar(id, annotatedType)
    case o => println(o); ???
  }

  def generateShowInstancesBases(baseTypes: List[ValueType])(using ShowContext)(using DeclarationContext): List[Toplevel.Def] =
    baseTypes flatMap generateShowInstance

  // The cases here should match the list effekt.core.Type.Builtins
  def generateShowInstance(vt: ValueType)(using ctx: ShowContext)(using DeclarationContext): Option[Toplevel.Def] = 
    ctx.backend match {
      case "llvm" => generateShowInstanceLLVM(vt) 
    }

  def generateShowInstanceLLVM(vt: ValueType)(using ctx: ShowContext)(using dctx: DeclarationContext): Option[Toplevel.Def] =
    val showId = freshShowId
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
      case Type.TInt | Type.TChar =>
        /* 
        (value: Int) {
          %z = call %Pos @c_bytearray_show_Int(%Int ${value})
          ret %Pos %z
        }
        */
        Some(generateShow(Type.TInt))
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
        generateShowInstance(data)
      // FIXME: Placeholder case so we don't just crash
      case ValueType.Var(name) => 
        val stmt = Stmt.Return(Expr.Literal("Var(" + name + ")", TString))
        Some(generateDef(stmt))
      case _ => println(s"Missing case for vt: ${vt}"); None

    }

  def findExternShowDef(valueType: ValueType)(using dctx: DeclarationContext): Block.BlockVar = 
    findExternDef("showBuiltin", List(valueType))
  
  def findExternDef(name: String, vts: List[ValueType])(using dctx: DeclarationContext): Block.BlockVar =
    dctx.findExternDef(name, vts) match
      case None => throw new Exception(s"Could not find show definition for ${vts}")
      case Some(Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body)) => 
        Block.BlockVar(id, BlockType.Function(tparams, cparams, vparams map (_.tpe), bparams map (_.tpe), ret), annotatedCapture)

  def generateShowInstancesDecls(declarations: List[Declaration])(using ShowContext)(using DeclarationContext): List[Toplevel.Def] = 
    declarations flatMap generateShowInstance

  def generateShowInstance(decl: Declaration)(using ctx: ShowContext)(using DeclarationContext): Option[Toplevel.Def] = decl match {
    case dataDecl: Declaration.Data if dataDecl.constructors.size > 0 => 
      val freshId = freshShowId
      val defn = generateShowInstance(dataDecl, freshId)
      val dataType = ValueType.Data(dataDecl.id, List.empty)
      ctx.showDefns += (dataType -> defn)
      Some(defn)
    case _ => None
  }

  def generateShowInstance(declarations: List[Declaration], vt: ValueType, id: Id)(using ctx: ShowContext)(using DeclarationContext): Toplevel.Def =
    vt match {
      case ValueType.Data(name, targs) => 
        declarations.find(decl => decl.id == name).get match {
          case d: Data => return generateShowInstance(d, id)
          case i: Interface => ???
        }
      case _ => ???
    }
    ???

  def generateShowInstance(decl: Declaration.Data, id: Id)(using ShowContext)(using DeclarationContext): Toplevel.Def =
    val defnId = id
    val valueId = Id("value")
    val valueTpe = ValueType.Data(decl.id, List.empty)
    val vparam = ValueParam(valueId, valueTpe)
    
    val constructorIds = decl.constructors.zipWithIndex.map((constr, index) => Id(s"b_k$index"))
    val constructorVparams = decl.constructors.map(constructorVparam)
    val constructorIdVparams = constructorIds.zip(constructorVparams)
    val v_r = Id("v_r")
    val clauses = decl.constructors.zip(constructorIdVparams).map(constructorClauses)
    val matchStmt = Stmt.Match(
      ValueVar(valueId, valueTpe),
      clauses,
      None
    )
    val returnVal = Stmt.Val(
      v_r,
      TString,
      matchStmt,
      Return(ValueVar(v_r, TString))
    )

    val constructorStmts = decl.constructors.map(constructorStmt)
    val defns = constructorStmts.zip(constructorIdVparams)
    val stmt = nestDefns(defns, returnVal)

    Toplevel.Def(defnId, Block.BlockLit(List.empty, List.empty, List(vparam), List.empty, stmt))

  def nestDefns(defns: List[(Stmt, (Id, List[ValueParam]))], default: Stmt): Stmt = defns match 
    case (head, (id, vparams)) :: rest => 
      val recur = nestDefns(rest, default)
      Stmt.Def(id, BlockLit(List.empty, List.empty, vparams, List.empty, head), recur)
    case Nil => default

  def constructorClauses(constridvparams: (Constructor, (Id, List[ValueParam])))(using ShowContext): (Id, BlockLit) = constridvparams match
    case (Constructor(id, tparams, fields), (fnId, vparams)) => 
      (id, BlockLit(List.empty, List.empty, vparams, List.empty, 
        Stmt.App(BlockVar(fnId, BlockType.Function(List.empty, List.empty, vparams.map(_.tpe), List.empty, TString), Set.empty), List.empty, fields map fieldValueVar, List.empty)))

  def constructorVparam(constr: Constructor)(using ShowContext): List[ValueParam] = constr match 
    case Constructor(id, tparams, fields) => fields.map(f => ValueParam(f.id, f.tpe))

  def constructorStmt(constr: Constructor)(using ctx: ShowContext)(using dctx: DeclarationContext): Stmt = constr match
    case Constructor(id, tparams, List()) => Return(Literal(id.name.name, TString))
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
      val app: Stmt.App = App(getShowBlockVar(tpe), List.empty, List(ValueVar(id, tpe)), List.empty)
      (Id("field"), app)

  var freshShowCounter = 0
  def freshShowId: Id = 
    val freshId = Id(FUNCTION_NAME ++ freshShowCounter.toString())
    freshShowCounter += 1
    freshId
}
