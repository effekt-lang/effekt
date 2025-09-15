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

  case class ShowContext(showDefns: collection.mutable.Map[ValueType, Id], backend: String, externFns: Map[String, Extern.Def]) {
    def getShowBlockVar(vt: ValueType): Block.BlockVar =
      val showId = showDefns(vt)
      Block.BlockVar(showId, BlockType.Function(List.empty, List.empty, List(vt), List.empty, TString), Set.empty)

    def externBlockVar(name: String): Block.BlockVar =
      val fn = externFns(name)
      Block.BlockVar(fn.id, BlockType.Function(fn.tparams, fn.cparams, fn.vparams map (_.tpe), fn.bparams map (_.tpe), fn.ret), fn.annotatedCapture)
  }

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      // 1. Find ids of relevant extern functions by name
      val externFnNames = List(
        "myshow",
        "infixConcat",
        "c_bytearray_show_Int",
        "c_bytearray_show_Byte",
        "c_bytearray_show_Double",
      )
      val externFns = findRelevantExterns(core.externs, externFnNames)

      // 2. Define list of base types to synthesize definitions for
      // 2.1 actual base types (Int, Char, ...)
      var builtinTypes = Type.Builtins
      // 2.2 data types without tparams
      val declarationTypes = core.declarations.flatMap(decl => decl match
        case Data(id, List(), constructors) => Some(decl)
        case _ => None
      )
      // 2.3 data types with targs (later fixed by Mono)
      // 2.4 externs (hard, if not impossible)
      // 2.5 Types with user defined show definitions (skip for now, unclear if we even want this)

      // 3. Synthesize `show` definitions, create (ValueType -> (show) Id) map for context
      // 3.1 figure out which backend we are generating for
      val backend: String = externFns("myshow").body match {
        case StringExternBody(featureFlag: FeatureFlag.NamedFeatureFlag, contents) => featureFlag.id
        case _ => sys error "Should never happen"
      }
      implicit val ctx: ShowContext = ShowContext(collection.mutable.Map.empty, backend, externFns)

      // 3.2 generate show instances for base types in that backend
      val baseShowInstances = generateShowInstancesBases(builtinTypes)
      // 3.3 generate show instances for declarations
      val declShowInstances = generateShowInstancesDecls(declarationTypes)

      // var transformed = transform(core)
      // println(util.show(transformed))
      // Some(CoreTransformed(source, tree, mod, transformed))

      None
    }
  }

  def findRelevantExterns(externs: List[Extern], names: List[String]): Map[String, Extern.Def] = 
    var retMap = Map[String, Extern.Def]()
    externs.foreach({
      case defn@Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) if names.contains(id.name.name) => 
        retMap += (id.name.name -> defn)
      case _ => ()
    })
    retMap

  def transform(decl: ModuleDecl)(using ctx: ShowContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) => 
      val transformedDefns = definitions map transform
      // Remove myshow extern
      // because it only has a dummy implementation which will fail when run
      val filteredExterns = decl.externs.flatMap(ext => ext match
        case e: Extern.Def => List(e)
        case _ => List()
      )
      .filter(defn => defn.id.name.name != "myshow")
      ModuleDecl(path, includes, declarations, filteredExterns, transformedDefns, exports)
  }

  def transform(toplevel: Toplevel)(using ShowContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, tpe, binding) => ???
  }

  def transform(block: Block)(using ShowContext): Block = block match {
    case BlockLit(tparams, cparams, vparams, bparams, body) => BlockLit(tparams, cparams, vparams, bparams, transform(body))
    case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, annotatedTpe, annotatedCapt)
    case New(impl) => ???
    case Unbox(pure) => ???
  }

  def transform(stmt: Stmt)(using ctx: ShowContext): Stmt = 
  stmt match {
    case Let(id, annotatedTpe, PureApp(BlockVar(bid, bannotatedTpe, bannotatedCapt), targs, vargs), body) if bid.name.name == "myshow" => 
      if (targs.length != 1) sys error "expected targs to only have one argument"
      Stmt.Val(id, annotatedTpe, Stmt.App(ctx.getShowBlockVar(targs(0)), List.empty, vargs, List.empty), transform(body))
    case Let(id, annotatedTpe, binding, body) => Let(id, annotatedTpe, transform(binding), transform(body))
    case Return(expr) => Return(transform(expr))
    // TODO: We might need to do the same thing as in PureApp if we want to allow show(something) instead of something.show
    case ImpureApp(id, callee, targs, vargs, bargs, body) => ImpureApp(id, callee, targs, vargs, bargs, body)
    case o => println(o); ???
  }

  def transform(expr: Expr)(using ctx: ShowContext): Expr = expr match {
    case Make(data, tag, targs, vargs) => Make(data, tag, targs, vargs)
    case PureApp(BlockVar(id, annotatedTpe, annotatedCapt), targs, vargs) if id.name.name == "myshow" => 
      if (targs.length != 1) sys error "expected targs to only have one argument"
      Expr.PureApp(ctx.getShowBlockVar(targs(0)), List.empty, vargs)
    case PureApp(b, targs, vargs) => PureApp(b, targs, vargs)
    case o => println(o); ???
  }

  def generateShowInstancesBases(baseTypes: List[ValueType])(using ShowContext): List[Toplevel.Def] =
    baseTypes flatMap generateShowInstance

  // The cases here should match the list effekt.core.Type.Builtins
  def generateShowInstance(vt: ValueType)(using ctx: ShowContext): Option[Toplevel.Def] = 
    ctx.backend match {
      case "llvm" => generateShowInstanceLLVM(vt) 
    }

  def generateShowInstanceLLVM(vt: ValueType)(using ctx: ShowContext): Option[Toplevel.Def] =
    val showId = freshShowId
    val paramId = Id("value")
    val vparam = ValueParam(paramId, vt)
    val valueVar = Expr.ValueVar(paramId, vt)

    def generateBlockLit(stmt: Stmt) = BlockLit(List.empty, List.empty, List(vparam), List.empty, stmt)
    def generateDef(stmt: Stmt): Toplevel.Def = 
      ctx.showDefns += (vt -> showId)
      val block = generateBlockLit(stmt)
      Toplevel.Def(showId, block)

    vt match {
      case Type.TString =>
        // (value: String) { return value }
        val stmt = Stmt.Return(valueVar)
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
        val blockVar = ctx.externBlockVar("c_bytearray_show_Int")
        val call = Expr.PureApp(blockVar, List.empty, List(valueVar))
        val stmt = Stmt.Return(call)
        Some(generateDef(stmt))
      case Type.TByte =>
        /* 
        (value: Byte) {
          %z = call %Pos @c_bytearray_show_Byte(%Byte ${value})
          ret %Pos %z
        }
        */
        val blockVar = ctx.externBlockVar("c_bytearray_show_Byte")
        val call = Expr.PureApp(blockVar, List.empty, List(valueVar))
        val stmt = Stmt.Return(call)
        Some(generateDef(stmt))
      case Type.TDouble =>
        /* 
        (value: Double) {
          %z = call %Pos @c_bytearray_show_Double(%Double ${value})
          ret %Pos %z
        }
        */
        val blockVar = ctx.externBlockVar("c_bytearray_show_Double")
        val call = Expr.PureApp(blockVar, List.empty, List(valueVar))
        val stmt = Stmt.Return(call)
        Some(generateDef(stmt))
      case Type.TBoolean =>
        // if (value) "true" else "false"
        val stmt = Stmt.If(valueVar, Stmt.Return(Expr.Literal("true", TString)), Stmt.Return(Expr.Literal("false", TString)))
        Some(generateDef(stmt))

      case _ => println(s"Missing case for vt: ${vt}"); None

    }

  def generateShowInstancesDecls(declarations: List[Declaration])(using ShowContext): List[Toplevel.Def] = 
    declarations flatMap generateShowInstance

  def generateShowInstance(decl: Declaration)(using ctx: ShowContext): Option[Toplevel.Def] = decl match {
    case dataDecl: Declaration.Data => 
      val freshId = freshShowId
      val toplevel = generateShowInstance(dataDecl, freshId)
      ctx.showDefns += (ValueType.Data(dataDecl.id, List.empty) -> freshId)
      Some(toplevel)
    case _ => None
  }

  def generateShowInstance(declarations: List[Declaration], vt: ValueType, id: Id)(using ctx: ShowContext): Toplevel.Def =
    vt match {
      case ValueType.Data(name, targs) => 
        declarations.find(decl => decl.id == name).get match {
          case d: Data => return generateShowInstance(d, id)
          case i: Interface => ???
        }
      case _ => ???
    }
    ???

  def generateShowInstance(decl: Declaration.Data, id: Id)(using ShowContext): Toplevel.Def =
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

  def constructorStmt(constr: Constructor)(using ctx: ShowContext): Stmt = constr match
    case Constructor(id, tparams, List()) => Return(Literal(id.name.name, TString))
    case Constructor(id, tparams, fields) => 
      val infixConcatBlockVar: Block.BlockVar = ctx.externBlockVar("infixConcat")
      val pureFields = fields map fieldPure
      val concatenated = PureApp(infixConcatBlockVar, List.empty, List(Literal(id.name.name ++ "(", TString), concatPure(pureFields)))
      Return(concatenated)

  // Convert a list of pure statements to comma-separated concatenated version
  // Literal("Just("), PureApp(show, x), Literal(", "), PureApp(show, y), Literal(")")
  //  =>
  // PureApp(concat, List(Literal("Just("), PureApp(concat, List(PureApp(show, x), PureApp(concat, List(Literal(", "), ...))))
  def concatPure(pures: List[Expr])(using ctx: ShowContext): Expr = pures match 
    case head :: next :: rest => PureApp(ctx.externBlockVar("infixConcat"), List.empty, List(head, PureApp(ctx.externBlockVar("infixConcat"), List.empty, List(Literal(", ", TString), concatPure(next :: rest)))))
    case head :: Nil => PureApp(ctx.externBlockVar("infixConcat"), List.empty, List(head, Literal(")", TString)))
    case Nil => Literal(")", TString)

  def fieldValueVar(field: Field): Expr = field match
    case Field(id, tpe) => ValueVar(id, tpe)

  def fieldPure(field: Field)(using ctx: ShowContext): Expr.PureApp = field match
      case Field(id, tpe) => PureApp(ctx.getShowBlockVar(tpe), List.empty, List(ValueVar(id, tpe)))

  var freshShowCounter = 0
  def freshShowId: Id = 
    val freshId = Id("show" ++ freshShowCounter.toString())
    freshShowCounter += 1
    freshId
}
