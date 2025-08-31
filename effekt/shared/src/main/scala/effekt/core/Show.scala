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

object Show extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "show"

  case class ShowContext(showDefnsMap: Map[List[ValueType], Block.BlockVar], groundTypes: List[ValueType], var defnsToGenerate: collection.mutable.Map[ValueType, Id], infixConcatFn: Block.BlockVar) 

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {

      // Collect all extern definitions that are named "show"
      // Record what type of values these functions take as arguments
      // Store these in a Map so we know our ground types
      // Map[Int -> show#1, Char -> show#2, ...] 
      val extShowDefns: List[Extern.Def] = core.externs.flatMap(ext => ext match
        case e: Extern.Def => List(e)
        case _ => List()
      )
      // TODO: Could be more specific in filtering here
      //       for example only allow functions with one argument
      //       which return Strings
      .filter(defn => defn.id.name.name == "show")

      val myshow = core.externs.flatMap(ext => ext match
        case e: Extern.Def => List(e)
        case _ => List()
      )
      .filter(defn => defn.id.name.name == "myshow")
      println(myshow)

      val blockVars: List[Block.BlockVar] = extShowDefns.map(defn => 
        Block.BlockVar(defn.id, BlockType.Function(defn.tparams, defn.cparams, defn.vparams map (_.tpe), defn.bparams map (_.tpe), defn.ret), Set.empty))
      val showDefnsMap: Map[List[ValueType], Block.BlockVar] = extShowDefns.zip(blockVars).map((defn, bv) => 
        (defn.vparams.map(_.tpe), bv)).toMap


      // TODO: This already assumes there is only one vparam
      val extGroundTypes = extShowDefns.map(defn => defn.vparams.map(_.tpe)(0))

      // TODO: We also want to include declarations with targs
      //       if those are called with ground types or types we already have generated a show instance for 
      val declGroundTypes = core.declarations.flatMap(decl => decl match {
        case Data(id, List(), constructors) =>
          List(ValueType.Data(id, List.empty))
        case _ => List()
      })
      val groundTypes = extGroundTypes ++ declGroundTypes

      // TODO: There has to be a better way... right?
      val infixConcatDef = core.externs.find(ext => ext match {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) if id.name.name == "infixConcat" => 
          true
        case _ => false
      }).get
      var infixConcatFn: Block.BlockVar = infixConcatDef match {
        case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => 
          BlockVar(id, BlockType.Function(tparams, cparams, vparams map (_.tpe), bparams map (_.tpe), ret), Set())
        case _ => sys error "Does not happen"
      }

      implicit val ctx: ShowContext = ShowContext(showDefnsMap, groundTypes, collection.mutable.Map.empty, infixConcatFn)

      var transformed = transform(core)

      println(util.show(transformed))

      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using ctx: ShowContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) => 
      val transformedDefns = definitions map transform
      val additionalDefns = generateShowInstances(declarations)
      additionalDefns.foreach(d => println(util.show(d)))
      ModuleDecl(path, includes, declarations, externs, transformedDefns ++ additionalDefns, exports)
  }

  // Q: May externs have show instances on any type
  //    Ignoring these for now 
  // def transform(extern: Extern)(using ShowContext): Extern = extern match {
  //   case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => ???
  //   case Extern.Include(featureFlag, contents) => Extern.Include(featureFlag, contents)
  // }

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

  def transform(stmt: Stmt)(using ShowContext): Stmt = stmt match {
    case Let(id, annotatedTpe, binding, body) => Let(id, annotatedTpe, transform(binding), transform(body))
    case Return(expr) => Return(transform(expr))
    case o => println(o); ???
  }

  def transform(pure: Pure)(using ShowContext): Pure = pure match {
    case ValueVar(id, annotatedType) => ValueVar(id, annotatedType)
    case o => println(o); ???
  }

  def transform(expr: Expr)(using ctx: ShowContext): Expr = expr match {
    case Make(data, tag, targs, vargs) => Make(data, tag, targs, vargs)
    case PureApp(BlockVar(id, annotatedTpe, annotatedCapt), targs, vargs) if id.name.name == "myshow" => 
      if (targs.length != 1) sys error "expected targs to only have one argument"
      val targ = targs(0)
      if (isGroundType(targ))
        val bvar = getOrAddShow(targ)
        return Pure.PureApp(bvar, List.empty, vargs)
      sys error "targ was not ground type in PureApp"
    case PureApp(b, targs, vargs) => PureApp(b, targs, vargs)
    // TODO: We might need to do the same thing as in PureApp if we want to allow show(something) instead of something.show
    case DirectApp(b, targs, vargs, bargs) => DirectApp(b, targs, vargs, bargs)
    case o => println(o); ???
  }

  def generateShowInstances(declarations: List[Declaration])(using ctx: ShowContext): List[Toplevel] = 
    ctx.defnsToGenerate.map((vt, id) => generateShowInstance(declarations, vt, id)).toList

  def generateShowInstance(declarations: List[Declaration], vt: ValueType, id: Id)(using ctx: ShowContext): Toplevel =
    vt match {
      case ValueType.Data(name, targs) => 
        declarations.find(decl => decl.id == name).get match {
          case d: Data => return generateShowInstance(d, id)
          case i: Interface => ???
        }
      case _ => ???
    }
    ???

  def generateShowInstance(decl: Declaration.Data, id: Id)(using ShowContext): Toplevel =
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

    val finalDefn = Toplevel.Def(defnId, Block.BlockLit(List.empty, List.empty, List(vparam), List.empty, stmt))
    finalDefn

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
      val infixConcatBlockVar: Block.BlockVar = ctx.infixConcatFn
      val pureFields = fields map fieldPure
      val concatenated = PureApp(ctx.infixConcatFn, List.empty, List(Literal(id.name.name ++ "(", TString), concatPure(pureFields)))
      Return(concatenated)

  // Convert a list of pure statements to comma-separated concatenated version
  // Literal("Just("), PureApp(show, x), Literal(", "), PureApp(show, y), Literal(")")
  //  =>
  // PureApp(concat, List(Literal("Just("), PureApp(concat, List(PureApp(show, x), PureApp(concat, List(Literal(", "), ...))))
  def concatPure(pures: List[Pure])(using ctx: ShowContext): Pure = pures match 
    case head :: next :: rest => PureApp(ctx.infixConcatFn, List.empty, List(head, PureApp(ctx.infixConcatFn, List.empty, List(Literal(", ", TString), concatPure(next :: rest)))))
    case head :: Nil => PureApp(ctx.infixConcatFn, List.empty, List(head, Literal(")", TString)))
    case Nil => Literal(")", TString)

  def fieldValueVar(field: Field): Pure = field match
    case Field(id, tpe) => ValueVar(id, tpe)

  def fieldPure(field: Field)(using ctx: ShowContext): Pure = field match
    case Field(id, tpe) => PureApp(ctx.showDefnsMap(List(tpe)), List.empty, List(ValueVar(id, tpe)))

  def getOrAddShow(vt: ValueType)(using ctx: ShowContext): BlockVar = vt match
    case ValueType.Boxed(tpe, capt) => ???
    case ValueType.Data(name, targs) => 
      val id = ctx.defnsToGenerate.getOrElse(vt, {
        val freshShowId = Id("myshow")
        ctx.defnsToGenerate += (vt -> freshShowId)
        freshShowId
      })
      val freshBlockType = BlockType.Function(List.empty, List.empty, List(vt), List.empty, TString)
      BlockVar(id, freshBlockType, Set.empty)
    case ValueType.Var(name) => ???

  def isGroundType(vt: ValueType)(using ctx: ShowContext): Boolean = ctx.groundTypes.contains(vt)
}
