package effekt
package cps

import effekt.core.CoreParsers.definition
import effekt.symbols.{ Constructor, Name, Symbol}

import effekt.symbols.builtins

export effekt.core.Id

case class ModuleDecl(
  path: String,
  includes: List[String],
  decls: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Symbol]
)

/**
 * Toplevel data and interface declarations
 */
enum Declaration {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Field])
case class Field(id: Id, tpe: ValueType)
case class Property(id: Id, tpe: BlockType)

/**
 * FFI external definitions
 */
enum Extern {
  // WARNING: builtins do not take evidence. If they are passed as function argument, they need to be eta-expanded.
  //   (however, if they _would_ take evidence, we could model mutable state with this)
  // TODO revisit
  case Def(id: Id, tparams: List[Id], params: List[Param], ret: ValueType, body: Template[Expr])
  case Include(contents: String)
}



enum Definition {
  case Function(name: Id, params: List[Param], cont: Id, body: Term)
  case Let(id: Id, binding: Expr)
}


enum Expr{
  case Var(name: Id)
  case Lit(n: Int)
  case PureApp(b: Block, targs: List[ValueType], args: List[Either[Expr, Block]])

  case Make(data: ValueType.Data, tag: Id, vargs: List[Expr])
  case Select(target: Expr, field: Id, annotatedType: ValueType)
  case Box(b: Block)

  case Run(t: Term)
  //val tpe: ValueType = Type.inferType(this)
}
export Expr.*

/**
 * Blocks
 */
enum Block{

  case BlockVar(id: Id, annotatedType: BlockType)
  case BlockLit(tparams: List[Id], params: List[Param], body: Term)
  case Member(b: Block, field: Id, annotatedTpe: BlockType)

  // WARNING not officially supported, yet
  case Unbox(e: Expr)
  case New(impl: Implementation)

  //val tpe: BlockType = Type.inferType(this)
}
export Block.*

enum Term {
  case LetCont(cont: Id, param: Param, body: Term, rest: Term)
  case Let(name: Id, expr: Expr, rest: Term)
  case AppCont(cont: Id, arg: Expr)
  case App(func: Id, arg: List[Expr], cont: Id)
  case Scope(definitions: List[Definition], body: Term)
  case Val(id: Id, binding: Term, body: Term)
  case If(cond: Expr, thn: Term, els: Term)
  case Match(scrutinee: Expr, clauses: List[(Id, BlockLit)], default: Option[Term])
  //val tpe: ValueType = Type.inferType(this)
} 
export Term.*




/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]){
  val tpe = interface
}

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: symbols.Symbol, implementation: Block.BlockLit)



enum Param {
  def id: Id

  case ValueParam(id: Id, tpe: ValueType)
  case BlockParam(id: Id, tpe: BlockType)
  case EvidenceParam(id: Id)
  }

sealed trait Type

enum ValueType extends Type {
  case Var(name: Id)
  case Data(name: Id, targs: List[ValueType])
  case Boxed(tpe: BlockType) // WARNING not supported
}

case class EvidenceType() extends Type

enum BlockType extends Type {

  //   [A, B, C] (EV, EV, X, Y, Z, (EV, T) => T)    =>    T
  //    ^^^^^^^   ^^^^^^  ^^^^^^^  ^^^^^^^^^^^^^         ^^^
  //    tparams   evid.   vparams    bparams            result
  case Function(tparams: List[Id], eparams: List[EvidenceType], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(name: Id, targs: List[ValueType])
}

object Type {

  // The subtyping lattice
  val TTop = ValueType.Data(builtins.TopSymbol, Nil)
  val TBottom = ValueType.Data(builtins.BottomSymbol, Nil)

  val TUnit   = ValueType.Data(builtins.UnitSymbol, Nil)
  val TInt = ValueType.Data(builtins.IntSymbol, Nil)
  val TBoolean = ValueType.Data(builtins.BooleanSymbol, Nil)
  val TString = ValueType.Data(builtins.StringSymbol, Nil)
  val TDouble = ValueType.Data(builtins.DoubleSymbol, Nil)

  val TRegion = BlockType.Interface(builtins.RegionSymbol, Nil)
  def TState(tpe: ValueType) = BlockType.Interface(builtins.TState.interface, List(tpe))

  /**
   * Function types are the only type constructor that we have subtyping on.
   *
   * Copy and paste from core
   */
  def merge(tpe1: ValueType, tpe2: ValueType, covariant: Boolean): ValueType = (tpe1, tpe2) match {
    case (ValueType.Boxed(btpe1), ValueType.Boxed(btpe2)) =>
      ValueType.Boxed(merge(btpe1, btpe2, covariant))
    case (tpe1, tpe2) if covariant =>
      if (isSubtype(tpe1, tpe2)) tpe2 else tpe1
    case (tpe1, tpe2) if !covariant =>
      if (isSubtype(tpe1, tpe2)) tpe1 else tpe2
    case _ => tpe1
  }
  private def isSubtype(tpe1: ValueType, tpe2: ValueType): Boolean = (tpe1, tpe2) match {
    case (tpe1, TTop) => true
    case (TBottom, tpe1) => true
    case _ => false // conservative :)
  }

  def merge(tpe1: BlockType, tpe2: BlockType, covariant: Boolean): BlockType = (tpe1, tpe2) match {
    case (BlockType.Function(tparams1, eparams1, vparams1, bparams1, result1), tpe2: BlockType.Function) =>
      val BlockType.Function(_, eparams2, vparams2, bparams2, result2) = instantiate(tpe2, tparams1.map(ValueType.Var.apply))
      val vparams = (vparams1 zip vparams2).map { case (tpe1, tpe2) => merge(tpe1, tpe2, !covariant) }
      val bparams = (bparams1 zip bparams2).map { case (tpe1, tpe2) => merge(tpe1, tpe2, !covariant) }
      BlockType.Function(tparams1, eparams1, vparams, bparams, merge(result1, result2, covariant))
    case (tpe1, tpe2) => tpe1
  }

  def instantiate(f: BlockType.Function, targs: List[ValueType]): BlockType.Function = f match {
    case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
      assert(targs.size == tparams.size, "Wrong number of type arguments")

      val vsubst = (tparams zip targs).toMap
      BlockType.Function(Nil, eparams,
        vparams.map { tpe => substitute(tpe, vsubst) },
        bparams.map { tpe => substitute(tpe, vsubst) },
        substitute(result, vsubst))
  }

  def substitute(tpe: BlockType, vsubst: Map[Id, ValueType]): BlockType = tpe match {
    case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
      // names are unique symbols so shadowing should NOT take place; we still subtract to be safe.
      val vsubstLocal = vsubst -- tparams

      BlockType.Function(tparams, eparams,
        vparams.map { tpe => substitute(tpe, vsubstLocal) },
        bparams.map { tpe => substitute(tpe, vsubstLocal) },
        substitute(result, vsubstLocal))

    case BlockType.Interface(sym, targs) =>
      BlockType.Interface(sym, targs map { tpe => substitute(tpe, vsubst) })
  }

  def substitute(tpe: ValueType, vsubst: Map[Id, ValueType]): ValueType = tpe match {
    case ValueType.Var(id) if vsubst.isDefinedAt(id) => vsubst(id)
    case ValueType.Var(id) => tpe
    case ValueType.Data(sym, targs) => ValueType.Data(sym, targs.map(t => substitute(t, vsubst)))
    case ValueType.Boxed(tpe) => ValueType.Boxed(substitute(tpe, vsubst))
  }

  /*def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(id, tpe) => tpe

    case Block.BlockLit(tparams, params, body) =>

      val eparams = params.collect { case Param.EvidenceParam(id) => EvidenceType() }
      val vparams = params.collect { case Param.ValueParam(id, tpe) => tpe }
      val bparams = params.collect { case Param.BlockParam(id, tpe) => tpe }
      BlockType.Function(tparams, eparams, vparams, bparams, body.tpe)

    case Block.Member(b, field, tpe) => tpe
    case Block.Unbox(pure) => pure.tpe.asInstanceOf[ValueType.Boxed].tpe
    case Block.New(impl) => impl.tpe
  }*/
  def inferType(term: Term): ValueType = term match {
    /*case Stmt.Scope(definitions, body) => body.tpe
    case Stmt.Return(expr) => expr.tpe
    case Stmt.Val(id, binding, body) => body.tpe
    case Stmt.App(callee, targs, args) => instantiate(callee.functionType, targs).result

    case Stmt.If(cond, thn, els) => merge(thn.tpe, els.tpe, covariant = true)
    case Stmt.Match(scrutinee, clauses, default) =>
      val allTypes = clauses.map { case (_, cl) => cl.returnType } ++ default.map(_.tpe).toList
      allTypes.fold(TBottom) { case (tpe1, tpe2) => merge(tpe1, tpe2, covariant = true) }

    case Stmt.Alloc(id, init, region, ev, body) => body.tpe
    case Stmt.Var(init, body) => body.returnType
    case Stmt.Get(ev, id, tpe) => tpe
    case Stmt.Put(ev, id, value) => TUnit
    case Stmt.Try(body, handler) => body.returnType
    case Stmt.Reset(body) => body.tpe
    case Stmt.Region(body) => body.returnType

    case Stmt.Shift(ev, BlockLit(tparams, List(BlockParam(kid, tpe)), body)) => tpe match {
      // shift { {k: tau => ???} => ... } : tau
      case BlockType.Function(_, _, List(tpe), _, _) => tpe
      // shift { {k: {{Exc} => tau} => ???} => ... } : tau
      case BlockType.Function(_, _, _, List(BlockType.Function(_, _, _, _, tpe)), _) => tpe
      case _ =>
        INTERNAL_ERROR("Should not happen: the continuation takes either one value or a function that returns a value")
    }

    case Stmt.Shift(ev, body) =>
      INTERNAL_ERROR("Should not happen: shift takes exactly one block literal")

    case Stmt.Hole() => TBottom
*/
    case _ => ???}

  def inferType(expr: Expr): ValueType = expr match {
    //case Lit(value) => 
    // case DirectApp(callee, targs, vargs, bargs) => instantiate(callee.functionType, targs).result
    /*case Expr.Run(s) => s.tpe
    case Expr.ValueVar(id, tpe) => tpe
    case Expr.Literal(value, tpe) => tpe
    case Expr.Make(data, tag, args) => data
    case Expr.PureApp(callee, targs, args) => instantiate(callee.functionType, targs).result
    case Expr.Select(target, field, annotatedType) => annotatedType
    case Expr.Box(block) => ValueType.Boxed(block.tpe)*/
    case _ => println(expr); ???
  }

  /*extension (block: Block) {
    def returnType: ValueType = block.functionType.result
    def functionType: BlockType.Function = block.tpe.asInstanceOf
  }*/ 
}


