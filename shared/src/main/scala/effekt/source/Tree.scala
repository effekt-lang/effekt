package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol

/**
 * Data type representing source program trees.
 *
 * Terms:
 *
 *   Tree
 *   |- Id
 *   |  |- IdDef
 *   |  |- IdRef
 *   |
 *   |- ModuleDecl
 *   |
 *   |- ParamSection
 *   |  |- ValueParams
 *   |  |- BlockParam
 *   |  |- CapabilityParam (*)
 *   |
 *   |- ArgSection
 *   |  |- ValueArgs
 *   |  |- BlockArg
 *   |  |- CapabilityArg (*)
 *   |
 *   |- Def
 *   |  |- FunDef
 *   |  |- ValDef
 *   |  |- VarDef
 *   |  |- EffDef
 *   |  |- DataDef
 *   |  |- RecordDef
 *   |  |- ExternType
 *   |  |- ExternFun
 *   |  |- ExternInclude
 *   |
 *   |- Stmt
 *   |  |- DefStmt
 *   |  |- ExprStmt
 *   |  |- BlockStmt
 *   |  |- Return
 *   |
 *   |- Expr
 *   |  |- Var
 *   |  |- Assign
 *   |  |- Literal
 *   |  |  |- UnitLit
 *   |  |  |- IntLit
 *   |  |  |- BooleanLit
 *   |  |  |- DoubleLit
 *   |  |  |- StringLit
 *   |  |
 *   |  |- Lambda
 *   |  |- Call
 *   |  |- If
 *   |  |- While
 *   |  |- TryHandle
 *   |  |- MatchExpr
 *   |  |- Hole
 *   |
 *
 * Types
 *   ...
 *   |- Type
 *   |  |- ValueType
 *   |  |  |- ValueTypeTree (*)
 *   |  |  |- TypeVar
 *   |  |  |- TypeApp
 *   |  |
 *   |  |- CapabilityType
 *   |  |- BlockType
 *
 * We extend product to allow reflective access by Kiama.
 */
sealed trait Tree extends Product {
  def inheritPosition(from: Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * Used for builtin and synthesized trees
 */
case object NoSource extends Tree

// only used by the lexer
case class Comment() extends Tree

/**
 * We distinguish between identifiers corresponding to
 * - binding sites (IdDef)
 * - use site (IdRef)
 * in the syntax. This way, we can simplify the traversal in Namer
 */
sealed trait Id extends Tree {
  def name: String
  def symbol(implicit C: Context): Symbol = C.symbolOf(this)
  def clone(implicit C: Context): Id

}
case class IdDef(name: String) extends Id {
  def clone(implicit C: Context): Id = {
    val copy = IdDef(name)
    C.positions.dupPos(this, copy)
    copy
  }
}
case class IdRef(name: String) extends Id {
  def clone(implicit C: Context): Id = {
    val copy = IdRef(name)
    C.positions.dupPos(this, copy)
    copy
  }
}

sealed trait Named extends Tree {
  def id: Id
  type symbol <: Symbol
}

// Something that later will be stored in the symbol table
sealed trait Definition extends Named {
  def id: IdDef
  def symbol(implicit C: Context): symbol = C.symbolOf(this)
}

// Something that later can be looked up in the symbol table
sealed trait Reference extends Named {
  def id: IdRef
  def definition(implicit C: Context): symbol = C.symbolOf(this)
}

/**
 * The type of whole compilation units
 *
 * Only a subset of definitions (FunDef and EffDef) is allowed on the toplevel
 *
 * A module declartion, the path should be an Effekt include path, not a system dependent file path
 *
 */
case class ModuleDecl(path: String, imports: List[Import], defs: List[Def]) extends Tree
case class Import(path: String) extends Tree

/**
 * Parameters and arguments
 */
sealed trait ParamSection extends Tree
case class ValueParams(params: List[ValueParam]) extends ParamSection
case class ValueParam(id: IdDef, tpe: Option[ValueType]) extends Definition { type symbol = symbols.ValueParam }

case class BlockParam(id: IdDef, tpe: BlockType) extends ParamSection with Definition { type symbol = symbols.BlockParam }

sealed trait ArgSection extends Tree
case class ValueArgs(args: List[Expr]) extends ArgSection
case class BlockArg(params: List[ParamSection], body: Stmt) extends ArgSection
case class CapabilityArg(id: IdRef) extends ArgSection with Reference {
  type symbol = symbols.BlockParam
}

/**
 * Global and local definitions
 */
sealed trait Def extends Definition {
  def id: IdDef
}
case class FunDef(id: IdDef, tparams: List[Id], params: List[ParamSection], ret: Option[ValueType], body: Stmt) extends Def {
  type symbol = symbols.UserFunction
}
case class ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.ValBinder
}
case class VarDef(id: IdDef, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.VarBinder
}
case class EffDef(id: IdDef, tparams: List[Id], ops: List[Operation]) extends Def {
  type symbol = symbols.Interface
}
case class Operation(id: IdDef, tparams: List[Id], params: List[ValueParams], ret: ValueType) extends Definition {
  type symbol = symbols.Operation
}
case class DataDef(id: IdDef, tparams: List[Id], ctors: List[Constructor]) extends Def {
  type symbol = symbols.DataType
}
case class Constructor(id: IdDef, params: ValueParams) extends Definition {
  type symbol = symbols.Record
}

// only valid on the toplevel!
case class ExternType(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinType
}
//case class ExternEffect(id: IdDef, tparams: List[Id]) extends Def {
//  type symbol = symbols.BuiltinEffect
//}
case class ExternFun(pure: Boolean, id: IdDef, tparams: List[Id], params: List[ParamSection], ret: ValueType, body: String) extends Def {
  type symbol = symbols.BuiltinFunction
}
case class ExternInclude(path: String) extends Def {
  def id = IdDef("includes don't have names")
  // Namer resolves the path and loads the contents
  var contents: String = ""
}

sealed trait Stmt extends Tree
case class DefStmt(d: Def, rest: Stmt) extends Stmt
case class ExprStmt(d: Expr, rest: Stmt) extends Stmt
case class Return(d: Expr) extends Stmt
case class BlockStmt(stmts: Stmt) extends Stmt

/**
 * In our source language, almost everything is an expression.
 * Effectful calls, if, while, ...
 */
sealed trait Expr extends Tree

// Variable / Value use (can now also stand for blocks)
case class Var(id: IdRef) extends Expr with Reference {
  type symbol = symbols.TermSymbol
}
case class Assign(id: IdRef, expr: Expr) extends Expr with Reference {
  type symbol = symbols.VarBinder
}

sealed trait Literal[T] extends Expr {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

/**
 * Selecting a capability or a method out of a capability
 */
case class Select(receiver: Expr, selector: IdRef) extends Expr

// maybe replace `fun: Id` here with BlockVar
// TODO should we have one Call-node and a selector tree, or multiple different call nodes?
case class Call(receiver: Expr, targs: List[ValueType], args: List[ArgSection]) extends Expr

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Expr
case class While(cond: Expr, block: Stmt) extends Expr

//case class TryHandle(prog: Stmt, handlers: List[Handler]) extends Expr

/**
 * Currently, the source language does not allow us to explicitly bind the capabilities.
 * The capability parameter here is annotated by the capability-passing transformation
 *
 *   try {
 *     <prog>
 *   } with <eff> : <Effect> { ... }
 *
 * Here eff is the capability parameter, as introduced by the transformation.
 */
//case class Handler(effect: Effect, capability: Option[CapabilityParam] = None, clauses: List[OpClause]) extends Reference {
//  def id = effect.id
//  type symbol = symbols.UserEffect
//}
//case class OpClause(id: IdRef, params: List[ParamSection], body: Stmt, resume: IdDef) extends Reference {
//  type symbol = symbols.EffectOp
//}

case class Hole(stmts: Stmt) extends Expr

case class MatchExpr(scrutinee: Expr, clauses: List[MatchClause]) extends Expr
case class MatchClause(pattern: MatchPattern, body: Stmt) extends Tree

sealed trait MatchPattern extends Tree

/**
 * Pattern matching anything:
 *
 *   case a => ...
 */
case class AnyPattern(id: IdDef) extends MatchPattern with Definition { type symbol = symbols.ValueParam }

/**
 * Pattern matching on a constructor
 *
 *   case Cons(a, as) => ...
 */
case class TagPattern(id: IdRef, patterns: List[MatchPattern]) extends MatchPattern with Reference {
  type symbol = symbols.Record
}

/**
 * A wildcard pattern ignoring the matched value
 *
 *   case _ => ...
 */
case class IgnorePattern() extends MatchPattern

/**
 * A pattern that matches a single literal value
 */
case class LiteralPattern[T](l: Literal[T]) extends MatchPattern

/**
 * Something that can be resolved by namer
 *
 * It does not need to be a symbol
 */
sealed trait Resolvable extends Tree {
  type resolved
  def resolve(implicit C: Context): resolved
}

/**
 * Types and Effects
 *
 * TODO generalize to blocks that can take blocks
 */
sealed trait Type extends Tree with Resolvable {
  type resolved <: symbols.Type
  def resolve(implicit C: Context) = C.resolvedType(this)
}
sealed trait ValueType extends Type {
  type resolved <: symbols.ValueType
}

/**
 * Trees that represent inferred or synthesized types
 */
case class ValueTypeTree(tpe: symbols.ValueType) extends ValueType

/**
 * Types of first-class functions
 */
case class BoxedType(tpe: BlockType) extends ValueType

// Used for both binding and bound vars
case class TypeVar(id: IdRef) extends ValueType with Reference {
  type symbol = symbols.Symbol with symbols.ValueType
}
case class TypeApp(id: IdRef, params: List[ValueType]) extends ValueType with Reference {
  type symbol = symbols.DataType
}

sealed trait BlockType extends Type

// TODO we simplify it for now and don't allow type constructor application on effects
case class InterfaceType(id: IdRef) extends BlockType {
  type resolved = symbols.InterfaceType
}
case class FunctionType(params: List[ValueType], ret: ValueType) extends BlockType {
  type resolved = symbols.FunctionType
}

//case class Effect(id: IdRef, tparams: List[ValueType] = Nil) extends Tree with Resolvable {
//  // TODO we need to drop Effect <: Symbol and refactor this here
//  // TODO maybe we should use Type or something like this instead of Symbol as an upper bound
//  type resolved = symbols.Effect
//  def resolve(implicit C: Context) = {
//    val eff = C.symbolOf(id).asInstanceOf[symbols.Effect]
//    if (tparams.isEmpty) eff else symbols.EffectApp(eff, tparams.map(t => C.resolvedType(t)))
//  }
//}
