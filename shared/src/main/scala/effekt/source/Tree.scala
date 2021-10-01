package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol

/**
 * Data type representing source program trees.
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
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 *
 */
case class ModuleDecl(path: String, imports: List[Import], defs: List[Def]) extends Tree
case class Import(path: String) extends Tree

/**
 * Parameters and arguments
 */
case class ValueParam(id: IdDef, tpe: ValueType) extends Definition { type symbol = symbols.ValueParam }

case class BlockParam(id: IdDef, tpe: BlockType) extends Definition { type symbol = symbols.BlockParam }

sealed trait BlockArg extends Tree
case class FunctionArg(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends BlockArg

// TODO we might want to add anonymous instances here
case class InterfaceArg(id: IdRef) extends BlockArg with Reference {
  type symbol = symbols.BlockParam
}
case class UnboxArg(body: Term) extends BlockArg

/**
 * Global and local definitions
 */
sealed trait Def extends Definition {
  def id: IdDef
}
case class FunDef(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[ValueType], body: Stmt) extends Def {
  type symbol = symbols.UserFunction
}

case class InterfaceDef(id: IdDef, tparams: List[Id], ops: List[Operation]) extends Def {
  type symbol = symbols.Interface
}
case class Operation(id: IdDef, tparams: List[Id], vparams: List[ValueParam], ret: ValueType) extends Definition {
  type symbol = symbols.Operation
}
case class DataDef(id: IdDef, tparams: List[Id], ctors: List[Constructor]) extends Def {
  type symbol = symbols.DataType
}
case class Constructor(id: IdDef, vparams: List[ValueParam]) extends Definition {
  type symbol = symbols.Record
}

// only valid on the toplevel!
case class ExternType(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinType
}
//case class ExternEffect(id: IdDef, tparams: List[Id]) extends Def {
//  type symbol = symbols.BuiltinEffect
//}
case class ExternFun(pure: Boolean, id: IdDef, tparams: List[Id], vparams: List[ValueParam], ret: ValueType, body: String) extends Def {
  type symbol = symbols.BuiltinFunction
}
case class ExternInclude(path: String) extends Def {
  def id = IdDef("includes don't have names")
  // Namer resolves the path and loads the contents
  var contents: String = ""
}

sealed trait Stmt extends Tree
case class MutualStmt(defs: List[Def], rest: Stmt) extends Stmt
case class ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt, rest: Stmt) extends Stmt with Definition {
  type symbol = symbols.ValBinder
}
case class VarDef(id: IdDef, annot: Option[ValueType], region: Option[IdRef], binding: Stmt, rest: Stmt) extends Stmt with Definition {
  type symbol = symbols.VarBinder
}
case class BlockStmt(stmts: Stmt) extends Stmt
case class ExprStmt(d: Term, rest: Stmt) extends Stmt
case class Return(d: Term) extends Stmt

/**
 * In our source language, almost everything is an expression.
 * Effectful calls, if, while, ...
 */
sealed trait Term extends Tree

// Variable / Value use (can now also stand for blocks)
case class Var(id: IdRef) extends Term with Reference {
  type symbol = symbols.TermSymbol
}
case class Assign(id: IdRef, expr: Term) extends Term with Reference {
  type symbol = symbols.VarBinder
}

sealed trait Literal[T] extends Term {
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
case class Select(receiver: Term, selector: IdRef) extends Term

// maybe replace `fun: Id` here with BlockVar
// TODO should we have one Call-node and a selector tree, or multiple different call nodes?
case class Call(receiver: Term, targs: List[ValueType], vargs: List[Term], bargs: List[BlockArg]) extends Term

case class Box(capt: Option[CaptureSet], block: BlockArg) extends Term

case class Unbox(term: Term) extends Term

case class If(cond: Term, thn: Stmt, els: Stmt) extends Term
case class While(cond: Term, block: Stmt) extends Term

case class TryHandle(prog: Stmt, handlers: List[Handler]) extends Term

case class Region(id: IdDef, body: Stmt) extends Term with Definition {
  type symbol = symbols.BlockParam
}

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
case class Handler(capability: BlockParam, clauses: List[OpClause]) extends Tree
case class OpClause(id: IdRef, tparams: List[Id], vparams: List[ValueParam], body: Stmt, resume: IdDef) extends Reference {
  type symbol = symbols.Operation
}

case class Hole(stmts: Stmt) extends Term

case class Match(scrutinee: Term, clauses: List[MatchClause]) extends Term
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
case class BoxedType(tpe: BlockType, capt: CaptureSet) extends ValueType

// Used for both binding and bound vars
case class TypeVar(id: IdRef) extends ValueType with Reference {
  type symbol = symbols.Symbol with symbols.ValueType
}
case class ValueTypeApp(id: IdRef, params: List[ValueType]) extends ValueType with Reference {
  type symbol = symbols.DataType
}

case class CaptureSet(captures: List[IdRef]) extends Resolvable {
  type resolved = symbols.CaptureSet
  def resolve(implicit C: Context): resolved = C.resolvedCapture(this)
}

sealed trait BlockType extends Type

// TODO we simplify it for now and don't allow type constructor application on effects
case class InterfaceType(id: IdRef) extends BlockType {
  type resolved = symbols.InterfaceType
}

case class BlockTypeApp(id: IdRef, params: List[ValueType]) extends BlockType with Reference {
  type symbol = symbols.Symbol with symbols.InterfaceType
}

// TODO generalize to blocks that can take blocks
case class FunctionType(tparams: List[Id], vparams: List[ValueType], bparams: List[(Option[IdDef], BlockType)], ret: ValueType) extends BlockType {
  type resolved = symbols.FunctionType
}

