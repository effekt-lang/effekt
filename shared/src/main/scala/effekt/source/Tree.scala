package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol

/**
 * We extend product to allow reflective copying by Kiama.
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

// Something that later will be stored in the symbol table
sealed trait Definition extends Tree {
  def id: IdDef
  type symbol <: Symbol

  def symbol(implicit C: Context): symbol = C.symbolOf(this)
}

// Something that later can be looked up in the symbol table
sealed trait Reference extends Tree {
  def id: IdRef
  type symbol <: Symbol

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
case class BlockArg(params: ValueParams, body: Stmt) extends ArgSection

/**
 * Global (and later, local) definitions
 */
sealed trait Def extends Definition {
  def id: IdDef
}
case class FunDef(id: IdDef, tparams: List[Id], params: List[ParamSection], ret: Option[Effectful], body: Stmt) extends Def {
  type symbol = symbols.UserFunction
}
case class ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.ValBinder
}
case class VarDef(id: IdDef, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.VarBinder
}
case class EffDef(id: IdDef, ops: List[Operation]) extends Def {
  type symbol = symbols.UserEffect
}
case class Operation(id: IdDef, tparams: List[Id], params: List[ValueParams], ret: Effectful) extends Definition {
  type symbol = symbols.EffectOp
}
case class DataDef(id: IdDef, tparams: List[Id], ctors: List[Constructor]) extends Def {
  type symbol = symbols.DataType
}
case class Constructor(id: IdDef, params: ValueParams) extends Definition {
  type symbol = symbols.Record
}
case class RecordDef(id: IdDef, tparams: List[Id], fields: ValueParams) extends Def {
  type symbol = symbols.Record
}

/**
 * Type aliases like `type Matrix[T] = List[List[T]]`
 */
case class TypeDef(id: IdDef, tparams: List[Id], tpe: ValueType) extends Def {
  type symbol = symbols.TypeAlias
}

/**
 * Effect aliases like `effect Set = { Get, Put }`
 */
case class EffectDef(id: IdDef, effs: Effects) extends Def {
  type symbol = symbols.EffectAlias
}

// only valid on the toplevel!
case class ExternType(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinType
}
case class ExternEffect(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinEffect
}
case class ExternFun(pure: Boolean, id: IdDef, tparams: List[Id], params: List[ParamSection], ret: Effectful, body: String) extends Def {
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
 * Effectful calls, if, while,
 */
sealed trait Expr extends Tree

// Variable / Value use
case class Var(id: IdRef) extends Expr with Reference {
  type symbol = symbols.ValueSymbol with symbols.TermSymbol
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

// maybe replace `fun: Id` here with BlockVar
case class Call(id: IdRef, targs: List[ValueType], args: List[ArgSection]) extends Expr with Reference {
  type symbol = symbols.BlockSymbol
}

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Expr
case class While(cond: Expr, block: Stmt) extends Expr

case class TryHandle(prog: Stmt, handlers: List[Handler]) extends Expr
case class Handler(id: IdRef, clauses: List[OpClause]) extends Reference {
  type symbol = symbols.UserEffect
}
case class OpClause(id: IdRef, params: List[ValueParams], body: Stmt, resume: IdDef) extends Reference {
  type symbol = symbols.EffectOp
}

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
 * Types and Effects
 *
 * TODO generalize to blocks that can take blocks
 */
sealed trait Type extends Tree {
  type symbol <: symbols.Type
}
sealed trait ValueType extends Type {
  type symbol <: symbols.ValueType
}

// Used for both binding and bound vars
case class TypeVar(id: IdRef) extends ValueType with Reference {
  type symbol = symbols.Symbol with symbols.ValueType
}
case class TypeApp(id: IdRef, params: List[ValueType]) extends ValueType with Reference {
  type symbol = symbols.DataType
}
case class BlockType(params: List[ValueType], ret: Effectful) extends Type {
  type symbol = symbols.BlockType
}

case class Effect(id: IdRef) extends Tree with Reference {
  type symbol = symbols.UserEffect
}
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

case class Effects(effs: List[Effect]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: Effect*): Effects = Effects(effs.toSet)
  def apply(effs: Set[Effect]): Effects = Effects(effs.toList)
}
