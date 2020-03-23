package effekt
package source

import effekt.context.SymbolsDB
import effekt.symbols.Symbol

/**
 * We extend product to allow reflective copying by Kiama.
 */
sealed trait Tree extends Product

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
  def symbol(implicit db: SymbolsDB): Symbol = db.symbolOf(this)
}
case class IdDef(name: String) extends Id
case class IdRef(name: String) extends Id

// Something that later will be stored in the symbol table
sealed trait Definition extends Tree {
  def id: Id
  type symbol <: Symbol

  def symbol(implicit db: SymbolsDB): symbol = db.symbolOf(this)
}

// Something that later can be looked up in the symbol table
sealed trait Reference extends Tree {
  def id: Id
  type symbol <: Symbol

  def definition(implicit db: SymbolsDB): symbol = db.symbolOf(this)
}

/**
 * The type of whole compilation units
 *
 * Only a subset of definitions (FunDef and EffDef) is allowed on the toplevel
 */
case class ModuleDecl(path: String, imports: List[Import], defs: List[Def]) extends Tree
case class Import(path: String) extends Tree

/**
 * Parameters and arguments
 */
sealed trait ParamSection extends Tree
case class ValueParams(params: List[ValueParam]) extends ParamSection
case class ValueParam(id: Id, tpe: Option[ValueType]) extends Definition { type symbol = symbols.ValueParam }
case class BlockParam(id: Id, tpe: BlockType) extends ParamSection with Definition { type symbol = symbols.BlockParam }

sealed trait ArgSection extends Tree
case class ValueArgs(args: List[Expr]) extends ArgSection
case class BlockArg(params: ValueParams, body: Stmt) extends ArgSection


/**
 * Global (and later, local) definitions
 */
sealed trait Def extends Definition
case class FunDef(id: Id, tparams: List[Id], params: List[ParamSection], ret: Option[Effectful], body: Stmt) extends Def {
  type symbol = symbols.UserFunction
}
case class EffDef(id: Id, tparams: List[Id], params: List[ValueParams], ret: ValueType) extends Def {
  type symbol = symbols.UserEffect
}
case class ValDef(id: Id, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.ValBinder
}
case class VarDef(id: Id, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.VarBinder
}

case class DataDef(id: Id, tparams: List[Id], ctors: List[Constructor]) extends Def {
  type symbol = symbols.DataType
}
case class Constructor(id: Id, params: List[ValueParams]) extends Definition {
  type symbol = symbols.Constructor
}

// only valid on the toplevel!
case class ExternType(id: Id, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinType
}
case class ExternEffect(id: Id, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinEffect
}
case class ExternFun(pure: Boolean, id: Id, tparams: List[Id], params: List[ParamSection], ret: Effectful, body: String) extends Def {
  type symbol = symbols.BuiltinFunction
}
case class ExternInclude(path: String) extends Def {
  def id = IdRef("includes don't have names")
  // Namer resolves the path and loads the contents
  var contents: String = ""
}

sealed trait Stmt extends Tree
case class DefStmt(d: Def, rest: Stmt) extends Stmt
case class ExprStmt(d: Expr, rest: Stmt) extends Stmt
case class Return(d: Expr) extends Stmt


/**
 * In our source language, almost everything is an expression.
 * Effectful calls, if, while,
 */
sealed trait Expr extends Tree

// Variable / Value use
case class Var(id: Id) extends Expr with Reference {
  type symbol = symbols.ValueSymbol with symbols.TermSymbol
}
case class Assign(id: Id, expr: Expr) extends Expr with Reference {
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
case class Call(id: Id, targs: List[ValueType], args: List[ArgSection]) extends Expr with Reference

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Expr
case class While(cond: Expr, block: Stmt) extends Expr

case class TryHandle(prog: Stmt, clauses: List[OpClause]) extends Expr
case class OpClause(id: Id, params: List[ValueParams], body: Stmt, resume: IdDef = IdDef("resume")) extends Reference {
  type symbol = symbols.EffectOp
}

case class MatchExpr(matchee: Expr, clauses: List[Clause]) extends Expr
case class Clause(id: Id, params: List[ValueParams], body: Stmt) extends Reference {
  type symbol = symbols.Constructor
}

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
case class TypeVar(id: Id) extends ValueType with Reference {
  type symbol = symbols.Symbol with symbols.ValueType
}
case class TypeApp(id: Id, params: List[ValueType]) extends ValueType with Reference {
  type symbol = symbols.DataType
}
case class BlockType(params: List[ValueType], ret: Effectful) extends Type {
  type symbol = symbols.BlockType
}

case class Effect(id: Id) extends Tree with Reference {
  type symbol = symbols.Effect
}
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

case class Effects(effs: List[Effect]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: Effect*): Effects = Effects(effs.toSet)
  def apply(effs: Set[Effect]): Effects = Effects(effs.toList)
}