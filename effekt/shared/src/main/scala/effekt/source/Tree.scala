package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol

/**
 * Data type representing source program trees.
 *
 * ----------[[ effekt.source.Tree ]]----------
 *
 *   ─ [[ Tree ]]
 *     │─ [[ NoSource ]]
 *     │─ [[ Comment ]]
 *     │─ [[ Id ]]
 *     │  │─ [[ IdDef ]]
 *     │  │─ [[ IdRef ]]
 *     │
 *     │─ [[ Named ]]
 *     │  │─ [[ Definition ]]
 *     │  │─ [[ Reference ]]
 *     │
 *     │─ [[ ModuleDecl ]]
 *     │─ [[ Include ]]
 *     │─ [[ Stmt ]]
 *     │  │─ [[ DefStmt ]]
 *     │  │─ [[ ExprStmt ]]
 *     │  │─ [[ Return ]]
 *     │  │─ [[ BlockStmt ]]
 *     │
 *     │─ [[ Term ]]
 *     │  │─ [[ Var ]]
 *     │  │─ [[ Assign ]]
 *     │  │─ [[ Literal ]]
 *     │  │─ [[ Hole ]]
 *     │  │─ [[ Box ]]
 *     │  │─ [[ Unbox ]]
 *     │  │─ [[ Select ]]
 *     │  │─ [[ Do ]]
 *     │  │─ [[ Call ]]
 *     │  │─ [[ MethodCall ]]
 *     │  │─ [[ If ]]
 *     │  │─ [[ While ]]
 *     │  │─ [[ Match ]]
 *     │  │─ [[ TryHandle ]]
 *     │  │─ [[ Region ]]
 *     │  │─ [[ BlockLiteral ]]
 *     │  │─ [[ New ]]
 *     │
 *     │─ [[ CallTarget ]]
 *     │  │─ [[ IdTarget ]]
 *     │  │─ [[ ExprTarget ]]
 *     │
 *     │─ [[ MatchClause ]]
 *     │─ [[ MatchPattern ]]
 *     │  │─ [[ AnyPattern ]]
 *     │  │─ [[ TagPattern ]]
 *     │  │─ [[ IgnorePattern ]]
 *     │  │─ [[ LiteralPattern ]]
 *     │
 *     │─ [[ Type ]]
 *     │  │─ [[ ValueType ]]
 *     │  │─ [[ BlockType ]]
 *     │
 *     │─ [[ Effectful ]]
 *     │─ [[ Effects ]]
 *     │─ [[ CaptureSet ]]
 *
 * --------------------------------------------------------------
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

case class Template[+T](strings: List[String], args: List[T])

/**
 * We distinguish between identifiers corresponding to
 * - binding sites (IdDef)
 * - use site (IdRef)
 * in the syntax. This way, we can simplify the traversal in Namer
 */
sealed trait Id extends Tree {
  def name: String
  def symbol(using C: Context): Symbol = ???
  def clone(using C: Context): Id
}
case class IdDef(name: String) extends Id {
  def clone(using C: Context): IdDef = {
    val copy = IdDef(name)
    C.positions.dupPos(this, copy)
    copy
  }
}
case class IdRef(path: List[String], name: String) extends Id {
  def clone(using C: Context): IdRef = {
    val copy = IdRef(path, name)
    C.positions.dupPos(this, copy)
    copy
  }
}

sealed trait Named extends Tree

// Something that later will be stored in the symbol table
sealed trait Definition extends Named {
  def id: IdDef
}

// Something that later can be looked up in the symbol table
sealed trait Reference extends Named {
  def id: IdRef
}

/**
 * The type of whole compilation units
 *
 * Only a subset of definitions (FunDef and EffDef) is allowed on the toplevel
 *
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 *
 */
case class ModuleDecl(path: String, includes: List[Include], defs: List[Def]) extends Tree
case class Include(path: String) extends Tree

/**
 * Parameters and arguments
 */
enum Param extends Definition {
  case ValueParam(id: IdDef, tpe: Option[ValueType])
  case BlockParam(id: IdDef, tpe: BlockType)
}
export Param.*


/**
 * Global and local definitions
 *
 * ----------[[ effekt.source.Def ]]----------
 *
 *   ─ [[ Def ]]
 *     │─ [[ FunDef ]]
 *     │─ [[ ValDef ]]
 *     │─ [[ RegDef ]]
 *     │─ [[ VarDef ]]
 *     │─ [[ DefDef ]]
 *     │─ [[ InterfaceDef ]]
 *     │─ [[ DataDef ]]
 *     │─ [[ RecordDef ]]
 *     │─ [[ TypeDef ]]
 *     │─ [[ EffectDef ]]
 *     │─ [[ ExternType ]]
 *     │─ [[ ExternDef ]]
 *     │─ [[ ExternResource ]]
 *     │─ [[ ExternInterface ]]
 *     │─ [[ ExternInclude ]]
 *
 * -------------------------------------------
 */
enum Def extends Definition {

  case FunDef(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[Effectful], body: Stmt)
  case ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt)
  case RegDef(id: IdDef, annot: Option[ValueType], region: IdRef, binding: Stmt)
  case VarDef(id: IdDef, annot: Option[ValueType], binding: Stmt)
  case DefDef(id: IdDef, annot: Option[BlockType], block: Term)

  case NamespaceDef(id: IdDef, definitions: List[Def])

  case InterfaceDef(id: IdDef, tparams: List[Id], ops: List[Operation], isEffect: Boolean = true)
  case DataDef(id: IdDef, tparams: List[Id], ctors: List[Constructor])
  case RecordDef(id: IdDef, tparams: List[Id], fields: List[ValueParam])

  /**
   * Type aliases like `type Matrix[T] = List[List[T]]`
   */
  case TypeDef(id: IdDef, tparams: List[Id], tpe: ValueType)

  /**
   * Effect aliases like `effect Set = { Get, Put }`
   */
  case EffectDef(id: IdDef, tparams: List[Id], effs: Effects)

  /**
   * Only valid on the toplevel!
   */
  case ExternType(id: IdDef, tparams: List[Id])

  case ExternDef(capture: CaptureSet, id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful, body: Template[Term]) extends Def

  case ExternResource(id: IdDef, tpe: BlockType)

  case ExternInterface(id: IdDef, tparams: List[Id])

  /**
   * Namer resolves the path and loads the contents in field [[contents]]
   *
   * @note Storing content and id as user-visible fields is a workaround for the limitation that Enum's cannot
   *   have case specific refinements.
   */
  case ExternInclude(path: String, var contents: Option[String] = None, val id: IdDef = IdDef(""))
}
object Def {
  type Extern = ExternType | ExternDef | ExternResource | ExternInterface | ExternInclude
  type Declaration = InterfaceDef | DataDef | RecordDef
  type Alias = TypeDef | EffectDef
  type Toplevel = FunDef | ValDef | DefDef | Alias | Extern | Declaration
  type Local = FunDef | ValDef | DefDef | Alias | VarDef
}
export Def.*




/**
 * ----------[[ effekt.source.Stmt ]]----------
 *
 *   ─ [[ Stmt ]]
 *     │─ [[ DefStmt ]]
 *     │─ [[ ExprStmt ]]
 *     │─ [[ Return ]]
 *     │─ [[ BlockStmt ]]
 *
 * --------------------------------------------
 *
 */
enum Stmt extends Tree {
  case DefStmt(d: Def, rest: Stmt)
  case ExprStmt(d: Term, rest: Stmt)
  case Return(d: Term)
  case BlockStmt(stmts: Stmt)
}
export Stmt.*


/**
 * In our source language, almost everything is an expression.
 * Effectful calls, if, while, ...
 *
 * ----------[[ effekt.source.Term ]]----------
 *
 *   ─ [[ Term ]]
 *     │─ [[ Var ]]
 *     │─ [[ Assign ]]
 *     │─ [[ Literal ]]
 *     │─ [[ Hole ]]
 *     │─ [[ Box ]]
 *     │─ [[ Unbox ]]
 *     │─ [[ Select ]]
 *     │─ [[ Do ]]
 *     │─ [[ Call ]]
 *     │─ [[ MethodCall ]]
 *     │─ [[ If ]]
 *     │─ [[ While ]]
 *     │─ [[ Match ]]
 *     │─ [[ TryHandle ]]
 *     │─ [[ Region ]]
 *     │─ [[ BlockLiteral ]]
 *     │─ [[ New ]]
 *
 * --------------------------------------------
 */
enum Term extends Tree {

  // Variable / Value use (can now also stand for blocks)
  case Var(id: IdRef) extends Term, Reference
  case Assign(id: IdRef, expr: Term) extends Term, Reference

  case Literal(value: Any, tpe: symbols.ValueType)
  case Hole(stmts: Stmt)

  // Boxing and unboxing to represent first-class values
  case Box(capt: Option[CaptureSet], block: Term)
  case Unbox(term: Term)

  /**
   * Models:
   * - field selection, i.e., `record.field` (receiver is an expression, result is an expression)
   * - future: nested capability / module selection, i.e., `mymod.nested.foo` (receiver is a block, result is a block)
   *
   * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
   */
  case Select(receiver: Term, id: IdRef) extends Term, Reference

  /**
   * A call to an effect operation, i.e., `do raise()`.
   *
   * The [[effect]] is the optionally annotated effect type (not possible in source ATM). In the future, this could
   * look like `do Exc.raise()`, or `do[Exc] raise()`, or do[Exc].raise(), or simply Exc.raise() where Exc is a type.
   */
  case Do(effect: Option[BlockType], id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[Term]) extends Term, Reference

  /**
   * A call to either an expression, i.e., `(fun() { ...})()`; or a named function, i.e., `foo()`
   */
  case Call(target: CallTarget, targs: List[ValueType], vargs: List[Term], bargs: List[Term])

  /**
   * Models:
   * - uniform function call, i.e., `list.map { ... }` (receiver is an expression, result is an expression)
   * - capability call, i.e., `exc.raise()` (receiver is a block, result is an expression)
   *
   * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
   */
  case MethodCall(receiver: Term, id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[Term]) extends Term, Reference

  // Control Flow
  case If(guards: List[MatchGuard], thn: Stmt, els: Stmt)
  case While(guards: List[MatchGuard], block: Stmt, default: Option[Stmt])
  case Match(scrutinee: Term, clauses: List[MatchClause], default: Option[Stmt])

  /**
   * Handling effects
   *
   * try {
   * <prog>
   * } with <capability> : <Effect> { ... }
   *
   * Each with-clause is modeled as an instance of type [[Handler]].
   */
  case TryHandle(prog: Stmt, handlers: List[Handler])
  case Region(id: IdDef, body: Stmt) extends Term, Definition

  /**
   * Lambdas / function literals (e.g., { x => x + 1 })
   */
  case BlockLiteral(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends Term
  case New(impl: Implementation)
}
export Term.*

// Smart Constructors for literals
// -------------------------------
def UnitLit(): Literal = ???
def IntLit(value: Long): Literal = ???
def BooleanLit(value: Boolean): Literal = ???
def DoubleLit(value: Double): Literal = ???
def StringLit(value: String): Literal = ???

type CallLike = Call | Do | Select | MethodCall


enum CallTarget extends Tree {

  // potentially overloaded
  case IdTarget(id: IdRef) extends CallTarget, Reference

  // not overloaded
  case ExprTarget(receiver: Term)
}
export CallTarget.*


// Declarations
// ------------
case class Constructor(id: IdDef, tparams: List[Id], params: List[ValueParam]) extends Definition
case class Operation(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful) extends Definition

// Implementations
// ---------------

/**
 * An implementation of a given interface
 *
 *     <Interface> {
 *       def <opClause> = ...
 *     }
 *
 * Called "template" or "class" in other languages.
 */
case class Implementation(interface: BlockType, clauses: List[OpClause]) extends Reference {
  def id = ???
}

/**
 * A handler is a pair of an optionally named capability and the handler [[Implementation]]
 */
case class Handler(capability: Option[BlockParam] = None, impl: Implementation) extends Reference {
  def effect = impl.interface
  def clauses = impl.clauses
  def id = impl.id
}

// `ret` is an optional user-provided type annotation for the return type
// currently the annotation is rejected by [[Typer]] -- after that phase, `ret` should always be `None`
case class OpClause(id: IdRef,  tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[Effectful], body: Stmt, resume: IdDef) extends Reference

// Pattern Matching
// ----------------

case class MatchClause(pattern: MatchPattern, guards: List[MatchGuard], body: Stmt) extends Tree

enum MatchGuard extends Tree {

  case BooleanGuard(condition: Term)

  /**
   * i.e. <EXPR> is <PATTERN>
   */
  case PatternGuard(scrutinee: Term, pattern: MatchPattern)
}
export MatchGuard.*

enum MatchPattern extends Tree {

  /**
   * Pattern matching anything:
   *
   *   case a => ...
   */
  case AnyPattern(id: IdDef) extends MatchPattern, Definition

}
export MatchPattern.*


sealed trait Type extends Tree

/**
 * Value Types
 */
enum ValueType extends Type {

  /**
   * Trees that represent inferred or synthesized types (not present in the source)
   */
  case ValueTypeTree(tpe: symbols.ValueType)

}
export ValueType.*

/**
 * Block Types
 */
enum BlockType extends Type {

  case BlockTypeTree(eff: symbols.BlockType)
}

export BlockType.*

// We have Effectful as a tree in order to apply code actions on it (see Server.inferEffectsAction)
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

/**
 * Represents an annotated set of effects. Before name resolution, we cannot know
 * the concrete nature of its elements (so it is generic [[BlockTypeRef]]).
 */
case class Effects(effs: List[BlockType]) extends Tree
object Effects {
  val Pure: Effects = ???
}

case class CaptureSet(captures: List[IdRef]) extends Tree


// MOVE TO NAMER
object Resolvable {

  // Value Types
  // -----------
  extension (t: ValueType) {
    def resolve(using C: Context): symbols.ValueType = ???
  }

  // BLock Types
  // -----------
  // we need to avoid widening, so here we define BlockType as a sum without a common parent
  // (see https://github.com/lampepfl/dotty/issues/16299)
  type BlockTypes = BlockTypeTree

  type Resolved[T <: BlockTypes] = T match {
    case BlockTypeTree => symbols.BlockType
  }

  extension [T <: BlockTypes] (t: T) {
    def resolve(using C: Context): Resolved[T] = ???
  }

  // Capture Sets
  // ------------
  extension (capt: source.CaptureSet) {
    def resolve(using C: Context): symbols.CaptureSet = ???
  }
}
export Resolvable.resolve
