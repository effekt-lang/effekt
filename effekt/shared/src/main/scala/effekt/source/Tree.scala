package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol

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

enum Stmt extends Tree {
  case Return(d: Term)
}
export Stmt.*

enum Term extends Tree {

  // Variable / Value use (can now also stand for blocks)
  case Var(id: IdRef) extends Term, Reference

}
export Term.*


// Declarations
// ------------
case class Constructor(id: IdDef, tparams: List[Id], params: List[ValueParam]) extends Definition
case class Operation(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful) extends Definition

// Implementations

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
