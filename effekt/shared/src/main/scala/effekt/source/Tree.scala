package effekt
package source

import effekt.context.Context
import effekt.symbols.Symbol
import kiama.util.{Source, StringSource}
import scala.annotation.tailrec

/**
 * Data type representing source program trees.
 *
 * ----------[[ effekt.source.Tree ]]----------
 *
 *   ─ [[ Tree ]]
 *     │─ [[ NoSource ]]
 *     │─ [[ Comment ]]
 *     │─ [[ FeatureFlag ]]
 *     │  │─ [[ NamedFeatureFlag ]]
 *     │  │─ [[ Default ]]
 *     │
 *     │─ [[ ExternBody ]]
 *     │  │─ [[ StringExternBody ]]
 *     │  │─ [[ EffektExternBody ]]
 *     │  │─ [[ Unsupported ]]
 *     │
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
 *     │─ [[ MatchGuard ]]
 *     │  │─ [[ BooleanGuard ]]
 *     │  │─ [[ PatternGuard ]]
 *     │
 *     │─ [[ MatchPattern ]]
 *     │  │─ [[ AnyPattern ]]
 *     │  │─ [[ TagPattern ]]
 *     │  │─ [[ IgnorePattern ]]
 *     │  │─ [[ LiteralPattern ]]
 *     │  │─ [[ MultiPattern ]]
 *     │
 *     │─ [[ Type ]]
 *     │  │─ [[ ValueTypeTree ]]
 *     │  │─ [[ BlockTypeTree ]]
 *     │  │─ [[ TypeRef ]]
 *     │  │─ [[ BoxedType ]]
 *     │  │─ [[ FunctionType ]]
 *     │  │─ [[ Effectful ]]
 *     │
 *     │─ [[ Effects ]]
 *     │─ [[ CaptureSet ]]
 *
 * --------------------------------------------------------------
 *
 * We extend product to allow reflective access by Kiama.
 */
sealed trait Tree extends Product {
  val span: Span = Span.missing

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

type Doc = Option[String]

/**
 * The origin of the span
 *
 * Possible values:
 * - Real: the node has direct representation in the source code and this is its span
 * - Synthesized: the node does not have a direct representation in the source code, but this is the span
 *                we would like to point at for any error messages that need to refer to this node.
 *                If the user can express this node explicitly, it should also point to the place where the
 *                user could write it out explicitly.
 * - Missing: this node doesn't or can't have a span yet, but we would like to add one later
 */
enum Origin {
  case Real, Synthesized, Missing
}

case class Span(source: kiama.util.Source, from: Int, to: Int, origin: Origin = Origin.Real) {
  /**
   * creates a fake empty Span immediately after this one
   * Example:
   * [ - Span - ]
   *             [] <- emptyAfter
   */
  def emptyAfter: Span = Span(source, to + 1,  to + 1, origin = Origin.Synthesized)

  /**
   * creates a fake copy of this span
   */
  def synthesized: Span = Span(source, from, to, origin = Origin.Synthesized)

  def range: kiama.util.Range = kiama.util.Range(source.offsetToPosition(from), source.offsetToPosition(to))
}

object Span {
  def missing(source: Source) = Span(source, 0, 0, origin = Origin.Missing)
  def missing = Span(StringSource(""), 0, 0, origin = Origin.Missing)
}

/**
 * Used to mark externs for different backends
 */
enum FeatureFlag extends Tree {
  case NamedFeatureFlag(id: String, override val span: Span)
  case Default(override val span: Span)

  def matches(name: String, matchDefault: Boolean = true): Boolean = this match {
    case NamedFeatureFlag(n, span) if n == name => true
    case Default(span) => matchDefault
    case _ => false
  }
  def isDefault: Boolean = this match {
    case Default(_) => true
    case _          => false
  }

  def matches(names: List[String]): Boolean = this match {
    case NamedFeatureFlag(n, span) if names.contains(n) => true
    case Default(span) => true
    case _ => false
  }

  override def toString: String = this match {
    case FeatureFlag.NamedFeatureFlag(id, span) => id
    case FeatureFlag.Default(span) => "else"
  }
}
object FeatureFlag {
  extension (self: List[ExternBody]) {
    @tailrec
    def supportedByFeatureFlags(names: List[String]): Boolean = names match {
      case Nil => false
      case name :: other =>
        self.collectFirst {
          case ExternBody.StringExternBody(flag, a, span) if flag.matches(name) => ()
          case ExternBody.EffektExternBody(flag, a, span) if flag.matches(name) => ()
        }.isDefined || (self.supportedByFeatureFlags(other))
    }
  }
}

sealed trait ExternBody extends Tree {
  def featureFlag: FeatureFlag
}
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, template: Template[source.Term], override val span: Span) extends ExternBody
  case class EffektExternBody(featureFlag: FeatureFlag, body: source.Stmt, override val span: Span) extends ExternBody
  case class Unsupported(message: util.messages.EffektError) extends ExternBody {
    override def featureFlag: FeatureFlag = FeatureFlag.Default(Span.missing)
  }
}


/**
 * We distinguish between identifiers corresponding to
 * - binding sites (IdDef)
 * - use site (IdRef)
 * in the syntax. This way, we can simplify the traversal in Namer
 */
sealed trait Id extends Tree {
  def name: String
  def symbol(using C: Context): Symbol = C.symbolOf(this)
  def clone(using C: Context): Id
}
case class IdDef(name: String, override val span: Span) extends Id {
  def clone(using C: Context): IdDef = {
    val copy = IdDef(name, span)
    C.positions.dupPos(this, copy)
    copy
  }
}
case class IdRef(path: List[String], name: String, override val span: Span) extends Id {
  def clone(using C: Context): IdRef = {
    val copy = IdRef(path, name, span)
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
case class ModuleDecl(path: String, includes: List[Include], defs: List[Def], doc: Doc, override val span: Span) extends Tree
case class Include(path: String, override val span: Span) extends Tree

/**
 * Parameters and arguments
 */
enum Param extends Definition {
  case ValueParam(id: IdDef, tpe: Option[ValueType], override val span: Span)
  case BlockParam(id: IdDef, tpe: Option[BlockType], override val span: Span)
}
export Param.*

/**
 * A `List` (usually of `Tree`s) with a `span` that spans all elements of the list
 */
case class Many[T](unspan: List[T], span: Span) {
  def map[B](f: T => B): Many[B] =
    Many(unspan.map(f), span)

  def unzip [A1, A2](implicit asPair: T => (A1, A2)): (Many[A1], Many[A2]) = {
    val (list1: List[A1], list2: List[A2]) = unspan.unzip(asPair)
    (Many(list1, span), Many(list2, span))
  }

  def collect[B](pf: PartialFunction[T, B]): Many[B] =
    Many(unspan.collect(pf), span)

  def zip[B](that: IterableOnce[B]): Many[(T, B)] = Many(unspan.zip(that), span)

  def flatMap[B](f: T => IterableOnce[B]): Many[B] =
    Many(unspan.flatMap(f), span)

  export unspan.{foreach, toSet, isEmpty, nonEmpty, mkString, size, length, init, last}
}

object Many {
   def empty[T](span: Span) = Many[T](Nil, span)
}

/**
 * An `Option` with a `span`
 *
 * If the `Option` is `Some`, the `span` is the span of the value.
 * If the `Option` is `None`, the `span` is an empty span pointing to where the optional value would be expected.
 */
case class Maybe[T](unspan: Option[T], span: Span) {
  def map[B](f: T => B): Maybe[B] = Maybe(unspan.map(f), span)
  def flatMap[B](f: T => Maybe[B]): Maybe[B] = unspan match {
    case None => Maybe(None, span)
    case Some(x) => f(x)
  }

  def orElse[B >: T](alternative: => Maybe[B]): Maybe[B] =
   unspan match {
      case Some(x) => Maybe(unspan, span)
      case None => alternative
    }

  export unspan.{foreach, get, getOrElse, isEmpty}
}

object Maybe {
  def Some[T](value: T, span: Span): Maybe[T] = Maybe(scala.Some(value), span)

  def None[T](span: Span): Maybe[T] = Maybe(scala.None, span)
}

object SpannedOps {
  extension [T](self: Option[T]) {
    inline def spanned(span: Span): Maybe[T] = Maybe(self, span)
  }

  extension [T](self: List[T]) {
    inline def spanned(span: Span): Many[T] = Many(self, span)
  }
}
export SpannedOps._

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
 *     │─ [[ NamespaceDef ]]
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

  case FunDef(id: IdDef, tparams: Many[Id], vparams: Many[ValueParam], bparams: Many[BlockParam], ret: Maybe[Effectful], body: Stmt, doc: Doc, override val span: Span)
  case ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt, doc: Doc, override val span: Span)
  case RegDef(id: IdDef, annot: Option[ValueType], region: IdRef, binding: Stmt, doc: Doc, override val span: Span)
  case VarDef(id: IdDef, annot: Option[ValueType], binding: Stmt, doc: Doc, override val span: Span)
  case DefDef(id: IdDef, annot: Option[BlockType], block: Term, doc: Doc, override val span: Span)

  case NamespaceDef(id: IdDef, definitions: List[Def], doc: Doc, override val span: Span)

  case InterfaceDef(id: IdDef, tparams: Many[Id], ops: List[Operation], doc: Doc, override val span: Span)
  case DataDef(id: IdDef, tparams: Many[Id], ctors: List[Constructor], doc: Doc, override val span: Span)
  case RecordDef(id: IdDef, tparams: Many[Id], fields: Many[ValueParam], doc: Doc, override val span: Span)

  /**
   * Type aliases like `type Matrix[T] = List[List[T]]`
   */
  case TypeDef(id: IdDef, tparams: List[Id], tpe: ValueType, doc: Doc, override val span: Span)

  /**
   * Effect aliases like `effect Set = { Get, Put }`
   */
  case EffectDef(id: IdDef, tparams: List[Id], effs: Effects, doc: Doc, override val span: Span)

  /**
   * Only valid on the toplevel!
   */
  case ExternType(id: IdDef, tparams: Many[Id], doc: Doc, override val span: Span)

  case ExternDef(capture: CaptureSet, id: IdDef,
                 tparams: Many[Id], vparams: Many[ValueParam], bparams: Many[BlockParam], ret: Effectful,
                 bodies: List[ExternBody], doc: Doc, override val span: Span) extends Def

  case ExternResource(id: IdDef, tpe: BlockType, doc: Doc, override val span: Span)

  case ExternInterface(id: IdDef, tparams: List[Id], doc: Doc, override val span: Span)

  /**
   * Namer resolves the path and loads the contents in field [[contents]]
   *
   * @note Storing content and id as user-visible fields is a workaround for the limitation that Enum's cannot
   *   have case specific refinements.
   */
  case ExternInclude(featureFlag: FeatureFlag, path: String, var contents: Option[String] = None, val id: IdDef, doc: Doc, override val span: Span)

  def doc: Doc
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
  case DefStmt(d: Def, rest: Stmt, override val span: Span)
  case ExprStmt(d: Term, rest: Stmt, override val span: Span)
  case Return(d: Term, override val span: Span)
  case BlockStmt(stmts: Stmt, override val span: Span)
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
  case Var(id: IdRef, override val span: Span) extends Term, Reference
  case Assign(id: IdRef, expr: Term, override val span: Span) extends Term, Reference

  case Literal(value: Any, tpe: symbols.ValueType, override val span: Span)
  case Hole(id: IdDef, stmts: Stmt, override val span: Span)

  // Boxing and unboxing to represent first-class values
  case Box(capt: Maybe[CaptureSet], block: Term, override val span: Span)
  case Unbox(term: Term, override val span: Span)

  /**
   * Models:
   * - field selection, i.e., `record.field` (receiver is an expression, result is an expression)
   * - future: nested capability / module selection, i.e., `mymod.nested.foo` (receiver is a block, result is a block)
   *
   * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
   */
  case Select(receiver: Term, id: IdRef, override val span: Span) extends Term, Reference

  /**
   * A call to an effect operation, i.e., `do raise()`.
   *
   * The [[effect]] is the optionally annotated effect type (not possible in source ATM). In the future, this could
   * look like `do Exc.raise()`, or `do[Exc] raise()`, or do[Exc].raise(), or simply Exc.raise() where Exc is a type.
   */
  case Do(effect: Option[TypeRef], id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[Term], override val span: Span) extends Term, Reference

  /**
   * A call to either an expression, i.e., `(fun() { ...})()`; or a named function, i.e., `foo()`
   */
  case Call(target: CallTarget, targs: List[ValueType], vargs: List[Term], bargs: List[Term], override val span: Span)

  /**
   * Models:
   * - uniform function call, i.e., `list.map { ... }` (receiver is an expression, result is an expression)
   * - capability call, i.e., `exc.raise()` (receiver is a block, result is an expression)
   *
   * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
   */
  case MethodCall(receiver: Term, id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[Term], override val span: Span) extends Term, Reference

  // Control Flow
  case If(guards: List[MatchGuard], thn: Stmt, els: Stmt, override val span: Span)
  case While(guards: List[MatchGuard], block: Stmt, default: Option[Stmt], override val span: Span)
  case Match(scrutinees: List[Term], clauses: List[MatchClause], default: Option[Stmt], override val span: Span)

  /**
   * Handling effects
   *
   * try {
   * <prog>
   * } with <capability> : <Effect> { ... }
   *
   * Each with-clause is modeled as an instance of type [[Handler]].
   */
  case TryHandle(prog: Stmt, handlers: List[Handler], override val span: Span)
  case Region(id: IdDef, body: Stmt, override val span: Span) extends Term, Definition

  /**
   * Lambdas / function literals (e.g., { x => x + 1 })
   */
  case BlockLiteral(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt, override val span: Span) extends Term
  case New(impl: Implementation, override val span: Span)
}
export Term.*

// Smart Constructors for literals
// -------------------------------
def UnitLit(span: Span): Literal = Literal((), symbols.builtins.TUnit, span)
def IntLit(value: Long, span: Span): Literal = Literal(value, symbols.builtins.TInt, span)
def BooleanLit(value: Boolean, span: Span): Literal = Literal(value, symbols.builtins.TBoolean, span)
def DoubleLit(value: Double, span: Span): Literal = Literal(value, symbols.builtins.TDouble, span)
def StringLit(value: String, span: Span): Literal = Literal(value, symbols.builtins.TString, span)
def CharLit(value: Int, span: Span): Literal = Literal(value, symbols.builtins.TChar, span)

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
case class Constructor(id: IdDef, tparams: Many[Id], params: Many[ValueParam], doc: Doc, override val span: Span) extends Definition

case class Operation(id: IdDef, tparams: Many[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful, doc: Doc, override val span: Span) extends Definition

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
case class Implementation(interface: TypeRef, clauses: List[OpClause], override val span: Span) extends Reference {
  def id = interface.id
}

/**
 * A handler is a pair of an optionally named capability and the handler [[Implementation]]
 */
case class Handler(capability: Option[BlockParam] = None, impl: Implementation, override val span: Span) extends Reference {
  def effect = impl.interface
  def clauses = impl.clauses
  def id = impl.id
}

// `ret` is an optional user-provided type annotation for the return type
// currently the annotation is rejected by [[Typer]] -- after that phase, `ret` should always be `None`
case class OpClause(id: IdRef,  tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[Effectful], body: Stmt, resume: IdDef) extends Reference

// Pattern Matching
// ----------------

case class MatchClause(pattern: MatchPattern, guards: List[MatchGuard], body: Stmt, override val span: Span) extends Tree

enum MatchGuard extends Tree {

  case BooleanGuard(condition: Term, override val span: Span)

  /**
   * i.e. <EXPR> is <PATTERN>
   */
  case PatternGuard(scrutinee: Term, pattern: MatchPattern, override val span: Span)
}
export MatchGuard.*

enum MatchPattern extends Tree {

  /**
   * Pattern matching anything:
   *
   *   case a => ...
   */
  case AnyPattern(id: IdDef, override val span: Span) extends MatchPattern, Definition

  /**
   * Pattern matching on a constructor
   *
   *   case Cons(a, as) => ...
   */
  case TagPattern(id: IdRef, patterns: List[MatchPattern], override val span: Span) extends MatchPattern, Reference

  /**
   * A wildcard pattern ignoring the matched value
   *
   *   case _ => ...
   */
  case IgnorePattern(override val span: Span)

  /**
   * A pattern that matches a single literal value
   */
  case LiteralPattern(l: Literal, override val span: Span)

  /**
   * A pattern for multiple values
   *
   *   case a, b => ...
   *
   * Currently should *only* occur in lambda-cases during Parsing
   */
  case MultiPattern(patterns: List[MatchPattern], override val span: Span) extends MatchPattern
}
export MatchPattern.*


/**
 * Types and Effects
 *
 * TODO generalize to blocks that can take blocks
 *
 * ----------[[ effekt.source.Type ]]----------
 *
 *   ─ [[ Type ]]
 *     │─ [[ ValueTypeTree ]]
 *     │─ [[ BlockTypeTree ]]
 *     │─ [[ TypeRef ]]
 *     │─ [[ BoxedType ]]
 *     │─ [[ FunctionType ]]
 *     │─ [[ Effectful ]]
 *
 * --------------------------------------------
 */
sealed trait Type extends Tree

/**
 * Trees that represent inferred or synthesized *value* types (not present in the source = not parsed)
 */
case class ValueTypeTree(tpe: symbols.ValueType, override val span: Span) extends Type

/**
 * Trees that represent inferred or synthesized *block* types (not present in the source = not parsed)
 */
case class BlockTypeTree(eff: symbols.BlockType, override val span: Span) extends Type

/*
 * Reference to a type, potentially with bound occurences in `args`
 */
case class TypeRef(id: IdRef, args: Many[ValueType], override val span: Span) extends Type, Reference

/**
 * Types of first-class computations
 */
case class BoxedType(tpe: BlockType, capt: CaptureSet, override val span: Span) extends Type

/**
 * Types of (second-class) functions
 */
case class FunctionType(tparams: Many[Id], vparams: Many[ValueType], bparams: Many[(Maybe[IdDef], BlockType)], result: ValueType, effects: Effects, override val span: Span) extends Type


/**
 * Type-and-effect annotations
 */
case class Effectful(tpe: ValueType, eff: Effects, override val span: Span) extends Type

// These are just type aliases for documentation purposes.
type BlockType = Type
type ValueType = Type

/**
 * Represents an annotated set of effects. Before name resolution, we cannot know
 * the concrete nature of its elements (so it is generic [[TypeRef]]).
 */
case class Effects(effs: List[TypeRef], override val span: Span) extends Tree
object Effects {
  def Pure(span: Span): Effects = Effects(List(), span)
}

case class CaptureSet(captures: List[IdRef], override val span: Span) extends Tree


object Named {

  type Params = ValueParam | BlockParam
  type Externs = ExternDef | ExternResource | ExternInterface | ExternType
  type Defs = FunDef | ValDef | VarDef | DefDef | RegDef | InterfaceDef | DataDef | RecordDef | TypeDef | EffectDef
  type Definitions =  Externs | Defs | Params | Operation | Constructor | Region | AnyPattern

  type Vars = Var | Assign
  type Calls = Do | Select | MethodCall | IdTarget
  type References = Vars | Calls | TagPattern | Handler | OpClause | Implementation

  type ResolvedDefinitions[T <: Definitions] = T match {
    // Defs
    case FunDef       => symbols.UserFunction
    case ValDef       => symbols.Binder.ValBinder // export Binder.* doesn't seem to work here (maybe because the packages are cyclic?)
    case VarDef       => symbols.Binder.VarBinder
    case RegDef       => symbols.Binder.RegBinder
    case DefDef       => symbols.Binder.DefBinder
    case InterfaceDef => symbols.BlockTypeConstructor.Interface
    case DataDef      => symbols.TypeConstructor.DataType
    case RecordDef    => symbols.TypeConstructor.Record
    case TypeDef      => symbols.TypeAlias
    case EffectDef    => symbols.EffectAlias

    // Params
    case ValueParam => symbols.ValueParam
    case BlockParam => symbols.TrackedParam.BlockParam

    // Externs
    case ExternDef       => symbols.ExternFunction
    case ExternResource  => symbols.TrackedParam.BlockParam
    case ExternInterface => symbols.BlockTypeConstructor.ExternInterface
    case ExternType      => symbols.TypeConstructor.ExternType

    // Others
    case Operation   => symbols.Operation
    case Constructor => symbols.Constructor
    case Region      => symbols.TrackedParam
    case AnyPattern  => symbols.ValueParam
    case Hole        => symbols.Hole
  }

  type ResolvedReferences[T <: References] = T match {
    case TypeRef => symbols.TypeConstructor | symbols.BlockTypeConstructor

    // Vars
    case Var    => symbols.TermSymbol
    case Assign => symbols.RefBinder

    // CallLike
    case Do         => symbols.Operation
    case Select     => symbols.Field
    case MethodCall => symbols.Operation | symbols.CallTarget | symbols.BlockParam
    case IdTarget   => symbols.TermSymbol

    // Others
    case Handler => symbols.BlockTypeConstructor.Interface
    case OpClause => symbols.Operation
    case Implementation => symbols.BlockTypeConstructor.Interface
    case TagPattern => symbols.Constructor
  }

  extension [T <: Definitions](t: T & Definition) {
    def symbol(using C: Context): ResolvedDefinitions[T] = C.symbolOf(t).asInstanceOf
  }
  extension [T <: References](t: T & Reference) {
    def definition(using C: Context): ResolvedReferences[T] = C.symbolOf(t).asInstanceOf
  }

}
export Named.symbol

// Can we actually move this to namer?

// MOVE TO NAMER
object Resolvable {
  // Block Types
  // -----------
  extension (t: BlockTypeTree) {
    def resolve(using C: Context): symbols.BlockType = C.resolvedType(t).asInstanceOf[symbols.BlockType]
  }

  extension (t: TypeRef) {
    def resolveBlockRef(using C: Context): symbols.InterfaceType = C.resolvedType(t).asInstanceOf[symbols.InterfaceType]
  }

  // NOTE(jiribenes, 2024-04-21):
  // So these are technically unsafe, but we ought to report the errors after Namer anyways,
  // so they should be "safe" within Typer and other phases, I think?
  extension (t: Type) {
    def resolveValueType(using C: Context): symbols.ValueType = C.resolvedType(t).asInstanceOf[symbols.ValueType]
    def resolveBlockType(using C: Context): symbols.BlockType = C.resolvedType(t).asInstanceOf[symbols.BlockType]
  }

  // Capture Sets
  // ------------
  extension (capt: source.CaptureSet) {
    def resolve(using C: Context): symbols.CaptureSet = C.resolvedCapture(capt)
  }
}
export Resolvable.*

extension [T](positioned: T) def sourceOfOpt(using C: Context): Option[String] = {
  positioned match {
    case m: Many[_] if m.span.origin != Origin.Missing =>
      C.positions.substring(m.span.range.from, m.span.range.to)

    case m: Maybe[_] if m.span.origin != Origin.Missing =>
      C.positions.substring(m.span.range.from, m.span.range.to)

    case t: Tree if t.span.origin != Origin.Missing =>
      C.positions.substring(t.span.range.from, t.span.range.to)

    case _ =>
      C.positions.getRange(positioned).flatMap { range =>
        C.positions.substring(range.from, range.to)
      }
  }
}

extension [T](positioned: T) def sourceOf(using C: Context): String =
  positioned.sourceOfOpt.getOrElse { s"${positioned}" }

object Tree {

  // Generic traversal of trees, applying the partial function `f` to every contained
  // element of type Tree.
  def visit(obj: Any)(f: PartialFunction[Tree, Unit]): Unit = obj match {
    case t: Iterable[t] => t.foreach { t => visit(t)(f) }
    case p: Product => p.productIterator.foreach {
      case t: Tree => f(t)
      case other   => ()
    }
    case leaf => ()
  }

  // This solution is between a fine-grained visitor and a untyped and unsafe traversal.
  trait Rewrite extends util.Structural {

    def Context(using C: Context): Context = C

    // Hooks to override
    def expr(using Context): PartialFunction[Term, Term] = PartialFunction.empty
    def stmt(using Context): PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn(using Context): PartialFunction[Def, Def] = PartialFunction.empty

    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: Term)(using Context): Term = structuralVisit(e, expr)
    def rewrite(t: Def)(using Context): Def = structuralVisit(t, defn)
    def rewrite(t: Stmt)(using Context): Stmt = structuralVisit(t, stmt)

    def rewrite(e: ModuleDecl)(using Context): ModuleDecl = structuralVisit(e)
    def rewrite(h: Handler)(using Context): Handler = structuralVisit(h)
    def rewrite(i: Implementation)(using Context): Implementation = structuralVisit(i)
    def rewrite(h: OpClause)(using Context): OpClause = structuralVisit(h)
    def rewrite(c: MatchClause)(using Context): MatchClause = structuralVisit(c)
    def rewrite(c: MatchGuard)(using Context): MatchGuard = structuralVisit(c)
    def rewrite(t: source.CallTarget)(using Context): source.CallTarget = structuralVisit(t)
    def rewrite(b: ExternBody)(using Context): source.ExternBody = structuralVisit(b)

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     *
     * Copies all annotations and position information from source to target
     */
    def visit[T](source: T)(visitor: T => T)(using Context): T =
      val target = visitor(source)
      (source, target) match {
        case (src: Tree, tgt: Tree) =>
          tgt.inheritPosition(src)
          Context.copyAnnotations(src, tgt)
        case _ =>
      }
      target

    inline def structuralVisit[T](sc: T, p: PartialFunction[T, T])(using Context): T =
      visit(sc) { t => rewriteStructurally(t, p) }

    inline def structuralVisit[T](sc: T)(using Context): T =
      visit(sc) { t => rewriteStructurally(t) }
  }

  trait Visit[Ctx] extends Query[Ctx, Unit] {
    override def empty = ()
    override def combine(r1: Unit, r2: Unit): Unit = ()
    override def combineAll(rs: List[Unit]): Unit = ()
  }

  trait Query[Ctx, Res] extends util.Structural {

    def empty: Res
    def combine(r1: Res, r2: Res): Res
    def combineAll(rs: List[Res]): Res = rs.foldLeft(empty)(combine)

    // Hooks to override
    def expr(using Context, Ctx): PartialFunction[Term, Res] = PartialFunction.empty
    def stmt(using Context, Ctx): PartialFunction[Stmt, Res] = PartialFunction.empty
    def defn(using Context, Ctx): PartialFunction[Def, Res] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T <: Tree](t: T)(visitor: T => Res)(using Context, Ctx): Res = scoped { visitor(t) }

    /**
     * Hook that can be overriden to perform something for each new lexical scope
     */
    def scoped(action: => Res)(using Context, Ctx): Res = action

    //
    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def query(e: Term)(using C: Context, ctx: Ctx): Res = structuralQuery(e, expr)
    def query(t: Def)(using C: Context, ctx: Ctx): Res = structuralQuery(t, defn)
    def query(t: Stmt)(using C: Context, ctx: Ctx): Res = structuralQuery(t, stmt)

    def query(e: ModuleDecl)(using Context, Ctx): Res = structuralQuery(e)
    def query(h: Handler)(using Context, Ctx): Res = structuralQuery(h)
    def query(h: Implementation)(using Context, Ctx): Res = structuralQuery(h)
    def query(h: OpClause)(using Context, Ctx): Res = structuralQuery(h)
    def query(c: MatchClause)(using Context, Ctx): Res = structuralQuery(c)
    def query(c: MatchGuard)(using Context, Ctx): Res = structuralQuery(c)
    def query(b: ExternBody)(using Context, Ctx): Res = structuralQuery(b)

    def query(t: Template[Term])(using Context, Ctx): Res =
      combineAll(t.args.map(query))


    inline def structuralQuery[T <: Tree](el: T, pf: PartialFunction[T, Res] = PartialFunction.empty)(using Context, Ctx): Res =
      visit(el) { t =>
        if pf.isDefinedAt(el) then pf.apply(el) else queryStructurally(t, empty, combine)
      }
  }
}
