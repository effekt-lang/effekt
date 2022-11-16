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
 *     │─ [[ Import ]]
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
case class IdDef(name: String) extends Id {
  def clone(using C: Context): Id = {
    val copy = IdDef(name)
    C.positions.dupPos(this, copy)
    copy
  }
}
case class IdRef(name: String) extends Id {
  def clone(using C: Context): Id = {
    val copy = IdRef(name)
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
case class ModuleDecl(path: String, imports: List[Import], defs: List[Def]) extends Tree
case class Import(path: String) extends Tree

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

  def id: IdDef

  case FunDef(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[Effectful], body: Stmt)
  case ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt)
  case VarDef(id: IdDef, annot: Option[ValueType], region: Option[IdRef], binding: Stmt)
  case DefDef(id: IdDef, annot: Option[BlockType], block: Term)
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

  case ExternDef(capture: CaptureSet, id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful, body: String)

  case ExternResource(id: IdDef, tpe: BlockType)

  case ExternInterface(id: IdDef, tparams: List[Id])

  /**
   * Namer resolves the path and loads the contents in field [[contents]]
   *
   * @note Storing content and id as user-visible fields is a workaround for the limitation that Enum's cannot
   *   have case specific refinements.
   */
  case ExternInclude(path: String, var contents: String = "", val id: IdDef = IdDef(""))
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
  case Do(effect: Option[BlockType.BlockTypeRef], id: IdRef, targs: List[ValueType], vargs: List[Term]) extends Term, Reference

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
  case If(cond: Term, thn: Stmt, els: Stmt)
  case While(cond: Term, block: Stmt)
  case Match(scrutinee: Term, clauses: List[MatchClause])

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
def UnitLit(): Literal = Literal((), symbols.builtins.TUnit)
def IntLit(value: Int): Literal = Literal(value, symbols.builtins.TInt)
def BooleanLit(value: Boolean): Literal = Literal(value, symbols.builtins.TBoolean)
def DoubleLit(value: Double): Literal = Literal(value, symbols.builtins.TDouble)
def StringLit(value: String): Literal = Literal(value, symbols.builtins.TString)

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
case class Constructor(id: IdDef, params: List[ValueParam]) extends Definition
case class Operation(id: IdDef, tparams: List[Id], params: List[ValueParam], ret: Effectful) extends Definition


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
case class Implementation(interface: BlockType.BlockTypeRef, clauses: List[OpClause]) extends Reference {
  def id = interface.id
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
case class OpClause(id: IdRef,  tparams: List[Id], vparams: List[ValueParam], ret: Option[Effectful], body: Stmt, resume: IdDef) extends Reference

// Pattern Matching
// ----------------

case class MatchClause(pattern: MatchPattern, body: Stmt) extends Tree

enum MatchPattern extends Tree {

  /**
   * Pattern matching anything:
   *
   *   case a => ...
   */
  case AnyPattern(id: IdDef) extends MatchPattern, Definition

  /**
   * Pattern matching on a constructor
   *
   *   case Cons(a, as) => ...
   */
  case TagPattern(id: IdRef, patterns: List[MatchPattern]) extends MatchPattern, Reference

  /**
   * A wildcard pattern ignoring the matched value
   *
   *   case _ => ...
   */
  case IgnorePattern()

  /**
   * A pattern that matches a single literal value
   */
  case LiteralPattern(l: Literal)
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
 *     │─ [[ ValueType ]]
 *     │  │─ [[ ValueTypeTree ]]
 *     │  │─ [[ BoxedType ]]
 *     │  │─ [[ ValueTypeRef ]]
 *     │
 *     │─ [[ BlockType ]]
 *     │  │─ [[ BlockTypeTree ]]
 *     │  │─ [[ FunctionType ]]
 *     │  │─ [[ BlockTypeRef ]]
 *     │
 *
 * --------------------------------------------
 */
sealed trait Type extends Tree

/**
 * Value Types
 */
enum ValueType extends Type {

  /**
   * Trees that represent inferred or synthesized types (not present in the source)
   */
  case ValueTypeTree(tpe: symbols.ValueType)

  /**
   * Types of first-class functions
   */
  case BoxedType(tpe: BlockType, capt: CaptureSet)

  // Bound occurrences (args can be empty)
  case ValueTypeRef(id: IdRef, args: List[ValueType]) extends ValueType, Reference
}
export ValueType.*

/**
 * Block Types
 */
enum BlockType extends Type {

  /**
   * Trees that represent inferred or synthesized types (not present in the source)
   */
  case BlockTypeTree(eff: symbols.BlockType)
  case FunctionType(vparams: List[ValueType], result: ValueType, effects: Effects)
  case BlockTypeRef(id: IdRef, args: List[ValueType]) extends BlockType, Reference
}

export BlockType.*

// We have Effectful as a tree in order to apply code actions on it (see Server.inferEffectsAction)
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

/**
 * Represents an annotated set of effects. Before name resolution, we cannot know
 * the concrete nature of its elements (so it is generic [[BlockTypeRef]]).
 */
case class Effects(effs: List[BlockType.BlockTypeRef]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: BlockTypeRef*): Effects = Effects(effs.toSet)
  def apply(effs: Set[BlockTypeRef]): Effects = Effects(effs.toList)
}

case class CaptureSet(captures: List[IdRef]) extends Tree


object Named {

  type Params = ValueParam | BlockParam
  type Externs = ExternDef | ExternResource | ExternInterface | ExternType
  type Defs = FunDef | ValDef | VarDef | DefDef | InterfaceDef | DataDef | RecordDef | TypeDef | EffectDef
  type Definitions =  Externs | Defs | Params | Operation | Constructor | Region | AnyPattern

  type Types = ValueTypeRef | BlockTypeRef
  type Vars = Var | Assign
  type Calls = Do | Select | MethodCall | IdTarget
  type References = Types | Vars | Calls | TagPattern | Handler | OpClause | Implementation

  type ResolvedDefinitions[T <: Definitions] = T match {
    // Defs
    case FunDef       => symbols.UserFunction
    case ValDef       => symbols.Binder.ValBinder // export Binder.* doesn't seem to work here (maybe because the packages are cyclic?)
    case VarDef       => symbols.Binder.VarBinder
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
  }

  type ResolvedReferences[T <: References] = T match {
    // Types
    case ValueTypeRef => symbols.TypeConstructor
    case BlockTypeRef => symbols.BlockTypeConstructor

    // Vars
    case Var    => symbols.TermSymbol
    case Assign => symbols.Binder.VarBinder

    // CallLike
    case Do         => symbols.Operation
    case Select     => symbols.Field
    case MethodCall => symbols.TermSymbol
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

// MOVE TO NAMER
object Resolvable {

  // Value Types
  // -----------
  extension (t: ValueType) {
    def resolve(using C: Context): symbols.ValueType = C.resolvedType(t).asInstanceOf
  }

  // BLock Types
  // -----------
  // we need to avoid widening, so here we define BlockType as a sum without a common parent
  // (see https://github.com/lampepfl/dotty/issues/16299)
  type BlockTypes = BlockTypeTree | FunctionType | BlockTypeRef

  type Resolved[T <: BlockTypes] = T match {
    case BlockTypeTree => symbols.BlockType
    case FunctionType => symbols.FunctionType
    case BlockTypeRef => symbols.InterfaceType
  }

  extension [T <: BlockTypes] (t: T) {
    def resolve(using C: Context): Resolved[T] = C.resolvedType(t).asInstanceOf
  }

  // Capture Sets
  // ------------
  extension (capt: source.CaptureSet) {
    def resolve(using C: Context): symbols.CaptureSet = C.resolvedCapture(capt)
  }
}
export Resolvable.resolve


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
  trait Rewrite {

    def Context(using C: Context): Context = C

    // Hooks to override
    def expr(using C: Context): PartialFunction[Term, Term] = PartialFunction.empty
    def stmt(using C: Context): PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn(using C: Context): PartialFunction[Def, Def] = PartialFunction.empty

    /**
     * Hook that can be overriden to perform an action at every node in the tree
     *
     * Copies all annotations and position information from source to target
     */
    def visit[T <: Tree](source: T)(visitor: T => T)(using Context): T = {
      val target = visitor(source)
      target.inheritPosition(source)
      Context.copyAnnotations(source, target)
      target
    }

    //
    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: ModuleDecl)(using Context): ModuleDecl = visit(e) {
      case ModuleDecl(path, imports, defs) =>
        ModuleDecl(path, imports, defs.map(rewrite))
    }

    def rewrite(e: Term)(using Context): Term = visit(e) {
      case e if expr.isDefinedAt(e) => expr(e)
      case v: Var                   => v
      case l: Literal               => l

      case Assign(id, expr) =>
        Assign(id, rewrite(expr))

      case If(cond, thn, els) =>
        If(rewrite(cond), rewrite(thn), rewrite(els))

      case While(cond, body) =>
        While(rewrite(cond), rewrite(body))

      case Match(sc, clauses) =>
        Match(rewrite(sc), clauses.map(rewrite))

      case Select(recv, name) =>
        Select(rewrite(recv), name)

      case Do(effect, id, targs, vargs) =>
        Do(effect, id, targs, vargs.map(rewrite))

      case Call(fun, targs, vargs, bargs) =>
        Call(rewrite(fun), targs, vargs.map(rewrite), bargs.map(rewrite))

      case MethodCall(receiver, id, targs, vargs, bargs) =>
        MethodCall(rewrite(receiver), id, targs, vargs.map(rewrite), bargs.map(rewrite))

      case TryHandle(prog, handlers) =>
        TryHandle(rewrite(prog), handlers.map(rewrite))

      case New(impl) =>
        New(rewrite(impl))

      case BlockLiteral(tps, vps, bps, body) =>
        BlockLiteral(tps, vps, bps, rewrite(body))

      case Region(name, body) =>
        Region(name, rewrite(body))

      case Hole(stmts) =>
        Hole(rewrite(stmts))

      case Box(c, b) =>
        Box(c, rewrite(b))

      case Unbox(b) =>
        Unbox(rewrite(b))
    }

    def rewrite(t: Def)(using C: Context): Def = visit(t) {
      case t if defn.isDefinedAt(t) => defn(t)

      case FunDef(id, tparams, vparams, bparams, ret, body) =>
        FunDef(id, tparams, vparams, bparams, ret, rewrite(body))

      case ValDef(id, annot, binding) =>
        ValDef(id, annot, rewrite(binding))

      case VarDef(id, annot, region, binding) =>
        VarDef(id, annot, region, rewrite(binding))

      case DefDef(id, annot, block) =>
        DefDef(id, annot, rewrite(block))

      case d: InterfaceDef   => d
      case d: DataDef        => d
      case d: RecordDef      => d
      case d: TypeDef        => d
      case d: EffectDef      => d

      case d: ExternType     => d
      case d: ExternDef      => d
      case d: ExternResource => d
      case d: ExternInterface => d
      case d: ExternInclude  => d
    }

    def rewrite(t: Stmt)(using C: Context): Stmt = visit(t) {
      case s if stmt.isDefinedAt(s) => stmt(s)

      case DefStmt(d, rest) =>
        DefStmt(rewrite(d), rewrite(rest))

      case ExprStmt(e, rest) =>
        ExprStmt(rewrite(e), rewrite(rest))

      case Return(e) =>
        Return(rewrite(e))

      case BlockStmt(b) =>
        BlockStmt(rewrite(b))
    }

    def rewrite(h: Handler)(using C: Context): Handler = visit(h) {
      case Handler(capability, impl) =>
        Handler(capability, rewrite(impl))
    }

    def rewrite(impl: Implementation)(using Context): Implementation = visit(impl) {
      case source.Implementation(interface, clauses) =>
        Implementation(interface, clauses.map(rewrite))
    }

    def rewrite(h: OpClause)(using C: Context): OpClause = visit(h) {
      case OpClause(id, tparams, params, ret, body, resume) =>
        OpClause(id, tparams, params, ret, rewrite(body), resume)
    }

    def rewrite(c: MatchClause)(using C: Context): MatchClause = visit(c) {
      case MatchClause(pattern, body) =>
        MatchClause(pattern, rewrite(body))
    }

    def rewrite(target: source.CallTarget)(implicit C: Context): source.CallTarget = visit(target) {
      case i: IdTarget => target
      case ExprTarget(expr) => ExprTarget(rewrite(expr))
    }
  }

  trait Visit[Ctx] extends Query[Ctx, Unit] {
    override def empty = ()
    override def combine(r1: Unit, r2: Unit): Unit = ()
    override def combineAll(rs: List[Unit]): Unit = ()
  }

  trait Query[Ctx, Res] {

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
    def visit[T <: Tree](t: T)(visitor: T => Res)(using Context, Ctx): Res = visitor(t)

    /**
     * Hook that can be overriden to perform something for each new lexical scope
     */
    def scoped(action: => Res)(using Context, Ctx): Res = action

    //
    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def query(e: ModuleDecl)(using Context, Ctx): Res = visit(e) {
      case ModuleDecl(path, imports, defs) => scoped { combineAll(defs.map(query)) }
    }

    def query(e: Term)(using C: Context, ctx: Ctx): Res = visit(e) {
      case e if expr.isDefinedAt(e) => expr.apply(e)
      case v: Var                   => empty
      case l: Literal               => empty

      case Assign(id, expr) => scoped { query(expr) }

      case If(cond, thn, els) =>
        combineAll(query(cond) :: scoped { query(thn) } :: scoped { query(els) } :: Nil)

      case While(cond, body) =>
        combineAll(scoped { query(cond) } :: scoped { query(body) } :: Nil)

      case Match(sc, clauses) =>
        combineAll(query(sc) :: clauses.map(cl => scoped { query(cl) }))

      case Select(recv, name) =>
        query(recv)

      case Do(effect, id, targs, vargs) =>
        combineAll(vargs.map(query))

      case Call(fun, targs, vargs, bargs) =>
        combineAll(vargs.map(query) ++ bargs.map(query))

      case MethodCall(receiver, id, targs, vargs, bargs) =>
        combineAll(query(receiver) :: vargs.map(query) ++ bargs.map(query))

      case TryHandle(prog, handlers) =>
        combineAll(scoped { query(prog) } :: handlers.map(h => scoped { query(h) }))

      case New(impl) =>
        query(impl)

      case BlockLiteral(tps, vps, bps, body) =>
        scoped { query(body) }

      case Region(name, body) =>
        query(body)

      case Hole(stmts) =>
        query(stmts)

      case Box(c, b) =>
        query(b)

      case Unbox(b) =>
        query(b)
    }

    def query(t: Def)(using C: Context, ctx: Ctx): Res = visit(t) {
      case t if defn.isDefinedAt(t) => defn.apply(t)

      case FunDef(id, tparams, vparams, bparams, ret, body) =>
        scoped { query(body) }

      case ValDef(id, annot, binding) =>
        scoped { query(binding) }

      case VarDef(id, annot, region, binding) =>
        scoped { query(binding) }

      case DefDef(id, annot, block) =>
        scoped { query(block) }

      case d: InterfaceDef   => empty
      case d: DataDef        => empty
      case d: RecordDef      => empty
      case d: TypeDef        => empty
      case d: EffectDef      => empty

      case d: ExternType     => empty
      case d: ExternDef      => empty
      case d: ExternResource => empty
      case d: ExternInterface => empty
      case d: ExternInclude  => empty
    }

    def query(t: Stmt)(using C: Context, ctx: Ctx): Res = visit(t) {
      case s if stmt.isDefinedAt(s) => stmt.apply(s)

      case DefStmt(d, rest) =>
        combineAll(query(d) :: query(rest) :: Nil)

      case ExprStmt(e, rest) =>
        combineAll(query(e) :: query(rest) :: Nil)

      case Return(e) =>
        query(e)

      case BlockStmt(b) =>
        scoped { query(b) }
    }

    def query(h: Handler)(using Context, Ctx): Res = visit(h) {
      case Handler(capability, impl) => query(impl)
    }

    def query(h: Implementation)(using Context, Ctx): Res = visit(h) {
      case Implementation(interface, clauses) =>
        combineAll(clauses.map(cl => scoped { query(cl) }))
    }

    def query(h: OpClause)(using Context, Ctx): Res = visit(h) {
      case OpClause(id, tparams, params, ret, body, resume) =>
        scoped { query(body) }
    }

    def query(c: MatchClause)(using Context, Ctx): Res = visit(c) {
      case MatchClause(pattern, body) =>
        scoped { query(body) }
    }
  }
}
