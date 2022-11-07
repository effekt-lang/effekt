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
 *   |- Def
 *   |  |- FunDef
 *   |  |- ValDef
 *   |  |- VarDef
 *   |  |- EffDef
 *   |  |- DataDef
 *   |  |- RecordDef
 *   |  |- TypeDef
 *   |  |- EffectDef
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
 *   |- Term
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
 *   |
 *   |- Effect
 *   |- Effectful
 *   |- Effects
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
sealed trait Param extends Definition
case class ValueParam(id: IdDef, tpe: Option[ValueType]) extends Param { type symbol = symbols.ValueParam }
case class BlockParam(id: IdDef, tpe: BlockType) extends Param { type symbol = symbols.BlockParam }

/**
 * Lambdas / function literals (e.g., { x => x + 1 })
 */
case class BlockLiteral(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends Term


/**
 * Global and local definitions
 */
sealed trait Def extends Definition {
  def id: IdDef
}
case class FunDef(id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Option[Effectful], body: Stmt) extends Def {
  type symbol = symbols.UserFunction
}
case class ValDef(id: IdDef, annot: Option[ValueType], binding: Stmt) extends Def {
  type symbol = symbols.ValBinder
}
case class VarDef(id: IdDef, annot: Option[ValueType], region: Option[IdRef], binding: Stmt) extends Def {
  type symbol = symbols.VarBinder
}
case class DefDef(id: IdDef, annot: Option[BlockType], block: Term) extends Def {
  type symbol = symbols.DefBinder
}
case class InterfaceDef(id: IdDef, tparams: List[Id], ops: List[Operation], isEffect: Boolean = true) extends Def {
  type symbol = symbols.Interface
}
case class Operation(id: IdDef, tparams: List[Id], params: List[ValueParam], ret: Effectful) extends Definition {
  type symbol = symbols.Operation
}
case class DataDef(id: IdDef, tparams: List[Id], ctors: List[Constructor]) extends Def {
  type symbol = symbols.DataType
}
case class Constructor(id: IdDef, params: List[ValueParam]) extends Definition {
  type symbol = symbols.Constructor
}
case class RecordDef(id: IdDef, tparams: List[Id], fields: List[ValueParam]) extends Def {
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
case class EffectDef(id: IdDef, tparams: List[Id], effs: Effects) extends Def {
  type symbol = symbols.EffectAlias
}

// only valid on the toplevel!
case class ExternType(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinType
}

case class ExternDef(capture: CaptureSet, id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful, body: String) extends Def {
  type symbol = symbols.BuiltinFunction
}

case class ExternResource(id: IdDef, tpe: BlockType) extends Def {
  type symbol = symbols.BlockParam
}

case class ExternInterface(id: IdDef, tparams: List[Id]) extends Def {
  type symbol = symbols.BuiltinInterface
}

case class ExternInclude(path: String) extends Def {
  def id = IdDef("includes don't have names")
  // Namer resolves the path and loads the contents
  var contents: String = ""
}

sealed trait Stmt extends Tree
case class DefStmt(d: Def, rest: Stmt) extends Stmt
case class ExprStmt(d: Term, rest: Stmt) extends Stmt
case class Return(d: Term) extends Stmt
case class BlockStmt(stmts: Stmt) extends Stmt

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
 * Represents a first-class function
 */
case class Box(capt: Option[CaptureSet], block: Term) extends Term

case class Unbox(term: Term) extends Term

type CallLike = Call | Do | Select | MethodCall

/**
 * Models:
 * - field selection, i.e., `record.field` (receiver is an expression, result is an expression)
 * - future: nested capability / module selection, i.e., `mymod.nested.foo` (receiver is a block, result is a block)
 *
 * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
 */
case class Select(receiver: Term, id: IdRef) extends Term, Reference {
  type symbol = symbols.Field
}

/**
 * A call to an effect operation, i.e., `do raise()`.
 *
 * The [[effect]] is the optionally annotated effect type (not possible in source ATM). In the future, this could
 * look like `do Exc.raise()`, or `do[Exc] raise()`, or do[Exc].raise(), or simply Exc.raise() where Exc is a type.
 */
case class Do(effect: Option[BlockTypeRef], id: IdRef, targs: List[ValueType], vargs: List[Term]) extends Term, Reference {
  type symbol = symbols.Operation
}

/**
 * A call to either an expression, i.e., `(fun() { ...})()`; or a named function, i.e., `foo()`
 */
case class Call(target: CallTarget, targs: List[ValueType], vargs: List[Term], bargs: List[Term]) extends Term

/**
 * Models:
 * - uniform function call, i.e., `list.map { ... }` (receiver is an expression, result is an expression)
 * - capability call, i.e., `exc.raise()` (receiver is a block, result is an expression)
 *
 * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
 */
case class MethodCall(receiver: Term, id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[Term]) extends Term, Reference {
  type symbol = symbols.TermSymbol
}

sealed trait CallTarget extends Tree

// potentially overloaded
case class IdTarget(id: IdRef) extends CallTarget with Reference {
  // can refer to either a block OR a term symbol
  type symbol = symbols.TermSymbol
}
// not overloaded
case class ExprTarget(receiver: Term) extends CallTarget

case class If(cond: Term, thn: Stmt, els: Stmt) extends Term
case class While(cond: Term, block: Stmt) extends Term

/**
 * Handling effects
 *
 *   try {
 *     <prog>
 *   } with <capability> : <Effect> { ... }
 *
 * Each with-clause is modeled as an instance of type [[Handler]].
 */
case class TryHandle(prog: Stmt, handlers: List[Handler]) extends Term

/**
 * An implementation of a given interface
 *
 *     <Interface> {
 *       def <opClause> = ...
 *     }
 *
 * Called "template" or "class" in other languages.
 */
case class Implementation(interface: BlockTypeRef, clauses: List[OpClause]) extends Reference {
  def id = interface.id
  type symbol = symbols.Interface
}

/**
 * A handler is a pair of an optionally named capability and the handler [[Implementation]]
 */
case class Handler(capability: Option[BlockParam] = None, impl: Implementation) extends Reference {
  def effect = impl.interface
  def clauses = impl.clauses
  def id = impl.id
  type symbol = symbols.Interface
}

case class New(impl: Implementation) extends Term


// TODO also allow block params and add a check in TryHandle to rule out continuation capture and block params.

// `ret` is an optional user-provided type annotation for the return type
// currently the annotation is rejected by [[Typer]] -- after that phase, `ret` should always be `None`
case class OpClause(id: IdRef,  tparams: List[Id], vparams: List[ValueParam], ret: Option[Effectful], body: Stmt, resume: IdDef) extends Reference {
  type symbol = symbols.Operation
}

case class Region(id: IdDef, body: Stmt) extends Term with Definition {
  type symbol = symbols.TrackedParam
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
  type symbol = symbols.Constructor
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
sealed trait Type extends Tree, Resolvable {
  type resolved <: symbols.Type
  def resolve(implicit C: Context) = C.resolvedType(this)
}

/**
 * Value Types
 */
sealed trait ValueType extends Type {
  type resolved <: symbols.ValueType
}

/**
 * Trees that represent inferred or synthesized types (not present in the source)
 */
case class ValueTypeTree(tpe: symbols.ValueType) extends ValueType

/**
 * Types of first-class functions
 */
case class BoxedType(tpe: BlockType, capt: CaptureSet) extends ValueType

// Used for bindings
case class TypeVar(id: IdDef) extends ValueType, Definition {
  type symbol = symbols.Symbol with symbols.ValueType
}
// Bound occurrences (args can be empty)
case class ValueTypeRef(id: IdRef, args: List[ValueType]) extends ValueType, Reference {
  // can be applied to type aliases or data types
  type symbol = symbols.TypeSymbol
}

/**
 * Block Types
 */
sealed trait BlockType extends Type

/**
 * Trees that represent inferred or synthesized types (not present in the source)
 */
case class BlockTypeTree(eff: symbols.BlockType) extends BlockType {
  type resolved = symbols.BlockType
}

case class FunctionType(vparams: List[ValueType], result: ValueType, effects: Effects) extends BlockType {
  type resolved = symbols.FunctionType
}

case class BlockTypeRef(id: IdRef, args: List[ValueType]) extends BlockType, Reference {
  type symbol = symbols.TypeSymbol
  type resolved <: symbols.InterfaceType
}

// We have Effectful as a tree in order to apply code actions on it (see Server.inferEffectsAction)
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

/**
 * Represents an annotated set of effects. Before name resolution, we cannot know
 * the concrete nature of its elements (so it is generic [[BlockTypeRef]]).
 */
case class Effects(effs: List[BlockTypeRef]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: BlockTypeRef*): Effects = Effects(effs.toSet)
  def apply(effs: Set[BlockTypeRef]): Effects = Effects(effs.toList)
}

case class CaptureSet(captures: List[IdRef]) extends Resolvable {
  type resolved = symbols.CaptureSet
  def resolve(using C: Context): resolved = C.resolvedCapture(this)
}

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
      case l: Literal[t]            => l

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
      case l: Literal[t]            => empty

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
