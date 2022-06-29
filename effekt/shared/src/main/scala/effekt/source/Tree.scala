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
 *   |  |- TypeDef
 *   |  |- EffectDef
 *   |  |- ExternType
 *   |  |- ExternEffect
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

sealed trait BlockArg extends Tree
case class FunctionArg(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends BlockArg
case class InterfaceArg(id: IdRef) extends BlockArg with Reference {
  type symbol = symbols.BlockParam
}

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
  type symbol = symbols.Record
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

object ExternFlag extends Enumeration {
  type Purity = Value
  // Probably these three are not enough, yet.
  val Pure, IO, Control = Value
  def directStyle(p: Purity): Boolean = p == Pure || p == IO
}

case class ExternFun(purity: ExternFlag.Purity, id: IdDef, tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: Effectful, body: String) extends Def {
  type symbol = symbols.BuiltinFunction
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
case class Box(capt: Option[CaptureSet], block: BlockArg) extends Term

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
case class Do(effect: Option[InterfaceType], id: IdRef, targs: List[ValueType], vargs: List[Term]) extends Term, Reference {
  type symbol = symbols.Operation
}

/**
 * A call to either an expression, i.e., `(fun() { ...})()`; or a named function, i.e., `foo()`
 */
case class Call(target: CallTarget, targs: List[ValueType], vargs: List[Term], bargs: List[BlockArg]) extends Term

/**
 * Models:
 * - uniform function call, i.e., `list.map { ... }` (receiver is an expression, result is an expression)
 * - capability call, i.e., `exc.raise()` (receiver is a block, result is an expression)
 *
 * The resolved target can help to determine whether the receiver needs to be type-checked as first- or second-class.
 */
case class MethodCall(receiver: Term, id: IdRef, targs: List[ValueType], vargs: List[Term], bargs: List[BlockArg]) extends Term, Reference {
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

case class TryHandle(prog: Stmt, handlers: List[Handler]) extends Term

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
case class Handler(effect: InterfaceType, capability: Option[BlockParam] = None, clauses: List[OpClause]) extends Reference {
  def id = effect.id
  type symbol = symbols.Interface
}
case class OpClause(id: IdRef, vparams: List[ValueParam], body: Stmt, resume: IdDef) extends Reference {
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
 *
 * TODO generalize to blocks that can take blocks
 */
sealed trait Type extends Tree with Resolvable {
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
case class ValueTypeApp(id: IdRef, args: List[ValueType]) extends ValueType with Reference {
  // can be applied to type aliases or data types
  type symbol = symbols.TypeSymbol
}

/**
 * Block Types
 */
sealed trait BlockType extends Type

// not userdefinable, only for inferred type arguments
case class BlockTypeTree(eff: symbols.BlockType) extends BlockType {
  type resolved = symbols.BlockType
}

case class FunctionType(vparams: List[ValueType], result: ValueType, effects: Effects) extends BlockType {
  type resolved = symbols.FunctionType
}

sealed trait InterfaceType extends BlockType {
  def id: IdRef
  type resolved <: symbols.InterfaceType
}

case class BlockTypeApp(id: IdRef, args: List[ValueType]) extends InterfaceType with Reference {
  type symbol = symbols.TypeSymbol
}

/**
 * **Reference** to an interface type (like an effect)
 */
case class InterfaceVar(id: IdRef) extends InterfaceType with Resolvable {
  type resolved = symbols.Interface
}

// We have Effectful as a tree in order to apply code actions on it (see Server.inferEffectsAction)
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

case class Effects(effs: List[InterfaceType]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: InterfaceType*): Effects = Effects(effs.toSet)
  def apply(effs: Set[InterfaceType]): Effects = Effects(effs.toList)
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
    // Hooks to override
    def expr(implicit C: Context): PartialFunction[Term, Term] = PartialFunction.empty
    def stmt(implicit C: Context): PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn(implicit C: Context): PartialFunction[Def, Def] = PartialFunction.empty

    /**
     * Hook that can be overriden to perform an action at every node in the tree
     */
    def visit[T <: Tree](t: T)(visitor: T => T)(implicit C: Context): T = visitor(t)

    //
    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: ModuleDecl)(implicit C: Context): ModuleDecl = visit(e) {
      case ModuleDecl(path, imports, defs) =>
        ModuleDecl(path, imports, defs.map(rewrite))
    }

    def rewrite(e: Term)(implicit C: Context): Term = visit(e) {
      case e if expr.isDefinedAt(e) => expr(C)(e)
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
        Call(fun, targs, vargs.map(rewrite), bargs.map(rewrite))

      case MethodCall(receiver, id, targs, vargs, bargs) =>
        MethodCall(rewrite(receiver), id, targs, vargs.map(rewrite), bargs.map(rewrite))

      case TryHandle(prog, handlers) =>
        TryHandle(rewrite(prog), handlers.map(rewrite))

      case Hole(stmts) =>
        Hole(rewrite(stmts))

      case Box(c, b) =>
        Box(c, rewrite(b))

      case Unbox(b) =>
        Unbox(rewrite(b))
    }

    def rewrite(t: Def)(implicit C: Context): Def = visit(t) {
      case t if defn.isDefinedAt(t) => defn(C)(t)

      case FunDef(id, tparams, vparams, bparams, ret, body) =>
        FunDef(id, tparams, vparams, bparams, ret, rewrite(body))

      case ValDef(id, annot, binding) =>
        ValDef(id, annot, rewrite(binding))

      case VarDef(id, annot, region, binding) =>
        VarDef(id, annot, region, rewrite(binding))

      case d: InterfaceDef        => d
      case d: DataDef       => d
      case d: RecordDef     => d
      case d: TypeDef       => d
      case d: EffectDef     => d

      case d: ExternType    => d
      case d: ExternEffect  => d
      case d: ExternFun     => d
      case d: ExternInclude => d
    }

    def rewrite(t: Stmt)(implicit C: Context): Stmt = visit(t) {
      case s if stmt.isDefinedAt(s) => stmt(C)(s)

      case DefStmt(d, rest) =>
        DefStmt(rewrite(d), rewrite(rest))

      case ExprStmt(e, rest) =>
        ExprStmt(rewrite(e), rewrite(rest))

      case Return(e) =>
        Return(rewrite(e))

      case BlockStmt(b) =>
        BlockStmt(rewrite(b))
    }

    def rewrite(b: BlockArg)(implicit C: Context): BlockArg = b match {
      case b: FunctionArg  => rewrite(b)
      case b: InterfaceArg => b
    }

    def rewrite(b: FunctionArg)(implicit C: Context): FunctionArg = b match {
      case FunctionArg(tps, vps, bps, body) => FunctionArg(tps, vps, bps, rewrite(body))
    }

    def rewrite(h: Handler)(implicit C: Context): Handler = visit(h) {
      case Handler(effect, capability, clauses) =>
        Handler(effect, capability, clauses.map(rewrite))
    }

    def rewrite(h: OpClause)(implicit C: Context): OpClause = visit(h) {
      case OpClause(id, params, body, resume) =>
        OpClause(id, params, rewrite(body), resume)
    }

    def rewrite(c: MatchClause)(implicit C: Context): MatchClause = visit(c) {
      case MatchClause(pattern, body) =>
        MatchClause(pattern, rewrite(body))
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
     * Hook that can be overriden to perform an action at every node in the tree
     */
    def visit[T <: Tree](t: T)(visitor: T => Res)(using Context, Ctx): Res = visitor(t)

    //
    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def query(e: ModuleDecl)(using Context, Ctx): Res = visit(e) {
      case ModuleDecl(path, imports, defs) => combineAll(defs.map(query))
    }

    def query(e: Term)(using C: Context, ctx: Ctx): Res = visit(e) {
      case e if expr.isDefinedAt(e) => expr.apply(e)
      case v: Var                   => empty
      case l: Literal[t]            => empty

      case Assign(id, expr) => query(expr)

      case If(cond, thn, els) =>
        combineAll(query(cond) :: query(thn) :: query(els) :: Nil)

      case While(cond, body) =>
        combineAll(query(cond) :: query(body) :: Nil)

      case Match(sc, clauses) =>
        combineAll(query(sc) :: clauses.map(query))

      case Select(recv, name) =>
        query(recv)

      case Do(effect, id, targs, vargs) =>
        combineAll(vargs.map(query))

      case Call(fun, targs, vargs, bargs) =>
        combineAll(vargs.map(query) ++ bargs.map(query))

      case MethodCall(receiver, id, targs, vargs, bargs) =>
        combineAll(query(receiver) :: vargs.map(query) ++ bargs.map(query))

      case TryHandle(prog, handlers) =>
        combineAll(query(prog) :: handlers.map(query))

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
        query(body)

      case ValDef(id, annot, binding) =>
        query(binding)

      case VarDef(id, annot, region, binding) =>
        query(binding)

      case d: InterfaceDef  => empty
      case d: DataDef       => empty
      case d: RecordDef     => empty
      case d: TypeDef       => empty
      case d: EffectDef     => empty

      case d: ExternType    => empty
      case d: ExternEffect  => empty
      case d: ExternFun     => empty
      case d: ExternInclude => empty
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
        query(b)
    }

    def query(b: BlockArg)(using Context, Ctx): Res = b match {
      case b: FunctionArg  => query(b)
      case b: InterfaceArg => empty
    }

    def query(b: FunctionArg)(using Context, Ctx): Res = b match {
      case FunctionArg(tps, vps, bps, body) => query(body)
    }

    def query(h: Handler)(using Context, Ctx): Res = visit(h) {
      case Handler(effect, capability, clauses) =>
        combineAll(clauses.map(query))
    }

    def query(h: OpClause)(using Context, Ctx): Res = visit(h) {
      case OpClause(id, params, body, resume) =>
        query(body)
    }

    def query(c: MatchClause)(using Context, Ctx): Res = visit(c) {
      case MatchClause(pattern, body) =>
        query(body)
    }
  }
}
