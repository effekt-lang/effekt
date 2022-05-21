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

// TODO fuse into one kind of parameter

sealed trait TrackedParam extends ParamSection with Definition
case class BlockParam(id: IdDef, tpe: BlockType) extends TrackedParam { type symbol = symbols.BlockParam }
case class CapabilityParam(id: IdDef, tpe: CapabilityType) extends TrackedParam { type symbol = symbols.CapabilityParam }

sealed trait ArgSection extends Tree
case class ValueArgs(args: List[Expr]) extends ArgSection
case class BlockArg(params: List[ParamSection], body: Stmt) extends ArgSection
case class CapabilityArg(id: IdRef) extends ArgSection with Reference {
  type symbol = symbols.CapabilityParam
}

/**
 * Global and local definitions
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
case class EffDef(id: IdDef, tparams: List[Id], ops: List[Operation]) extends Def {
  type symbol = symbols.ControlEffect
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

object ExternFlag extends Enumeration {
  type Purity = Value
  // Probably these three are not enough, yet.
  val Pure, IO, Control = Value
  def directStyle(p: Purity): Boolean = p == Pure || p == IO
}

case class ExternFun(purity: ExternFlag.Purity, id: IdDef, tparams: List[Id], params: List[ParamSection], ret: Effectful, body: String) extends Def {
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

/**
 * Represents a first-class function
 *
 * Maybe surprisingly, lambdas definitions. This makes it easier to associate it with
 * its parameter symbols.
 */
case class Lambda(id: IdDef, params: List[ParamSection], body: Stmt) extends Expr with Definition {
  type symbol = symbols.Lambda
}

// maybe replace `fun: Id` here with BlockVar
// TODO should we have one Call-node and a selector tree, or multiple different call nodes?
case class Call(target: CallTarget, targs: List[ValueType], args: List[ArgSection]) extends Expr

sealed trait CallTarget extends Tree
case class IdTarget(id: IdRef) extends CallTarget with Reference {
  // can refer to either a block OR a term symbol
  type symbol = symbols.TermSymbol
}
case class MemberTarget(receiver: IdRef, id: IdRef) extends CallTarget with Reference {
  type symbol = symbols.EffectOp
}
case class ExprTarget(receiver: Expr) extends CallTarget

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Expr
case class While(cond: Expr, block: Stmt) extends Expr

sealed trait Clause extends Tree

case class OnResume(prog: BlockArg) extends Clause

case class OnSuspend(prog: Stmt) extends Clause

case class OnReturn(prog: BlockArg) extends Clause

case class FinallyClause(prog: Stmt) extends Clause

case class TryHandle(
  prog: Stmt, handlers: List[Handler], on_suspend: Option[OnSuspend], on_resume: Option[OnResume], on_return: Option[OnReturn]
// Syntactic sugar: finally { s } <=> ... on suspend { s } on return { _ => s }
) extends Expr

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
case class Handler(effect: Effect, capability: Option[CapabilityParam] = None, clauses: List[OpClause]) extends Reference {
  def id = effect.id
  type symbol = symbols.ControlEffect
}
case class OpClause(id: IdRef, params: List[ParamSection], body: Stmt, resume: IdDef) extends Reference {
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
case class FunType(tpe: BlockType) extends ValueType

// Used for both binding and bound vars
case class TypeVar(id: IdRef) extends ValueType with Reference {
  type symbol = symbols.Symbol with symbols.ValueType
}
case class TypeApp(id: IdRef, params: List[ValueType]) extends ValueType with Reference {
  type symbol = symbols.DataType
}

// for now those are not user definable and thus refer to symbols.Effect
case class CapabilityType(eff: symbols.Effect) extends Type {
  type resolved = symbols.CapabilityType
}
case class BlockType(params: List[ValueType], result: ValueType, effects: Effects) extends Type {
  type resolved = symbols.BlockType
}

case class Effect(id: IdRef, tparams: List[ValueType] = Nil) extends Tree with Resolvable {
  // TODO we need to drop Effect <: Symbol and refactor this here
  // TODO maybe we should use Type or something like this instead of Symbol as an upper bound
  type resolved = symbols.Effect
  def resolve(implicit C: Context) = {
    val eff = C.symbolOf(id).asInstanceOf[symbols.Effect]
    if (tparams.isEmpty) eff else symbols.EffectApp(eff, tparams.map(t => C.resolvedType(t)))
  }
}

// We have Effectful as a tree in order to apply code actions on it (see Server.inferEffectsAction)
case class Effectful(tpe: ValueType, eff: Effects) extends Tree

case class Effects(effs: List[Effect]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: Effect*): Effects = Effects(effs.toSet)
  def apply(effs: Set[Effect]): Effects = Effects(effs.toList)
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
    def expr(implicit C: Context): PartialFunction[Expr, Expr] = PartialFunction.empty
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

    def rewrite(e: Expr)(implicit C: Context): Expr = visit(e) {
      case e if expr.isDefinedAt(e) => expr(C)(e)
      case v: Var                   => v
      case l: Literal[t]            => l

      case Assign(id, expr) =>
        Assign(id, rewrite(expr))

      case If(cond, thn, els) =>
        If(rewrite(cond), rewrite(thn), rewrite(els))

      case While(cond, body) =>
        While(rewrite(cond), rewrite(body))

      case MatchExpr(sc, clauses) =>
        MatchExpr(rewrite(sc), clauses.map(rewrite))

      case Call(fun, targs, args) =>
        Call(fun, targs, args.map(rewrite))

      case TryHandle(prog, handlers, suspend, resume, ret) =>
        val suspend_ = suspend map { _ match { case OnSuspend(s) => OnSuspend(rewrite(s)) } }
        val resume_ = resume map { _ match { case OnResume(blkArgRes) => OnResume(rewrite(blkArgRes)) } }
        val ret_ = ret map { _ match { case OnReturn(blkArgRet) => OnReturn(rewrite(blkArgRet)) } }
        TryHandle(rewrite(prog), handlers.map(rewrite), suspend_, resume_, ret_)

      case Hole(stmts) =>
        Hole(rewrite(stmts))

      case Lambda(id, params, body) =>
        Lambda(id, params, rewrite(body))
    }

    def rewrite(t: Def)(implicit C: Context): Def = visit(t) {
      case t if defn.isDefinedAt(t) => defn(C)(t)

      case FunDef(id, tparams, params, ret, body) =>
        FunDef(id, tparams, params, ret, rewrite(body))

      case ValDef(id, annot, binding) =>
        ValDef(id, annot, rewrite(binding))

      case VarDef(id, annot, binding) =>
        VarDef(id, annot, rewrite(binding))

      case d: EffDef        => d
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
      case BlockArg(ps, body) => BlockArg(ps, rewrite(body))
    }

    def rewrite(t: ArgSection)(implicit C: Context): ArgSection = visit(t) {
      case ValueArgs(as)    => ValueArgs(as.map(rewrite))
      case b: BlockArg      => rewrite(b)
      case c: CapabilityArg => c
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
}
