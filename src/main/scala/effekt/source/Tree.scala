package effekt
package source

/**
 * We extend product to allow reflective copying by Kiama.
 */
sealed trait Tree extends Product

/**
 * We distinguish between identifiers corresponding to
 * - binding sites (IdDef)
 * - use site (IdRef)
 * in the syntax. This way, we can simplify the traversal in Namer
 */
sealed trait Id extends Tree {
  def name: String
}
case class IdDef(name: String) extends Id
case class IdRef(name: String) extends Id

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
case class ValueParam(id: Id, tpe: Option[Type]) extends Tree
case class BlockParam(id: Id, tpe: BlockType) extends ParamSection

sealed trait ArgSection extends Tree
case class ValueArgs(args: List[Expr]) extends ArgSection
case class BlockArg(params: List[ValueParam], body: Stmt) extends ArgSection


/**
 * Global (and later, local) definitions
 */
sealed trait Def extends Tree {
  def id: Id
}
case class FunDef(id: Id, tparams: List[Id], params: List[ParamSection], ret: Option[Effectful], body: Stmt) extends Def
case class EffDef(id: Id, tparams: List[Id], params: List[ValueParams], ret: Type) extends Def
case class ValDef(id: Id, annot: Option[Type], binding: Stmt) extends Def
case class VarDef(id: Id, annot: Option[Type], binding: Stmt) extends Def

case class DataDef(id: Id, tparams: List[Id], ctors: List[Constructor]) extends Def
case class Constructor(id: Id, params: List[ValueParams]) extends Tree

// only valid on the toplevel!
case class ExternType(id: Id, tparams: List[Id]) extends Def
case class ExternEffect(id: Id, tparams: List[Id]) extends Def
case class ExternFun(pure: Boolean, id: Id, tparams: List[Id], params: List[ParamSection], ret: Effectful, body: String) extends Def
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
case class Var(id: Id) extends Expr
case class Assign(id: Id, expr: Expr) extends Expr

sealed trait Literal[T] extends Expr {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

sealed trait InvokeExpr extends Expr
case class Call(fun: Id, targs: List[Type], args: List[ArgSection]) extends InvokeExpr

// for uniformity also pass targs here (maybe empty at first)
//case class Yield(block: Id, args: List[ArgSection]) extends InvokeExpr

// TODO remove special case for resume -- it is just a call
//case class Resume(args: ArgSection) extends InvokeExpr

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Expr
case class While(cond: Expr, block: Stmt) extends Expr

case class TryHandle(prog: Stmt, clauses: List[OpClause]) extends Expr
case class OpClause(op: Id, params: List[ValueParams], body: Stmt, resume: IdDef = IdDef("resume")) extends Tree

case class MatchExpr(matchee: Expr, clauses: List[Clause]) extends Expr
case class Clause(op: Id, params: List[ValueParams], body: Stmt) extends Tree

/**
 * Types and Effects
 *
 * TODO generalize to blocks that can take blocks
 */
sealed trait Type extends Tree
case class TypeVar(id: Id) extends Type
case class TypeApp(id: Id, params: List[Type]) extends Type
case class BlockType(params: List[Type], ret: Effectful) extends Type

case class Effect(id: Id) extends Tree
case class Effectful(tpe: Type, eff: Effects) extends Tree

case class Effects(effs: List[Effect]) extends Tree
object Effects {
  val Pure: Effects = Effects()
  def apply(effs: Effect*): Effects = Effects(effs.toSet)
  def apply(effs: Set[Effect]): Effects = Effects(effs.toList)
}

/**
 * Traversal Utils
 */

object traversal {

  type Traversal[T, Ctx] = (given Ctx) => T => Unit

  def all[Ctx](traverse: Traversal[Tree, Ctx]): Traversal[Any, Ctx] = {
    case p: Product => p.productIterator.foreach {
      case t: Tree => traverse(t)
      case other => all(traverse)(other)
    }
    case t: Traversable[t] => t.foreach(all(traverse))
    case leaf => ()
  }

  type Fold[T, R, Ctx] = (given Ctx) => T => R => R

  def foldAll[R, Ctx](f: Fold[Tree, R, Ctx]): Fold[Any, R, Ctx] = {
    case p: Product => r => p.productIterator.foldLeft(r) {
      case (r, t: Tree) => f(t)(r)
      case (r, other) => foldAll(f)(other)(r)
    }
    case t: Traversable[_] => r => t.foldLeft(r) { (r, t) => foldAll(f)(t)(r) }
    case leaf => r => r
  }
}