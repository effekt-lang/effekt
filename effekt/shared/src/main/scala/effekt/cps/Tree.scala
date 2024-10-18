package effekt
package cps

import core.{ Id, ValueType, BlockType, Captures }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter


sealed trait Tree extends Product {
  /**
   * The number of nodes of this tree (used by inlining heuristics)
   */
  lazy val size: Int = {
    var nodeCount = 1

    def all(t: IterableOnce[_]): Unit = t.iterator.foreach(one)
    def one(obj: Any): Unit = obj match {
      case t: Tree => nodeCount += t.size
      case s: effekt.symbols.Symbol => ()
      case p: Product => all(p.productIterator)
      case t: Iterable[t] => all(t)
      case leaf           => ()
    }
    this.productIterator.foreach(one)
    nodeCount
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  includes: List[String],
  declarations: List[core.Declaration],
  externs: List[Extern],
  definitions: List[ToplevelDefinition],
  exports: List[Id]
) extends Tree

enum ToplevelDefinition {
  case Def(id: Id, block: Block)
  case Val(id: Id, ks: Id, k: Id, binding: Stmt) // this is a let-run
  case Let(id: Id, binding: Pure)
}

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, vparams: List[Id], bparams: List[Id], async: Boolean, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
sealed trait ExternBody extends Tree
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Pure]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody {
    def report(using E: ErrorReporter): Unit = E.report(err)
  }
}

case class Def(id: Id, block: Block) extends Tree

sealed trait Expr extends Tree

/**
 * Impure FFI calls.
 */
case class DirectApp(id: Id, vargs: List[Pure], bargs: List[Block]) extends Expr

enum Pure extends Expr {

  case ValueVar(id: Id)

  case Literal(value: Any)

  /**
   * Pure FFI calls. Invariant, block b is pure.
   */
  case PureApp(id: Id, vargs: List[Pure])

  case Make(data: ValueType.Data, tag: Id, vargs: List[Pure])

  /**
   * Record Selection
   */
  case Select(target: Pure, field: Id)

  case Box(b: Block)
}
export Pure.*


enum Block extends Tree {
  case BlockVar(id: Id)
  case BlockLit(vparams: List[Id], bparams: List[Id], ks: Id, k: Id, body: Stmt)
  case Unbox(pure: Pure)
  case New(impl: Implementation)
}
export Block.*

enum Stmt extends Tree {

  case Jump(k: Id, arg: Pure, ks: MetaCont) // if the continuation is known, we inline and don't jump

  // these could in principle be mutually recursive
  case Scope(definitions: List[Def], body: Stmt)

  case App(callee: Block, vargs: List[Pure], bargs: List[Block], ks: MetaCont, k: Cont)

  case Invoke(callee: Block, method: Id, vargs: List[Pure], bargs: List[Block], ks: MetaCont, k: Cont)

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, Clause)], default: Option[Stmt])

  case LetExpr(id: Id, binding: Expr, body: Stmt)
  case LetCont(id: Id, binding: Cont.ContLam, body: Stmt)

  // Regions
  case Region(id: Id, ks: MetaCont, body: Stmt)
  case Alloc(id: Id, init: Pure, region: Id, body: Stmt)

  // creates a fresh state handler to model local (backtrackable) state.
  // [[capture]] is a binding occurence.
  // val id = ks.fresh(init); body
  case Var(id: Id, init: Pure, ks: MetaCont, body: Stmt)
  // dealloc(ref); body
  case Dealloc(ref: Id, body: Stmt)

  // val id = !ref; body
  case Get(ref: Id, id: Id, body: Stmt)

  case Put(ref: Id, value: Pure, body: Stmt)

  // reset( { (p, ks, k) => STMT }, ks, k)
  case Reset(prog: BlockLit, ks: MetaCont, k: Cont)

  // shift(p, { (resume, ks, k) => STMT }, ks, k)
  case Shift(prompt: Id, body: BlockLit, ks: MetaCont, k: Cont)

  // Others
  case Hole()
}
export Stmt.*

case class Clause(vparams: List[Id], body: Stmt) extends Tree

enum Cont extends Tree {
  case ContVar(id: Id)
  case ContLam(result: Id, ks: Id, body: Stmt)
}

case class MetaCont(id: Id) extends Tree

case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree

case class Operation(name: Id, vparams: List[Id], bparams: List[Id], ks: Id, k: Id, body: Stmt) extends Tree


// unless we need some more information, we keep it simple for now:
type Variable = Id
type Variables = Set[Id]

object Variables {

  def value(id: Id) = Set(id)
  def block(id: Id) = Set(id)
  def cont(id: Id) = Set(id)
  def meta(id: Id) = Set(id)

  def empty: Variables = Set.empty

  def all[T](t: IterableOnce[T], f: T => Variables): Variables =
    t.iterator.foldLeft(Variables.empty) { case (xs, t) => f(t) ++ xs }


  def free(e: Expr): Variables = e match {
    case DirectApp(id, vargs, bargs) => block(id) ++ all(vargs, free) ++ all(bargs, free)
    case Pure.ValueVar(id) => value(id)
    case Pure.Literal(value) => empty
    case Pure.PureApp(id, vargs) => block(id) ++ all(vargs, free)
    case Pure.Make(data, tag, vargs) => all(vargs, free)
    case Pure.Select(target, field) => free(target)
    case Pure.Box(b) => free(b)
  }

  def free(b: Block): Variables = b match {
    case Block.BlockVar(id) => block(id)
    case Block.BlockLit(vparams, bparams, ks, k, body) => free(body) -- vparams -- bparams - ks - k
    case Block.Unbox(pure) => free(pure)
    case Block.New(impl) => free(impl)
  }

  def free(impl: Implementation): Variables = all(impl.operations, free)

  def free(op: Operation): Variables = op match {
    case Operation(name, vparams, bparams, ks, k, body) =>
      free(body) -- all(vparams, value) -- all(bparams, block) -- meta(ks) -- cont(k)
  }
  def free(s: Stmt): Variables = s match {
    case Stmt.Jump(k, arg, ks) => cont(k) ++ free(arg) ++ free(ks)
    case Stmt.Scope(definitions, body) =>
      all(definitions, free) ++ free(body) -- all(definitions, d => block(d.id))
    case Stmt.App(callee, vargs, bargs, ks, k) => free(callee) ++ all(vargs, free) ++ all(bargs, free) ++ free(ks) ++ free(k)
    case Stmt.Invoke(callee, method, vargs, bargs, ks, k) => free(callee) ++ all(vargs, free) ++ all(bargs, free) ++ free(ks) ++ free(k)
    case Stmt.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Stmt.Match(scrutinee, clauses, default) => free(scrutinee) ++ all(clauses, free) ++ all(default, free)
    case Stmt.LetExpr(id, binding, body) => free(binding) ++ (free(body) -- value(id))
    case Stmt.LetCont(id, binding, body) => free(binding) ++ (free(body) -- cont(id))

    case Stmt.Region(id, ks, body) => free(ks) ++ (free(body) -- block(id))
    case Stmt.Alloc(id, init, region, body) => free(init) ++ block(region) ++ (free(body) -- block(id))

    case Stmt.Var(id, init, ks, body) => free(init) ++ free(ks) ++ (free(body) -- block(id))
    case Stmt.Dealloc(ref, body) => block(ref) ++ free(body)
    case Stmt.Get(ref, id, body) => block(ref) ++ (free(body) -- value(id))
    case Stmt.Put(ref, value, body) => block(ref) ++ free(value) ++ free(body)

    case Stmt.Reset(prog, ks, k) => free(prog) ++ free(ks) ++ free(k)
    case Stmt.Shift(prompt, body, ks, k) => block(prompt) ++ free(body) ++ free(ks) ++ free(k)
    case Stmt.Hole() => empty
  }

  def free(cl: (Id, Clause)): Variables = cl match {
    case (_, Clause(vparams, body)) => free(body) -- all(vparams, value)
  }

  def free(d: Def): Variables = d match {
    case Def(id, binding) => free(binding) -- block(id)
  }

  def free(ks: MetaCont): Variables = meta(ks.id)
  def free(k: Cont): Variables = k match {
    case Cont.ContVar(id) => cont(id)
    case Cont.ContLam(result, ks, body) => free(body) -- value(result) -- meta(ks)
  }
}
