package effekt
package cps

import core.{ Id, ValueType, BlockType, Captures }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter
import effekt.util.messages.INTERNAL_ERROR


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

  case App(callee: Block, vargs: List[Pure], bargs: List[Block], ks: MetaCont, k: Cont)

  case Invoke(callee: Block, method: Id, vargs: List[Pure], bargs: List[Block], ks: MetaCont, k: Cont)

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, Clause)], default: Option[Stmt])

  case LetDef(id: Id, binding: Block, body: Stmt)
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

  // resume(r, (ks, k) => STMT, ks, k)
  case Resume(resumption: Id, body: BlockLit, ks: MetaCont, k: Cont)

  // Others
  case Hole()
}
export Stmt.*

case class Clause(vparams: List[Id], body: Stmt) extends Tree

enum Cont extends Tree {
  case ContVar(id: Id)
  case ContLam(result: Id, ks: Id, body: Stmt)
  case Abort
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
    case Stmt.App(callee, vargs, bargs, ks, k) => free(callee) ++ all(vargs, free) ++ all(bargs, free) ++ free(ks) ++ free(k)
    case Stmt.Invoke(callee, method, vargs, bargs, ks, k) => free(callee) ++ all(vargs, free) ++ all(bargs, free) ++ free(ks) ++ free(k)
    case Stmt.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Stmt.Match(scrutinee, clauses, default) => free(scrutinee) ++ all(clauses, free) ++ all(default, free)
    case Stmt.LetDef(id, binding, body)  => (free(binding) ++ free(body)) -- block(id)
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
    case Stmt.Resume(r, body, ks, k) => block(r) ++ free(body) ++ free(ks) ++ free(k)
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
    case Cont.Abort => Set()
    case Cont.ContLam(result, ks, body) => free(body) -- value(result) -- meta(ks)
  }
}


object substitutions {

  case class Substitution(
    values: Map[Id, Pure] = Map.empty,
    blocks: Map[Id, Block] = Map.empty,
    conts: Map[Id, Cont] = Map.empty,
    metaconts: Map[Id, MetaCont] = Map.empty
  ) {
    def shadowValues(shadowed: IterableOnce[Id]): Substitution = copy(values = values -- shadowed)
    def shadowBlocks(shadowed: IterableOnce[Id]): Substitution = copy(blocks = blocks -- shadowed)
    def shadowConts(shadowed: IterableOnce[Id]): Substitution = copy(conts = conts -- shadowed)
    def shadowMetaconts(shadowed: IterableOnce[Id]): Substitution = copy(metaconts = metaconts -- shadowed)

    def shadowParams(vparams: Seq[Id], bparams: Seq[Id]): Substitution =
      copy(values = values -- vparams, blocks = blocks -- bparams)
  }

  def substitute(expression: Expr)(using Substitution): Expr = expression match {
    case DirectApp(id, vargs, bargs) =>
      DirectApp(id, vargs.map(substitute), bargs.map(substitute))
    case p: Pure => substitute(p)
  }

  def substitute(pure: Pure)(using subst: Substitution): Pure = pure match {
    case ValueVar(id) if subst.values.isDefinedAt(id) => subst.values(id)
    case ValueVar(id) => ValueVar(id)
    case Literal(value) => Literal(value)
    case Make(tpe, tag, vargs) => Make(tpe, tag, vargs.map(substitute))
    case PureApp(id, vargs) => PureApp(id, vargs.map(substitute))
    case Box(b) => Box(substitute(b))
  }

  def substitute(block: Block)(using subst: Substitution): Block = block match {
    case BlockVar(id) if subst.blocks.isDefinedAt(id) => subst.blocks(id)
    case BlockVar(id) => BlockVar(id)
    case b: BlockLit => substitute(b)
    case Unbox(pure) => Unbox(substitute(pure))
    case New(impl) => New(substitute(impl))
  }

  def substitute(b: BlockLit)(using subst: Substitution): BlockLit = b match {
    case BlockLit(vparams, bparams, ks, k, body) =>
      BlockLit(vparams, bparams, ks, k,
        substitute(body)(using subst
          .shadowParams(vparams, bparams)
          .shadowMetaconts(List(ks))
          .shadowConts(List(k))))
  }

  def substitute(stmt: Stmt)(using subst: Substitution): Stmt = stmt match {
    case Jump(k, arg, ks) =>
      Jump(
        substituteAsContVar(k),
        substitute(arg),
        substitute(ks))

    case App(callee, vargs, bargs, ks, k) =>
      App(
        substitute(callee),
        vargs.map(substitute),
        bargs.map(substitute),
        substitute(ks),
        substitute(k))

    case Invoke(callee, method, vargs, bargs, ks, k) =>
      Invoke(
        substitute(callee),
        method,
        vargs.map(substitute),
        bargs.map(substitute),
        substitute(ks),
        substitute(k))

    case If(cond, thn, els) =>
      If(substitute(cond), substitute(thn), substitute(els))

    case Match(scrutinee, clauses, default) =>
      Match(
        substitute(scrutinee),
        clauses.map { case (id, cl) => (id, substitute(cl)) },
        default.map(substitute))

    case LetDef(id, binding, body) =>
      LetDef(id, substitute(binding),
        substitute(body)(using subst.shadowBlocks(List(id))))

    case LetExpr(id, binding, body) =>
      LetExpr(id, substitute(binding),
        substitute(body)(using subst.shadowValues(List(id))))

    case LetCont(id, binding, body) =>
      LetCont(id, substitute(binding),
        substitute(body)(using subst.shadowConts(List(id))))

    case Region(id, ks, body) =>
      Region(id, substitute(ks),
        substitute(body)(using subst.shadowBlocks(List(id))))

    case Alloc(id, init, region, body) =>
      Alloc(id, substitute(init), substituteAsBlockVar(region),
        substitute(body)(using subst.shadowBlocks(List(id))))

    case Var(id, init, ks, body) =>
      Var(id, substitute(init), substitute(ks),
        substitute(body)(using subst.shadowBlocks(List(id))))

    case Dealloc(ref, body) =>
      Dealloc(substituteAsBlockVar(ref), substitute(body))

    case Get(ref, id, body) =>
      Get(substituteAsBlockVar(ref), id,
        substitute(body)(using subst.shadowValues(List(id))))

    case Put(ref, value, body) =>
      Put(substituteAsBlockVar(ref), substitute(value), substitute(body))

    case Reset(prog, ks, k) =>
      Reset(substitute(prog), substitute(ks), substitute(k))

    case Shift(prompt, body, ks, k) =>
      Shift(substituteAsBlockVar(prompt), substitute(body), substitute(ks), substitute(k))

    case Resume(r, body, ks, k) =>
      Resume(substituteAsBlockVar(r), substitute(body), substitute(ks), substitute(k))

    case h: Hole => h
  }

  def substitute(impl: Implementation)(using Substitution): Implementation = impl match {
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(substitute))
  }

  def substitute(op: Operation)(using subst: Substitution): Operation = op match {
    case Operation(name, vparams, bparams, ks, k, body) =>
      Operation(name, vparams, bparams, ks, k,
        substitute(body)(using subst
          .shadowParams(vparams, bparams)
          .shadowMetaconts(List(ks))
          .shadowConts(List(k))))
  }

  def substitute(clause: Clause)(using subst: Substitution): Clause = clause match {
    case Clause(vparams, body) =>
      Clause(vparams, substitute(body)(using subst.shadowValues(vparams)))
  }

  def substitute(k: Cont)(using subst: Substitution): Cont = k match {
    case Cont.ContVar(id) if subst.conts.isDefinedAt(id) => subst.conts(id)
    case Cont.ContVar(id) => Cont.ContVar(id)
    case lam @ Cont.ContLam(result, ks, body) => substitute(lam)
    case Cont.Abort => Cont.Abort
  }

  def substitute(k: Cont.ContLam)(using subst: Substitution): Cont.ContLam = k match {
    case Cont.ContLam(result, ks, body) =>
      Cont.ContLam(result, ks,
        substitute(body)(using subst
          .shadowValues(List(result))
          .shadowMetaconts(List(ks))))
  }

  def substitute(ks: MetaCont)(using subst: Substitution): MetaCont =
    subst.metaconts.getOrElse(ks.id, ks)

  def substituteAsBlockVar(id: Id)(using subst: Substitution): Id =
    subst.blocks.get(id) map {
      case BlockVar(x) => x
      case _ => INTERNAL_ERROR("References should always be variables")
    } getOrElse id

  def substituteAsContVar(id: Id)(using subst: Substitution): Id =
    subst.conts.get(id) map {
      case Cont.ContVar(x) => x
      case _ => INTERNAL_ERROR("Continuation references should always be variables")
    } getOrElse id
}
