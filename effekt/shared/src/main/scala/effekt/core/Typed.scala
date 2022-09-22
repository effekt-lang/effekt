package effekt
package core
package typed

import effekt.core.typed.ValueType.Boxed
import effekt.lifted.BlockLit


// TODO add
// - Match
// - State
// - Region
// - Hole

type Name = String

// A unique type symbol
type TypeSymbol = String

// TODO TBD
// TODO forbid local datatype, effect, and extern declarations in source.

type Declaration = Unit

case class Module(
  declarations: Map[TypeSymbol, Declaration],
  externals: List[Extern],
  definitions: List[Definition]
)

enum Extern {
  case Include(contents: String)
  case Function(
    name: Name,
    tparams: List[ValueType.FreeVar],
    vparams: List[(Name, ValueType)],
    bparams: List[(Name, BlockType)],
    result: ValueType,
    capture: Captures,
    body: String)
}

sealed trait Term

sealed trait Expression extends Term {
  val tpe: ValueType = typing.inferType(this)
  val capt: Captures = typing.inferCapt(this)
}
object Expression {
  // invariant: block `callee` and all bargs have capt {io}.
  case class DirectApp(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]) extends Expression

  // invariant: only inserted by the transformer if stmt is pure / io
  case class Run(stmt: Statement) extends Expression
}

enum Pure extends Expression {
  case ValueVar(name: Name, annotatedType: ValueType)
  case Literal(value: Any, annotatedType: ValueType)
  case Box(block: Block, annotatedCapture: Captures)
  case PureApp(callee: Block, targs: List[ValueType], vargs: List[Pure])
}

/**
 * Definitions that can be mutually recursive
 */
case class Definition(name: Name, binding: Block) {
  val tpe = binding.tpe
  val capt = binding.capt
}

/**
 * Implementation of a method / effect operation.
 *
 * Note that this is very similar to [[Definition]], but at the moment we restrict
 * implementations to be block literals. The reason is, that when implemented as
 * part of an effect handler, an effect operation can capture the continuation.
 *
 * The backends thus make assumptions about the shape of the implementation to
 * provide the additional continuation argument.
 */
case class Operation(name: Name, implementation: Block.BlockLit) {
  val tpe = implementation.tpe
  val capt = implementation.capt
}

/**
 * An instance of type [[interface]], concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Instance(interface: BlockType.Interface, operations: List[Operation]) {
  val tpe = interface
  val capt = operations.flatMap(_.capt).toSet
}

enum Statement extends Term {
  case Val(name: Name, binding: Statement, body: Statement)
  case If(cond: Pure, thn: Statement, els: Statement)
  case While(cond: Statement, body: Statement)
  case Scope(bindings: List[Definition], body: Statement)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])
  case Try(body: Block.BlockLit, handlers: List[Instance])
  case Return(expr: Pure)

  val tpe: ValueType = typing.inferType(this)
  val capt: Captures = typing.inferCapt(this)
}

enum Block extends Term {
  case BlockVar(name: Name, annotatedType: BlockType, annotatedCapt: Captures)
  case BlockLit(tparams: List[ValueType.FreeVar], vparams: List[(Name, ValueType)], bparams: List[(Name, BlockType)], body: Statement)
  case Member(receiver: Block, selector: Name, annotatedType: BlockType)
  case New(instance: Instance)
  case Unbox(expr: Pure)

  val tpe: BlockType = typing.inferType(this)
  val capt: Captures = typing.inferCapt(this)
}


sealed trait Type

enum BlockType extends Type {
  case Function(tpeArity: Int, vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(symbol: TypeSymbol, targs: List[ValueType])
  // TODO what about builtin blocktypes???
}
object BlockType {
  def Function(tparams: List[ValueType.FreeVar], vparams: List[(Name, ValueType)], bparams: List[(Name, BlockType)], result: ValueType): BlockType.Function =
    BlockType.Function(tparams.size,
      vparams.map { case (_, tpe) => typing.close(tpe, tparams, Nil, 0) },
      bparams.map { case (_, tpe) => typing.close(tpe, tparams, Nil, 0) },
      typing.close(result, tparams, bparams.map(_._1), 0))
}

/**
 * Types are represented in "locally nameless" style. That is,
 * - bound variables are represented by DeBruijn indices [[ValueType.BoundVar]],
 * - and free type variables are represented with names [[ValueType.FreeVar]].
 *
 * [[ValueType.Builtin]] models builtin types like String, Int, etc.; they might take type parameters (like Array)
 * [[ValueType.DataType]] represents user defined data types. Their declaration is stored in a (global) symbol table.
 * [[ValueType.Boxed]] represent block types, which are boxed to become first class.
 */
enum ValueType extends Type {
  case FreeVar(name: Name)
  case BoundVar(level: Int, index: Int)
  case Builtin(name: Name, targs: List[ValueType])
  case DataType(symbol: TypeSymbol, targs: List[ValueType])
  case Boxed(tpe: BlockType, capt: Captures)
}

enum Capture {
  case FreeVar(name: Name)
  case BoundVar(level: Int, index: Int)
}
type Captures = Set[Capture]

/**
 * This is NOT a type checker. It simply *infers* the type of a given expression, statement, ...
 *
 * The big difference is, that if a transformation generates ill-typed core terms, then this
 * inference will NOT discover it.
 */
object typing {

  def inferType(expr: Expression): ValueType = expr match {
    case Expression.DirectApp(callee, targs, vargs, bargs) => instantiate(callee.tpe.asInstanceOf, targs, bargs.map(_.capt)).result
    case Expression.Run(s) => s.tpe
    case Pure.ValueVar(name, tpe) => tpe
    case Pure.Literal(value, tpe) => tpe
    case Pure.Box(block, capt) => ValueType.Boxed(block.tpe, capt)
    case Pure.PureApp(callee, targs, vargs) => instantiate(callee.tpe.asInstanceOf, targs, Nil).result
  }

  // invariant: can only be {} or {io}
  def inferCapt(expr: Expression): Captures = expr match {
    case Expression.DirectApp(callee, targs, vargs, bargs) => callee.capt ++ bargs.flatMap(_.capt).toSet
    case Expression.Run(s) => s.capt
    case pure: Pure => Set.empty
  }

  def inferType(stmt: Statement): ValueType = stmt match {
    case Statement.Return(expr) => expr.tpe
    case Statement.Val(name, binding, body) => body.tpe
    case Statement.Scope(defs, body) => body.tpe
    case Statement.If(cond, thn, els) => thn.tpe // TODO join.
    case Statement.While(cond, body) => body.tpe // TODO should always be TUnit
    case Statement.App(callee, targs, vargs, bargs) =>
      instantiate(callee.tpe.asInstanceOf, targs, bargs.map(_.capt)).result
    case Statement.Try(body, handlers) =>
      body.tpe.asInstanceOf[BlockType.Function].result
  }

  def inferCapt(stmt: Statement): Captures = stmt match {
    case Statement.Return(expr) => Set.empty
    case Statement.Val(name, binding, body) => binding.capt ++ body.capt
    case Statement.Scope(defs, body) => defs.flatMap(_.capt).toSet ++ body.capt
    case Statement.If(cond, thn, els) => thn.capt ++ els.capt
    case Statement.While(cond, body) => cond.capt ++ body.capt
    case Statement.App(callee, targs, vargs, bargs) => callee.capt ++ bargs.flatMap(_.capt).toSet
    case Statement.Try(body, handlers) => body.capt ++ handlers.flatMap(_.capt).toSet
  }

  def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(name, tpe, capt) => tpe
    case Block.BlockLit(tparams, vparams, bparams, body) =>
      BlockType.Function(tparams, vparams, bparams, body.tpe)

    case Block.New(instance) => instance.tpe

    case Block.Member(recv, sel, tpe) => tpe
      //      val tpe = recv.tpe.asInstanceOf[BlockType.Interface] // TODO could be an applied type...
      //      tpe.operations(sel)

    case Block.Unbox(expr) => expr.tpe.asInstanceOf[ValueType.Boxed].tpe
  }

  def inferCapt(block: Block): Captures = block match {
    case Block.BlockVar(name, tpe, capt) => capt
    case Block.Member(recv, sel, tpe) => recv.capt
    case Block.BlockLit(tparams, vparams, bparams, body) => body.capt -- bparams.map { case (name, _) => Capture.FreeVar(name) }
    case Block.Unbox(expr) => expr.tpe.asInstanceOf[ValueType.Boxed].capt
    case Block.New(instance) => instance.capt
  }

  // Tooling...

  def close(tpe: ValueType, typeVars: List[ValueType.FreeVar], blockVars: List[Name], level: Int): ValueType = tpe match {
    case f : ValueType.FreeVar if typeVars.contains(f) =>
      ValueType.BoundVar(level, typeVars.indexOf(f))

    case f: ValueType.FreeVar => f

    // bump all other bound var
    case ValueType.BoundVar(level, index) =>
      ValueType.BoundVar(level + 1, index)

    case ValueType.Boxed(tpe, capt) =>
      ValueType.Boxed(close(tpe, typeVars, blockVars, level), close(capt, blockVars, level))

    case ValueType.Builtin(name, targs) =>
      ValueType.Builtin(name, targs map { tpe => close(tpe, typeVars, blockVars, level) })

    case ValueType.DataType(name, targs) =>
      ValueType.DataType(name, targs map { tpe => close(tpe, typeVars, blockVars, level) })
  }

  def close(tpe: BlockType, typeVars: List[ValueType.FreeVar], blockVars: List[Name], level: Int): BlockType = tpe match {
    case BlockType.Function(tpeArity, vparams, bparams, result) =>
      BlockType.Function(tpeArity,
        vparams map { tpe => close(tpe, typeVars, blockVars, level + 1) },
        bparams map { tpe => close(tpe, typeVars, blockVars, level + 1) },
        close(result, typeVars, blockVars, level + 1))

    case BlockType.Interface(name, targs) =>
      BlockType.Interface(name, targs map { tpe => close(tpe, typeVars, blockVars, level) })
  }

  def close(capt: Captures, blockVars: List[Name], level: Int): Captures = capt map {
    case Capture.FreeVar(f) if blockVars.contains(f) =>
      Capture.BoundVar(level, blockVars.indexOf(f))
    case Capture.BoundVar(level, index) =>
      Capture.BoundVar(level + 1, index)
    case c: Capture.FreeVar => c
  }

  def instantiate(f: BlockType.Function, targs: List[ValueType], cargs: List[Captures]): BlockType.Function = f match {
    case tpe @ BlockType.Function(n, vparams, bparams, result) =>
      if (targs.size != n) sys error "Wrong number of type arguments"
      BlockType.Function(0,
        vparams map { tpe => substitute(tpe, targs, Nil, 0) },
        bparams map { tpe => substitute(tpe, targs, Nil, 0) },
        substitute(result, targs, cargs, 0))
  }


  // [A](a: A) => [B]() => (A, B)
  // subst {0,0} !-> Int  into   [1]({0,0}) => [1]() => ({1,0}, {0,0})
  // [0](Int) => [1]() => (Int, {0,0})

  def substitute(tpe: ValueType, vsubst: List[ValueType], csubst: List[Captures], level: Int): ValueType = tpe match {
    case ValueType.BoundVar(lvl, idx) if level == lvl =>
      vsubst(idx)
    case ValueType.Boxed(tpe, capt) =>
      ValueType.Boxed(substitute(tpe, vsubst, csubst, level), substitute(capt, csubst, level))

    // all other cases, leave as is
    case tpe => tpe
  }

  def substitute(capt: Captures, csubst: List[Captures], level: Int): Captures =
    capt flatMap {
      case Capture.BoundVar(lvl, idx) if level == lvl => csubst(idx)
      case other => Set(other)
    }

  def substitute(tpe: BlockType, vsubst: List[ValueType], csubst: List[Captures], level: Int): BlockType = tpe match {
    case BlockType.Function(arity, vparams, bparams, result) =>
      BlockType.Function(arity,
        vparams map { tpe => substitute(tpe, vsubst, csubst, level + 1) }, // TODO check! The level is technically not correct for captures.
        bparams map { tpe => substitute(tpe, vsubst, csubst, level + 1) },
        substitute(result, vsubst, csubst, level + 1))

    case b : BlockType.Interface => ???
  }
}

object examples extends scala.App {

  val TInt = ValueType.Builtin("Int", Nil)
  val TBool = ValueType.Builtin("Bool", Nil)

  val polymorphicIdentity: BlockType.Function  = BlockType.Function(1, List(ValueType.BoundVar(0, 0)), Nil, ValueType.BoundVar(0, 0))

  val polymorphicIdentity2: BlockType.Function = BlockType.Function(1, List(ValueType.BoundVar(0, 0)), Nil, ValueType.BoundVar(0, 0))

  println(polymorphicIdentity == polymorphicIdentity2)

  println(typing.instantiate(polymorphicIdentity, List(TInt), Nil))

  println(typing.instantiate(polymorphicIdentity, List(TInt), Nil) == typing.instantiate(polymorphicIdentity, List(TInt), Nil))
  println(typing.instantiate(polymorphicIdentity, List(TInt), Nil) == typing.instantiate(polymorphicIdentity, List(TBool), Nil))

  val call = Statement.App(Block.BlockVar("f", polymorphicIdentity, Set.empty), List(TInt), List(Pure.Literal(42, TInt)), Nil)

  println(call.tpe)

  val id = Block.BlockLit(
    List(ValueType.FreeVar("A")),
    List(("a", ValueType.FreeVar("A"))),
    Nil,
    Statement.Return(Pure.ValueVar("a", ValueType.FreeVar("A"))))

  val call2 = Statement.App(id, List(TInt), List(Pure.Literal(42, TInt)), Nil)

  println(call2.tpe)

  val blockTypePoly: BlockType.Function =
    BlockType.Function(1, List(ValueType.BoundVar(0, 0)), List(polymorphicIdentity),
      ValueType.Boxed(polymorphicIdentity, Set(Capture.BoundVar(0, 0))))

  println(typing.instantiate(blockTypePoly, List(TInt), List(Set(Capture.FreeVar("exc")))))
}
