package effekt
package cps

import core.{ Id, ValueType, BlockType, Captures }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter


sealed trait Tree

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
  case Val(id: Id, binding: Stmt) // this is a let-run
  case Let(id: Id, binding: Pure)
}

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  // TODO async defs!
  case Def(id: Id, vparams: List[Id], bparams: List[Id], annotatedCapture: Captures, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
sealed trait ExternBody extends Tree
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Pure]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody {
    def report(using E: ErrorReporter): Unit = E.report(err)
  }
}

case class Def(id: Id, block: Block)

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

  // (Type-monomorphic?) Regions
  //  case Region(body: Block)
  //  case Alloc(id: Id, init: Pure, region: Id, body: Stmt)

  // creates a fresh state handler to model local (backtrackable) state.
  // [[capture]] is a binding occurence.
  // e.g. state(init) { [x]{x: Ref} => ... }
  //  case Var(id: Id, init: Expr, capture: Id, body: Stmt)
  //  case Get(id: Id, annotatedCapt: Captures, annotatedTpe: ValueType)
  //  case Put(id: Id, annotatedCapt: Captures, value: Pure)

  // reset( { (p, ks, k) => STMT }, ks, k)
  case Reset(prog: BlockLit, ks: MetaCont, k: Cont)

  // TODO bidirectional
  // shift(p, { (resume, ks, k) => STMT }, ks, k)
  case Shift(prompt: Id, body: BlockLit, ks: MetaCont, k: Cont)

  // Others
  case Hole()
}
export Stmt.*

case class Clause(vparams: List[Id], body: Stmt)

enum Cont {
  case ContVar(id: Id)
  case ContLam(result: Id, ks: Id, body: Stmt)
}

case class MetaCont(id: Id)

case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree

case class Operation(name: Id, vparams: List[Id], bparams: List[Id], ks: Id, k: Id, body: Stmt)
