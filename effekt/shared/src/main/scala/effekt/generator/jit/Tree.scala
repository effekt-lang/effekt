package effekt
package generator
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, Symbol, ValueSymbol}

sealed trait Tree

case class Program(
  blocks: List[BasicBlock],
  datatypes: List[String],
  codatatypes: List[String],
  frameSize: FrameDescriptor
) extends Tree

case class BasicBlock(
  id: String,
  frameDescriptor: FrameDescriptor,
  instructions: List[Instruction],
  terminator: Terminator) extends Tree // TODO: frame descriptor

case class VariableDescriptor(typ: Type, id: Register) extends Tree
case class FrameDescriptor(locals: Map[RegisterType, Int]) extends Tree
case class RegList(regs: Map[RegisterType, List[Register]])
object RegList {
  val empty = RegList(Map.empty)
}
case class Clause(params: RegList, target: BlockLabel)

sealed trait Instruction extends Tree
case class Const(out: Register, value: Int) extends Instruction
case class ConstDouble(out: Register, value: scala.Double) extends Instruction
case class ConstString(out: Register, value: String) extends Instruction
case class PrimOp(name: String, out: RegList, in: RegList) extends Instruction
case class Add(out: Register, in1: Register, in2: Register) extends Instruction
case class Mul(out: Register, in1: Register, in2: Register) extends Instruction
case class Push(target: BlockLabel, args: RegList) extends Instruction
case class Shift(out: Register, n: Int) extends Instruction
case class ShiftDyn(out: Register, n: Register) extends Instruction
case class Reset() extends Instruction
case class Print(arg: Register) extends Instruction
case class IfZero(arg: Register, thenClause: Clause) extends Instruction
case class IsZero(out: Register, arg: Register) extends Instruction
case class Subst(args: RegList) extends Instruction
case class Copy(tpe: RegisterType, from: Register, to: Register) extends Instruction
case class Drop(tpe: RegisterType, reg: Register) extends Instruction
case class Swap(tpe: RegisterType, a: Register, b: Register) extends Instruction
case class Allocate(out: Register, tpe: RegisterType, init: Register, region: Register) extends Instruction
case class Load(out: Register, tpe: RegisterType, ref: Register) extends Instruction
case class Store(ref: Register, tpe: RegisterType, value: Register) extends Instruction

case class Construct(out: Register, adt_type: Int, tag: ConstructorTag, args: RegList) extends Instruction
case class PushStack(arg: Register) extends Instruction
case class NewStack(out: Register, region: Register, target: BlockLabel, args: RegList) extends Instruction
case class New(out: Register, targets: List[BlockLabel], args: RegList) extends Instruction

sealed trait Terminator extends Tree
case class Return(args: RegList) extends Terminator
case class Jump(target: BlockLabel) extends Terminator
case class Resume(cont: Register) extends Terminator
case class Match(adt_type: Int, scrutinee: Register, clauses: List[Clause], default: Clause) extends Terminator
case class Invoke(receiver: Register, tag: MethodTag, args: RegList) extends Terminator

enum Type extends Tree {
  case Unit()
  case Continuation()
  case Integer()
  case Double()
  case String()
  case Datatype(index: Int)
  case Codata(index: Int)
  case Region()
  case Reference(to: Type)

  def registerType: RegisterType = this match {
    case Unit() => RegisterType.Erased
    case Continuation() => RegisterType.Ptr
    case Integer() => RegisterType.Number
    case Double() => RegisterType.Number
    case String() => RegisterType.Ptr
    case Codata(index) => RegisterType.Ptr
    case Datatype(index) => RegisterType.Ptr
    case Region() => RegisterType.Ptr
    case Reference(_) => RegisterType.Ptr
  }
}

enum RegisterType {
  case Number, Erased, Ptr

  def isErased: Boolean = this match {
    case Erased => true
    case _ => false
  }
}
object RegisterType {
  def valuesNonErased = RegisterType.values.filterNot(_.isErased)
}

type ConstructorTag = Int
type MethodTag = Int

sealed trait Register extends Tree
case class NamedRegister(name: String) extends Register
case class RegisterIndex(index: Int) extends Register
case class ErasedRegister() extends Register

sealed trait BlockLabel

/**
 * Are used to represent named blocks from machine [[machine.Def]]; can potentially
 * model forward references (during transformation).
 *
 * Invariant: After post-processing ([[BlockNumbering]]) all block names are replaced by their indices ([[BlockIndex]])
 */
case class BlockName(name: String) extends BlockLabel

case class BlockIndex(index: Int) extends BlockLabel
