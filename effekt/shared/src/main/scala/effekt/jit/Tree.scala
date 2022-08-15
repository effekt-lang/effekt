package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, Symbol, ValueSymbol}

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

case class Program(blocks: List[BasicBlock],
                   datatypes: List[List[List[Type]]]
                  ) extends Tree

case class BasicBlock(id: BlockLabel,
                      frameDescriptor: FrameDescriptor,
                      instructions: List[Instruction],
                      terminator: Terminator) extends Tree // TODO: frame descriptor

case class VariableDescriptor(typ: Type, id: Register) extends Tree
case class FrameDescriptor(locals: Map[RegisterType, Int]) extends Tree
case class RegList(regs: Map[RegisterType, List[Register]])
case class Clause(closesOver: RegList, target: BlockLabel)

sealed trait Instruction extends Tree
case class Const(out: Register, value: Int) extends Instruction
case class PrimOp(name: String, out: RegList, in: RegList) extends Instruction
case class Add(out: Register, in1: Register, in2: Register) extends Instruction
case class Mul(out: Register, in1: Register, in2: Register) extends Instruction
case class Push(target: BlockLabel, args: RegList) extends Instruction
case class Shift(out: Register, n: Int) extends Instruction // TODO n from reg
case class Reset() extends Instruction
case class Print(arg: Register) extends Instruction
case class IfZero(arg: Register, thenClause: Clause) extends Instruction
case class IsZero(out: Register, arg: Register) extends Instruction
case class Subst(args: RegList) extends Instruction
case class Construct(out: Register, adt_type: Int, tag: ConstructorTag, args: RegList) extends Instruction
case class PushStack(arg: Register) extends Instruction
case class NewStack(out: Register, target: BlockLabel, args: RegList) extends Instruction

sealed trait Terminator extends Tree
case class Return(args: RegList) extends Terminator
case class Jump(target: BlockLabel) extends Terminator
case class Resume(cont: Register) extends Terminator
case class Match(adt_type: Int, scrutinee: Register, clauses: List[Clause]) extends Terminator

sealed trait Type extends Tree
object Type {
  case class Unit() extends Type
  case class Continuation() extends Type
  case class Integer() extends Type
  case class Datatype(index: Int) extends Type

  extension(self: Type) def registerType: Option[RegisterType] = {
    self match
      case Unit() => None
      case Continuation() => Some(RegisterType.Continuation)
      case Integer() => Some(RegisterType.Integer)
      case Datatype(index) => Some(RegisterType.Datatype)
  }
}
enum RegisterType { case Integer, Continuation, Datatype }

type ConstructorTag = Int

sealed trait Register extends Tree
case class NamedRegister(name: String) extends Register
case class RegisterIndex(index: Int) extends Register
case class ErasedRegister() extends Register

sealed trait BlockLabel
case class BlockName(name: String) extends BlockLabel
case class BlockIndex(index: Int) extends BlockLabel
class FreshBlockLabel() extends BlockLabel
