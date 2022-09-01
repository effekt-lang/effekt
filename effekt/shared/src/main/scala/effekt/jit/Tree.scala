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

case class Program(blocks: List[BasicBlock]) extends Tree

case class VariableDescriptor(typ: Type, id: Register) extends Tree

case class BasicBlock(id: BlockLabel,
                      frameDescriptor: List[VariableDescriptor],
                      instructions: List[Instruction],
                      terminator: Terminator) extends Tree // TODO: frame descriptor

case class RegList(intRegs: List[Register], contRegs: List[Register], datatypeRegs: List[Register])
case class Clause(args: RegList, target: BlockLabel)

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

sealed trait Terminator extends Tree
case class Return(args: RegList) extends Terminator
case class Jump(target: BlockLabel) extends Terminator
case class Resume(cont: Register) extends Terminator
case class Match(adt_type: Int, scrutinee: Register, clauses: List[Clause]) extends Terminator

// TODO + NewStack, PushStack

enum Type { case Integer, Datatype, Continuation, Unit }
def typeIndex(typ: Type): Int = {
  typ match
    case Type.Integer => 0
    case Type.Datatype => 2
    case Type.Continuation => 1
    case Type.Unit => -1
}


type ConstructorTag = Int

sealed trait Register extends Tree
case class NamedRegister(name: String) extends Register
case class RegisterIndex(index: Int) extends Register
case class ErasedRegister() extends Register

sealed trait BlockLabel extends Tree
case class BlockName(name: String) extends BlockLabel
case class BlockIndex(index: Int) extends BlockLabel
class FreshBlockLabel() extends BlockLabel
