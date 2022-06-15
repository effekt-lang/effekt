package effekt
package llvm

import effekt.context.Context
import effekt.symbols.{ Symbol, ValueSymbol, BlockSymbol }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * Toplevel definitions
 */

sealed trait Top extends Tree
case class DefCnt(id: BlockSymbol, params: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefFrm(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefClo(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefFun(typ: machine.Type, id: BlockSymbol, params: List[machine.Param], body: String) extends Top
case class DefScn(id: BlockSymbol, env: List[machine.Param]) extends Top
case class Include(content: String) extends Top

case class BasicBlock(id: BlockSymbol, instructions: List[Instruction], terminator: Terminator) extends Tree

// TODO this is neither super- nor subset of LLVM
// TODO `machine.Type` SHOULD be `LLVM.Type`
sealed trait Instruction extends Tree
case class Call(id: ValueSymbol, typ: machine.Type, func: BlockSymbol, args: List[machine.Value]) extends Instruction
case class Phi(param: machine.Param, args: List[(BlockSymbol, machine.Value)]) extends Instruction
case class InsertValues(id: ValueSymbol, typ: machine.Record, args: List[machine.Value]) extends Instruction
case class ExtractValue(id: ValueSymbol, target: machine.Value, field: Int) extends Instruction
case class Inject(id: ValueSymbol, typ: machine.Variant, arg: machine.Value, variant: Int) extends Instruction
case class PushFrame(cntType: List[machine.Type], id: BlockSymbol, args: List[machine.Value]) extends Instruction
case class NewStack(cntType: List[machine.Type], id: BlockSymbol, blockName: BlockSymbol, args: List[machine.Value]) extends Instruction
case class PushStack(stack: machine.Value) extends Instruction
case class PopStack(id: BlockSymbol) extends Instruction
case class CopyStack(id: BlockSymbol, stack: machine.Value) extends Instruction
case class EraseStack(stack: machine.Value) extends Instruction
case class EviPlus(id: ValueSymbol, l: machine.Value, r: machine.Value) extends Instruction
case class EviDecr(id: ValueSymbol, l: machine.Value) extends Instruction
case class EviIsZero(id: ValueSymbol, l: machine.Value) extends Instruction

sealed trait Terminator extends Tree
case class Ret(values: List[machine.Value]) extends Terminator
case class Jump(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM `tail call`
case class JumpLocal(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM unconditional `br`
case class If(cond: machine.Value, thenBlock: BlockSymbol, thenArgs: List[machine.Value], elseBlock: BlockSymbol, elseArgs: List[machine.Value]) extends Terminator
case class Switch(arg: machine.Value, default: BlockSymbol, labels: List[(Int, BlockSymbol)]) extends Terminator
case class Panic() extends Terminator
