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

// TODO rename `s/TrueLLVMType/LLVMType/` or even `s/TrueLLVMType/Type/` (since we are in `llvm/Tree.scala`)
/*
All these types form compile-time statically known data. These Scala values are
lifted to LLVM type values during compilation.
*/
sealed trait TrueLLVMType
case class Int64() extends TrueLLVMType
case class Pointer(to: TrueLLVMType) extends TrueLLVMType
case class Void() extends TrueLLVMType

/*
A frame is one function's environment together with its static behaviour.
The static `@scanner` function pointer knows about the frame's type and layout
and can therefore both copy and destroy the frame (given its dynamically known
absolute reference position on the heap).
frame:
    @scanner
    primitive_1
    primitive_2
    ...
    primitive_n
    boxed_1
    boxed_2
    ...
    boxed_n
*/
case class EffektFrame(scanner: Pointer, primitives: List[Int64], boxed: List[Pointer]) extends TrueLLVMType

/*
From a function's point of view, its stacks form an array-singly-linked-list-
hybrid data structure (a "metastack" of sorts) which defines the entire
environment known to it.
When no effect arbitration is necessary, a subfunction simply gets its frame
pushed onto the heap relative to the stack's `top`.
If an effect split happens, a new stack is created with evidence-sufficiently
deep parent copying.
A stack is copiable and destructible to varying depths (the necessary depth is
called evidence) only utilizing the various frames' `@scanner` implementations.
*/
case class EffektStack(base: Pointer, top: Pointer, cap: Pointer, parent: Pointer) extends TrueLLVMType
