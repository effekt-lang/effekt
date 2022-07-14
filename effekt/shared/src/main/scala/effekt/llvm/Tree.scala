package effekt
package llvm

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Definition
 */
sealed trait Definition
case class Function(returnType: Type, name: String, parameters: List[Parameter], basicBlocks: List[BasicBlock]) extends Definition
case class VerbatimFunction(returnType: Type, name: String, parameters: List[Parameter], body: String) extends Definition
case class Verbatim(content: String) extends Definition

case class BasicBlock(name: String, instructions: List[Instruction], terminator: Terminator)

// case class DefCnt(id: BlockSymbol, params: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
// case class DefFrm(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
// case class DefClo(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
// case class DefScn(id: BlockSymbol, env: List[machine.Param]) extends Top
// case class Include(content: String) extends Top
// case class BasicBlock(id: BlockSymbol, instructions: List[Instruction], terminator: Terminator) extends Tree

// TODO this is neither super- nor subset of LLVM
// TODO `machine.Type` SHOULD be `LLVM.Type`
// TODO make into a proper subset of LLVM (possibly *dropping* some nuances, not adding anything)
sealed trait Instruction
case class TailCall(function: Operand, arguments: List[Operand]) extends Instruction
case class Load(result: String, address: Operand) extends Instruction
case class Store(address: Operand, value: Operand) extends Instruction
case class GetElementPtr(result: String, address: Operand, indices: List[Int]) extends Instruction
case class BitCast(result: String, operand: Operand, typ: Type) extends Instruction
case class Add(result: String, operand0: Operand, operand1: Operand) extends Instruction

// case class Call(id: ValueSymbol, typ: machine.Type, func: BlockSymbol, args: List[machine.Value]) extends Instruction
// case class Phi(param: machine.Param, args: List[(BlockSymbol, machine.Value)]) extends Instruction
// case class InsertValues(id: ValueSymbol, typ: machine.Record, args: List[machine.Value]) extends Instruction
// case class ExtractValue(id: ValueSymbol, target: machine.Value, field: Int) extends Instruction
// case class Inject(id: ValueSymbol, typ: machine.Variant, arg: machine.Value, variant: Int) extends Instruction
// case class PushFrame(cntType: List[machine.Type], id: BlockSymbol, args: List[machine.Value]) extends Instruction
// case class NewStack(cntType: List[machine.Type], id: BlockSymbol, blockName: BlockSymbol, args: List[machine.Value]) extends Instruction
// case class PushStack(stack: machine.Value) extends Instruction
// case class PopStack(id: BlockSymbol) extends Instruction
// case class CopyStack(id: BlockSymbol, stack: machine.Value) extends Instruction
// case class EraseStack(stack: machine.Value) extends Instruction
// case class EviPlus(id: ValueSymbol, l: machine.Value, r: machine.Value) extends Instruction
// case class EviDecr(id: ValueSymbol, l: machine.Value) extends Instruction
// case class EviIsZero(id: ValueSymbol, l: machine.Value) extends Instruction

sealed trait Terminator
case class RetVoid() extends Terminator

// case class Ret(values: List[machine.Value]) extends Terminator

// `jump` is in reality `tail call ; ret void` (LLVM does **not** know of a global jump instruction)
// case class Jump(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM `tail call`
// case class JumpLocal(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM unconditional `br`
// case class If(cond: machine.Value, thenBlock: BlockSymbol, thenArgs: List[machine.Value], elseBlock: BlockSymbol, elseArgs: List[machine.Value]) extends Terminator
// case class Switch(arg: machine.Value, default: BlockSymbol, labels: List[(Int, BlockSymbol)]) extends Terminator
// case class Panic() extends Terminator

case class Parameter(typ: Type, name: String)

sealed trait Operand
case class LocalReference(typ: Type, name: String) extends Operand
case class ConstantNumber(n: Int) extends Operand
case class ConstantGlobal(typ: Type, name: String) extends Operand

/*
see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Type
*/
sealed trait Type
case class VoidType() extends Type
case class I64() extends Type
case class I8() extends Type
case class StructureType(elementTypes: List[Type]) extends Type
case class FunctionType(resultType: Type, argumentTypes: List[Type]) extends Type
case class PointerType(pointerReferent: Type) extends Type
case class NamedType(name: String) extends Type

// /*
// A frame is one function's environment together with its static behaviour.
//  dule
// The static `@scanner` function pointer knows about the frame's type and layout
// and can therefore both copy and destroy the frame (given its dynamically known
// absolute reference position on the heap).
// frame:
//     @scanner
//     primitive_1
//     primitive_2
//     ...
//     primitive_n
//     boxed_1
//     boxed_2
//     ...
//     boxed_n
// */
// case class EffektFrame(scanner: Pointer, primitives: List[Int64], boxed: List[Pointer]) extends Type

// /*
// From a function's point of view, its stacks form an array-singly-linked-list-
// hybrid data structure (a "metastack" of sorts) which defines the entire
// environment known to it.
// When no effect arbitration is necessary, a subfunction simply gets its frame
// pushed onto the heap relative to the stack's `top`.
// If an effect split happens, a new stack is created with evidence-sufficiently
// deep parent copying.
// A stack is copiable and destructible to varying depths (the necessary depth is
// called evidence) only utilizing the various frames' `@scanner` implementations.
// */
// case class EffektStack(base: Pointer, top: Pointer, cap: Pointer, parent: Pointer) extends Type
