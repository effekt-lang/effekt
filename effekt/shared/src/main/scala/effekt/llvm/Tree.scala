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

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-Instruction.html#t:Instruction
 */
sealed trait Instruction
case class TailCall(function: Operand, arguments: List[Operand]) extends Instruction
case class Load(result: String, address: Operand) extends Instruction
case class Store(address: Operand, value: Operand) extends Instruction
case class GetElementPtr(result: String, address: Operand, indices: List[Int]) extends Instruction
case class BitCast(result: String, operand: Operand, typ: Type) extends Instruction
case class Add(result: String, operand0: Operand, operand1: Operand) extends Instruction

sealed trait Terminator
case class RetVoid() extends Terminator

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

