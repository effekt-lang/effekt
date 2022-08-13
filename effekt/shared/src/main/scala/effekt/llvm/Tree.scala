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
case class Call(result: String, resultType: Type, function: Operand, arguments: List[Operand]) extends Instruction
case class TailCall(function: Operand, arguments: List[Operand]) extends Instruction
case class Load(result: String, address: Operand) extends Instruction
case class Store(address: Operand, value: Operand) extends Instruction
case class GetElementPtr(result: String, address: Operand, indices: List[Int]) extends Instruction
case class BitCast(result: String, operand: Operand, typ: Type) extends Instruction
case class Add(result: String, operand0: Operand, operand1: Operand) extends Instruction
case class InsertValue(result: String, aggregate: Operand, element: Operand, index: Int) extends Instruction
case class ExtractValue(result: String, aggregate: Operand, index: Int) extends Instruction

sealed trait Terminator
case class RetVoid() extends Terminator
case class Switch(operand: Operand, defaultDest: String, dests: List[(Int, String)]) extends Terminator
case class CondBr(condition: Operand, trueDest: String, falseDest: String) extends Terminator

case class Parameter(typ: Type, name: String)

sealed trait Operand
case class LocalReference(tpe: Type, name: String) extends Operand
case class ConstantInt(n: Int) extends Operand
case class ConstantAggregateZero(typ: Type) extends Operand
case class ConstantGlobal(tpe: Type, name: String) extends Operand
case class ConstantNull(tpe: Type) extends Operand

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Type
 */
sealed trait Type
case class VoidType() extends Type
case class IntegerType64() extends Type
case class IntegerType8() extends Type
case class IntegerType1() extends Type
case class StructureType(elementTypes: List[Type]) extends Type
case class FunctionType(resultType: Type, argumentTypes: List[Type]) extends Type
case class PointerType(pointerReferent: Type) extends Type
case class NamedType(name: String) extends Type
